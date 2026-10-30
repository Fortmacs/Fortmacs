! editfns_fm.f90 -- point, narrowing, and reading and editing buffer text.
! Ported from GNU Emacs 31.1 src/editfns.c.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Fortmacs is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

!> Editing primitives on the current buffer.
!>
!> Lisp functions that return nil for "no such position" take a `found'
!> (or similar) logical here.  Text comes back as byte strings in the
!> buffer's representation.
!>
!> Point is set with `temp_set_point_both': upstream's SET_PT also runs
!> text-property hooks (`point_entered' etc.), which arrive with
!> intervals.  Labeled restrictions (`with_restriction') are deferred, as
!> are `format', fields, `transpose_regions' and
!> `compare_buffer_substrings' (they need case tables, intervals or undo).
module editfns_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_ARGS_OUT_OF_RANGE, &
                        FM_WRONG_TYPE_ARGUMENT, FM_GENERIC_ERROR, FM_MARK_INACTIVE
    use handle_fm, only: handle_type
    use character_fm, only: char_head_p, char_valid_p, char_string, &
                            MAX_MULTIBYTE_LENGTH
    use buffer_fm, only: buffers, cur, BEG, BEG_BYTE, BUF_BYTES_MAX, &
                         buf_fetch_byte, buf_fetch_char, buf_prev_char_len, buf_next_char_len, buf_byte_index, &
                         buf_bytes, temp_set_point_both, current_buffer, &
                         resolve_buffer, transient_mark_mode
    use marker_fm, only: char_to_byte, byte_to_char, buf_charpos_to_bytepos, &
                         markers, resolve_marker, make_marker, set_marker_both
    use callint_fm, only: mark_even_if_inactive
    use insdel_fm, only: insert, del_range, del_range_1, modify_text, signal_after_change
    use undo_fm, only: record_delete, record_insert
    use search_fm, only: scan_newline_from_point, find_before_next_newline, find_newline
    use lisp_fm, only: value_type, nilp, nil
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use textprop_fm, only: get_text_property, text_property_stickiness, &
                           next_single_char_property_change, previous_single_char_property_change
    use error_fm, only: format_message
    implicit none(type, external)
    private

    public :: point, point_min, point_max, buffer_size, buffer_size_of, &
              gap_position, gap_size
    public :: goto_char, goto_char_marker, set_point_from_marker
    public :: point_marker, point_min_marker, point_max_marker
    public :: mark_marker, mark_position, region_beginning, region_end, interactive_region, region_position_proc
    public :: position_bytes, byte_to_position
    public :: following_char, preceding_char, bobp, eobp, bolp, eolp, &
              char_after, char_before
    public :: insert_char_n, narrow_to_region, widen, buffer_narrowed_p, current_time_string, validate_region, subst_char_in_region
    public :: buffer_substring, buffer_string, insert_buffer_substring, delete_region, &
              delete_and_extract_region
    public :: pos_bol, pos_eol, line_beginning_position, line_end_position
    public :: get_pos_property, find_field, field_beginning, field_end, field_string, &
              delete_field, constrain_to_field
    public :: format_message
    public :: user_login_name, user_real_login_name, user_full_name, system_name

    !> The names editfns.c:init_editfns computes (Vuser_login_name,
    !> Vuser_real_login_name, Vuser_full_name); unallocated until then.
    !> Vuser_full_name is the variable `user_full_name', renamed as it has
    !> the name of the function `user_full_name'.
    character(len=:), allocatable :: vuser_login_name, vuser_real_login_name, user_full_name_variable

    !> True means text motion commands don't notice fields.
    !> Port of editfns.c inhibit-field-text-motion.
    logical, public :: inhibit_field_text_motion = .false.

    abstract interface
        !> A function of no arguments returning a buffer position.
        !> Not a port.
        recursive subroutine region_position_proc(pos, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(out) :: pos
            type(status_type), intent(out) :: status
        end subroutine region_position_proc
    end interface

    !> What `region_beginning' and `region_end' do: the primitives, or
    !> the advice around them (rect.el's rectangle--region-beginning and
    !> rectangle--region-end, which call the primitives outside
    !> rectangle_mark_mode).  (Ours: upstream's advice-add as a procedure
    !> pointer.)
    procedure(region_position_proc), pointer, public :: region_beginning_function => primitive_region_beginning
    !> The function `region_end' calls, as `region_beginning_function' for
    !> `region_beginning'.
    procedure(region_position_proc), pointer, public :: region_end_function => primitive_region_end

contains

    ! ------------------------------------------------------------------
    ! Positions
    ! ------------------------------------------------------------------

    !> Return value of point, as an integer.
    !> Beginning of buffer is position `point_min()'.  Port of editfns.c:Fpoint.
    integer(int64) function point()
        point = buffers(cur)%pt
    end function point

    !> Return the minimum permissible value of point in the current buffer.
    !> This is 1, unless narrowing (a buffer restriction) is in effect.
    !> Port of editfns.c:Fpoint_min.
    integer(int64) function point_min()
        point_min = buffers(cur)%begv
    end function point_min

    !> Return the maximum permissible value of point in the current buffer.
    !> This is `buffer_size() + 1', unless narrowing (a buffer restriction)
    !> is in effect, in which case it is less.  Port of editfns.c:Fpoint_max.
    integer(int64) function point_max()
        point_max = buffers(cur)%zv
    end function point_max

    !> Characters in the current buffer, ignoring narrowing.
    !> Port of editfns.c:Fbuffer_size.
    integer(int64) function buffer_size()
        buffer_size = buffers(cur)%text%z - BEG
    end function buffer_size

    !> Like `buffer_size', for buffer `h'.  Port of editfns.c:Fbuffer_size.
    subroutine buffer_size_of(h, size, status)
        type(handle_type), intent(in) :: h
        integer(int64), intent(out) :: size
        type(status_type), intent(out) :: status
        integer(int64) :: i

        size = 0
        call resolve_buffer(h, i, status)
        if (.not. status%ok()) return
        size = buffers(i)%text%z - BEG
    end subroutine buffer_size_of

    !> Port of editfns.c:Fgap_position.
    integer(int64) function gap_position()
        gap_position = buffers(cur)%text%gpt
    end function gap_position

    !> Port of editfns.c:Fgap_size.
    integer(int64) function gap_size()
        gap_size = buffers(cur)%text%gap_size
    end function gap_size

    !> Set point to `position', clipped to the accessible region.
    !> Port of editfns.c:Fgoto_char (integer POSITION).
    subroutine goto_char(position)
        integer(int64), intent(in) :: position
        integer(int64) :: pos

        pos = max(buffers(cur)%begv, min(position, buffers(cur)%zv))
        call temp_set_point_both(buffers(cur), pos, char_to_byte(pos))
    end subroutine goto_char

    !> Set point to marker `m'.  Port of editfns.c:Fgoto_char (marker
    !> POSITION), i.e. intervals.c:set_point_from_marker.
    subroutine goto_char_marker(m, status)
        type(handle_type), intent(in) :: m
        type(status_type), intent(out) :: status

        call set_point_from_marker(m, status)
    end subroutine goto_char_marker

    !> Port of intervals.c:set_point_from_marker.  The marker's byte
    !> position is only trusted if it is in the current buffer.
    subroutine set_point_from_marker(m, status)
        type(handle_type), intent(in) :: m
        type(status_type), intent(out) :: status
        integer(int64) :: i, charpos, bytepos

        call resolve_marker(m, i, status)
        if (.not. status%ok()) return

        if (markers(i)%buffer == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Marker does not point anywhere")
            return
        end if

        associate (b => buffers(cur))
            charpos = max(b%begv, min(markers(i)%charpos, b%zv))

            if (markers(i)%buffer /= cur) then
                bytepos = buf_charpos_to_bytepos(cur, charpos)
            else
                bytepos = max(b%begv_byte, min(markers(i)%bytepos, b%zv_byte))
            end if

            call temp_set_point_both(b, charpos, bytepos)
        end associate
    end subroutine set_point_from_marker

    !> A new marker at point; the caller frees it.
    !> Port of editfns.c:Fpoint_marker.
    subroutine point_marker(m)
        type(handle_type), intent(out) :: m

        call new_marker_at(buffers(cur)%pt, buffers(cur)%pt_byte, m)
    end subroutine point_marker

    !> Return this buffer's mark, as a marker object.
    !> Watch out!  Moving this marker changes the mark position.
    !> If you set the marker not to point anywhere, the buffer will have no
    !> mark.  Port of editfns.c:Fmark_marker (made on first use here).
    subroutine mark_marker(m)
        type(handle_type), intent(out) :: m
        integer(int64) :: i
        type(status_type) :: st

        call resolve_marker(buffers(cur)%mark, i, st)
        if (.not. st%ok()) call make_marker(buffers(cur)%mark)
        m = buffers(cur)%mark
    end subroutine mark_marker

    !> The position of the current buffer's mark, or 0 if it has none (nil
    !> upstream): `(marker-position (mark-marker))'.  Not a port.
    integer(int64) function mark_position() result(pos)
        integer(int64) :: i
        type(status_type) :: st

        pos = 0
        call resolve_marker(buffers(cur)%mark, i, st)
        if (.not. st%ok()) return
        if (markers(i)%buffer /= 0) pos = markers(i)%charpos
    end function mark_position

    !> Point or the mark, the smaller (`beginningp') or the larger,
    !> clipped to the accessible part.  Port of editfns.c:region_limit.
    subroutine region_limit(beginningp, pos, status)
        logical, intent(in) :: beginningp
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status
        integer(int64) :: m

        pos = 0

        if (transient_mark_mode%value .and. .not. mark_even_if_inactive .and. .not. buffers(cur)%mark_active) then
            call set_error(status, FM_MARK_INACTIVE)
            return
        end if

        m = mark_position()

        if (m == 0) then
            call set_error(status, FM_GENERIC_ERROR, "The mark is not set now, so there is no region")
            return
        end if

        if ((buffers(cur)%pt < m) .eqv. beginningp) then
            pos = buffers(cur)%pt
        else
            pos = min(max(m, buffers(cur)%begv), buffers(cur)%zv)
        end if
    end subroutine region_limit

    !> Return the integer value of point or mark, whichever is smaller.
    !> Port of editfns.c:Fregion_beginning (through
    !> `region_beginning_function', which rectangle_mark_mode advises).
    recursive subroutine region_beginning(pos, status)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status

        call region_beginning_function(pos, status)
    end subroutine region_beginning

    !> Return the integer value of point or mark, whichever is larger.
    !> Port of editfns.c:Fregion_end (through `region_end_function').
    recursive subroutine region_end(pos, status)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status

        call region_end_function(pos, status)
    end subroutine region_end

    !> Port of editfns.c:Fregion_beginning.
    recursive subroutine primitive_region_beginning(pos, status)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status

        call region_limit(.true., pos, status)
    end subroutine primitive_region_beginning

    !> Port of editfns.c:Fregion_end.
    recursive subroutine primitive_region_end(pos, status)
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status

        call region_limit(.false., pos, status)
    end subroutine primitive_region_end

    !> The region as the arguments of a command whose interactive spec
    !> has `r': point and the mark, the smaller first.  An inactive mark
    !> is an error as for `region_beginning', which rectangle_mark_mode's
    !> advice does not change here.  Port of the `r' code of
    !> callint.c:Fcall_interactively.
    subroutine interactive_region(beg, end, status)
        integer(int64), intent(out) :: beg, end
        type(status_type), intent(out) :: status

        end = 0
        call region_limit(.true., beg, status)
        if (status%ok()) call region_limit(.false., end, status)
    end subroutine interactive_region

    !> Port of editfns.c:Fpoint_min_marker.
    subroutine point_min_marker(m)
        type(handle_type), intent(out) :: m

        call new_marker_at(buffers(cur)%begv, buffers(cur)%begv_byte, m)
    end subroutine point_min_marker

    !> Port of editfns.c:Fpoint_max_marker.
    subroutine point_max_marker(m)
        type(handle_type), intent(out) :: m

        call new_marker_at(buffers(cur)%zv, buffers(cur)%zv_byte, m)
    end subroutine point_max_marker

    !> Port of alloc.c:build_marker (current buffer).
    subroutine new_marker_at(charpos, bytepos, m)
        integer(int64), intent(in) :: charpos, bytepos
        type(handle_type), intent(out) :: m
        type(status_type) :: st

        call make_marker(m)
        call set_marker_both(m, current_buffer(), charpos, bytepos, st)
    end subroutine new_marker_at

    !> Byte position of character `position'; `found' is false when it is
    !> outside the buffer.  Port of editfns.c:Fposition_bytes.
    subroutine position_bytes(position, bytepos, found)
        integer(int64), intent(in) :: position
        integer(int64), intent(out) :: bytepos
        logical, intent(out) :: found

        bytepos = 0
        found = BEG <= position .and. position <= buffers(cur)%text%z
        if (found) bytepos = char_to_byte(position)
    end subroutine position_bytes

    !> Character position containing byte `bytepos'; `found' is false when
    !> it is outside the buffer.  Port of editfns.c:Fbyte_to_position.
    subroutine byte_to_position(bytepos, position, found)
        integer(int64), intent(in) :: bytepos
        integer(int64), intent(out) :: position
        logical, intent(out) :: found
        integer(int64) :: pos_byte

        position = 0
        found = BEG_BYTE <= bytepos .and. bytepos <= buffers(cur)%text%z_byte
        if (.not. found) return
        pos_byte = bytepos

        if (buffers(cur)%text%z /= buffers(cur)%text%z_byte) then
            ! Back up to the start of the character containing BYTEPOS.
            do while (pos_byte < buffers(cur)%text%z_byte)
                if (char_head_p(buf_fetch_byte(buffers(cur), pos_byte))) exit
                pos_byte = pos_byte - 1
            end do
        end if

        position = byte_to_char(pos_byte)
    end subroutine byte_to_position

    ! ------------------------------------------------------------------
    ! Characters around point
    ! ------------------------------------------------------------------

    !> Character after point, or 0 at the end of the accessible region.
    !> Port of editfns.c:Ffollowing_char.
    integer(int32) function following_char()
        following_char = 0

        if (buffers(cur)%pt < buffers(cur)%zv) then
            following_char = buf_fetch_char(buffers(cur), buffers(cur)%pt_byte)
        end if
    end function following_char

    !> Character before point, or 0 at the start of the accessible region.
    !> Port of editfns.c:Fprevious_char.
    integer(int32) function preceding_char()
        integer(int64) :: pos

        preceding_char = 0

        associate (b => buffers(cur))
            if (b%pt <= b%begv) return

            if (b%multibyte) then
                pos = b%pt_byte - buf_prev_char_len(b, b%pt_byte)
                preceding_char = buf_fetch_char(b, pos)
            else
                preceding_char = buf_fetch_byte(b, b%pt_byte - 1)
            end if
        end associate
    end function preceding_char

    !> Return true if point is at the beginning of the buffer.
    !> If the buffer is narrowed, this means the beginning of the narrowed part.
    !> Port of editfns.c:Fbobp.
    logical function bobp()
        bobp = buffers(cur)%pt == buffers(cur)%begv
    end function bobp

    !> Return true if point is at the end of the buffer.
    !> If the buffer is narrowed, this means the end of the narrowed part.
    !> Port of editfns.c:Feobp.
    logical function eobp()
        eobp = buffers(cur)%pt == buffers(cur)%zv
    end function eobp

    !> Return true if point is at the beginning of a line.  Port of
    !> editfns.c:Fbolp.
    logical function bolp()
        associate (b => buffers(cur))
            bolp = b%pt == b%begv
            if (.not. bolp) bolp = buf_fetch_byte(b, b%pt_byte - 1) == 10
        end associate
    end function bolp

    !> Return true if point is at the end of a line.
    !> `End of a line' includes point being at the end of the buffer.  Port of
    !> editfns.c:Feolp.
    logical function eolp()
        associate (b => buffers(cur))
            eolp = b%pt == b%zv
            if (.not. eolp) eolp = buf_fetch_byte(b, b%pt_byte) == 10
        end associate
    end function eolp

    !> Character at `pos' (default: point); `found' is false outside the
    !> accessible region.  Port of editfns.c:Fchar_after (integer POS).
    subroutine char_after(c, found, pos)
        integer(int32), intent(out) :: c
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: pos
        integer(int64) :: pos_byte

        c = 0

        associate (b => buffers(cur))
            if (.not. present(pos)) then
                pos_byte = b%pt_byte
                found = b%begv_byte <= pos_byte .and. pos_byte < b%zv_byte
            else
                found = b%begv <= pos .and. pos < b%zv
                if (found) pos_byte = char_to_byte(pos)
            end if

            if (found) c = buf_fetch_char(b, pos_byte)
        end associate
    end subroutine char_after

    !> Character before `pos' (default: point); `found' is false outside
    !> the accessible region.  Port of editfns.c:Fchar_before (integer POS).
    subroutine char_before(c, found, pos)
        integer(int32), intent(out) :: c
        logical, intent(out) :: found
        integer(int64), intent(in), optional :: pos
        integer(int64) :: p, pos_byte

        c = 0

        associate (b => buffers(cur))
            p = b%pt
            if (present(pos)) p = pos
            found = b%begv < p .and. p <= b%zv
            if (.not. found) return
            pos_byte = char_to_byte(p)

            if (b%multibyte) then
                pos_byte = pos_byte - buf_prev_char_len(b, pos_byte)
                c = buf_fetch_char(b, pos_byte)
            else
                c = buf_fetch_byte(b, pos_byte - 1)
            end if
        end associate
    end subroutine char_before

    ! ------------------------------------------------------------------
    ! Inserting, narrowing, extracting, deleting
    ! ------------------------------------------------------------------

    !> Insert `count' copies of character `c' (nothing if count <= 0).
    !> Port of editfns.c:Finsert_char (without INHERIT).
    recursive subroutine insert_char_n(c, count, status)
        integer(int32), intent(in) :: c
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        character(len=MAX_MULTIBYTE_LENGTH) :: str
        character(len=:), allocatable :: block
        integer(int32) :: clen
        integer(int64) :: n, chunk, reps

        if (.not. char_valid_p(c)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "characterp")
            return
        end if

        if (buffers(cur)%multibyte) then
            call char_string(c, str, clen, status)
            if (.not. status%ok()) return
        else
            str(1:1) = char(iand(c, 255_int32))
            clen = 1
        end if

        if (count <= 0) return

        if (BUF_BYTES_MAX/clen < count) then
            call set_error(status, FM_GENERIC_ERROR, "Maximum buffer size exceeded")
            return
        end if

        ! Insert in chunks of at most ~4000 bytes, as upstream.
        reps = min(count, 4000_int64/clen)
        block = repeat(str(1:clen), reps)
        n = count

        do while (n > 0)
            chunk = min(n, reps)
            call insert(block(1:chunk*clen), status)
            if (.not. status%ok()) return
            n = n - chunk
        end do
    end subroutine insert_char_n

    !> Restrict editing to `start'..`end' (in either order); point moves
    !> into the region.  Port of editfns.c:Fnarrow_to_region.
    subroutine narrow_to_region(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: s, e

        s = min(start, end)
        e = max(start, end)

        associate (b => buffers(cur))
            if (.not. (BEG <= s .and. e <= b%text%z)) then
                call range_error(status, start, end)
                return
            end if

            b%begv = s
            b%begv_byte = char_to_byte(s)
            b%zv = e
            b%zv_byte = char_to_byte(e)
            if (b%pt < s) call temp_set_point_both(b, s, b%begv_byte)
            if (e < b%pt) call temp_set_point_both(b, e, b%zv_byte)
        end associate
    end subroutine narrow_to_region

    !> The current local time as `current_time_string' gives it:
    !> "Sun Sep 16 01:03:52 1973".  Port of editfns.c:Fcurrent_time_string.
    function current_time_string() result(s)
        character(len=:), allocatable :: s
        character(len=3), parameter :: days(0:6) = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
        character(len=3), parameter :: months(12) = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", &
                                                     "Sep", "Oct", "Nov", "Dec"]
        integer :: v(8), y, m, d, w
        character(len=32) :: buf

        call date_and_time(values=v)
        y = v(1)
        m = v(2)
        d = v(3)

        ! Zeller's congruence (Sunday = 0)
        if (m < 3) then
            w = mod(d + (13*(m + 13))/5 + (y - 1) + (y - 1)/4 - (y - 1)/100 + (y - 1)/400 + 6, 7)
        else
            w = mod(d + (13*(m + 1))/5 + y + y/4 - y/100 + y/400 + 6, 7)
        end if

        write (buf, "(a3,1x,a3,1x,i2,1x,i2.2,':',i2.2,':',i2.2,1x,i4)") days(w), months(m), d, v(5), v(6), v(7), y
        s = trim(buf)
    end function current_time_string

    ! ------------------------------------------------------------------
    ! User and system names
    ! ------------------------------------------------------------------

    !> Set up the user's names once, as editfns.c:init_editfns does: the
    !> real login name from the passwd entry of the real uid, the login
    !> name from LOGNAME or USER (else the effective uid's entry), and the
    !> full name of that user (or NAME, or "unknown").  Port of
    !> editfns.c:init_editfns (the user names).
    subroutine init_user_names()
        use sysdep_fm, only: passwd_entry_by_uid, emacs_getuid, emacs_geteuid
        character(len=:), allocatable :: name, gecos, user_name
        integer :: length, stat
        logical :: found

        if (allocated(vuser_login_name)) return
        call passwd_entry_by_uid(emacs_getuid(), name, gecos, found)

        if (found) then
            vuser_real_login_name = name
        else
            vuser_real_login_name = "unknown"
        end if

        ! Get the effective user name, by consulting environment variables,
        ! or the effective uid if those are unset.
        user_name = env("LOGNAME")
        if (.not. allocated(user_name)) user_name = env("USER")

        if (.not. allocated(user_name)) then
            call passwd_entry_by_uid(emacs_geteuid(), name, gecos, found)

            if (found) then
                user_name = name
            else
                user_name = "unknown"
            end if
        end if

        vuser_login_name = user_name

        ! If the user name claimed in the environment vars differs from
        ! the real uid, use the claimed name to find the full name.
        if (vuser_login_name == vuser_real_login_name) then
            call full_name_of(vuser_login_name, user_full_name_variable, found)
        else
            call full_name_of_uid(emacs_geteuid(), user_full_name_variable, found)
        end if

        call get_environment_variable("NAME", length=length, status=stat)

        if (stat == 0) then
            user_full_name_variable = env("NAME")
            if (.not. allocated(user_full_name_variable)) user_full_name_variable = ""
        else if (.not. found) then
            user_full_name_variable = "unknown"
        end if

    contains

        !> The value of the environment variable VAR, unallocated if unset.
        function env(var) result(value)
            character(len=*), intent(in) :: var
            character(len=:), allocatable :: value
            integer :: n, st

            call get_environment_variable(var, length=n, status=st)
            if (st /= 0) return
            allocate (character(len=n) :: value)
            if (n > 0) call get_environment_variable(var, value)
        end function env
    end subroutine init_user_names

    !> The full name in the passwd entry of the user called LOGIN.
    !> Not a port.
    subroutine full_name_of(login, full, found)
        use sysdep_fm, only: passwd_entry_by_name
        character(len=*), intent(in) :: login
        character(len=:), allocatable, intent(out) :: full
        logical, intent(out) :: found
        character(len=:), allocatable :: name, gecos
        integer :: uid

        call passwd_entry_by_name(login, name, gecos, uid, found)
        full = ""
        if (found) full = gecos_full_name(gecos, name)
    end subroutine full_name_of

    !> The full name in the passwd entry of user UID.  Not a port.
    subroutine full_name_of_uid(uid, full, found)
        use sysdep_fm, only: passwd_entry_by_uid
        integer, intent(in) :: uid
        character(len=:), allocatable, intent(out) :: full
        logical, intent(out) :: found
        character(len=:), allocatable :: name, gecos

        call passwd_entry_by_uid(uid, name, gecos, found)
        full = ""
        if (found) full = gecos_full_name(gecos, name)
    end subroutine full_name_of_uid

    !> The full name in a gecos field: everything before the first comma,
    !> with `&' replaced by the login name LOGIN, its first character
    !> upcased (AMPERSAND_FULL_NAME).
    !> Port of part of editfns.c:Fuser_full_name.
    function gecos_full_name(gecos, login) result(full)
        character(len=*), intent(in) :: gecos, login
        character(len=:), allocatable :: full, sub
        integer :: q

        ! Chop off everything after the first comma, since `pw_gecos' is a
        ! comma-separated list.
        q = index(gecos, ",")

        if (q > 0) then
            full = gecos(1:q - 1)
        else
            full = gecos
        end if

        ! Substitute the login name for the &, upcasing the first character.
        q = index(full, "&")

        if (q > 0 .and. len(login) > 0) then
            sub = login
            if (sub(1:1) >= "a" .and. sub(1:1) <= "z") sub(1:1) = achar(iachar(sub(1:1)) - 32)
            full = full(1:q - 1)//sub//full(q + 1:)
        end if
    end function gecos_full_name

    !> Return the name under which the user logged in, as a string.
    !> This is based on the effective uid, not the real uid.
    !> Also, if the environment variables LOGNAME or USER are set,
    !> that determines the value of this function.  Port of
    !> editfns.c:Fuser_login_name (no UID argument).
    function user_login_name() result(name)
        character(len=:), allocatable :: name

        call init_user_names()
        name = vuser_login_name
    end function user_login_name

    !> Return the name of the user's real uid, as a string.
    !> This ignores the environment variables LOGNAME and USER, so it differs from
    !> `user_login_name' when running under `su'.  Port of
    !> editfns.c:Fuser_real_login_name.
    function user_real_login_name() result(name)
        character(len=:), allocatable :: name

        call init_user_names()
        name = vuser_real_login_name
    end function user_real_login_name

    !> Return the full name of the user logged in, as a string.
    !> If the full name corresponding to Emacs's userid is not known,
    !> return "unknown".
    !>
    !> If the full name includes commas, remove everything starting with
    !> the first comma, because the `gecos' field of the `/etc/passwd' file
    !> is in general a comma-separated list.  Port of
    !> editfns.c:Fuser_full_name (no UID argument).
    function user_full_name() result(name)
        character(len=:), allocatable :: name

        call init_user_names()
        name = user_full_name_variable
    end function user_full_name

    !> Return the host name of the machine you are running on, as a string.
    !> Port of editfns.c:Fsystem_name (sysdep.c:init_system_name: spaces
    !> and tabs become dashes).
    function system_name() result(name)
        use sysdep_fm, only: emacs_gethostname
        character(len=:), allocatable :: name
        integer :: k

        name = emacs_gethostname()

        do k = 1, len(name)
            if (name(k:k) == " " .or. name(k:k) == achar(9)) name(k:k) = "-"
        end do
    end function system_name

    !> Return true if the current buffer is narrowed.  Port of
    !> subr.el:buffer-narrowed-p.
    logical function buffer_narrowed_p()
        buffer_narrowed_p = buffers(cur)%zv - buffers(cur)%begv /= buffers(cur)%text%z - BEG
    end function buffer_narrowed_p

    !> Remove narrowing.  Port of editfns.c:Fwiden.
    subroutine widen()
        associate (b => buffers(cur))
            b%begv = BEG
            b%begv_byte = BEG_BYTE
            b%zv = b%text%z
            b%zv_byte = b%text%z_byte
        end associate
    end subroutine widen

    !> Order `start', `end' and check they lie in the accessible region.
    !> Port of buffer.c:validate_region.
    subroutine validate_region(start, end, status)
        integer(int64), intent(inout) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: s, e

        s = min(start, end)
        e = max(start, end)

        if (.not. (buffers(cur)%begv <= s .and. e <= buffers(cur)%zv)) then
            call range_error(status, start, end)
            return
        end if

        start = s
        end = e
    end subroutine validate_region

    ! An args-out-of-range error for A and B (args_out_of_range).
    ! Not a port.
    subroutine range_error(status, a, b)
        type(status_type), intent(inout) :: status
        integer(int64), intent(in) :: a, b
        character(len=24) :: sa, sb

        write (sa, "(i0)") a
        write (sb, "(i0)") b
        call set_error(status, FM_ARGS_OUT_OF_RANGE, trim(sa)//", "//trim(sb))
    end subroutine range_error

    !> Text between `start' and `end' (either order).
    !> Port of editfns.c:Fbuffer_substring (text properties pending).
    subroutine buffer_substring(start, end, text, status)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        integer(int64) :: s, e

        s = start
        e = end
        call validate_region(s, e, status)
        if (.not. status%ok()) return
        text = buf_bytes(buffers(cur), char_to_byte(s), char_to_byte(e))
    end subroutine buffer_substring

    !> The accessible text.  Port of editfns.c:Fbuffer_string.
    function buffer_string() result(text)
        character(len=:), allocatable :: text

        text = buf_bytes(buffers(cur), buffers(cur)%begv_byte, buffers(cur)%zv_byte)
    end function buffer_string

    !> Insert before point a substring of the contents of BUFFER.
    !> BUFFER may be a buffer or a buffer name.
    !> Arguments START and END are character positions specifying the substring.
    !> They default to the values of (point-min) and (point-max) in BUFFER.
    !>
    !> Point and before-insertion markers move forward to end up after the
    !> inserted text.
    !> Any other markers at the point of insertion remain before the text.
    !>
    !> If the current buffer is multibyte and BUFFER is unibyte, or vice
    !> versa, strings are converted from unibyte to multibyte or vice versa
    !> using `string_make_multibyte' or `string_make_unibyte', which see.
    !> Port of editfns.c:Finsert_buffer_substring (text properties pending,
    !> as for `buffer_substring'; both buffers multibyte).
    recursive subroutine insert_buffer_substring(buffer, status, start, end)
        use buffer_fm, only: set_buffer, buffer_live_p
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: b, e

        status = status_type()

        if (.not. buffer_live_p(buffer)) then
            call set_error(status, FM_GENERIC_ERROR, "Selecting deleted buffer")
            return
        end if

        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        b = point_min()
        e = point_max()
        if (present(start)) b = start
        if (present(end)) e = end
        call buffer_substring(b, e, text, status)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        call insert(text, status)
    end subroutine insert_buffer_substring

    !> Delete the text between `start' and `end' (either order).
    !> Port of editfns.c:Fdelete_region.
    recursive subroutine delete_region(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        integer(int64) :: s, e

        s = start
        e = end
        call validate_region(s, e, status)
        if (.not. status%ok()) return
        call del_range(s, e, status)
    end subroutine delete_region

    !> Delete the text between `start' and `end' and return it.
    !> Port of editfns.c:Fdelete_and_extract_region.
    recursive subroutine delete_and_extract_region(start, end, text, status)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        integer(int64) :: s, e

        text = ""
        s = start
        e = end
        call validate_region(s, e, status)
        if (.not. status%ok()) return
        if (s == e) return
        call del_range_1(s, e, .true., status, text)
    end subroutine delete_and_extract_region

    ! ------------------------------------------------------------------
    ! Line positions
    ! ------------------------------------------------------------------

    !> Start of the current line, after moving `n' - 1 lines (default 1);
    !> `count' receives the number of lines scanned (n - 1).
    !> Port of editfns.c:bol.
    integer(int64) function bol(n, count)
        integer(int64), intent(in), optional :: n
        integer(int64), intent(out), optional :: count
        integer(int64) :: cnt, charpos, bytepos, counted

        cnt = 0
        if (present(n)) cnt = max(-BUF_BYTES_MAX, min(n - 1, BUF_BYTES_MAX))
        if (present(count)) count = cnt
        counted = scan_newline_from_point(cnt, charpos, bytepos)
        bol = charpos
    end function bol

    !> Position of the first character on the current line (after moving
    !> `n' - 1 lines first).  Does not move point.  Port of editfns.c:Fpos_bol.
    integer(int64) function pos_bol(n)
        integer(int64), intent(in), optional :: n

        pos_bol = bol(n)
    end function pos_bol

    !> Like `pos_bol', but constrained to the field point is in (so it
    !> stops after a minibuffer prompt).
    !> Port of editfns.c:Fline_beginning_position.
    integer(int64) function line_beginning_position(n)
        integer(int64), intent(in), optional :: n
        integer(int64) :: count, charpos

        charpos = bol(n, count)
        line_beginning_position = constrain_to_field(charpos, buffers(cur)%pt, count /= 0, .true.)
    end function line_beginning_position

    !> Port of editfns.c:eol.
    integer(int64) function eol(n)
        integer(int64), intent(in), optional :: n
        integer(int64) :: cnt

        cnt = 1
        if (present(n)) cnt = max(-BUF_BYTES_MAX, min(n, BUF_BYTES_MAX))
        if (cnt <= 0) cnt = cnt - 1
        eol = find_before_next_newline(buffers(cur)%pt, cnt)
    end function eol

    !> Position of the last character on the current line (after moving
    !> `n' - 1 lines first).  Does not move point.  Port of editfns.c:Fpos_eol.
    integer(int64) function pos_eol(n)
        integer(int64), intent(in), optional :: n

        pos_eol = eol(n)
    end function pos_eol

    !> Like `pos_eol', but constrained to the field point is in.
    !> Port of editfns.c:Fline_end_position.
    integer(int64) function line_end_position(n)
        integer(int64), intent(in), optional :: n

        line_end_position = constrain_to_field(eol(n), buffers(cur)%pt, .false., .true.)
    end function line_end_position

    ! ------------------------------------------------------------------
    ! Fields: runs of text with the same `field` property (text
    ! properties only, as there are no overlays yet).
    ! ------------------------------------------------------------------

    !> The `field' symbol.  Not a port.
    function qfield() result(v)
        type(value_type) :: v

        v = sym("field")
    end function qfield

    !> `prop' of the current buffer's character at `pos' (nil outside the
    !> accessible text); get_char_property without overlays.  Not a port.
    function char_property(pos, prop) result(v)
        integer(int64), intent(in) :: pos
        type(value_type), intent(in) :: prop
        type(value_type) :: v
        type(status_type) :: status

        call get_text_property(pos, prop, v, status)
    end function char_property

    !> The value of `prop' that a character inserted at `position' would
    !> get from its neighbours.  Port of editfns.c:Fget_pos_property (text
    !> properties only).
    function get_pos_property(position, prop) result(v)
        integer(int64), intent(in) :: position
        type(value_type), intent(in) :: prop
        type(value_type) :: v
        integer :: stickiness

        stickiness = text_property_stickiness(prop, position)

        if (stickiness > 0) then
            v = char_property(position, prop)
        else if (stickiness < 0 .and. position > buffers(cur)%begv) then
            v = char_property(position - 1, prop)
        else
            v = nil()
        end if
    end function get_pos_property

    !> The field around `pos' (default point): `beg' and `end', limited
    !> by `beg_limit' and `end_limit'.  With `merge_at_boundary', a
    !> position at the start of a field belongs to the previous one, and
    !> `boundary' fields are skipped.  Port of editfns.c:find_field.
    subroutine find_field(pos, merge_at_boundary, beg, end, beg_limit, end_limit)
        integer(int64), intent(in) :: pos
        logical, intent(in) :: merge_at_boundary
        integer(int64), intent(out) :: beg, end
        integer(int64), intent(in), optional :: beg_limit, end_limit
        type(value_type) :: before_field, after_field, field, boundary
        integer(int64) :: p
        logical :: at_field_start, at_field_end

        at_field_start = .false.
        at_field_end = .false.
        boundary = sym("boundary")
        after_field = char_property(pos, qfield())

        if (pos > buffers(cur)%begv) then
            before_field = char_property(pos - 1, qfield())
        else
            ! Not nil: a buffer may start with a non-sticky field.
            before_field = after_field
        end if

        if (.not. merge_at_boundary) then
            field = get_pos_property(pos, qfield())
            if (.not. value_eq(field, after_field)) at_field_end = .true.
            if (.not. value_eq(field, before_field)) at_field_start = .true.

            ! A nil field between non-nil ones is a field not meant for
            ! editing (like a prompt), not a zero-length field.
            if (nilp(field) .and. at_field_start .and. at_field_end) then
                at_field_end = .false.
                at_field_start = .false.
            end if
        end if

        if (at_field_start) then
            beg = pos
        else
            p = pos

            if (merge_at_boundary .and. value_eq(before_field, boundary)) then
                p = previous_single_char_property_change(p, qfield(), beg_limit)
            end if

            beg = previous_single_char_property_change(p, qfield(), beg_limit)
        end if

        if (at_field_end) then
            end = pos
        else
            p = pos

            if (merge_at_boundary .and. value_eq(after_field, boundary)) then
                p = next_single_char_property_change(p, qfield(), end_limit)
            end if

            end = next_single_char_property_change(p, qfield(), end_limit)
        end if
    end subroutine find_field

    !> Start of the field around `pos'.  Port of editfns.c:Ffield_beginning.
    integer(int64) function field_beginning(pos, escape_from_edge, limit)
        integer(int64), intent(in) :: pos
        logical, intent(in) :: escape_from_edge
        integer(int64), intent(in), optional :: limit
        integer(int64) :: end

        call find_field(pos, escape_from_edge, field_beginning, end, beg_limit=limit)
    end function field_beginning

    !> End of the field around `pos'.  Port of editfns.c:Ffield_end.
    integer(int64) function field_end(pos, escape_from_edge, limit)
        integer(int64), intent(in) :: pos
        logical, intent(in) :: escape_from_edge
        integer(int64), intent(in), optional :: limit
        integer(int64) :: beg

        call find_field(pos, escape_from_edge, beg, field_end, end_limit=limit)
    end function field_end

    !> Text of the field around `pos'.  Port of
    !> editfns.c:Ffield_string_no_properties.
    function field_string(pos) result(text)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable :: text
        integer(int64) :: beg, end
        type(status_type) :: status

        call find_field(pos, .false., beg, end)
        call buffer_substring(beg, end, text, status)
    end function field_string

    !> Delete the field around `pos'.  Port of editfns.c:Fdelete_field.
    recursive subroutine delete_field(pos, status)
        integer(int64), intent(in) :: pos
        type(status_type), intent(out) :: status
        integer(int64) :: beg, end

        call find_field(pos, .false., beg, end)
        if (beg /= end) call del_range(beg, end, status)
    end subroutine delete_field

    !> `new_pos' moved, if needed, to stay in the field of `old_pos'.  With
    !> `escape_from_edge', `old_pos' at a field edge may count as in the
    !> neighbouring field; with `only_in_line', constrain only if the
    !> field edge is on the same line.  Port of
    !> editfns.c:Fconstrain_to_field (NEW-POS given; no
    !> INHIBIT-CAPTURE-PROPERTY).
    integer(int64) function constrain_to_field(new_pos, old_pos, escape_from_edge, only_in_line) &
        result(pos)
        integer(int64), intent(in) :: new_pos, old_pos
        logical, intent(in) :: escape_from_edge, only_in_line
        integer(int64) :: field_bound, counted, nl
        logical :: fwd

        pos = new_pos
        if (inhibit_field_text_motion .or. new_pos == old_pos) return

        if (nilp(char_property(new_pos, qfield())) .and. nilp(char_property(old_pos, qfield()))) then
            ! hazard-ok(short-circuit): char_property() takes any position.
            if (.not. (new_pos > buffers(cur)%begv .and. &
                       .not. nilp(char_property(new_pos - 1, qfield())))) then
                ! hazard-ok(short-circuit): char_property() takes any position.
                if (.not. (old_pos > buffers(cur)%begv .and. &
                           .not. nilp(char_property(old_pos - 1, qfield())))) return
            end if
        end if

        ! NEW_POS may not be in OLD_POS's field: try to move it there.
        fwd = new_pos > old_pos

        if (fwd) then
            field_bound = field_end(old_pos, escape_from_edge, new_pos)
        else
            field_bound = field_beginning(old_pos, escape_from_edge, new_pos)
        end if

        ! Unless escaping put the bound beyond NEW_POS already, constrain
        ! it (with ONLY_IN_LINE, only to a bound on the same line).
        if (merge(fwd,.not. fwd, field_bound < new_pos)) then
            if (only_in_line) then
                if (fwd) then
                    nl = find_newline(new_pos, field_bound, -1_int64, counted)
                else
                    nl = find_newline(new_pos, field_bound, 1_int64, counted)
                end if

                if (counted /= 0) return
            end if

            pos = field_bound
        end if
    end function constrain_to_field

    !> From START to END, replace FROMCHAR with TOCHAR each time it occurs.
    !> If optional arg NOUNDO is true, don't record this change for undo
    !> and don't mark the buffer as really changed.
    !> Both characters must have the same length of multi-byte form.  Port of
    !> editfns.c:Fsubst_char_in_region (compositions are not ported).
    recursive subroutine subst_char_in_region(start, end, fromchar, tochar, status, noundo)
        integer(int64), intent(in) :: start, end
        integer(int32), intent(in) :: fromchar, tochar
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noundo
        integer(int64) :: pos, pos_byte, end_byte, pos_byte_next, s, e, k, m, changed, last_changed
        integer(int32) :: nlen, tolen
        character(len=MAX_MULTIBYTE_LENGTH) :: fromstr, tostr
        character(len=:), allocatable :: saved_filename
        logical :: no_undo, saved_undo, had_filename, rescan

        status = status_type()
        no_undo = .false.
        if (present(noundo)) no_undo = noundo
        s = start
        e = end
        call validate_region(s, e, status)
        if (.not. status%ok()) return

        if (.not. char_valid_p(fromchar)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "characterp, "//int_text(int(fromchar, int64)))
            return
        end if

        if (.not. char_valid_p(tochar)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "characterp, "//int_text(int(tochar, int64)))
            return
        end if

        if (buffers(cur)%multibyte) then
            call char_string(fromchar, fromstr, nlen, status)
            if (.not. status%ok()) return
            call char_string(tochar, tostr, tolen, status)
            if (.not. status%ok()) return

            if (tolen /= nlen) then
                call set_error(status, FM_GENERIC_ERROR, &
                               format_message("Characters in `subst_char_in_region' have different byte-lengths"))
                return
            end if
        else
            nlen = 1
            fromstr(1:1) = achar(iand(fromchar, 255_int32))
            tostr(1:1) = achar(iand(tochar, 255_int32))
        end if

        ! Keep track of the first change in the buffer: 0 if we haven't
        ! found it yet, < 0 if we've found it and run the before-change
        ! functions, > 0 if we've actually performed it (its position).
        changed = 0
        last_changed = 0
        ! If we don't want undo, turn off putting stuff on the list.
        ! Also inhibit locking the file.
        saved_undo = buffers(cur)%undo_enabled
        had_filename = allocated(buffers(cur)%filename)

        if (no_undo) then
            buffers(cur)%undo_enabled = .false.
            if (had_filename) call move_alloc(buffers(cur)%filename, saved_filename)
        end if

        do
            rescan = .false.
            pos = s
            pos_byte = char_to_byte(pos)
            end_byte = char_to_byte(e)

            do while (pos_byte < end_byte)
                pos_byte_next = pos_byte

                if (buffers(cur)%multibyte) then
                    pos_byte_next = pos_byte_next + buf_next_char_len(buffers(cur), pos_byte_next)
                else
                    pos_byte_next = pos_byte_next + 1
                end if

                if (pos_byte_next - pos_byte == nlen) then
                    if (buf_bytes(buffers(cur), pos_byte, pos_byte_next) == fromstr(1:nlen)) then
                        if (changed < 0) then
                            ! We've already seen this and run the
                            ! before-change functions; this time we only
                            ! need to record the actual position.
                            changed = pos
                        else if (changed == 0) then
                            changed = -1
                            call modify_text(pos, e, status)
                            if (.not. status%ok()) exit

                            if (no_undo) then
                                m = buffers(cur)%text%modiff
                                if (buffers(cur)%text%save_modiff == m - 1) buffers(cur)%text%save_modiff = m
                            end if

                            ! The before-change functions may have moved the
                            ! gap or even modified the buffer so we should
                            ! start over.
                            rescan = .true.
                            exit
                        end if

                        if (.not. no_undo) call record_change(pos, pos_byte, pos_byte_next)

                        do k = 0, nlen - 1
                            associate (t => buffers(cur)%text)
                                t%beg(buf_byte_index(buffers(cur), pos_byte + k):buf_byte_index(buffers(cur), pos_byte + k)) = &
                                    tostr(k + 1:k + 1)
                            end associate
                        end do

                        last_changed = pos + 1
                    end if
                end if

                pos_byte = pos_byte_next
                pos = pos + 1
            end do

            if (.not. rescan) exit
            s = start
            e = end
            call validate_region(s, e, status)
            if (.not. status%ok()) exit
        end do

        if (status%ok() .and. changed > 0) then
            call signal_after_change(changed, last_changed - changed, last_changed - changed, status)
        end if

        if (no_undo) then
            buffers(cur)%undo_enabled = saved_undo

            if (had_filename) then
                call move_alloc(saved_filename, buffers(cur)%filename)
            else if (allocated(buffers(cur)%filename)) then
                deallocate (buffers(cur)%filename)
            end if
        end if

    contains

        !> Record that the character at POS (bytes FROM_BYTE..TO_BYTE) is
        !> about to change.  Port of undo.c:record_change.
        subroutine record_change(p, from_byte, to_byte)
            integer(int64), intent(in) :: p, from_byte, to_byte

            call record_delete(p, buf_bytes(buffers(cur), from_byte, to_byte), 1_int64)
            call record_insert(p, 1_int64)
        end subroutine record_change

        !> The decimal text of N.
        pure function int_text(n) result(text)
            integer(int64), intent(in) :: n
            character(len=:), allocatable :: text
            character(len=21) :: buf

            write (buf, "(i0)") n
            text = trim(buf)
        end function int_text
    end subroutine subst_char_in_region
end module editfns_fm

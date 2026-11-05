! utility_functions_buffer_mod.f90 -- the base64 region functions.
! Ported from GNU Emacs 31.1 src/fns.c.
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

!> Part of utility_functions_mod: the base64 functions that replace a region of the
!> current buffer, and their commands.  A submodule can use insert_delete_mod
!> and editing_functions_mod, which a module below them cannot.
submodule(utility_functions_mod) utility_functions_buffer_mod
    use commands_mod, only: commands
    use buffer_mod, only: buf_bytes, temporary_set_point_both
    use marker_mod, only: character_to_byte
    use insert_delete_mod, only: insert, insert_1_both, signal_after_change, delete_range_byte, delete_range_both, move_gap_both
    use text_properties_mod, only: set_point, set_point_both
    use editing_functions_mod, only: validate_region, interactive_region
    implicit none(type, external)

contains

    recursive module subroutine base64_encode_region(beginning, end, length, status, no_line_break)
        integer(int64), intent(in) :: beginning, end
        integer(int64), intent(out) :: length
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_line_break
        logical :: line_break

        line_break = .true.
        if (present(no_line_break)) line_break = .not. no_line_break
        call base64_encode_region_1(beginning, end, line_break, .true., .false., length, status)
    end subroutine base64_encode_region

    recursive module subroutine base64url_encode_region(beginning, end, length, status, no_pad)
        integer(int64), intent(in) :: beginning, end
        integer(int64), intent(out) :: length
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_pad
        logical :: pad

        pad = .true.
        if (present(no_pad)) pad = .not. no_pad
        call base64_encode_region_1(beginning, end, .false., pad, .true., length, status)
    end subroutine base64url_encode_region

    !> Base64-encode the region between BEGINNING_IN and END_IN as
    !> LINE_BREAK, PAD and BASE64URL ask, giving the LENGTH of the encoded
    !> text.  Port of fns.c:base64_encode_region_1.
    recursive subroutine base64_encode_region_1(beginning_in, end_in, line_break, pad, base64url, length, status)
        integer(int64), intent(in) :: beginning_in, end_in
        logical, intent(in) :: line_break, pad, base64url
        integer(int64), intent(out) :: length
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: encoded
        integer(int64) :: beginning, end, beginning_byte, end_byte, old_point
        logical :: ok

        length = 0
        old_point = buffers(current)%point
        beginning = beginning_in
        end = end_in
        call validate_region(beginning, end, status)
        if (.not. status%ok()) return
        beginning_byte = character_to_byte(beginning)
        end_byte = character_to_byte(end)
        call move_gap_both(beginning, beginning_byte)
        call base64_encode_1(buf_bytes(buffers(current), beginning_byte, end_byte), line_break, pad, base64url, &
                             buffers(current)%multibyte, encoded, ok)

        if (.not. ok) then
            ! The encoding wasn't possible.
            call set_error(status, GENERIC_ERROR_CODE, "Multibyte character in data for base64 encoding")
            return
        end if

        ! Now we have encoded the region, so we insert the new contents
        ! and delete the old.  (Insert first in order to preserve markers.)
        length = len(encoded, kind=int64)
        call set_point_both(beginning, beginning_byte, status)
        if (.not. status%ok()) return
        call insert(encoded, status)
        if (.not. status%ok()) return
        call delete_range_byte(beginning_byte + length, end_byte + length, status)
        if (.not. status%ok()) return

        ! If point was outside of the region, restore it exactly; else just
        ! move to the beginning of the region.
        if (old_point >= end) then
            old_point = old_point + length - (end - beginning)
        else if (old_point > beginning) then
            old_point = beginning
        end if

        call set_point(old_point, status)
    end subroutine base64_encode_region_1

    recursive module subroutine base64_decode_region(beginning, end, length, status, base64url, ignore_invalid)
        integer(int64), intent(in) :: beginning, end
        integer(int64), intent(out) :: length
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: base64url, ignore_invalid
        character(len=:), allocatable :: decoded
        integer(int64) :: b, e, beginning_byte, end_byte, old_point, inserted_characters
        logical :: url, ignore, multibyte, ok

        length = 0
        old_point = buffers(current)%point
        multibyte = buffers(current)%multibyte
        url = .false.
        if (present(base64url)) url = base64url
        ignore = .false.
        if (present(ignore_invalid)) ignore = ignore_invalid
        b = beginning
        e = end
        call validate_region(b, e, status)
        if (.not. status%ok()) return
        beginning_byte = character_to_byte(b)
        end_byte = character_to_byte(e)
        call move_gap_both(b, beginning_byte)
        call base64_decode_1(buf_bytes(buffers(current), beginning_byte, end_byte), url, multibyte, ignore, decoded, &
                             inserted_characters, ok)

        if (.not. ok) then
            ! The decoding wasn't possible.
            call set_error(status, GENERIC_ERROR_CODE, "Invalid base64 data")
            return
        end if

        ! Now we have decoded the region, so we insert the new contents
        ! and delete the old.  (Insert first in order to preserve markers.)
        call temporary_set_point_both(buffers(current), b, beginning_byte)
        call insert_1_both(decoded, inserted_characters, len(decoded, kind=int64), .false., .true., .false., status)
        if (.not. status%ok()) return
        call signal_after_change(b, 0_int64, inserted_characters, status)
        if (.not. status%ok()) return
        ! Delete the original text.
        call delete_range_both(buffers(current)%point, buffers(current)%point_byte, e + inserted_characters, &
                               end_byte + len(decoded, kind=int64), .true., status)
        if (.not. status%ok()) return

        ! If point was outside of the region, restore it exactly; else just
        ! move to the beginning of the region.
        if (old_point >= e) then
            old_point = old_point + inserted_characters - (e - b)
        else if (old_point > b) then
            old_point = b
        end if

        call set_point(min(old_point, buffers(current)%end_of_accessible_portion), status)
        length = inserted_characters
    end subroutine base64_decode_region

    ! The commands read the region (interactive "r").
    subroutine execute_base64_encode_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end, length

        call interactive_region(beginning, end, status)
        if (.not. status%ok()) return
        call base64_encode_region(beginning, end, length, status)
    end subroutine execute_base64_encode_region

    subroutine execute_base64url_encode_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end, length

        call interactive_region(beginning, end, status)
        if (.not. status%ok()) return
        call base64url_encode_region(beginning, end, length, status)
    end subroutine execute_base64url_encode_region

    subroutine execute_base64_decode_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: beginning, end, length

        call interactive_region(beginning, end, status)
        if (.not. status%ok()) return
        call base64_decode_region(beginning, end, length, status)
    end subroutine execute_base64_decode_region

    module subroutine initialize_functions()
        commands%base64_encode_region%execute => execute_base64_encode_region
        commands%base64url_encode_region%execute => execute_base64url_encode_region
        commands%base64_decode_region%execute => execute_base64_decode_region
    end subroutine initialize_functions
end submodule utility_functions_buffer_mod

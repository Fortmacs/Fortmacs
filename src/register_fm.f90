! register_fm.f90 -- register commands.
! Ported from GNU Emacs 31.1 lisp/register.el.
!
! Copyright (C) 1985, 1993-1994, 2001-2026 Free Software Foundation,
! Inc.
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

!> Registers: named places (characters) holding a buffer position, text,
!> a rectangle, a number, a window configuration with a position, a file
!> name, a file and position to visit again, or a buffer.  The registers
!> are `register_table', an array of `register_type' (a name and a
!> typed `register_contents_type'), read with `get_register' and set with
!> `set_register' or the commands; the markers and window configurations
!> in it belong to it and are freed when a register is set again.
!>
!> The commands are on C-x r (bindings_fm): C-x r SPC (C-SPC, C-@), j,
!> s (x), i (g), r, n, +, w, F and B.  Register names are read the
!> traditional way (`register_use_preview' `traditional', the default):
!> one key, C-h shows *Register Preview*.
!>
!> Deferred: frame configurations and framesets (C-u C-x r SPC is an
!> error; `frameset_to_register', C-x r f, needs frameset.el), the
!> other values of `register_use_preview' (navigation, highlighting,
!> filtering and defaults: `register_read_with_preview_fancy',
!> `register_preview_default_keys', `register_preview_next',
!> `register_preview_previous'), `register_preview_delay' (no timers: the
!> preview comes only with C-h), `register_preview_function' (the
!> preview always uses `register_preview_default'),
!> `register_preview_display_buffer_table' (the preview is shown at the
!> bottom, fitted to its buffer; preserve-size is not ported),
!> `help_event_list', `register_swap_out' and
!> `register_buffer_to_file_query' (they need `kill_buffer_hook'), the
!> obsolete `registerv' values, keyboard macros, bookmarks and other
!> packages' register values, the dired directory of `file_to_register',
!> `yank_excluded_properties' in descriptions (strings have no
!> properties), and the `register__name' text property of the preview.
module register_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR, &
                        FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum, xfixnum, xsymbol, symbols, xstring, vtag, V_NULL, V_INTEGER, V_NAME
    use lread_fm, only: sym
    use data_fm, only: value_eq, kill_all_local_variables
    use character_fm, only: MAX_CHAR, string_char_and_length
    use keymap_fm, only: single_key_description
    use print_fm, only: fortran_string, marker_repr
    use buffer_fm, only: buffers, cur, current_buffer, is_buffer_live, get_file_buffer, get_buffer_create, &
                         set_buffer, set_buffer_read_only, erase_buffer, kill_buffer, barf_if_buffer_read_only, &
                         deactivate_mark_variable, get_buffer
    use marker_fm, only: free_marker, marker_position, marker_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, goto_char_marker, point_marker, buffer_substring, delete_region, &
                          delete_and_extract_region, region_beginning, region_end
    use search_fm, only: looking_at, match_beginning, match_end
    use simple_fm, only: push_mark, insert_for_yank, exchange_point_and_mark, indicate_copied_region, &
                         region_extract_function, REGION_EXTRACT_COPY, REGION_EXTRACT_DELETE
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: last_command
    use subr_fm, only: read_key, read_y_or_n
    use frame_fm, only: selected_frame, frame_selected_window_ref
    use window_fm, only: window_type, window_configuration_type, current_window_configuration, &
                         set_window_configuration, free_window_configuration, switch_to_buffer, temp_buffer_window_show, &
                         display_action_table_type, DISPLAY_ACTION_AT_BOTTOM, fit_window_to_buffer, get_buffer_window, &
                         delete_window, is_window_live, window_leaves, windows, same_window, window_text_cols
    use fileio_fm, only: expand_file_name
    use files_fm, only: find_file
    use minibuffer_fm, only: read_file_name, read_buffer, REQUIRE_MATCH_YES
    use help_fm, only: with_output_to_temp_buffer
    use xdisp_fm, only: message
    use rect_fm, only: rectangle_line_type, extract_rectangle, delete_extract_rectangle, insert_rectangle
    implicit none(type, external)
    private

    public :: get_register, set_register, register_describe_oneline, register_preview_default, register_preview, &
              register_read_with_preview, point_to_register, window_configuration_to_register, jump_to_register, &
              file_to_register, buffer_to_register, is_register_jumpable, is_register_insertable, number_to_register, &
              increment_register, view_register, list_registers, describe_register_1, insert_register, &
              copy_to_register, append_to_register, prepend_to_register, copy_rectangle_to_register, syms_of_register

    !> The kinds of register contents: none, a buffer position (a marker),
    !> text, a rectangle, a number, a window configuration with a position,
    !> a file, a file and position to visit again (a file-query reference)
    !> and a buffer.
    integer, parameter, public :: REGISTER_EMPTY = 0, REGISTER_MARKER = 1, REGISTER_TEXT = 2, &
                                  REGISTER_RECTANGLE = 3, REGISTER_NUMBER = 4, REGISTER_WINDOW_CONFIGURATION = 5, &
                                  REGISTER_FILE = 6, REGISTER_FILE_QUERY = 7, REGISTER_BUFFER = 8

    !> The contents of a register (upstream's register values: a marker,
    !> a string, a list of strings, a number, (WINDOW-CONFIGURATION
    !> POSITION), (file . FILE-NAME), (file-query FILE-NAME POSITION) or
    !> (buffer . BUFFER)).
    type, public :: register_contents_type
        !> One of the REGISTER_ kinds.
        integer :: kind = REGISTER_EMPTY
        !> The text, or the file name of a file or file-query reference.
        character(len=:), allocatable :: text
        !> The rectangle.
        type(rectangle_line_type), allocatable :: rectangle(:)
        !> The number, or the position of a file-query reference.
        integer(int64) :: number = 0
        !> The buffer position, or the position kept with a window
        !> configuration.
        type(handle_type) :: marker
        !> The buffer.
        type(handle_type) :: buffer
        !> The window configuration.
        type(window_configuration_type) :: configuration
    end type register_contents_type

    !> A register: its NAME (a character) and its contents.
    type, public :: register_type
        integer(int64) :: name = 0
        type(register_contents_type) :: value
    end type register_type

    !> The registers, one element for each Emacs register, the most
    !> recently made first.  NAME is a character (a number).  The value
    !> is text, a number, a buffer position, a rectangle (an array of
    !> strings), a file name, a position in a file to visit again (a
    !> file-query reference), a buffer, or a window configuration with
    !> a saved value of point.  Set it with `set_register' and the register
    !> commands.  Port of register.el register-alist.
    type(register_type), allocatable, protected, public :: register_table(:)

    !> Register containing the text to put between collected texts, or -1
    !> if none.
    !>
    !> When collecting text with \[append_to_register] (or \[prepend_to_register]),
    !> contents of this register is added to the beginning (or end, respectively)
    !> of the marked text.  Port of register.el register-separator (-1
    !> for nil).
    integer(int64), public :: register_separator = -1

    !> Set contents of Emacs register named REGISTER to VALUE (text, a
    !> number or a rectangle).
    !> See the documentation of the variable `register_table' for possible
    !> VALUEs.  Port of register.el:set-register.
    interface set_register
        module procedure set_register_text, set_register_number, set_register_rectangle
    end interface set_register

    abstract interface
        !> A test of register contents (the PRED of the preview).
        !> Not a port.
        logical function register_value_predicate(value)
            import :: register_contents_type
            implicit none(type, external)
            type(register_contents_type), intent(in) :: value
        end function register_value_predicate
    end interface

    public :: register_value_predicate

    !> The character that shows the register preview (keyboard.c
    !> help-char, C-h).
    integer(int64), parameter :: HELP_CHAR = 8
    !> The name of the register preview buffer.
    character(len=*), parameter :: PREVIEW_BUFFER = "*Register Preview*"

contains

    ! ------------------------------------------------------------------
    ! Register contents
    ! ------------------------------------------------------------------

    !> The index of register REGISTER in `register_table', 0 if none.
    !> Not a port.
    integer function register_index(register) result(k)
        integer(int64), intent(in) :: register

        if (allocated(register_table)) then
            do k = 1, size(register_table)
                if (register_table(k)%name == register) return
            end do
        end if

        k = 0
    end function register_index

    !> Return contents of Emacs register named REGISTER in VALUE (of kind
    !> `REGISTER_EMPTY' if none).  The markers and the window configuration
    !> in VALUE belong to the register.  Port of register.el:get-register.
    subroutine get_register(register, value)
        integer(int64), intent(in) :: register
        type(register_contents_type), intent(out) :: value
        integer :: k

        k = register_index(register)
        if (k > 0) value = register_table(k)%value
    end subroutine get_register

    !> The kind of the contents of REGISTER.  Not a port.
    integer function register_kind(register) result(kind)
        integer(int64), intent(in) :: register
        integer :: k

        kind = REGISTER_EMPTY
        k = register_index(register)
        if (k > 0) kind = register_table(k)%value%kind
    end function register_kind

    !> Free what VALUE owns: its marker and its window configuration.
    !> Not a port.
    subroutine free_register_value(value)
        type(register_contents_type), intent(inout) :: value
        type(status_type) :: st

        select case (value%kind)
        case (REGISTER_MARKER)
            call free_marker(value%marker, st)
        case (REGISTER_WINDOW_CONFIGURATION)
            call free_window_configuration(value%configuration)
            call free_marker(value%marker, st)
        case default
            continue
        end select

        value = register_contents_type()
    end subroutine free_register_value

    !> Set REGISTER to VALUE, which then belongs to the register (the
    !> (setf (alist-get register register-alist) value) of set-register: a
    !> new register goes first).  Port of part of register.el:set-register.
    subroutine store_register(register, value)
        integer(int64), intent(in) :: register
        type(register_contents_type), intent(in) :: value
        type(register_type), allocatable :: grown(:)
        integer :: k, n

        k = register_index(register)

        if (k > 0) then
            call free_register_value(register_table(k)%value)
            register_table(k)%value = value
            return
        end if

        if (.not. allocated(register_table)) allocate (register_table(0))
        n = size(register_table)
        allocate (grown(n + 1))
        grown(1)%name = register
        grown(1)%value = value
        grown(2:) = register_table
        call move_alloc(grown, register_table)
    end subroutine store_register

    !> `set_register' with text.  Port of register.el:set-register (text).
    subroutine set_register_text(register, value)
        integer(int64), intent(in) :: register
        character(len=*), intent(in) :: value
        type(register_contents_type) :: v

        v%kind = REGISTER_TEXT
        v%text = value
        call store_register(register, v)
    end subroutine set_register_text

    !> `set_register' with a number.
    !> Port of register.el:set-register (a number).
    subroutine set_register_number(register, value)
        integer(int64), intent(in) :: register
        integer(int64), intent(in) :: value
        type(register_contents_type) :: v

        v%kind = REGISTER_NUMBER
        v%number = value
        call store_register(register, v)
    end subroutine set_register_number

    !> `set_register' with a rectangle (an empty one empties the register,
    !> as nil).  Port of register.el:set-register (a rectangle).
    subroutine set_register_rectangle(register, value)
        integer(int64), intent(in) :: register
        type(rectangle_line_type), intent(in) :: value(:)
        type(register_contents_type) :: v

        if (size(value) > 0) then
            v%kind = REGISTER_RECTANGLE
            v%rectangle = value
        end if

        call store_register(register, v)
    end subroutine set_register_rectangle

    ! ------------------------------------------------------------------
    ! Descriptions and the preview
    ! ------------------------------------------------------------------

    !> The decimal digits of N.  Not a port.
    pure function int_text(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function int_text

    !> The description of register name REGISTER (single-key-description).
    !> Not a port.
    function register_name_text(register) result(s)
        integer(int64), intent(in) :: register
        character(len=:), allocatable :: s

        s = single_key_description(make_fixnum(register))
    end function register_name_text

    !> The buffer names of the windows of window configuration CONFIG, in
    !> the cyclic order from its selected window (the window list of
    !> a frame with that configuration, made in a save-window-excursion).
    !> Not a port.
    function configuration_buffers(config) result(s)
        type(window_configuration_type), intent(in) :: config
        character(len=:), allocatable :: s
        type(window_configuration_type) :: saved
        type(handle_type), allocatable :: leaves(:)
        type(handle_type) :: sel, b
        integer :: k, first, i

        s = ""
        call current_window_configuration(saved)
        call set_window_configuration(config)
        call window_leaves(selected_frame%root_window, leaves)
        sel = selected_frame%selected_window
        first = 1

        do k = 1, size(leaves)
            if (same_window(leaves(k), sel)) first = k
        end do

        do k = 0, size(leaves) - 1
            i = modulo(first - 1 + k, size(leaves)) + 1
            b = windows(leaves(i)%index)%buffer
            if (k > 0) s = s//", "
            if (is_buffer_live(b)) s = s//buffers(b%index)%name
        end do

        call set_window_configuration(saved)
        call free_window_configuration(saved)
    end function configuration_buffers

    !> "#<buffer NAME>" or "#<killed buffer>" (prin1 of a buffer).
    !> Not a port.
    function buffer_repr(b) result(s)
        type(handle_type), intent(in) :: b
        character(len=:), allocatable :: s

        if (is_buffer_live(b)) then
            s = "#<buffer "//buffers(b%index)%name//">"
        else
            s = "#<killed buffer>"
        end if
    end function buffer_repr

    !> The width of the selected window (window-width).  Not a port.
    integer function selected_window_width() result(width)
        type(window_type), pointer :: w

        w => frame_selected_window_ref()
        width = window_text_cols(w)
    end function selected_window_width

    !> The description of a string register value VAL.  Port of
    !> register.el:register-val-describe (the string method).
    function describe_string(val, verbose) result(s)
        character(len=*), intent(in) :: val
        logical, intent(in) :: verbose
        character(len=:), allocatable :: s
        integer(int64) :: i, j, n, limit
        integer(int32) :: c, clen

        if (verbose) then
            s = "the text:"//char(10)//val
            return
        end if

        ! Extract first N characters starting with first non-whitespace.
        i = verify(val, " "//char(9)//char(10), kind=int64)

        if (i > 0) then
            ! Deduct 6 for the spaces inserted below.
            limit = min(20, max(0, selected_window_width() - 6))
            ! [^ \t\n].\{,LIMIT\}
            call string_char_and_length(val, i, c, clen)
            j = i + clen
            n = 0

            do while (j <= len(val, kind=int64) .and. n < limit)
                call string_char_and_length(val, j, c, clen)
                if (c == 10) exit
                j = j + clen
                n = n + 1
            end do

            s = "text starting with"//char(10)//"    "//val(i:j - 1)
        else if (len(val) > 0) then
            s = "whitespace"
        else
            s = "the empty string"
        end if
    end function describe_string

    !> Print description of register value VAL (the text after "Register
    !> X contains ").  VERBOSE means produce a more detailed description.
    !> Port of register.el:register-val-describe.
    function register_val_describe(val, verbose) result(s)
        type(register_contents_type), intent(in) :: val
        logical, intent(in) :: verbose
        character(len=:), allocatable :: s
        type(handle_type) :: b
        type(status_type) :: st
        integer(int64) :: pos
        integer :: k

        select case (val%kind)
        case (REGISTER_NUMBER)
            s = int_text(val%number)
        case (REGISTER_MARKER)
            call marker_buffer(val%marker, b, st)

            if (.not. is_buffer_live(b)) then
                s = "a marker in no buffer"
            else
                call marker_position(val%marker, pos, st)
                s = "a buffer position:"//char(10)//"    buffer "//buffers(b%index)%name//", position "//int_text(pos)
            end if
        case (REGISTER_WINDOW_CONFIGURATION)
            s = "a window configuration: "//configuration_buffers(val%configuration)//"."
        case (REGISTER_FILE)
            s = "the file "//fortran_string(val%text)//"."
        case (REGISTER_BUFFER)
            s = "the buffer "//buffer_repr(val%buffer)//"."
        case (REGISTER_FILE_QUERY)
            s = "a file-query reference:"//char(10)//"    file "//fortran_string(val%text)//","//char(10)// &
                "    position "//int_text(val%number)//"."
        case (REGISTER_RECTANGLE)
            if (verbose) then
                s = "the rectangle:"//char(10)

                do k = 1, size(val%rectangle)
                    s = s//"    "//val%rectangle(k)%text//char(10)
                end do
            else
                s = "a rectangle starting with "//val%rectangle(1)%text
            end if
        case (REGISTER_TEXT)
            s = describe_string(val%text, verbose)
        case default
            ! The default method (nil).
            s = "Garbage:"//char(10)
            if (verbose) s = s//".false."
        end select
    end function register_val_describe

    !> The description of register REGISTER, as describe-register-1
    !> prints it: "Register X contains " and the contents.  VERBOSE means
    !> produce a more detailed description.  Port of
    !> register.el:describe-register-1.
    function describe_register_1(register, verbose) result(s)
        integer(int64), intent(in) :: register
        logical, intent(in), optional :: verbose
        character(len=:), allocatable :: s
        type(register_contents_type) :: val
        logical :: v

        v = .false.
        if (present(verbose)) v = verbose
        call get_register(register, val)
        s = "Register "//register_name_text(register)//" contains "//register_val_describe(val, v)
    end function describe_register_1

    !> Return a one-line description of register C.  Port of
    !> register.el:register-describe-oneline.
    function register_describe_oneline(c) result(d)
        integer(int64), intent(in) :: c
        character(len=:), allocatable :: d
        character(len=:), allocatable :: full
        integer :: i, k

        full = describe_register_1(c)
        ! (replace-regexp-in-string "\n[ \t]*" " " ...)
        d = ""
        i = 1

        do while (i <= len(full))
            if (full(i:i) == char(10)) then
                d = d//" "
                i = i + 1

                do while (i <= len(full))
                    if (full(i:i) /= " " .and. full(i:i) /= char(9)) exit
                    i = i + 1
                end do
            else
                d = d//full(i:i)
                i = i + 1
            end if
        end do

        ! "Register.+? contains \\(?:an? \\|the \\)?"
        i = index(d, "Register")
        if (i == 0) return
        k = index(d(i + 9:), " contains ")
        if (k == 0) return
        d = d(i + 9 + k - 1 + len(" contains "):)

        if (index(d, "a ") == 1) then
            d = d(3:)
        else if (index(d, "an ") == 1) then
            d = d(4:)
        else if (index(d, "the ") == 1) then
            d = d(5:)
        end if
    end function register_describe_oneline

    !> Function used to format a register for previewing.
    !> This is the default value of the variable `register_preview_function'.
    !> Port of register.el:register-preview-default.
    function register_preview_default(register) result(s)
        integer(int64), intent(in) :: register
        character(len=:), allocatable :: s

        s = register_name_text(register)//": "//register_describe_oneline(register)//char(10)
    end function register_preview_default

    !> The window-height function of the preview's display action
    !> (fit-window-to-buffer).  Not a port.
    subroutine fit_preview_window(window)
        type(handle_type), intent(in) :: window

        call fit_window_to_buffer(window)
    end subroutine fit_preview_window

    !> Pop up a window showing the preview of registers in BUFFER.
    !> If SHOW_EMPTY is true, show the preview window even if no registers.
    !> Optional argument PRED specifies the types of register to show;
    !> if it is absent, show all the registers.
    !> Format of each entry is controlled by the variable
    !> `register_preview_function'.  Port of register.el:register-preview.
    subroutine register_preview(buffer, show_empty, status, pred)
        character(len=*), intent(in) :: buffer
        logical, intent(in) :: show_empty
        type(status_type), intent(out) :: status
        procedure(register_value_predicate), optional :: pred
        integer(int64), allocatable :: names(:)
        integer(int64) :: t
        integer :: k, j
        type(handle_type) :: old, buf, window
        type(display_action_table_type) :: table
        type(status_type) :: st
        character(len=:), allocatable :: old_dir

        status = status_type()
        ! (register-of-type-alist pred)
        allocate (names(0))

        if (allocated(register_table)) then
            do k = 1, size(register_table)
                if (present(pred)) then
                    if (.not. pred(register_table(k)%value)) cycle
                end if

                names = [names, register_table(k)%name]
            end do
        end if

        if (.not. show_empty .and. size(names) == 0) return

        ! (sort registers :key #'car)
        do k = 2, size(names)
            t = names(k)
            j = k - 1

            do while (j >= 1)
                if (names(j) <= t) exit
                names(j + 1) = names(j)
                j = j - 1
            end do

            names(j + 1) = t
        end do

        ! with-current-buffer-window: temp-buffer-window-setup, the body,
        ! temp-buffer-window-show.
        old = current_buffer()
        if (allocated(buffers(cur)%directory)) old_dir = buffers(cur)%directory
        call get_buffer_create(buffer, buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        call kill_all_local_variables()
        if (allocated(old_dir)) buffers(cur)%directory = old_dir
        call set_buffer_read_only(buf, .false., st)
        if (allocated(buffers(cur)%filename)) deallocate (buffers(cur)%filename)
        buffers(cur)%undo_enabled = .false.
        call erase_buffer(status)

        do k = 1, size(names)
            if (.not. status%ok()) exit
            if (register_kind(names(k)) /= REGISTER_EMPTY) call insert(register_preview_default(names(k)), status)
        end do

        if (is_buffer_live(old)) call set_buffer(old, st)
        if (.not. status%ok()) return
        table%window_height => fit_preview_window
        call temp_buffer_window_show(buf, window, status, [DISPLAY_ACTION_AT_BOTTOM], table)
    end subroutine register_preview

    !> Delete the window showing the preview and kill its buffer.
    !> Not a port.
    subroutine remove_preview()
        type(handle_type) :: buf, w
        type(status_type) :: st

        buf = get_buffer(PREVIEW_BUFFER)
        if (.not. is_buffer_live(buf)) return
        w = get_buffer_window(buf)
        if (is_window_live(w)) call delete_window(w, st)
        call kill_buffer(buf, st)
    end subroutine remove_preview

    !> Read register name, prompting with PROMPT; possibly show existing
    !> registers.
    !> This reads and returns the name of a register.  PROMPT should be a
    !> string to prompt the user for the name.
    !> If `help_char' is pressed, display preview window unconditionally.
    !>
    !> PRED if present should be a function specifying the kinds of
    !> registers that can be used.  It is called with one argument, a
    !> register value, and should return true if and only if that register
    !> value can be used.  The register value of kind `REGISTER_EMPTY'
    !> represents an empty register.  Port of
    !> register.el:register-read-with-preview, which calls
    !> register-read-with-preview-traditional (`register_use_preview'
    !> `traditional'; no `register_preview_delay' timer).
    recursive subroutine register_read_with_preview(prompt, register, status, pred)
        character(len=*), intent(in) :: prompt
        integer(int64), intent(out) :: register
        type(status_type), intent(out) :: status
        procedure(register_value_predicate), optional :: pred
        type(value_type) :: key
        type(handle_type) :: buf
        type(status_type) :: st
        logical :: help

        register = 0
        ! The help characters that are not the name of a register.
        help = register_kind(HELP_CHAR) == REGISTER_EMPTY

        do
            call read_key(prompt, key, status)
            if (.not. status%ok()) exit
            if (.not. help) exit
            if (vtag(key) /= V_INTEGER) exit
            if (xfixnum(key) /= HELP_CHAR) exit
            buf = get_buffer(PREVIEW_BUFFER)

            if (.not. is_window_live(get_buffer_window(buf))) then
                call register_preview(PREVIEW_BUFFER, .true., st, pred)
            end if
        end do

        if (status%ok()) then
            if (is_quit_event(key)) then
                ! (keyboard-quit)
                if (associated(commands%keyboard_quit%run)) call commands%keyboard_quit%run(status)
            else if (vtag(key) == V_INTEGER) then
                if (xfixnum(key) >= 0 .and. xfixnum(key) <= MAX_CHAR) then
                    register = xfixnum(key)
                else
                    call set_error(status, FM_GENERIC_ERROR, "Non-character input-event")
                end if
            else
                call set_error(status, FM_GENERIC_ERROR, "Non-character input-event")
            end if
        end if

        call remove_preview()
    end subroutine register_read_with_preview

    !> Whether KEY is C-g, `escape' or C-[.  Not a port.
    logical function is_quit_event(key)
        type(value_type), intent(in) :: key

        is_quit_event = .false.

        if (vtag(key) == V_INTEGER) then
            is_quit_event = xfixnum(key) == 7 .or. xfixnum(key) == 27
        else if ((vtag(key) == V_NAME .or. vtag(key) == V_NULL)) then
            is_quit_event = symbols(xsymbol(key))%name == "escape"
        end if
    end function is_quit_event

    ! ------------------------------------------------------------------
    ! Storing positions and configurations, jumping
    ! ------------------------------------------------------------------

    !> Store current location of point in REGISTER.
    !> With prefix argument ARG, store current frame configuration (a.k.a.
    !> "frameset").
    !> Use \[jump_to_register] to go to that location or restore that
    !> configuration.
    !> Argument is a character, the name of the register.
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview'.
    !> Port of register.el:point-to-register (frame configurations are
    !> deferred: ARG true is an error; the marker does not become a
    !> file-query reference when its buffer is killed).
    subroutine point_to_register(register, status, arg)
        integer(int64), intent(in) :: register
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: arg
        type(register_contents_type) :: v

        status = status_type()

        if (present(arg)) then
            if (arg) then
                call set_error(status, FM_GENERIC_ERROR, "Frame configurations are not ported yet")
                return
            end if
        end if

        v%kind = REGISTER_MARKER
        call point_marker(v%marker)
        call store_register(register, v)
    end subroutine point_to_register

    !> Store the window configuration of the selected frame in REGISTER.
    !> Use \[jump_to_register] to restore the configuration.
    !> Argument is a character, the name of the register.
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview'.
    !> Port of register.el:window-configuration-to-register.
    subroutine window_configuration_to_register(register)
        integer(int64), intent(in) :: register
        type(register_contents_type) :: v

        ! current-window-configuration does not include the value
        ! of point in the current buffer, so record that separately.
        v%kind = REGISTER_WINDOW_CONFIGURATION
        call current_window_configuration(v%configuration)
        call point_marker(v%marker)
        call store_register(register, v)
    end subroutine window_configuration_to_register

    !> Go to location stored in REGISTER, or restore configuration stored there.
    !> Push the mark if going to the location moves point, unless called in succession.
    !> If the register contains a file name, find that file.
    !> If the register references a buffer, switch to that buffer.
    !> If the register contains a window configuration (one frame) or a frameset
    !> (all frames), restore the configuration of that frame or of all frames
    !> accordingly.
    !> First argument REGISTER is a character, the name of the register.
    !> Optional second arg DELETE true (interactively, prefix argument) says
    !> to delete any existing frames that the frameset doesn't mention.
    !> (Otherwise, these frames are iconified.)  This argument is currently
    !> ignored if the register contains anything but a frameset.
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview'.
    !> Port of register.el:jump-to-register (with the
    !> register-val-jump-to methods).
    recursive subroutine jump_to_register(register, status, delete)
        integer(int64), intent(in) :: register
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delete
        type(register_contents_type) :: val
        type(handle_type) :: b
        type(status_type) :: st
        integer(int64) :: pos
        logical :: ok

        status = status_type()
        call get_register(register, val)

        select case (val%kind)
        case (REGISTER_MARKER)
            call marker_buffer(val%marker, b, st)

            if (.not. is_buffer_live(b)) then
                call set_error(status, FM_USER_ERROR, format_message("That register's buffer no longer exists"))
                return
            end if

            call switch_to_buffer(buffers(b%index)%name, status)
            if (.not. status%ok()) return
            call marker_position(val%marker, pos, st)
            if (point() /= pos .and. .not. value_eq(last_command, sym("jump_to_register"))) call push_mark()
            call goto_char_marker(val%marker, status)
        case (REGISTER_WINDOW_CONFIGURATION)
            call set_window_configuration(val%configuration)
            call goto_char_marker(val%marker, status)
        case (REGISTER_FILE)
            call find_file(val%text, status)
        case (REGISTER_BUFFER)
            if (.not. is_buffer_live(val%buffer)) then
                ! As set-window-buffer signals.
                call set_error(status, FM_GENERIC_ERROR, "Attempt to display deleted buffer")
                return
            end if

            call switch_to_buffer(buffers(val%buffer%index)%name, status)
        case (REGISTER_FILE_QUERY)
            if (.not. is_buffer_live(find_buffer_visiting(val%text))) then
                call read_y_or_n("Visit file "//val%text//" again? ", ok, status)
                if (.not. status%ok()) return

                if (.not. ok) then
                    call set_error(status, FM_USER_ERROR, "Register access aborted")
                    return
                end if
            end if

            call find_file(val%text, status)
            if (status%ok()) call goto_char(val%number)
        case default
            call set_error(status, FM_USER_ERROR, &
                           format_message("Register doesn't contain a buffer position or configuration"))
        end select
    end subroutine jump_to_register

    !> The buffer visiting FILE, or the null handle (find-buffer-visiting
    !> without truenames).  Port of files.el:find-buffer-visiting (in part).
    function find_buffer_visiting(file) result(h)
        character(len=*), intent(in) :: file
        type(handle_type) :: h
        character(len=:), allocatable :: expanded
        type(status_type) :: st

        h = handle_type()
        call expand_file_name(file, expanded, st)
        if (st%ok()) h = get_file_buffer(expanded)
    end function find_buffer_visiting

    !> Insert FILE_NAME into REGISTER.
    !> To visit the file, use \[jump_to_register].
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview'.
    !> With a prefix-argument, prompt for FILE_NAME using `read_file_name',
    !> With no prefix-argument, use the currently visited file or directory
    !> for FILE_NAME.  Port of register.el:file-to-register (the command
    !> does nothing when there is no FILE_NAME, as for nil).
    subroutine file_to_register(file_name, register)
        character(len=*), intent(in) :: file_name
        integer(int64), intent(in) :: register
        type(register_contents_type) :: v

        v%kind = REGISTER_FILE
        v%text = file_name
        call store_register(register, v)
    end subroutine file_to_register

    !> Store reference to BUFFER in REGISTER.
    !> To visit the buffer, use \[jump_to_register].
    !>
    !> Interactively, use current buffer as BUFFER, and prompt for REGISTER.
    !> With a prefix argument, prompt for BUFFER as well.  Port of
    !> register.el:buffer-to-register (the reference does not become a
    !> file-query reference when the buffer is killed).
    subroutine buffer_to_register(buffer, register)
        type(handle_type), intent(in) :: buffer
        integer(int64), intent(in) :: register
        type(register_contents_type) :: v

        v%kind = REGISTER_BUFFER
        v%buffer = buffer
        call store_register(register, v)
    end subroutine buffer_to_register

    !> Return true if `register_val_jump_to' is implemented for REGVAL.
    !> Port of register.el:register--jumpable-p.
    logical function is_register_jumpable(regval)
        type(register_contents_type), intent(in) :: regval

        select case (regval%kind)
        case (REGISTER_MARKER, REGISTER_WINDOW_CONFIGURATION, REGISTER_FILE, REGISTER_BUFFER, REGISTER_FILE_QUERY)
            is_register_jumpable = .true.
        case default
            is_register_jumpable = .false.
        end select
    end function is_register_jumpable

    !> Return true if `register_val_insert' is implemented for REGVAL.
    !> Port of register.el:register--insertable-p.
    logical function is_register_insertable(regval)
        type(register_contents_type), intent(in) :: regval

        select case (regval%kind)
        case (REGISTER_TEXT, REGISTER_NUMBER, REGISTER_MARKER, REGISTER_RECTANGLE)
            is_register_insertable = .true.
        case default
            is_register_insertable = .false.
        end select
    end function is_register_insertable

    ! ------------------------------------------------------------------
    ! Numbers
    ! ------------------------------------------------------------------

    !> The integer at the start of TEXT, after spaces and tabs
    !> (string-to-number in base 10, integers only).  Not a port.
    pure integer(int64) function string_to_number(text) result(n)
        character(len=*), intent(in) :: text
        integer :: i
        logical :: negative

        n = 0
        i = 1

        do while (i <= len(text))
            if (text(i:i) /= " " .and. text(i:i) /= char(9)) exit
            i = i + 1
        end do

        negative = .false.

        if (i <= len(text)) then
            if (text(i:i) == "-") then
                negative = .true.
                i = i + 1
            else if (text(i:i) == "+") then
                i = i + 1
            end if
        end if

        do while (i <= len(text))
            if (verify(text(i:i), "0123456789") /= 0) exit
            if (n < 100000000000000000_int64) n = n*10 + (iachar(text(i:i)) - iachar("0"))
            i = i + 1
        end do

        if (negative) n = -n
    end function string_to_number

    !> Store NUMBER in REGISTER.
    !> REGISTER is a character, the name of the register.
    !> If NUMBER is absent, a decimal number is read from the buffer
    !> at point, and point moves to the end of that number.
    !> Interactively, NUMBER is the prefix arg (none means absent).
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview'.
    !> Port of register.el:number-to-register.
    subroutine number_to_register(register, status, number)
        integer(int64), intent(in) :: register
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: number
        character(len=:), allocatable :: text
        integer(int64) :: b, e

        if (present(number)) then
            status = status_type()
            call set_register(register, number)
            return
        end if

        if (looking_at("\s-*-?[0-9]+", status)) then
            b = match_beginning(0, status)
            e = match_end(0, status)
            call goto_char(e)
            call buffer_substring(b, e, text, status)
            if (.not. status%ok()) return
            call set_register(register, string_to_number(text))
        else if (status%ok()) then
            call set_register(register, 0_int64)
        end if
    end subroutine number_to_register

    !> Augment contents of REGISTER using PREFIX.
    !> Interactively, PREFIX is the raw prefix argument.
    !>
    !> If REGISTER contains a number, add `prefix_numeric_value' of
    !> PREFIX to it.
    !>
    !> If REGISTER is empty or if it contains text, call
    !> `append_to_register' with `delete_flag' set to PREFIX.
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview'.
    !> Port of register.el:increment-register (PREFIX is the numeric value
    !> of the prefix argument, absent for none).
    recursive subroutine increment_register(register, status, prefix)
        integer(int64), intent(in) :: register
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: prefix
        type(register_contents_type) :: val
        integer(int64) :: number, b, e

        status = status_type()
        call get_register(register, val)

        select case (val%kind)
        case (REGISTER_NUMBER)
            number = 1
            if (present(prefix)) number = prefix
            call set_register(register, number + val%number)
        case (REGISTER_EMPTY, REGISTER_TEXT)
            call region_beginning(b, status)
            if (status%ok()) call region_end(e, status)
            if (status%ok()) call append_to_register(register, b, e, status, present(prefix))
        case default
            call set_error(status, FM_USER_ERROR, "Register does not contain a number or text")
        end select
    end subroutine increment_register

    ! ------------------------------------------------------------------
    ! Viewing
    ! ------------------------------------------------------------------

    !> Display the description of the contents of REGISTER.
    !> REGISTER is a character, the name of the register.
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview'.
    !> Port of register.el:view-register.
    subroutine view_register(register, status)
        integer(int64), intent(in) :: register
        type(status_type), intent(out) :: status

        status = status_type()

        if (register_kind(register) == REGISTER_EMPTY) then
            call message("Register "//register_name_text(register)//" is empty")
        else
            call with_output_to_temp_buffer("*Output*", describe_register_1(register, .true.), status)
        end if
    end subroutine view_register

    !> Display the list of nonempty registers with brief descriptions of
    !> contents.  Port of register.el:list-registers.
    subroutine list_registers(status)
        type(status_type), intent(out) :: status
        integer(int64), allocatable :: names(:)
        integer(int64) :: t
        integer :: k, j
        character(len=:), allocatable :: text

        allocate (names(0))
        if (allocated(register_table)) names = register_table(:)%name

        ! (sort list (lambda (a b) (< (car a) (car b))))
        do k = 2, size(names)
            t = names(k)
            j = k - 1

            do while (j >= 1)
                if (names(j) <= t) exit
                names(j + 1) = names(j)
                j = j - 1
            end do

            names(j + 1) = t
        end do

        text = ""

        do k = 1, size(names)
            if (register_kind(names(k)) /= REGISTER_EMPTY) text = text//describe_register_1(names(k))//char(10)
        end do

        call with_output_to_temp_buffer("*Output*", text, status)
    end subroutine list_registers

    ! ------------------------------------------------------------------
    ! Text and rectangles
    ! ------------------------------------------------------------------

    !> Insert contents of REGISTER at point.
    !> REGISTER is a character, the name of the register.
    !> Normally puts point before and mark after the inserted text, but
    !> if optional second argument ARG is true, puts mark before and
    !> point after.  Interactively, ARG is false if prefix arg is supplied,
    !> and true otherwise.
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview'.
    !> Port of register.el:insert-register (with the register-val-insert
    !> methods).
    recursive subroutine insert_register(register, status, arg)
        integer(int64), intent(in) :: register
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: arg
        type(register_contents_type) :: val
        type(status_type) :: st
        integer(int64) :: pos
        logical :: after

        after = .false.
        if (present(arg)) after = arg
        call push_mark()
        call get_register(register, val)
        status = status_type()

        select case (val%kind)
        case (REGISTER_TEXT)
            call insert_for_yank(val%text, status)
        case (REGISTER_NUMBER)
            call insert(int_text(val%number), status)
        case (REGISTER_MARKER)
            call marker_position(val%marker, pos, st)

            if (st%ok()) then
                call insert(int_text(pos), status)
            else
                call set_error(status, FM_USER_ERROR, "Register does not contain text")
            end if
        case (REGISTER_RECTANGLE)
            call insert_rectangle(val%rectangle, status)
        case (REGISTER_FILE)
            ! insert-rectangle of (file . NAME) gives insert-for-yank the
            ! symbol `file'.
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "stringp, file")
        case (REGISTER_FILE_QUERY)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "stringp, file_query")
        case (REGISTER_BUFFER)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "stringp, buffer")
        case (REGISTER_WINDOW_CONFIGURATION)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "stringp, #<window-configuration>")
        case default
            call set_error(status, FM_USER_ERROR, "Register does not contain text")
        end select

        if (.not. status%ok()) return
        if (.not. after) call exchange_point_and_mark(.false., status)
    end subroutine insert_register

    !> Copy region of text between START and END into REGISTER.
    !> If DELETE_FLAG is true (interactively, prefix arg), delete the region
    !> after copying.
    !> Called from Fortran, takes five args: REGISTER, START, END, DELETE_FLAG,
    !> and REGION.  START and END are buffer positions indicating what to copy.
    !> The optional argument REGION, if true, means START..END denotes the
    !> region.
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview'
    !> and use mark and point as START and END; REGION is always true in
    !> this case.  Port of register.el:copy-to-register.
    recursive subroutine copy_to_register(register, start, end, status, delete_flag, region)
        integer(int64), intent(in) :: register, start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delete_flag, region
        character(len=:), allocatable :: text
        type(value_type) :: string
        integer(int64), allocatable :: starts(:), ends(:)
        logical :: del, reg

        del = .false.
        if (present(delete_flag)) del = delete_flag
        reg = .false.
        if (present(region)) reg = region

        if (reg) then
            if (del) then
                call region_extract_function(REGION_EXTRACT_DELETE, string, starts, ends, status)
            else
                call region_extract_function(REGION_EXTRACT_COPY, string, starts, ends, status)
            end if

            if (.not. status%ok()) return
            text = xstring(string)
        else
            call buffer_substring(start, end, text, status)
            if (.not. status%ok()) return
            if (del) call delete_region(start, end, status)
        end if

        call set_register(register, text)
        deactivate_mark_variable = .true.
    end subroutine copy_to_register

    !> The text of `register_separator' for `append_to_register' and
    !> `prepend_to_register': the concat of its contents.  Not a port.
    subroutine separator_text(text, status)
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        type(register_contents_type) :: sep

        status = status_type()
        text = ""
        if (register_separator < 0) return
        call get_register(register_separator, sep)

        select case (sep%kind)
        case (REGISTER_TEXT)
            text = sep%text
        case (REGISTER_NUMBER)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "sequencep, "//int_text(sep%number))
        case (REGISTER_MARKER)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "sequencep, "//marker_repr(sep%marker))
        case (REGISTER_RECTANGLE)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "characterp, "//fortran_string(sep%rectangle(1)%text))
        case (REGISTER_FILE)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "characterp, file")
        case (REGISTER_FILE_QUERY)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "characterp, file_query")
        case (REGISTER_BUFFER)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "characterp, buffer")
        case (REGISTER_WINDOW_CONFIGURATION)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "characterp, #<window-configuration>")
        case default
            continue
        end select
    end subroutine separator_text

    !> Append or prepend (BEFORE) START..END to REGISTER.
    !> Port of register.el:append-to-register and
    !> register.el:prepend-to-register (their shared body).
    recursive subroutine collect_to_register(register, start, end, delete_flag, before, status)
        integer(int64), intent(in) :: register, start, end
        logical, intent(in) :: delete_flag, before
        type(status_type), intent(out) :: status
        type(register_contents_type) :: reg
        character(len=:), allocatable :: text, separator

        call get_register(register, reg)
        call buffer_substring(start, end, text, status)
        if (.not. status%ok()) return

        select case (reg%kind)
        case (REGISTER_EMPTY)
            call set_register(register, text)
        case (REGISTER_TEXT)
            call separator_text(separator, status)
            if (.not. status%ok()) return

            if (before) then
                call set_register(register, text//separator//reg%text)
            else
                call set_register(register, reg%text//separator//text)
            end if
        case default
            call set_error(status, FM_USER_ERROR, "Register does not contain text")
            return
        end select

        deactivate_mark_variable = .true.
        if (delete_flag) call delete_region(start, end, status)
    end subroutine collect_to_register

    !> Append region of text between START and END to REGISTER.
    !> If DELETE_FLAG is true (interactively, prefix arg), delete the region
    !> after appending.
    !> Called from Fortran, takes four args: REGISTER, START, END and DELETE_FLAG.
    !> START and END are buffer positions indicating what to append.
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview',
    !> and use mark and point as START and END.  Port of
    !> register.el:append-to-register.
    recursive subroutine append_to_register(register, start, end, status, delete_flag)
        integer(int64), intent(in) :: register, start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delete_flag
        logical :: del

        del = .false.
        if (present(delete_flag)) del = delete_flag
        call collect_to_register(register, start, end, del, .false., status)
    end subroutine append_to_register

    !> Prepend region of text between START and END to REGISTER.
    !> If DELETE_FLAG is true (interactively, prefix arg), delete the region
    !> after prepending.
    !> Called from program, takes four args: REGISTER, START, END and DELETE_FLAG.
    !> START and END are buffer positions indicating what to prepend.
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview',
    !> and use mark and point as START and END.  Port of
    !> register.el:prepend-to-register.
    recursive subroutine prepend_to_register(register, start, end, status, delete_flag)
        integer(int64), intent(in) :: register, start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delete_flag
        logical :: del

        del = .false.
        if (present(delete_flag)) del = delete_flag
        call collect_to_register(register, start, end, del, .true., status)
    end subroutine prepend_to_register

    !> Copy rectangular region of text between START and END into REGISTER.
    !> If DELETE_FLAG is true (interactively, prefix arg), delete the region
    !> after copying.
    !> To insert this register into a buffer, use \[insert_register].
    !>
    !> Called from Fortran, takes four args: REGISTER, START, END and DELETE_FLAG.
    !> START and END are buffer positions giving two corners of rectangle.
    !>
    !> Interactively, prompt for REGISTER using `register_read_with_preview',
    !> and use mark and point as START and END.  Port of
    !> register.el:copy-rectangle-to-register.
    recursive subroutine copy_rectangle_to_register(register, start, end, status, delete_flag)
        integer(int64), intent(in) :: register, start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: delete_flag
        type(rectangle_line_type), allocatable :: rectangle(:)
        logical :: del

        del = .false.
        if (present(delete_flag)) del = delete_flag

        if (del) then
            call delete_extract_rectangle(start, end, rectangle, status)
        else
            call extract_rectangle(start, end, rectangle, status)
        end if

        if (.not. status%ok()) return
        call set_register(register, rectangle)
    end subroutine copy_rectangle_to_register

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Whether the prefix argument is given (a "P" argument that is not nil).
    !> Not a port.
    logical function prefix_given()
        prefix_given = allocated(current_prefix_arg%a)
    end function prefix_given

    recursive subroutine cmd_point_to_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register

        if (prefix_given()) then
            call register_read_with_preview("Frame configuration to register: ", register, status)
        else
            call register_read_with_preview("Point to register: ", register, status)
        end if

        if (status%ok()) call point_to_register(register, status, prefix_given())
    end subroutine cmd_point_to_register

    recursive subroutine cmd_window_configuration_to_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register

        call register_read_with_preview("Window configuration to register: ", register, status)
        if (status%ok()) call window_configuration_to_register(register)
    end subroutine cmd_window_configuration_to_register

    recursive subroutine cmd_jump_to_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register

        call register_read_with_preview("Jump to register: ", register, status, is_register_jumpable)
        if (status%ok()) call jump_to_register(register, status, prefix_given())
    end subroutine cmd_jump_to_register

    recursive subroutine cmd_file_to_register(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file_name
        integer(int64) :: register

        status = status_type()

        if (.not. prefix_given()) then
            ! (if (eq major-mode 'dired-mode) (dired-current-directory)
            ! (buffer-file-name)): no Dired mode is ported.
            if (allocated(buffers(cur)%filename)) file_name = buffers(cur)%filename
        else
            call read_file_name("File: ", file_name, status)
            if (.not. status%ok()) return
        end if

        call register_read_with_preview("File to register: ", register, status)
        if (.not. status%ok()) return
        if (allocated(file_name)) call file_to_register(file_name, register)
    end subroutine cmd_file_to_register

    recursive subroutine cmd_buffer_to_register(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(handle_type) :: buffer
        integer(int64) :: register

        status = status_type()

        if (prefix_given()) then
            call read_buffer("Store reference to buffer", name, status, def=buffers(cur)%name, require_match=REQUIRE_MATCH_YES)
            if (.not. status%ok()) return
            buffer = get_buffer(name)
        else
            buffer = current_buffer()
        end if

        if (is_buffer_live(buffer)) then
            name = buffers(buffer%index)%name
        else
            name = ".false."
        end if

        call register_read_with_preview(format_message("Store reference to buffer `")//name// &
                                        format_message("' in register: "), register, status)
        if (status%ok()) call buffer_to_register(buffer, register)
    end subroutine cmd_buffer_to_register

    !> Augment register predicate of `increment_register': a number, empty
    !> or text.  Not a port.
    logical function is_number_or_text(regval)
        type(register_contents_type), intent(in) :: regval

        is_number_or_text = regval%kind == REGISTER_NUMBER .or. regval%kind == REGISTER_EMPTY .or. &
                            regval%kind == REGISTER_TEXT
    end function is_number_or_text

    !> The register predicate of `append_to_register' and
    !> `prepend_to_register': empty or text.  Not a port.
    logical function is_empty_or_text(regval)
        type(register_contents_type), intent(in) :: regval

        is_empty_or_text = regval%kind == REGISTER_EMPTY .or. regval%kind == REGISTER_TEXT
    end function is_empty_or_text

    !> The register predicate of `view_register': not empty.  Not a port.
    logical function is_nonempty(regval)
        type(register_contents_type), intent(in) :: regval

        is_nonempty = regval%kind /= REGISTER_EMPTY
    end function is_nonempty

    recursive subroutine cmd_number_to_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register, number
        logical :: given

        given = prefix_given()
        number = prefix_numeric_value(current_prefix_arg)
        call register_read_with_preview("Number to register: ", register, status)
        if (.not. status%ok()) return

        if (given) then
            call number_to_register(register, status, number)
        else
            call number_to_register(register, status)
        end if
    end subroutine cmd_number_to_register

    recursive subroutine cmd_increment_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register, number
        logical :: given

        given = prefix_given()
        number = prefix_numeric_value(current_prefix_arg)
        call register_read_with_preview("Increment register: ", register, status, is_number_or_text)
        if (.not. status%ok()) return

        if (given) then
            call increment_register(register, status, number)
        else
            call increment_register(register, status)
        end if
    end subroutine cmd_increment_register

    recursive subroutine cmd_view_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register

        call register_read_with_preview("View register: ", register, status, is_nonempty)
        if (status%ok()) call view_register(register, status)
    end subroutine cmd_view_register

    recursive subroutine cmd_list_registers(status)
        type(status_type), intent(out) :: status

        call list_registers(status)
    end subroutine cmd_list_registers

    recursive subroutine cmd_insert_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register
        logical :: arg

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        arg = .not. prefix_given()
        call register_read_with_preview("Insert register: ", register, status, is_register_insertable)
        if (status%ok()) call insert_register(register, status, arg)
    end subroutine cmd_insert_register

    !> Read the register and the region for the region commands.
    !> Not a port.
    recursive subroutine read_register_and_region(prompt, register, start, end, status, pred)
        character(len=*), intent(in) :: prompt
        integer(int64), intent(out) :: register, start, end
        type(status_type), intent(out) :: status
        procedure(register_value_predicate), optional :: pred

        start = 0
        end = 0
        call register_read_with_preview(prompt, register, status, pred)
        if (.not. status%ok()) return
        call region_beginning(start, status)
        if (status%ok()) call region_end(end, status)
    end subroutine read_register_and_region

    recursive subroutine cmd_copy_to_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register, start, end
        logical :: del

        del = prefix_given()
        call read_register_and_region("Copy to register: ", register, start, end, status)
        if (.not. status%ok()) return
        call copy_to_register(register, start, end, status, del, .true.)
        if (status%ok() .and. .not. del) call indicate_copied_region()
    end subroutine cmd_copy_to_register

    recursive subroutine cmd_append_to_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register, start, end
        logical :: del

        del = prefix_given()
        call read_register_and_region("Append to register: ", register, start, end, status, is_empty_or_text)
        if (.not. status%ok()) return
        call append_to_register(register, start, end, status, del)
        if (status%ok() .and. .not. del) call indicate_copied_region()
    end subroutine cmd_append_to_register

    recursive subroutine cmd_prepend_to_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register, start, end
        logical :: del

        del = prefix_given()
        call read_register_and_region("Prepend to register: ", register, start, end, status, is_empty_or_text)
        if (.not. status%ok()) return
        call prepend_to_register(register, start, end, status, del)
        if (status%ok() .and. .not. del) call indicate_copied_region()
    end subroutine cmd_prepend_to_register

    recursive subroutine cmd_copy_rectangle_to_register(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register, start, end
        type(register_contents_type) :: val
        logical :: del

        del = prefix_given()
        call read_register_and_region("Copy rectangle to register: ", register, start, end, status)
        if (.not. status%ok()) return
        call copy_rectangle_to_register(register, start, end, status, del)

        if (status%ok() .and. .not. del) then
            deactivate_mark_variable = .true.
            call get_register(register, val)

            if (val%kind == REGISTER_RECTANGLE) then
                call indicate_copied_region(len_chars(val%rectangle(1)%text))
            else
                call indicate_copied_region(0_int64)
            end if
        end if
    end subroutine cmd_copy_rectangle_to_register

    !> The number of characters of the UTF-8 text S.  Not a port.
    pure integer(int64) function len_chars(s) result(n)
        character(len=*), intent(in) :: s
        integer(int64) :: k

        n = 0

        do k = 1, len(s, kind=int64)
            if (iand(iachar(s(k:k)), 192) /= 128) n = n + 1
        end do
    end function len_chars

    !> Register the commands.  Not a port.
    subroutine syms_of_register()
        commands%point_to_register%run => cmd_point_to_register
        commands%window_configuration_to_register%run => cmd_window_configuration_to_register
        commands%jump_to_register%run => cmd_jump_to_register
        commands%file_to_register%run => cmd_file_to_register
        commands%buffer_to_register%run => cmd_buffer_to_register
        commands%number_to_register%run => cmd_number_to_register
        commands%increment_register%run => cmd_increment_register
        commands%view_register%run => cmd_view_register
        commands%list_registers%run => cmd_list_registers
        commands%insert_register%run => cmd_insert_register
        commands%copy_to_register%run => cmd_copy_to_register
        commands%append_to_register%run => cmd_append_to_register
        commands%prepend_to_register%run => cmd_prepend_to_register
        commands%copy_rectangle_to_register%run => cmd_copy_rectangle_to_register
    end subroutine syms_of_register
end module register_fm

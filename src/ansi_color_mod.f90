! ansi_color_mod.f90 -- translate ANSI escape sequences into faces.
! Ported from GNU Emacs 31.1 lisp/ansi-color.el.
!
! Copyright (C) 1999-2026 Free Software Foundation, Inc.
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

!> Translating SGR control sequences to faces.  This translation
!> effectively colorizes strings and regions based upon SGR control
!> sequences embedded in the text.  SGR (Select Graphic Rendition)
!> control sequences are defined in section 8.3.117 of the ECMA-48
!> standard (identical to ISO/IEC 6429).
!>
!> `ansi_color_apply' and `ansi_color_filter_apply' work on strings,
!> `ansi_color_apply_on_region' and `ansi_color_filter_region' on the
!> buffer; `ansi_color_process_output' is on
!> `comint_output_filter_functions' and `ansi_color_compilation_filter'
!> can be put on `compilation_filter_hook'.
!>
!> Deferred: the obsolete
!> `ansi_color_faces_vector', `ansi_color_names_vector', `ansi_color_map'
!> and their functions (`ansi_color_make_color_map',
!> `ansi_color_map_update', `ansi_color_get_face_1',
!> `ansi_color_apply_sequence', `ansi_color_parse_sequence',
!> `ansi_color_set_extent_face', `ansi_color_make_face').
module ansi_color_mod
    use, intrinsic :: iso_fortran_env, only: int64, int32
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, list, xstring, make_list
    use data_mod, only: choice_setting_type, logical_setting_type, append_locals_clearer, set, symbol_value
    use buffer_mod, only: buffers, current, current_buffer
    use marker_mod, only: make_marker, destroy_marker, marker_position, set_marker, detach_marker, copy_marker_at, &
                          marker_buffer
    use editing_functions_mod, only: point, point_minimum, goto_character, delete_region, buffer_substring
    use search_mod, only: string_match, regular_expression_search_forward, match_beginning, match_end, SEARCH_NOERROR
    use text_properties_mod, only: put_text_property, string_put_text_property, propertized_substring, propertized_concat
    use interval_tree_mod, only: make_overlay, overlay_put, move_overlay, overlay_start, overlay_end
    use hooks_mod, only: property_function_type
    use faces_mod, only: named_face_type, face_attributes, face_box, display_when, face_attributes_type, face_named
    use commands_mod, only: commands
    use process_mod, only: get_buffer_process, process_mark
    use command_interpreter_mod, only: comint_output_filter_functions, comint_last_output_start
    use compile_mod, only: compilation_filter_start
    implicit none(type, external)
    private

    public :: ansi_color_filter_apply, ansi_color_apply, ansi_color_filter_region, ansi_color_apply_on_region, &
              ansi_color_apply_overlay_face, ansi_color_apply_text_property_face, ansi_color_make_extent, &
              ansi_color_process_output, ansi_color_compilation_filter, ansi_color_for_comint_mode_on, &
              ansi_color_for_comint_mode_off, ansi_color_for_comint_mode_filter, ansi_color_code_as_hexadecimal, &
              initialize_ansi_color, ansi_color_face_procedure, reset_ansi_color_context, ansi_color_face_value

    !> A pointer to a named face, an element of
    !> `ansi_color_face_type%faces'.  Not a port.
    type, public :: ansi_color_face_reference_type
        type(named_face_type), pointer :: face => null()
    end type ansi_color_face_reference_type

    !> The face of text after ANSI escape sequences: its named faces (a
    !> null pointer for an entry of `ansi_color_basic_faces' that is "")
    !> and its foreground and background colors ("" for a color face
    !> that has no color).  Empty (no faces, colors unallocated) means no
    !> face.  Not a port.
    type, public :: ansi_color_face_type
        type(ansi_color_face_reference_type), allocatable :: faces(:)
        character(len=:), allocatable :: foreground, background
    contains
        !> Whether it is empty (no face).
        procedure :: is_empty => ansi_color_face_is_empty
    end type ansi_color_face_type

    abstract interface
        !> An `ansi_color_apply_face_function': apply FACE (empty for
        !> none) to the text between BEGINNING and END.  Not a port.
        recursive subroutine ansi_color_face_procedure(beginning, end, face)
            import :: int64, ansi_color_face_type
            implicit none(type, external)
            integer(int64), intent(in) :: beginning, end
            type(ansi_color_face_type), intent(in) :: face
        end subroutine ansi_color_face_procedure
    end interface

    !> Face used to render bold text.  Port of ansi-color.el:ansi-color-bold.
    type(named_face_type), target, public :: ansi_color_bold

    !> Face used to render faint text.  Port of ansi-color.el:ansi-color-faint.
    type(named_face_type), target, public :: ansi_color_faint

    !> Face used to render italic text.  Port of ansi-color.el:ansi-color-italic.
    type(named_face_type), target, public :: ansi_color_italic

    !> Face used to render underlined text.  Port of ansi-color.el:ansi-color-underline.
    type(named_face_type), target, public :: ansi_color_underline

    !> Face used to render slowly blinking text.  Port of ansi-color.el:ansi-color-slow-blink.
    type(named_face_type), target, public :: ansi_color_slow_blink

    !> Face used to render rapidly blinking text.  Port of ansi-color.el:ansi-color-fast-blink.
    type(named_face_type), target, public :: ansi_color_fast_blink

    !> Face used to render inverted video text.  Port of ansi-color.el:ansi-color-inverse.
    type(named_face_type), target, public :: ansi_color_inverse

    !> Face used to render black color code.  Port of ansi-color.el:ansi-color-black.
    type(named_face_type), target, public :: ansi_color_black

    !> Face used to render red color code.  Port of ansi-color.el:ansi-color-red.
    type(named_face_type), target, public :: ansi_color_red

    !> Face used to render green color code.  Port of ansi-color.el:ansi-color-green.
    type(named_face_type), target, public :: ansi_color_green

    !> Face used to render yellow color code.  Port of ansi-color.el:ansi-color-yellow.
    type(named_face_type), target, public :: ansi_color_yellow

    !> Face used to render blue color code.  Port of ansi-color.el:ansi-color-blue.
    type(named_face_type), target, public :: ansi_color_blue

    !> Face used to render magenta color code.  Port of ansi-color.el:ansi-color-magenta.
    type(named_face_type), target, public :: ansi_color_magenta

    !> Face used to render cyan color code.  Port of ansi-color.el:ansi-color-cyan.
    type(named_face_type), target, public :: ansi_color_cyan

    !> Face used to render white color code.  Port of ansi-color.el:ansi-color-white.
    type(named_face_type), target, public :: ansi_color_white

    !> Face used to render bright black color code.  Port of ansi-color.el:ansi-color-bright-black.
    type(named_face_type), target, public :: ansi_color_bright_black

    !> Face used to render bright red color code.  Port of ansi-color.el:ansi-color-bright-red.
    type(named_face_type), target, public :: ansi_color_bright_red

    !> Face used to render bright green color code.  Port of ansi-color.el:ansi-color-bright-green.
    type(named_face_type), target, public :: ansi_color_bright_green

    !> Face used to render bright yellow color code.  Port of ansi-color.el:ansi-color-bright-yellow.
    type(named_face_type), target, public :: ansi_color_bright_yellow

    !> Face used to render bright blue color code.  Port of ansi-color.el:ansi-color-bright-blue.
    type(named_face_type), target, public :: ansi_color_bright_blue

    !> Face used to render bright magenta color code.  Port of ansi-color.el:ansi-color-bright-magenta.
    type(named_face_type), target, public :: ansi_color_bright_magenta

    !> Face used to render bright cyan color code.  Port of ansi-color.el:ansi-color-bright-cyan.
    type(named_face_type), target, public :: ansi_color_bright_cyan

    !> Face used to render bright white color code.  Port of ansi-color.el:ansi-color-bright-white.
    type(named_face_type), target, public :: ansi_color_bright_white

    !> Faces used for SGR control sequences determining a face.
    !> This vector holds the faces used for SGR control sequence
    !> parameters 0 to 7 ("" for none).
    !>
    !> Parameter  Description
    !>   0        default
    !>   1        bold
    !>   2        faint
    !>   3        italic
    !>   4        underlined
    !>   5        slowly blinking
    !>   6        rapidly blinking
    !>   7        negative image
    !>
    !> Port of ansi-color.el ansi-color-basic-faces-vector.
    character(len=32), public :: ansi_color_basic_faces(0:7) = [character(len=32) :: "", "ansi_color_bold", &
                                                                                          "ansi_color_faint", "ansi_color_italic", &
                                                                                  "ansi_color_underline", "ansi_color_slow_blink", &
                                                                                      "ansi_color_fast_blink", "ansi_color_inverse"]

    !> Faces used for SGR control sequences determining a color.
    !> This array holds the faces used for SGR control sequence
    !> parameters 30 to 37 (foreground colors) and 40 to 47 (background
    !> colors).
    !>
    !> Parameter  Color
    !>   30  40   black
    !>   31  41   red
    !>   32  42   green
    !>   33  43   yellow
    !>   34  44   blue
    !>   35  45   magenta
    !>   36  46   cyan
    !>   37  47   white
    !>
    !> Port of ansi-color.el ansi-color-normal-colors-vector.
    character(len=32), public :: ansi_color_normal_colors(0:7) = [character(len=32) :: "ansi_color_black", &
                                                                                             "ansi_color_red", "ansi_color_green", &
                                                                                           "ansi_color_yellow", "ansi_color_blue", &
                                                                                          "ansi_color_magenta", "ansi_color_cyan", &
                                                                                                                 "ansi_color_white"]

    !> Faces used for SGR control sequences determining a "bright"
    !> color.  This array holds the faces used for SGR control sequence
    !> parameters 90 to 97 (bright foreground colors) and 100 to 107
    !> (bright background colors).
    !>
    !> Parameter   Color
    !>   90  100   bright black
    !>   91  101   bright red
    !>   92  102   bright green
    !>   93  103   bright yellow
    !>   94  104   bright blue
    !>   95  105   bright magenta
    !>   96  106   bright cyan
    !>   97  107   bright white
    !>
    !> Port of ansi-color.el ansi-color-bright-colors-vector.
    character(len=32), public :: ansi_color_bright_colors(0:7) = [character(len=32) :: &
                                                                  "ansi_color_bright_black", &
                                                                  "ansi_color_bright_red", &
                                                                  "ansi_color_bright_green", &
                                                                  "ansi_color_bright_yellow", &
                                                                  "ansi_color_bright_blue", &
                                                                  "ansi_color_bright_magenta", &
                                                                  "ansi_color_bright_cyan", &
                                                                  "ansi_color_bright_white"]

    !> If set to true, combining ANSI bold and a color produces the bright
    !> version of that color.  Port of ansi-color.el
    !> ansi-color-bold-is-bright.
    type(logical_setting_type), public :: ansi_color_bold_is_bright = logical_setting_type(name="ansi_color_bold_is_bright", &
                                                                                           value=.false.)

    !> Regexp matching an ANSI control sequence.  Port of ansi-color.el
    !> ansi-color-control-seq-regexp.
    character(len=*), parameter, public :: ansi_color_control_seq_regular_expression = achar(27)//"\[[0-?]*[ -/]*[@-~]"

    ! Regexp matching a partial ANSI control sequence
    ! (ansi-color--control-seq-fragment-regexp).
    character(len=*), parameter :: control_seq_fragment_regular_expression = achar(27)//"\[[0-?]*[ -/]*\|"//achar(27)

    !> Regexp that matches SGR control sequence parameters.  Port of
    !> ansi-color.el ansi-color-parameter-regexp.
    character(len=*), parameter, public :: ansi_color_parameter_regular_expression = "\([0-9]*\)[m;]"

    !> Values of `ansi_color_for_comint_mode' and
    !> `ansi_color_for_compilation_mode' besides .true. and .false.
    integer, parameter, public :: ANSI_COLOR_FILTER = 2

    !> Determines what to do with comint output.
    !> If .false., do nothing.
    !> If ANSI_COLOR_FILTER, then filter all SGR control sequences.
    !> If anything else (such as .true.), then translate SGR
    !> control sequences into text properties.
    !>
    !> In order for this to have any effect, `ansi_color_process_output'
    !> must be in `comint_output_filter_functions'.
    !>
    !> This can be used to enable colorized ls --color=yes output
    !> in shell buffers.  You set this variable by calling one of:
    !> \[ansi_color_for_comint_mode_on]
    !> \[ansi_color_for_comint_mode_off]
    !> \[ansi_color_for_comint_mode_filter]
    !> Port of ansi-color.el ansi-color-for-comint-mode.
    type(choice_setting_type), public :: ansi_color_for_comint_mode = choice_setting_type(name="ansi_color_for_comint_mode", &
                                                                                          value=.true.)

    !> Determines what to do with compilation output.
    !> If .false., do nothing.
    !>
    !> If ANSI_COLOR_FILTER, then filter all ANSI graphical control
    !> sequences.
    !>
    !> If anything else (such as .true.), then translate ANSI
    !> graphical control sequences into text properties.
    !>
    !> In order for this to have any effect,
    !> `ansi_color_compilation_filter' must be in
    !> `compilation_filter_hook'.  Port of ansi-color.el
    !> ansi-color-for-compilation-mode.
    type(choice_setting_type), public :: ansi_color_for_compilation_mode = &
                                         choice_setting_type(name="ansi_color_for_compilation_mode", value=.true.)

    ! A buffer's own `ansi_color_apply_face_function'.
    type :: face_function_entry_type
        integer(int64) :: serial = 0
        procedure(ansi_color_face_procedure), pointer, nopass :: fn => null()
    end type face_function_entry_type

    !> Function for applying an Ansi Color face to text in a buffer.
    !> This function should accept three arguments: BEGINNING, END, and FACE,
    !> and it should apply face FACE to the text between BEGINNING and END.
    !> Port of ansi-color.el ansi-color-apply-face-function (set with
    !> `set_local'; the default is `ansi_color_apply_overlay_face').
    type, public :: ansi_color_apply_face_function_type
        character(len=32) :: name = "ansi_color_apply_face_function"
        type(face_function_entry_type), allocatable :: entries(:)
    contains
        !> Make FN the current buffer's value (set_local).
        procedure :: set_local => face_function_set_local
        !> The current buffer's value.
        procedure :: current => current_face_function
    end type ansi_color_apply_face_function_type

    !> See the type.  Port of ansi-color.el ansi-color-apply-face-function.
    type(ansi_color_apply_face_function_type), public :: ansi_color_apply_face_function

    ! The state of the faces (FACE-VEC): the basic faces and the
    ! foreground and background color codes (-1 for nil).
    type :: face_state_type
        logical :: basic(0:7) = .false.
        integer(int64) :: foreground = -1, background = -1
    end type face_state_type

    ! A buffer's ansi-color-context (FACE-VEC FRAGMENT) and
    ! ansi-color-context-region (FACE-VEC MARKER).
    type :: context_type
        integer(int64) :: serial = 0
        logical :: has_string = .false.
        type(face_state_type) :: string_face_state
        character(len=:), allocatable :: fragment
        logical :: has_region = .false.
        type(face_state_type) :: region_face_state
        type(handle_type) :: region_marker
    end type context_type

    type(context_type), allocatable :: contexts(:)
    !> The property function `ansi_color_freeze_overlay', named by the
    !> overlays of `ansi_color_make_extent'.
    type(property_function_type), target :: ansi_color_freeze_overlay_function

contains

    ! ------------------------------------------------------------------
    ! Buffer-local state
    ! ------------------------------------------------------------------

    ! The current buffer's context (0 if none).  Not a port.
    integer function context_slot() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(contexts)) return
        k = findloc(contexts%serial, buffers(current)%serial, dim=1)
    end function context_slot

    ! Find or make the current buffer's context, its index K (0 without a
    ! current buffer).  Not a port.
    subroutine make_context_slot(k)
        integer, intent(out) :: k
        type(context_type) :: context

        k = context_slot()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(contexts)) allocate (contexts(0))
        k = findloc(contexts%serial, 0_int64, dim=1)

        if (k == 0) then
            contexts = [contexts, context]
            k = size(contexts)
        end if

        contexts(k)%serial = buffers(current)%serial
    end subroutine make_context_slot

    !> Set the current buffer's `ansi_color_context' and
    !> `ansi_color_context_region' to empty (as `clear_all_local_variables'
    !> does).  Not a port.
    subroutine reset_ansi_color_context()
        type(status_type) :: st
        integer :: k

        k = context_slot()
        if (k == 0) return
        if (contexts(k)%region_marker%index /= 0) call destroy_marker(contexts(k)%region_marker, st)
        contexts(k) = context_type()
    end subroutine reset_ansi_color_context

    ! Set the current buffer's value of the face function to FN.
    ! Not a port.
    subroutine face_function_set_local(self, fn)
        class(ansi_color_apply_face_function_type), intent(inout) :: self
        procedure(ansi_color_face_procedure) :: fn
        type(face_function_entry_type) :: entry
        integer :: i

        if (current == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        i = findloc(self%entries%serial, buffers(current)%serial, dim=1)

        if (i == 0) then
            self%entries = [self%entries, entry]
            i = size(self%entries)
            self%entries(i)%serial = buffers(current)%serial
        end if

        self%entries(i)%fn => fn
    end subroutine face_function_set_local

    ! The current buffer's value of the face function.  Not a port.
    function current_face_function(self) result(fn)
        class(ansi_color_apply_face_function_type), intent(in) :: self
        procedure(ansi_color_face_procedure), pointer :: fn
        integer :: i

        fn => ansi_color_apply_overlay_face
        if (current == 0 .or. .not. allocated(self%entries)) return
        i = findloc(self%entries%serial, buffers(current)%serial, dim=1, back=.true.)
        if (i > 0) fn => self%entries(i)%fn
    end function current_face_function

    ! Forget the current buffer's values (kill-all-local-variables).
    ! Not a port.
    subroutine clear_ansi_color_locals()
        integer :: i

        call reset_ansi_color_context()
        if (current == 0 .or. .not. allocated(ansi_color_apply_face_function%entries)) return
        i = findloc(ansi_color_apply_face_function%entries%serial, buffers(current)%serial, dim=1)
        if (i > 0) ansi_color_apply_face_function%entries(i) = face_function_entry_type()
    end subroutine clear_ansi_color_locals

    ! ------------------------------------------------------------------
    ! Convenience functions for comint modes (eg. shell-mode)
    ! ------------------------------------------------------------------

    !> Set `ansi_color_for_comint_mode' to .true.  Port of
    !> ansi-color.el:ansi-color-for-comint-mode-on.
    subroutine ansi_color_for_comint_mode_on(status)
        type(status_type), intent(out) :: status

        status = status_type()
        ansi_color_for_comint_mode = .true.
    end subroutine ansi_color_for_comint_mode_on

    !> Set `ansi_color_for_comint_mode' to .false.  Port of
    !> ansi-color.el:ansi-color-for-comint-mode-off.
    subroutine ansi_color_for_comint_mode_off(status)
        type(status_type), intent(out) :: status

        status = status_type()
        ansi_color_for_comint_mode = .false.
    end subroutine ansi_color_for_comint_mode_off

    !> Set `ansi_color_for_comint_mode' to ANSI_COLOR_FILTER.  Port of
    !> ansi-color.el:ansi-color-for-comint-mode-filter.
    subroutine ansi_color_for_comint_mode_filter(status)
        type(status_type), intent(out) :: status

        status = status_type()
        ansi_color_for_comint_mode = ANSI_COLOR_FILTER
    end subroutine ansi_color_for_comint_mode_filter

    !> Maybe translate SGR control sequences of comint output into text
    !> properties.
    !>
    !> Depending on variable `ansi_color_for_comint_mode' the comint
    !> output is either not processed, SGR control sequences are filtered
    !> using `ansi_color_filter_region', or SGR control sequences are
    !> translated into text properties using `ansi_color_apply_on_region'.
    !>
    !> The comint output is assumed to lie between the marker
    !> `comint_last_output_start' and the process-mark.
    !>
    !> This is a good function to put in `comint_output_filter_functions'.
    !> Port of ansi-color.el:ansi-color-process-output.
    recursive subroutine ansi_color_process_output(ignored, status)
        character(len=*), intent(in) :: ignored
        type(status_type), intent(out) :: status
        type(handle_type) :: process
        integer(int64) :: start, end

        status = status_type()
        if (len(ignored) < 0) return
        process = get_buffer_process(current_buffer())
        if (process%index == 0) return
        start = comint_last_output_start()
        if (start <= 0) start = point_minimum()
        call marker_position(process_mark(process), end, status)

        if (.not. status%ok()) then
            status = status_type()
            return
        end if

        if (ansi_color_for_comint_mode%is_off()) return

        select case (ansi_color_for_comint_mode%choice)
        case (ANSI_COLOR_FILTER)
            call ansi_color_filter_region(start, end)
        case default
            call ansi_color_apply_on_region(start, end)
        end select
    end subroutine ansi_color_process_output

    !> Maybe translate SGR control sequences into text properties.
    !> This function depends on the `ansi_color_for_compilation_mode'
    !> variable, and is meant to be used in `compilation_filter_hook'.
    !> Port of ansi-color.el:ansi-color-compilation-filter.
    recursive subroutine ansi_color_compilation_filter(status)
        type(status_type), intent(out) :: status
        type(value_type) :: saved
        type(status_type) :: st

        status = status_type()
        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)

        if (.not. ansi_color_for_compilation_mode%is_off()) then
            if (ansi_color_for_compilation_mode%choice == ANSI_COLOR_FILTER) then
                call ansi_color_filter_region(compilation_filter_start, point())
            else
                call ansi_color_apply_on_region(compilation_filter_start, point())
            end if
        end if

        call set(value_type(name="inhibit_read_only"), saved, st)
    end subroutine ansi_color_compilation_filter

    ! ------------------------------------------------------------------
    ! Working with strings
    ! ------------------------------------------------------------------

    ! The current buffer's ansi-color-context, made if nil
    ! (ansi-color--ensure-context with POSITION nil).  Port of
    ! ansi-color.el:ansi-color--ensure-context (POSITION nil).
    integer function ensure_string_context() result(k)
        call make_context_slot(k)

        if (.not. contexts(k)%has_string) then
            contexts(k)%has_string = .true.
            contexts(k)%string_face_state = face_state_type()
            contexts(k)%fragment = ""
        end if
    end function ensure_string_context

    ! The number of characters of UTF-8 TEXT.  Not a port.
    integer(int64) function nchars_of(text) result(n)
        character(len=*), intent(in) :: text
        integer :: i

        n = 0

        do i = 1, len(text)
            if (iand(iachar(text(i:i)), 192) /= 128) n = n + 1
        end do
    end function nchars_of

    ! The characters FROM to TO (0-based, TO exclusive; -1 for the end)
    ! of TEXT.  Not a port.
    function character_range(text, from, to) result(s)
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: from, to
        character(len=:), allocatable :: s
        integer(int64) :: b, e

        b = character_byte_offset(text, from)

        if (to < 0) then
            e = len(text)
        else
            e = character_byte_offset(text, to)
        end if

        s = text(b + 1:e)
    end function character_range

    ! The byte offset of character N (0-based) of TEXT.  Not a port.
    integer(int64) function character_byte_offset(text, n) result(off)
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: n
        integer(int64) :: count

        off = 0
        count = 0

        do while (off < len(text) .and. count < n)
            off = off + 1

            do while (off < len(text))
                if (iand(iachar(text(off + 1:off + 1)), 192) /= 128) exit
                off = off + 1
            end do

            count = count + 1
        end do
    end function character_byte_offset

    !> Filter out all ANSI control sequences from STRING.
    !>
    !> Every call to this function will set and use the buffer-local
    !> variable `ansi_color_context' to save partial escape sequences.
    !> This information will be used for the next call to
    !> `ansi_color_apply'.  Set `ansi_color_context' to empty if you don't
    !> want this.
    !>
    !> This function can be added to `comint_preoutput_filter_functions'.
    !> Port of ansi-color.el:ansi-color-filter-apply.
    function ansi_color_filter_apply(string) result(res)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: res
        character(len=:), allocatable :: text, fragment
        type(status_type) :: st
        integer(int64) :: start, end, position
        integer :: k

        k = ensure_string_context()
        ! if context was saved and is a string, prepend it
        text = contexts(k)%fragment//string
        contexts(k)%fragment = ""
        res = ""
        start = 0

        ! find the next escape sequence
        do
            end = string_match(ansi_color_control_seq_regular_expression, text, st, start=start + 1)
            if (end == 0) exit
            res = res//character_range(text, start, end - 1)
            start = match_end(0, st) - 1
        end do

        ! save context, add the remainder of the string to the result
        fragment = ""
        position = string_match("\(?:"//control_seq_fragment_regular_expression//"\)\'", text, st, start=start + 1)

        if (position > 0) then
            position = position - 1
            fragment = character_range(text, position, -1_int64)
            res = res//character_range(text, start, position)
        else
            res = res//character_range(text, start, -1_int64)
        end if

        k = ensure_string_context()
        contexts(k)%fragment = fragment
    end function ansi_color_filter_apply

    !> Translates SGR control sequences into text properties.
    !> Delete all other control sequences without processing them.
    !>
    !> Applies SGR control sequences setting foreground and background
    !> colors to STRING using text properties and returns the result.
    !>
    !> Every call to this function will set and use the buffer-local
    !> variable `ansi_color_context' to save partial escape sequences and
    !> current ansi codes.  This information will be used for the next
    !> call to `ansi_color_apply'.  Set `ansi_color_context' to empty if you
    !> don't want this.
    !>
    !> This function can be added to `comint_preoutput_filter_functions'.
    !> Port of ansi-color.el:ansi-color-apply.
    function ansi_color_apply(string) result(res)
        type(value_type), intent(in) :: string
        type(value_type) :: res
        type(value_type) :: text, face
        type(face_state_type) :: face_state
        character(len=:), allocatable :: raw, seq
        integer(int64), allocatable :: codes(:)
        type(status_type) :: st
        integer(int64) :: start, end, escape_end, position, nchars
        integer :: k

        k = ensure_string_context()
        face_state = contexts(k)%string_face_state
        ! If context was saved and is a string, prepend it.
        text = propertized_concat(value_type(text=contexts(k)%fragment), string)
        contexts(k)%fragment = ""
        raw = xstring(text)
        nchars = nchars_of(raw)
        res = value_type(text="")
        start = 0

        ! Find the next escape sequence.
        do
            end = string_match(ansi_color_control_seq_regular_expression, raw, st, start=start + 1)
            if (end == 0) exit
            end = end - 1
            escape_end = match_end(0, st) - 1
            ! Colorize the old block from start to end using old face.
            face = ansi_color_face_value(face_state_face(face_state))
            if (allocated(face%a)) call string_put_text_property(start, end, value_type(name="font_lock_face"), face, text, st)
            res = propertized_concat(res, propertized_substring(text, start, end))
            start = escape_end
            ! If this is a color escape sequence,
            seq = character_range(raw, end, escape_end)

            if (seq(len(seq):len(seq)) == "m") then
                ! create a new face from it.
                call parse_codes(seq, codes)
                call update_face_state(face_state, codes)
            end if
        end do

        ! if the rest of the string should have a face, put it there
        face = ansi_color_face_value(face_state_face(face_state))
        if (allocated(face%a)) call string_put_text_property(start, nchars, value_type(name="font_lock_face"), face, text, st)
        ! save context, add the remainder of the string to the result
        k = ensure_string_context()
        contexts(k)%string_face_state = face_state
        position = string_match("\(?:"//control_seq_fragment_regular_expression//"\)\'", raw, st, start=start + 1)

        if (position > 0) then
            position = position - 1
            contexts(k)%fragment = character_range(raw, position, -1_int64)
            res = propertized_concat(res, propertized_substring(text, start, position))
        else
            res = propertized_concat(res, propertized_substring(text, start, nchars))
        end if
    end function ansi_color_apply

    ! The codes of the SGR sequence SEQ (from ESC to its final `m'): the
    ! numbers the iterator over `ansi_color_parameter_regular_expression' yields.
    ! Not a port.
    subroutine parse_codes(seq, codes)
        character(len=*), intent(in) :: seq
        integer(int64), allocatable, intent(out) :: codes(:)
        type(status_type) :: st
        integer(int64) :: position, index, n, mb, me
        integer :: ios
        character(len=:), allocatable :: digits

        allocate (codes(0))
        position = 0

        do
            index = string_match(ansi_color_parameter_regular_expression, seq, st, start=position + 1)
            if (index == 0) exit
            position = match_end(0, st) - 1
            mb = match_beginning(1, st) - 1
            me = match_end(1, st) - 1
            digits = seq(mb + 1:me)
            n = 0

            if (len(digits) > 0) then
                read (digits, *, iostat=ios) n
                if (ios /= 0) n = 0
            end if

            codes = [codes, n]
        end do
    end subroutine parse_codes

    ! Return the face corresponding to FACE-VEC (nil for none).  Port of
    ! ansi-color.el:ansi-color--face-vec-face.
    function face_state_face(face_state) result(face)
        type(face_state_type), intent(in) :: face_state
        type(ansi_color_face_type) :: face
        integer :: n, i
        logical :: bright

        bright = ansi_color_bold_is_bright%value .and. face_state%basic(1)
        n = count(face_state%basic)
        allocate (face%faces(n))
        n = 0

        do i = 0, 7
            if (face_state%basic(i)) then
                n = n + 1
                if (len_trim(ansi_color_basic_faces(i)) > 0) face%faces(n)%face => face_named(trim(ansi_color_basic_faces(i)))
            end if
        end do

        if (face_state%background >= 0) face%background = color_value(face_state%background, .false.)
        if (face_state%foreground >= 0) face%foreground = color_value(face_state%foreground, .true.)

    contains

        ! The color of CODE: its hex form, or the foreground (FOREGROUND) or
        ! background of its face.
        function color_value(code, foreground) result(v)
            integer(int64), intent(in) :: code
            logical, intent(in) :: foreground
            character(len=:), allocatable :: v
            character(len=:), allocatable :: hexadecimal, name
            type(named_face_type), pointer :: f

            hexadecimal = ansi_color_code_as_hexadecimal(code)

            if (len(hexadecimal) > 0) then
                v = hexadecimal
                return
            end if

            if (bright .or. code >= 8) then
                name = trim(ansi_color_bright_colors(modulo(code, 8_int64)))
            else
                name = trim(ansi_color_normal_colors(modulo(code, 8_int64)))
            end if

            v = ""
            f => face_named(name)
            if (.not. associated(f)) return

            if (foreground) then
                if (allocated(f%lface%foreground)) v = f%lface%foreground
            else
                if (allocated(f%lface%background)) v = f%lface%background
            end if
        end function color_value
    end function face_state_face

    ! Not a port.
    logical function ansi_color_face_is_empty(self)
        class(ansi_color_face_type), intent(in) :: self

        ansi_color_face_is_empty = .true.
        if (allocated(self%faces)) ansi_color_face_is_empty = size(self%faces) == 0
        if (allocated(self%foreground) .or. allocated(self%background)) ansi_color_face_is_empty = .false.
    end function ansi_color_face_is_empty

    !> FACE as the value of a `face' or `font_lock_face' text property:
    !> its face names, then (:background COLOR) and (:foreground COLOR);
    !> one element alone, unallocated for an empty face, as
    !> ansi-color--face-vec-face builds it.  Not a port.
    function ansi_color_face_value(face) result(v)
        type(ansi_color_face_type), intent(in) :: face
        type(value_type) :: v
        type(value_type), allocatable :: elements(:)
        integer :: n, k

        n = 0
        if (allocated(face%faces)) n = size(face%faces)
        if (allocated(face%background)) n = n + 1
        if (allocated(face%foreground)) n = n + 1
        allocate (elements(n))
        n = 0

        if (allocated(face%faces)) then
            do k = 1, size(face%faces)
                n = n + 1
                if (associated(face%faces(k)%face)) elements(n) = value_type(name=trim(face%faces(k)%face%name))
            end do
        end if

        if (allocated(face%background)) then
            n = n + 1
            elements(n) = list(value_type(name=":background"), color_string(face%background))
        end if

        if (allocated(face%foreground)) then
            n = n + 1
            elements(n) = list(value_type(name=":foreground"), color_string(face%foreground))
        end if

        if (n == 1) then
            v = elements(1)
        else if (n > 1) then
            v = make_list(elements)
        end if

    contains

        ! COLOR as a value: unallocated for "" (a face without the color).
        function color_string(color) result(c)
            character(len=*), intent(in) :: color
            type(value_type) :: c

            if (len(color) > 0) c = value_type(text=color)
        end function color_string
    end function ansi_color_face_value

    !> Convert COLOR to hexadecimal string representation.
    !> COLOR is an ANSI color code.  If it is between 16 and 255
    !> inclusive, it corresponds to a color from an 8-bit color cube.
    !> If it is greater or equal than 256, it is subtracted by 256 to
    !> directly specify a 24-bit color.
    !>
    !> Return a hexadecimal string, specifying the color, or "", if COLOR
    !> is less than 16.  Port of ansi-color.el:ansi-color--code-as-hex.
    function ansi_color_code_as_hexadecimal(color) result(hexadecimal)
        integer(int64), intent(in) :: color
        character(len=:), allocatable :: hexadecimal
        character(len=8) :: buffer
        integer(int64) :: c, res, frac, color_number

        if (color < 16) then
            hexadecimal = ""
            return
        end if

        if (color >= 256) then
            res = color - 256
        else if (color >= 232) then
            ! Grayscale
            res = int(z'010101', int64)*(8 + 10*(color - 232))
        else
            ! 6x6x6 color cube
            c = color - 16
            res = 0
            frac = 6*6

            do while (frac >= 1)
                ! Repeat 3 times
                res = res*int(z'000100', int64)
                color_number = modulo(c/frac, 6_int64)
                if (color_number /= 0) res = res + int(z'37', int64) + int(z'28', int64)*color_number
                frac = frac/6
            end do
        end if

        write (buffer, "(a,z6.6)") "#", res
        hexadecimal = trim(buffer)
    end function ansi_color_code_as_hexadecimal

    ! ------------------------------------------------------------------
    ! Working with regions
    ! ------------------------------------------------------------------

    ! The current buffer's ansi-color-context-region, made if nil, its
    ! marker set to POSITION if it points nowhere
    ! (ansi-color--ensure-context with POSITION).  Port of
    ! ansi-color.el:ansi-color--ensure-context (with POSITION).
    integer function ensure_region_context(position) result(k)
        integer(int64), intent(in) :: position
        type(status_type) :: st
        type(handle_type) :: mb

        call make_context_slot(k)

        if (.not. contexts(k)%has_region) then
            contexts(k)%has_region = .true.
            contexts(k)%region_face_state = face_state_type()
            call copy_marker_at(position, contexts(k)%region_marker, .false., st)
        else
            call marker_buffer(contexts(k)%region_marker, mb, st)
            if (mb%index == 0) call set_marker(contexts(k)%region_marker, position, st)
        end if
    end function ensure_region_context

    ! The position of marker M.  Not a port.
    integer(int64) function marked_position(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, marked_position, st)
    end function marked_position

    !> Filter out all ANSI control sequences from region BEGIN to END.
    !>
    !> Every call to this function will set and use the buffer-local
    !> variable `ansi_color_context_region' to save position.  This
    !> information will be used for the next call to
    !> `ansi_color_apply_on_region'.  Specifically, it will override
    !> BEGIN, the start of the region.  Set `ansi_color_context_region'
    !> to empty if you don't want this.  Port of
    !> ansi-color.el:ansi-color-filter-region.
    recursive subroutine ansi_color_filter_region(begin, end)
        integer(int64), intent(in) :: begin, end
        type(handle_type) :: end_marker, start
        type(status_type) :: st
        integer(int64) :: original_point, position, last_mb
        integer :: k

        call copy_marker_at(end, end_marker, .false., st)
        k = ensure_region_context(begin)
        start = contexts(k)%region_marker
        original_point = point()
        call goto_character(marked_position(start))

        ! Delete escape sequences.
        do
            call regular_expression_search_forward(ansi_color_control_seq_regular_expression, position, st, &
                                                   bound=marked_position(end_marker), noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. position == 0) exit
            call delete_region(match_beginning(0, st), match_end(0, st), st)
        end do

        ! save context, add the remainder of the string to the result
        call set_marker(start, point(), st)
        last_mb = 0

        do
            call regular_expression_search_forward(control_seq_fragment_regular_expression, position, st, &
                                                   bound=marked_position(end_marker), noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. position == 0) exit
            last_mb = match_beginning(0, st)
        end do

        if (point() /= marked_position(start) .and. point() == marked_position(end_marker)) then
            call set_marker(start, last_mb, st)
        else
            call detach_marker(start, st)
        end if

        call goto_character(original_point)
        call destroy_marker(end_marker, st)
    end subroutine ansi_color_filter_region

    !> Translates SGR control sequences into overlays or extents.
    !> Delete all other control sequences without processing them.
    !>
    !> SGR control sequences are applied by calling the function
    !> specified by `ansi_color_apply_face_function'.  The default
    !> function sets foreground and background colors to the text
    !> between BEGIN and END, using overlays.
    !>
    !> Every call to this function will set and use the buffer-local
    !> variable `ansi_color_context_region' to save position and current
    !> ansi codes.  This information will be used for the next call to
    !> `ansi_color_apply_on_region'.  Specifically, it will override
    !> BEGIN, the start of the region and set the face with which to
    !> start.  Set `ansi_color_context_region' to empty if you don't want
    !> this.
    !>
    !> If PRESERVE_SEQUENCES is true, the sequences are hidden instead of
    !> being deleted.  Port of ansi-color.el:ansi-color-apply-on-region.
    recursive subroutine ansi_color_apply_on_region(begin, end, preserve_sequences)
        integer(int64), intent(in) :: begin, end
        logical, intent(in), optional :: preserve_sequences
        procedure(ansi_color_face_procedure), pointer :: apply
        type(handle_type) :: start_marker, end_marker, ov
        type(face_state_type) :: face_state
        type(ansi_color_face_type) :: faces
        type(status_type) :: st
        character(len=:), allocatable :: seq
        integer(int64), allocatable :: codes(:)
        integer(int64) :: original_point, position, escape_beginning, escape_end, old_start, last_mb
        logical :: preserve
        integer :: k

        preserve = .false.
        if (present(preserve_sequences)) preserve = preserve_sequences
        apply => ansi_color_apply_face_function%current()
        k = ensure_region_context(begin)
        face_state = contexts(k)%region_face_state
        start_marker = contexts(k)%region_marker
        call copy_marker_at(end, end_marker, .false., st)
        original_point = point()
        call goto_character(marked_position(start_marker))

        ! Find the next escape sequence.
        do
            call regular_expression_search_forward(ansi_color_control_seq_regular_expression, position, st, &
                                                   bound=marked_position(end_marker), noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. position == 0) exit
            ! Extract escape sequence.
            escape_beginning = match_beginning(0, st)
            escape_end = point()
            ! Colorize the old block from start to end using old face.
            old_start = marked_position(start_marker)
            ! Store new start position.
            call set_marker(start_marker, escape_end, st)
            call apply(old_start, escape_beginning, face_state_face(face_state))
            ! If this is a color sequence,
            call buffer_substring(escape_beginning, escape_end, seq, st)

            if (seq(len(seq):len(seq)) == "m") then
                call parse_codes(seq, codes)
                call update_face_state(face_state, codes)
            end if

            if (preserve) then
                ! Make the escape sequence transparent.
                call make_overlay(ov, escape_beginning, escape_end, st)
                call overlay_put(ov, "invisible", value_type(.true.), st)
            else
                ! Otherwise, strip.
                call delete_region(escape_beginning, escape_end, st)
            end if

            call goto_character(marked_position(start_marker))
        end do

        ! search for the possible start of a new escape sequence
        last_mb = 0

        do
            call regular_expression_search_forward(control_seq_fragment_regular_expression, position, st, &
                                                   bound=marked_position(end_marker), noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. position == 0) exit
            last_mb = match_beginning(0, st)
        end do

        if (point() /= marked_position(start_marker) .and. point() == marked_position(end_marker)) then
            call goto_character(last_mb)
            call apply(marked_position(start_marker), point(), face_state_face(face_state))
            call set_marker(start_marker, point(), st)
        else
            faces = face_state_face(face_state)
            call apply(marked_position(start_marker), marked_position(end_marker), faces)

            ! Save a restart position when there are codes active.  It's
            ! convenient for man.el's process filter to pass `begin'
            ! positions that overlap regions previously colored; these
            ! `codes' should not be applied to that overlap, so we need to
            ! know where they should really start.
            if (faces%is_empty()) then
                call detach_marker(start_marker, st)
            else
                call set_marker(start_marker, marked_position(end_marker), st)
            end if
        end if

        call make_context_slot(k)
        contexts(k)%region_face_state = face_state
        call goto_character(original_point)
        ! Clean up our temporary marker.
        call destroy_marker(end_marker, st)
    end subroutine ansi_color_apply_on_region

    !> Make an overlay from BEGINNING to END, and apply face FACE.
    !> If FACE is empty, do nothing.  Port of
    !> ansi-color.el:ansi-color-apply-overlay-face.
    recursive subroutine ansi_color_apply_overlay_face(beginning, end, face)
        integer(int64), intent(in) :: beginning, end
        type(ansi_color_face_type), intent(in) :: face
        type(handle_type) :: ov
        type(status_type) :: st

        if (face%is_empty()) return
        call ansi_color_make_extent(beginning, end, ov)
        call overlay_put(ov, "face", ansi_color_face_value(face), st)
    end subroutine ansi_color_apply_overlay_face

    !> Set the `font_lock_face' property to FACE in region BEGINNING..END.
    !> If FACE is empty, do nothing.  Port of
    !> ansi-color.el:ansi-color-apply-text-property-face.
    recursive subroutine ansi_color_apply_text_property_face(beginning, end, face)
        integer(int64), intent(in) :: beginning, end
        type(ansi_color_face_type), intent(in) :: face
        type(status_type) :: st

        if (face%is_empty()) return
        call put_text_property(beginning, end, value_type(name="font_lock_face"), ansi_color_face_value(face), st)
    end subroutine ansi_color_apply_text_property_face

    !> Make an extent for the range [FROM, TO) in the current buffer.
    !> Port of ansi-color.el:ansi-color-make-extent.
    subroutine ansi_color_make_extent(from, to, overlay)
        integer(int64), intent(in) :: from, to
        type(handle_type), intent(out) :: overlay
        type(status_type) :: st

        ! The overlay might end at the process-mark in comint buffers.  In
        ! that case, new text will be inserted before the process-mark,
        ! ie. inside the overlay (using insert-before-marks).  In order to
        ! avoid this, we use the `insert-behind-hooks' overlay property to
        ! make sure it works.
        call make_overlay(overlay, from, to, st)
        call overlay_put(overlay, "evaporate", value_type(.true.), st)
        call overlay_put(overlay, "modification_hooks", list(value_type(name="ansi_color_freeze_overlay")), st)
        call overlay_put(overlay, "insert_behind_hooks", list(value_type(name="ansi_color_freeze_overlay")), st)
    end subroutine ansi_color_make_extent

    ! ------------------------------------------------------------------
    ! Helper functions
    ! ------------------------------------------------------------------

    ! Apply escape sequences to FACE-VEC: CODES are what the iterator
    ! returns, in order.
    !
    ! For each new code, the following happens: if it is 1-7, set the
    ! corresponding properties; if it is 21-25 or 27, unset appropriate
    ! properties; if it is 30-37 (or 90-97) or resp. 39, set the
    ! foreground color or resp. unset it; if it is 40-47 (or 100-107)
    ! resp. 49, set the background color or resp. unset it; if it is 38
    ! or 48, the following codes are used to set the foreground or
    ! background color and the correct color mode; any other code will
    ! unset all properties and colors.  Port of
    ! ansi-color.el:ansi-color--update-face-vec.
    subroutine update_face_state(face_state, codes)
        type(face_state_type), intent(inout) :: face_state
        integer(int64), intent(in) :: codes(:)
        integer(int64) :: new, q, r, red, green, blue, value
        integer :: i
        logical :: do_clear, foreground_cell, got_red, got_green, got_blue, got

        i = 0
        do_clear = .false.
        foreground_cell = .true.

        do
            call next(new, got)
            if (.not. got) exit
            q = new/10

            select case (q)
            case (0)
                if (new == 0 .or. new == 8 .or. new == 9) then
                    do_clear = .true.
                else
                    face_state%basic(new) = .true.
                end if
            case (2)
                if (new == 20 .or. new == 26 .or. new == 28 .or. new == 29) then
                    do_clear = .true.
                else
                    ! The standard says `21 doubly underlined' while
                    ! https://en.wikipedia.org/wiki/ANSI_escape_code claims
                    ! `21 Bright/Bold: off or Underline: Double'.
                    face_state%basic(new - 20) = .false.

                    select case (new)
                    case (22)
                        face_state%basic(1) = .false.
                    case (25)
                        face_state%basic(6) = .false.
                    case default
                        face_state%basic(0) = .false.
                    end select
                end if
            case (3, 4, 9, 10)
                r = modulo(new, 10_int64)
                foreground_cell = q == 3 .or. q == 9

                select case (r)
                case (8)
                    call next(value, got)

                    if (got .and. value == 5) then
                        call next(new, got)

                        if (got) then
                            call set_cell(new)
                            do_clear = new >= 256
                        else
                            call set_cell(-1_int64)
                            do_clear = .true.
                        end if
                    else if (got .and. value == 2) then
                        call next(red, got_red)
                        call next(green, got_green)
                        call next(blue, got_blue)

                        if (got_red .and. got_green .and. got_blue) then
                            new = int(z'010000', int64)*red + int(z'000100', int64)*green + blue

                            if (new <= int(z'FFFFFF', int64)) then
                                call set_cell(256 + new)
                            else
                                do_clear = .true.
                            end if
                        else
                            do_clear = .true.
                        end if
                    else
                        do_clear = .true.
                    end if
                case (9)
                    call set_cell(-1_int64)
                case default
                    if (q == 3 .or. q == 4) then
                        call set_cell(r)
                    else
                        call set_cell(8 + r)
                    end if
                end select
            case default
                do_clear = .true.
            end select

            if (do_clear) then
                do_clear = .false.
                face_state = face_state_type()
            end if
        end do

    contains

        subroutine next(v, ok)
            integer(int64), intent(out) :: v
            logical, intent(out) :: ok

            ok = i < size(codes)
            v = 0

            if (ok) then
                i = i + 1
                v = codes(i)
            end if
        end subroutine next

        subroutine set_cell(v)
            integer(int64), intent(in) :: v

            if (foreground_cell) then
                face_state%foreground = v
            else
                face_state%background = v
            end if
        end subroutine set_cell
    end subroutine update_face_state

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Prevent OVERLAY from being extended.
    !> This function can be used for the `modification_hooks' overlay
    !> property.  Port of ansi-color.el:ansi-color-freeze-overlay.
    recursive subroutine ansi_color_freeze_overlay(overlay, is_after, begin, end, status, len)
        type(handle_type), intent(in) :: overlay
        logical, intent(in) :: is_after
        integer(int64), intent(in) :: begin, end
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: len

        status = status_type()
        if (.not. is_after) return
        if (.not. present(len)) return
        if (len /= 0) return
        ! If stuff was inserted at the end of the overlay, reset the end of
        ! the overlay.
        if (end == overlay_end(overlay)) call move_overlay(overlay, overlay_start(overlay), begin, status)
    end subroutine ansi_color_freeze_overlay

    !> Define the faces, commands and property functions of ansi-color, and put
    !> `ansi_color_process_output' first in
    !> `comint_output_filter_functions' (as its default value lists it).
    !> Not a port.
    subroutine initialize_ansi_color()
        type(face_attributes_type) :: attributes

        ansi_color_freeze_overlay_function = property_function_type("ansi_color_freeze_overlay", &
                                                                    overlay_change=ansi_color_freeze_overlay)
        call ansi_color_freeze_overlay_function%define()
        call ansi_color_bold%define("ansi_color_bold", "Face used to render bold text.")
        call ansi_color_bold%specification_entry(display_when(), face_attributes(inherit="bold"))
        call ansi_color_faint%define("ansi_color_faint", "Face used to render faint text.")
        call ansi_color_faint%specification_entry(display_when(), face_attributes(weight="light"))
        call ansi_color_italic%define("ansi_color_italic", "Face used to render italic text.")
        call ansi_color_italic%specification_entry(display_when(), face_attributes(inherit="italic"))
        call ansi_color_underline%define("ansi_color_underline", "Face used to render underlined text.")
        call ansi_color_underline%specification_entry(display_when(), face_attributes(inherit="underline"))
        call ansi_color_slow_blink%define("ansi_color_slow_blink", "Face used to render slowly blinking text.")
        attributes = face_attributes()
        attributes%box = face_box(line_width=-1)
        call ansi_color_slow_blink%specification_entry(display_when(), attributes)
        call ansi_color_fast_blink%define("ansi_color_fast_blink", "Face used to render rapidly blinking text.")
        attributes = face_attributes()
        attributes%box = face_box(line_width=-1)
        call ansi_color_fast_blink%specification_entry(display_when(), attributes)
        call ansi_color_inverse%define("ansi_color_inverse", "Face used to render inverted video text.")
        call ansi_color_inverse%specification_entry(display_when(), face_attributes(inverse_video=.true.))
        call ansi_color_black%define("ansi_color_black", "Face used to render black color code.")
        call ansi_color_black%specification_entry(display_when(), face_attributes(foreground="black", background="black"))
        call ansi_color_red%define("ansi_color_red", "Face used to render red color code.")
        call ansi_color_red%specification_entry(display_when(), face_attributes(foreground="red3", background="red3"))
        call ansi_color_green%define("ansi_color_green", "Face used to render green color code.")
        call ansi_color_green%specification_entry(display_when(), face_attributes(foreground="green3", background="green3"))
        call ansi_color_yellow%define("ansi_color_yellow", "Face used to render yellow color code.")
        call ansi_color_yellow%specification_entry(display_when(), face_attributes(foreground="yellow3", background="yellow3"))
        call ansi_color_blue%define("ansi_color_blue", "Face used to render blue color code.")
        call ansi_color_blue%specification_entry(display_when(), face_attributes(foreground="blue2", background="blue2"))
        call ansi_color_magenta%define("ansi_color_magenta", "Face used to render magenta color code.")
        call ansi_color_magenta%specification_entry(display_when(), face_attributes(foreground="magenta3", background="magenta3"))
        call ansi_color_cyan%define("ansi_color_cyan", "Face used to render cyan color code.")
        call ansi_color_cyan%specification_entry(display_when(), face_attributes(foreground="cyan3", background="cyan3"))
        call ansi_color_white%define("ansi_color_white", "Face used to render white color code.")
        call ansi_color_white%specification_entry(display_when(), face_attributes(foreground="grey90", background="gray90"))
        call ansi_color_bright_black%define("ansi_color_bright_black", "Face used to render bright black color code.")
        call ansi_color_bright_black%specification_entry(display_when(), face_attributes(foreground="gray30", background="gray30"))
        call ansi_color_bright_red%define("ansi_color_bright_red", "Face used to render bright red color code.")
        call ansi_color_bright_red%specification_entry(display_when(), face_attributes(foreground="red2", background="red2"))
        call ansi_color_bright_green%define("ansi_color_bright_green", "Face used to render bright green color code.")
        call ansi_color_bright_green%specification_entry(display_when(), face_attributes(foreground="green2", background="green2"))
        call ansi_color_bright_yellow%define("ansi_color_bright_yellow", "Face used to render bright yellow color code.")
        call ansi_color_bright_yellow%specification_entry(display_when(), face_attributes(foreground="yellow2", &
                                                                                          background="yellow2"))
        call ansi_color_bright_blue%define("ansi_color_bright_blue", "Face used to render bright blue color code.")
        call ansi_color_bright_blue%specification_entry(display_when(), face_attributes(foreground="blue1", background="blue1"))
        call ansi_color_bright_magenta%define("ansi_color_bright_magenta", "Face used to render bright magenta color code.")
        call ansi_color_bright_magenta%specification_entry(display_when(), face_attributes(foreground="magenta2", &
                                                                                           background="magenta2"))
        call ansi_color_bright_cyan%define("ansi_color_bright_cyan", "Face used to render bright cyan color code.")
        call ansi_color_bright_cyan%specification_entry(display_when(), face_attributes(foreground="cyan2", background="cyan2"))
        call ansi_color_bright_white%define("ansi_color_bright_white", "Face used to render bright white color code.")
        call ansi_color_bright_white%specification_entry(display_when(), face_attributes(foreground="white", background="white"))
        commands%ansi_color_for_comint_mode_on%execute => ansi_color_for_comint_mode_on
        commands%ansi_color_for_comint_mode_off%execute => ansi_color_for_comint_mode_off
        commands%ansi_color_for_comint_mode_filter%execute => ansi_color_for_comint_mode_filter
        call append_locals_clearer(clear_ansi_color_locals)
        call comint_output_filter_functions%prepend(ansi_color_process_output)
    end subroutine initialize_ansi_color
end module ansi_color_mod

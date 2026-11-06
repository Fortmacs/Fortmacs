! warnings_mod.f90 -- log and display warnings.
! Ported from GNU Emacs 31.1 lisp/emacs-lisp/warnings.el (in part: and
! subr.el's delay-warning, display-delayed-warnings and
! collapse-delayed-warnings).
!
! Copyright (C) 2002-2026 Free Software Foundation, Inc.
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

!> Warnings: `display_warning' and `warn' log a warning in the *Warnings* buffer (in Special mode) and show
!> that buffer, as the levels (WARNING_LEVEL_EMERGENCY, WARNING_LEVEL_ERROR,
!> WARNING_LEVEL_WARNING, WARNING_LEVEL_DEBUG), `warning_minimum_level' and
!> `warning_minimum_log_level' (WARNING_MINIMUM_EMERGENCY to _DEBUG) and
!> the lists of warning types
!> (`warning_suppress_types', `warning_suppress_log_types',
!> `warning_inhibit_types') say.  A warning type is one name (upstream's
!> symbol), such as "holidays", or a list of names given as an array of
!> `string_type' (built with `append_string'), whose first name is the
!> group.  Warnings issued before the init file has been loaded wait in
!> keyboard_mod's `delayed_warnings' until `delayed_warnings_hook' runs.
!> What is not ported yet is listed in docs/status/warnings_mod.md.
module warnings_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, WRONG_TYPE_ARGUMENT_CODE
    use handle_mod, only: handle_type, null_handle, same_handle
    use string_type_mod, only: string_type, append_string
    use value_mod, only: value_type, command_procedure
    use data_mod, only: choice_setting_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_buffer, get_or_make_buffer, is_buffer_live, &
                          inhibit_read_only_begin, inhibit_read_only_end, fill_column
    use marker_mod, only: marker_buffer, marker_position, destroy_marker
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, point_maximum, goto_character, is_beginning_of_line, point_marker
    use editing_commands_mod, only: forward_line
    use execute_interactively_mod, only: prefix_argument_type, PREFIX_NUMBER
    use simple_mod, only: special_mode, newline, fill_prefix
    use fill_mod, only: fill_region
    use frame_mod, only: selected_window
    use window_mod, only: windows, display_buffer, fit_window_to_buffer, set_window_start, select_window, is_window_live, &
                          recenter, display_action_parameters_type, DISPLAY_ACTION_MAYBE_SAME_WINDOW, DISPLAY_ACTION_REUSE_WINDOW, &
                          DISPLAY_ACTION_AT_BOTTOM
    use keyboard_mod, only: sit_for, delayed_warnings, delayed_warning_type, delayed_warnings_hook
    use display_update_mod, only: ding
    use startup_mod, only: after_initialization_time
    implicit none(type, external)
    private

    public :: display_warning, warn, is_warning_suppressed, warning_numeric_level, &
              delay_warning, display_delayed_warnings, collapse_delayed_warnings, initialize_warnings
    public :: warning_level_type, warning_types_type, warning_series_choice_setting_type, warning_prefix_procedure

    !> Values of `warning_minimum_level' and `warning_minimum_log_level',
    !> which take no .true. or .false.: the severity levels of warnings,
    !> from the most severe.
    integer, parameter, public :: WARNING_MINIMUM_EMERGENCY = 2, WARNING_MINIMUM_ERROR = 3, WARNING_MINIMUM_WARNING = 4, &
                                  WARNING_MINIMUM_DEBUG = 5

    !> The severity levels of a warning (the LEVEL argument of
    !> `display_warning' and the `level' of a `warning_levels' entry),
    !> from the most severe.
    integer, parameter, public :: WARNING_LEVEL_EMERGENCY = 1, WARNING_LEVEL_ERROR = 2, WARNING_LEVEL_WARNING = 3, &
                                  WARNING_LEVEL_DEBUG = 4

    !> A severity level definition of `warning_levels': it defines LEVEL
    !> (a WARNING_LEVEL_* constant) as a severity level.  STRING specifies
    !> the description of this level, as a Fortran format: an `a' edit
    !> descriptor where the warning type information goes, or none so as
    !> not to include that information.  The optional PROCEDURE,
    !> if associated, is called with no arguments, to get the user's
    !> attention.
    type :: warning_level_type
        integer :: level = WARNING_LEVEL_WARNING
        character(len=:), allocatable :: string
        procedure(command_procedure), pointer, nopass :: procedure => null()
    end type warning_level_type

    ! A warning type of a list of warning types: its names.
    type :: warning_type_entry_type
        type(string_type), allocatable :: names(:)
    end type warning_type_entry_type

    !> A list of warning types, such as `warning_suppress_types': each
    !> entry is a warning type given by its names, added with `%append'
    !> ("foo", "bar" for the type whose group is foo and whose
    !> subcategory is bar).
    type :: warning_types_type
        type(warning_type_entry_type), allocatable, private :: entries(:)
    contains
        !> Add the warning type of the names NAME, NAME2, ... (one to
        !> eight) at the end.
        procedure :: append => warning_types_append
        !> Remove all the warning types.
        procedure :: clear => warning_types_clear
    end type warning_types_type

    abstract interface
        !> `warning_prefix_function': called with the severity LEVEL and
        !> its ENTRY of `warning_levels', it changes ENTRY into the entry
        !> that should actually be used.  The warnings buffer is current.
        !> Not a port.
        recursive subroutine warning_prefix_procedure(level, entry)
            import :: warning_level_type
            implicit none(type, external)
            integer, intent(in) :: level
            type(warning_level_type), intent(inout) :: entry
        end subroutine warning_prefix_procedure
    end interface

    !> The variable `warning_series': a choice setting (.true. or
    !> .false.) whose value can also be a procedure (set with `=>') or a
    !> marker (`marker', set by `display_warning').  Not a port.
    type, extends(choice_setting_type) :: warning_series_choice_setting_type
        procedure(command_procedure), pointer, nopass :: procedure => null()
        type(handle_type) :: marker = handle_type()
    contains
        procedure :: clear_kinds => warning_series_clear_kinds
        procedure :: other_value => warning_series_other_value
        procedure :: kind_hint => warning_series_kind_hint
    end type warning_series_choice_setting_type

    !> Display a warning message, MESSAGE.  See `display_warning_of_name'.
    interface display_warning
        module procedure display_warning_of_name, display_warning_of_names
    end interface display_warning

    !> True if a warning with type TYPE should be suppressed.  See
    !> `is_warning_of_name_suppressed'.
    interface is_warning_suppressed
        module procedure is_warning_of_name_suppressed, is_warning_of_names_suppressed
    end interface is_warning_suppressed

    !> Display a delayed warning.  See `delay_warning_of_name'.
    interface delay_warning
        module procedure delay_warning_of_name, delay_warning_of_names
    end interface delay_warning

    !> List of severity level definitions for `display_warning'.
    !> Each element is a `warning_level_type' (LEVEL, STRING, PROCEDURE)
    !> and defines LEVEL as a severity level.  STRING specifies the
    !> description of this level, as a Fortran format: an `a' edit
    !> descriptor where the warning type information goes, or none so as
    !> not to include that information.
    !>
    !> The optional PROCEDURE, if associated, is a procedure to call with
    !> no arguments, to get the user's attention.
    !>
    !> The standard levels are WARNING_LEVEL_EMERGENCY, WARNING_LEVEL_ERROR,
    !> WARNING_LEVEL_WARNING and WARNING_LEVEL_DEBUG.  See `display_warning' for
    !> documentation of their meanings.  Level WARNING_LEVEL_DEBUG is ignored by
    !> default (see `warning_minimum_level').  Port of warnings.el
    !> warning-levels (its entries are made by `initialize_warnings'; the
    !> procedure of WARNING_LEVEL_EMERGENCY is `ding').
    type(warning_level_type), public :: warning_levels(4)

    !> Minimum severity level for displaying the warning buffer.
    !> If a warning's severity level is lower than this,
    !> the warning is logged in the warnings buffer, but the buffer
    !> is not immediately displayed.  See also `warning_minimum_log_level'.
    !> The value is a WARNING_MINIMUM_* constant.  Port of warnings.el
    !> warning-minimum-level.
    type(choice_setting_type), public :: warning_minimum_level = &
                                         choice_setting_type(name="warning_minimum_level", true_valid=.false., &
                                                             false_valid=.false., choice=WARNING_MINIMUM_WARNING)

    !> Minimum severity level for logging a warning.
    !> If a warning severity level is lower than this,
    !> the warning is completely ignored.
    !> Value must be lower or equal than `warning_minimum_level',
    !> because warnings not logged aren't displayed either.  The value is
    !> a WARNING_MINIMUM_* constant.  Port of warnings.el
    !> warning-minimum-log-level.
    type(choice_setting_type), public :: warning_minimum_log_level = &
                                         choice_setting_type(name="warning_minimum_log_level", true_valid=.false., &
                                                             false_valid=.false., choice=WARNING_MINIMUM_WARNING)

    !> List of warning types that should not be logged.
    !> If any element of this list matches the TYPE argument to `display_warning',
    !> the warning is completely ignored.
    !> The element must match the first elements of TYPE.
    !> Thus, the element ("foo", "bar") matches the type ("foo", "bar")
    !> or ("foo", "bar", ANYTHING...).
    !> If TYPE is one name "foo", that is equivalent to the list ("foo"),
    !> so only the element ("foo") will match it.  Add an element with
    !> call warning_suppress_log_types%append("foo", "bar").  Port of
    !> warnings.el warning-suppress-log-types.
    type(warning_types_type), public :: warning_suppress_log_types

    !> List of warning types not to display immediately.
    !> If any element of this list matches the TYPE argument to `display_warning',
    !> the warning is logged nonetheless, but the warnings buffer is
    !> not immediately displayed.
    !> The element must match an initial segment of the list TYPE.
    !> Thus, the element ("foo", "bar") matches the type ("foo", "bar")
    !> or ("foo", "bar", ANYTHING...).
    !> If TYPE is one name "foo", that is equivalent to the list ("foo"),
    !> so only the element ("foo") will match it.
    !> See also `warning_suppress_log_types'.  Port of warnings.el
    !> warning-suppress-types.
    type(warning_types_type), public :: warning_suppress_types

    !> Whether to display the warning buffer at the bottom of the screen.
    !> If this is true (the default), Fortmacs will attempt to display the
    !> window showing the warning buffer at the bottom of the selected
    !> frame, whether by reusing the bottom-most window or by creating a
    !> new window at the bottom of the frame.  The resulting window will be
    !> scrolled to the bottom of the buffer to show the last warning message.
    !>
    !> If the value of this variable is .false., Fortmacs will display the warning
    !> buffer in some window, as determined by `display_buffer' and its
    !> customizations.
    !>
    !> This option affects display of all the buffers shown by `display_warning'.
    !> Port of warnings.el warning-display-at-bottom.
    logical, public :: warning_display_at_bottom = .true.

    !> Procedure to generate warning prefixes.
    !> This procedure, if associated, is called with two arguments,
    !> the severity level and its entry in `warning_levels',
    !> and should change the entry into the entry that should actually be used.
    !> The warnings buffer is current when this procedure is called
    !> and the procedure can insert text in it.  This text becomes
    !> the beginning of the warning.  Port of warnings.el
    !> warning-prefix-function.
    procedure(warning_prefix_procedure), pointer, public :: warning_prefix_function => null()

    !> True means treat multiple `display_warning' calls as a series.
    !> A marker (`warning_series%marker') indicates a position in the
    !> warnings buffer which is the start of the current series; it means
    !> that additional warnings in the same buffer should not move point.
    !> If .true., the next warning begins a series (and stores a marker here).
    !> A procedure (`warning_series%procedure => mine') is like .true.,
    !> except also call that procedure before the next warning.  Port of
    !> warnings.el warning-series.
    type(warning_series_choice_setting_type), public :: warning_series = &
                                                        warning_series_choice_setting_type(name="warning_series")

    !> If allocated, fill each warning text using this string as `fill_prefix'.
    !> Port of warnings.el warning-fill-prefix.
    character(len=:), allocatable, public :: warning_fill_prefix

    !> Value to use for `fill_column' when filling warnings.  Port of
    !> warnings.el warning-fill-column.
    integer(int64), public :: warning_fill_column = 78

    !> Format for displaying the warning type in the warning message: a
    !> Fortran format with an `a' edit descriptor for the type.
    !> The result of formatting the type this way gets included in the
    !> message under the control of the string in `warning_levels'.  Port
    !> of warnings.el warning-type-format.
    character(len=:), allocatable, public :: warning_type_format

    !> Like `warning_suppress_log_types', but intended for programs to
    !> set around their calls.  Port of warnings.el warning-inhibit-types.
    type(warning_types_type), public :: warning_inhibit_types

contains

    ! ------------------------------------------------------------------
    ! Lists of warning types
    ! ------------------------------------------------------------------

    ! Add the warning type of the names NAME, NAME2, ... at the end of
    ! SELF.  Not a port.
    subroutine warning_types_append(self, name, name2, name3, name4, name5, name6, name7, name8)
        class(warning_types_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: name2, name3, name4, name5, name6, name7, name8
        type(warning_type_entry_type) :: entry

        call append_string(entry%names, name, name2, name3, name4, name5, name6, name7, name8)
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        self%entries = [self%entries, entry]
    end subroutine warning_types_append

    ! Remove all the warning types of SELF.  Not a port.
    subroutine warning_types_clear(self)
        class(warning_types_type), intent(inout) :: self

        if (allocated(self%entries)) deallocate (self%entries)
        allocate (self%entries(0))
    end subroutine warning_types_clear

    ! ------------------------------------------------------------------
    ! The setting warning_series
    ! ------------------------------------------------------------------

    ! Forget the procedure and the marker of `warning_series' (every `='
    ! does), freeing the marker.  Not a port.
    subroutine warning_series_clear_kinds(self)
        class(warning_series_choice_setting_type), intent(inout) :: self
        type(status_type) :: st

        nullify (self%procedure)
        if (self%marker%index /= 0) call destroy_marker(self%marker, st)
        self%marker = null_handle()
    end subroutine warning_series_clear_kinds

    ! The procedure or marker of `warning_series' as help shows it.
    ! Not a port.
    function warning_series_other_value(self) result(text)
        class(warning_series_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""

        if (self%marker%index /= 0) then
            text = "a marker set by display_warning"
        else if (associated(self%procedure)) then
            text = "a procedure set by a program"
        end if
    end function warning_series_other_value

    ! The assignment that sets the procedure of `warning_series'.
    ! Not a port.
    function warning_series_kind_hint(self, number) result(text)
        class(warning_series_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a procedure is `"//trim(self%name)//"%procedure => mine'"
        if (len(number) < 0) text = number
    end function warning_series_kind_hint

    ! ------------------------------------------------------------------
    ! Levels and types
    ! ------------------------------------------------------------------

    !> Return a numeric measure of the warning severity level LEVEL.
    !> Port of warnings.el:warning-numeric-level.
    integer function warning_numeric_level(level)
        integer, intent(in) :: level
        integer :: k

        k = findloc(warning_levels%level, level, dim=1)
        warning_numeric_level = 0
        if (k > 0) warning_numeric_level = size(warning_levels) - k + 1
    end function warning_numeric_level

    !> True if a warning with type TYPE, one name, should be suppressed.
    !> SUPPRESS is the list of kinds of warnings to suppress: TYPE
    !> matches an element that is the list of the one name TYPE.
    !> Port of warnings.el:warning-suppress-p.
    logical function is_warning_of_name_suppressed(type, suppress) result(some_match)
        character(len=*), intent(in) :: type
        type(warning_types_type), intent(in) :: suppress
        integer :: k

        some_match = .false.
        if (.not. allocated(suppress%entries)) return

        do k = 1, size(suppress%entries)
            associate (names => suppress%entries(k)%names)
                ! If TYPE is one name, the element must be (TYPE).
                if (size(names) == 1) then
                    if (names(1)%text == type .and. len(names(1)%text) == len(type)) some_match = .true.
                end if
            end associate
        end do
    end function is_warning_of_name_suppressed

    !> True if a warning with type TYPES, a list of names, should be
    !> suppressed.  SUPPRESS is the list of kinds of warnings to suppress:
    !> an element matches TYPES or some initial segment of it.
    !> Port of warnings.el:warning-suppress-p.
    logical function is_warning_of_names_suppressed(types, suppress) result(some_match)
        type(string_type), intent(in) :: types(:)
        type(warning_types_type), intent(in) :: suppress
        integer :: k, j
        logical :: match

        some_match = .false.
        if (.not. allocated(suppress%entries)) return

        do k = 1, size(suppress%entries)
            associate (names => suppress%entries(k)%names)
                ! Check elements of the element until we run out of them.
                match = .true.

                do j = 1, size(names)
                    if (j > size(types)) then
                        match = .false.
                    else if (.not. (names(j)%text == types(j)%text .and. len(names(j)%text) == len(types(j)%text))) then
                        match = .false.
                    end if
                end do

                ! If the element is an initial segment of TYPES, MATCH is
                ! true now.
                if (match) some_match = .true.
            end associate
        end do
    end function is_warning_of_names_suppressed

    ! Whether the type TYPES (IS_LIST false: its one name) matches an
    ! element of SUPPRESS.  Not a port.
    logical function is_type_suppressed(types, is_list, suppress)
        type(string_type), intent(in) :: types(:)
        logical, intent(in) :: is_list
        type(warning_types_type), intent(in) :: suppress

        if (is_list) then
            is_type_suppressed = is_warning_suppressed(types, suppress)
        else
            is_type_suppressed = is_warning_suppressed(types(1)%text, suppress)
        end if
    end function is_type_suppressed

    ! ------------------------------------------------------------------
    ! Displaying warnings
    ! ------------------------------------------------------------------

    !> Display a warning message, MESSAGE.
    !> TYPE is the warning type: a group name, such as "holidays" (one
    !> name; `display_warning_of_names' takes a list of names, whose
    !> first element is a group name; the rest of the names represent
    !> subcategories, for warning purposes only, and you can use whatever
    !> names you like).
    !>
    !> LEVEL should be either WARNING_LEVEL_DEBUG, WARNING_LEVEL_WARNING, WARNING_LEVEL_ERROR,
    !> or WARNING_LEVEL_EMERGENCY (but see `warning_minimum_level' and
    !> `warning_minimum_log_level').  Default is WARNING_LEVEL_WARNING.
    !>
    !>     WARNING_LEVEL_EMERGENCY -- a problem that will seriously impair Fortmacs operation soon
    !>                          if you do not attend to it promptly.
    !>     WARNING_LEVEL_ERROR     -- data or circumstances that are inherently wrong.
    !>     WARNING_LEVEL_WARNING   -- data or circumstances that are not inherently wrong,
    !>                          but raise suspicion of a possible problem.
    !>     WARNING_LEVEL_DEBUG     -- info for debugging only.
    !>
    !> BUFFER_NAME, if specified, is the name of the buffer for logging
    !> the warning.  By default, it is `*Warnings*'.  If this function
    !> has to create the buffer, it disables undo in the buffer.
    !>
    !> See also `warning_series', `warning_prefix_function',
    !> `warning_fill_prefix', and `warning_fill_column' for additional
    !> programming features.  STATUS reports an error of what it runs.
    !> Port of warnings.el:display-warning.
    recursive subroutine display_warning_of_name(type, message, level, buffer_name, status)
        character(len=*), intent(in) :: type, message
        integer, intent(in), optional :: level
        character(len=*), intent(in), optional :: buffer_name
        type(status_type), intent(out), optional :: status
        type(string_type), allocatable :: types(:)
        type(status_type) :: st

        call append_string(types, type)
        call display_warning_internal(types, .false., message, level, buffer_name, st)
        if (present(status)) status = st
    end subroutine display_warning_of_name

    !> Display a warning message, MESSAGE, as `display_warning_of_name'
    !> does, of the type TYPES: a list of names whose first element is a
    !> group name.  Port of warnings.el:display-warning.
    recursive subroutine display_warning_of_names(types, message, level, buffer_name, status)
        type(string_type), intent(in) :: types(:)
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: level
        character(len=*), intent(in), optional :: buffer_name
        type(status_type), intent(out), optional :: status
        type(status_type) :: st

        if (size(types) == 0) then
            call set_error(st, WRONG_TYPE_ARGUMENT_CODE, "expected a warning type, got no names")
        else
            call display_warning_internal(types, .true., message, level, buffer_name, st)
        end if

        if (present(status)) status = st
    end subroutine display_warning_of_names

    ! What display_warning does for the type TYPES (its one name when
    ! IS_LIST is false).  Port of warnings.el:display-warning.
    recursive subroutine display_warning_internal(types, is_list, message, level, buffer_name, status)
        type(string_type), intent(in) :: types(:)
        logical, intent(in) :: is_list
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: level
        character(len=*), intent(in), optional :: buffer_name
        type(status_type), intent(out) :: status
        type(handle_type) :: old, buffer, saved_buffer, series_buffer
        type(warning_level_type) :: entry
        type(value_type) :: saved_inhibit
        type(status_type) :: st
        character(len=:), allocatable :: name, type_string, text
        integer :: level_value, k

        status = status_type()

        ! Ensure warnings that happen early in the startup sequence are
        ! visible when startup completes (bug#20792).
        if (.not. allocated(after_initialization_time)) then
            if (.not. is_type_suppressed(types, is_list, warning_inhibit_types)) then
                call push_delayed_warning(types, is_list, message, level, buffer_name)
            end if

            return
        end if

        level_value = WARNING_LEVEL_WARNING
        if (present(level)) level_value = level
        name = "*Warnings*"
        if (present(buffer_name)) name = buffer_name
        if (warning_numeric_level(level_value) < warning_numeric_level(minimum_level(warning_minimum_log_level))) return
        if (is_type_suppressed(types, is_list, warning_inhibit_types)) return
        if (is_type_suppressed(types, is_list, warning_suppress_log_types)) return
        old = get_buffer(name)
        buffer = old

        if (buffer%index == 0) then
            call get_or_make_buffer(name, buffer, status)
            if (.not. status%ok()) return
        end if

        k = findloc(warning_levels%level, level_value, dim=1)
        entry%string = ""
        if (k > 0) entry = warning_levels(k)
        saved_buffer = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return

        ! If we created the buffer, disable undo.
        if (old%index == 0) then
            call special_mode(status)
            buffers(current)%read_only = .true.
            buffers(current)%undo_enabled = .false.
        end if

        if (status%ok()) then
            call goto_character(point_maximum())
            call begin_series(status)
        end if

        if (status%ok()) then
            call inhibit_read_only_begin(saved_inhibit)
            call insert_warning(status)
            call inhibit_read_only_end(saved_inhibit)
        end if

        if (status%ok() .and. warning_series%marker%index /= 0) then
            call marker_buffer(warning_series%marker, series_buffer, st)
            if (same_handle(series_buffer, buffer)) call goto_series_start()
        end if

        if (is_buffer_live(saved_buffer)) call set_buffer(saved_buffer, st)
        if (.not. status%ok()) return

        if (associated(entry%procedure)) then
            call entry%procedure(status)
            if (.not. status%ok()) return
        end if

        ! Interactively, decide whether the warning merits immediate
        ! display.
        if (warning_numeric_level(level_value) < warning_numeric_level(minimum_level(warning_minimum_level))) return
        if (is_type_suppressed(types, is_list, warning_suppress_types)) return
        call display_warning_buffer(buffer, status)

    contains

        ! A warning series starting now: its marker at point, after
        ! calling the procedure of `warning_series'.
        recursive subroutine begin_series(status)
            type(status_type), intent(out) :: status
            type(handle_type) :: marker
            procedure(command_procedure), pointer :: series_procedure

            status = status_type()
            if (warning_series%marker%index /= 0) return
            if (.not. (warning_series%value .or. associated(warning_series%procedure))) return
            call point_marker(marker)
            series_procedure => warning_series%procedure

            if (associated(series_procedure)) then
                call series_procedure(status)

                if (.not. status%ok()) then
                    call destroy_marker(marker, st)
                    return
                end if
            end if

            warning_series = .false.
            warning_series%marker = marker
        end subroutine begin_series

        ! Insert the warning at point, from START to the end of its line.
        recursive subroutine insert_warning(status)
            type(status_type), intent(out) :: status
            character(len=:), allocatable :: saved_prefix
            integer(int64) :: start, saved_column
            logical :: had_prefix

            status = status_type()

            if (.not. is_beginning_of_line()) then
                call newline(status)
                if (.not. status%ok()) return
            end if

            start = point()
            if (associated(warning_prefix_function)) call warning_prefix_function(level_value, entry)
            call write_with_type(warning_type_format, types(1)%text, type_string, status)
            if (.not. status%ok()) return
            call write_with_type(entry%string, type_string, text, status)
            if (.not. status%ok()) return
            call insert(text//message, status)
            if (.not. status%ok()) return
            call newline(status)
            if (.not. status%ok()) return
            if (.not. allocated(warning_fill_prefix)) return
            if (index(message, achar(10)) > 0) return
            saved_column = fill_column%value
            had_prefix = allocated(fill_prefix%value)
            if (had_prefix) saved_prefix = fill_prefix%value
            fill_prefix%value = warning_fill_prefix
            fill_column%value = warning_fill_column
            call fill_region(start, point(), status)
            fill_column%value = saved_column

            if (had_prefix) then
                fill_prefix%value = saved_prefix
            else
                deallocate (fill_prefix%value)
            end if
        end subroutine insert_warning

        ! Go to the start of the current series.
        subroutine goto_series_start()
            integer(int64) :: position

            call marker_position(warning_series%marker, position, st)
            if (st%ok()) call goto_character(position)
        end subroutine goto_series_start
    end subroutine display_warning_internal

    ! Show BUFFER, the warnings buffer, as `warning_display_at_bottom'
    ! says.  Port of warnings.el:warning--display-buffer (a frame popped
    ! up for `pop_up_frames' and the `category' entry are Deferred).
    recursive subroutine display_warning_buffer(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(display_action_parameters_type) :: table
        type(handle_type) :: window, series_buffer, old_window, old_buffer
        type(status_type) :: st
        integer(int64) :: position, shortage

        if (warning_display_at_bottom) then
            ! display-buffer--maybe-at-bottom with the window height of
            ! fit-window-to-buffer at most 10 lines.
            table%window_height%procedure => fit_warning_window
            call display_buffer(buffer, window, status, &
                                actions=[DISPLAY_ACTION_MAYBE_SAME_WINDOW, DISPLAY_ACTION_REUSE_WINDOW, DISPLAY_ACTION_AT_BOTTOM], &
                                table=table)
        else
            call display_buffer(buffer, window, status)
        end if

        if (.not. status%ok()) return

        if (is_window_live(window) .and. warning_series%marker%index /= 0) then
            call marker_buffer(warning_series%marker, series_buffer, st)

            if (same_handle(series_buffer, buffer)) then
                call marker_position(warning_series%marker, position, st)
                if (st%ok()) call set_window_start(windows(window%index), position, st)
            end if
        end if

        if (is_window_live(window) .and. warning_display_at_bottom) then
            ! with-selected-window
            old_window = selected_window()
            old_buffer = current_buffer()
            call select_window(window, status, norecord=.true.)

            if (status%ok()) then
                call goto_character(point_maximum())
                shortage = forward_line(-1_int64)
                call recenter(prefix_argument_type(PREFIX_NUMBER, -1_int64), .false., status)
            end if

            if (is_window_live(old_window)) call select_window(old_window, st, norecord=.true.)
            if (is_buffer_live(old_buffer)) call set_buffer(old_buffer, st)
            if (.not. status%ok()) return
        end if

        call sit_for(0)
    end subroutine display_warning_buffer

    ! The window height of the warnings buffer's window: fit to its buffer,
    ! at most 10 lines.  Port of the window-height entry of
    ! warnings.el:warning--display-buffer.
    subroutine fit_warning_window(window)
        type(handle_type), intent(in) :: window

        call fit_window_to_buffer(window, maximum_height=10)
    end subroutine fit_warning_window

    ! The level (WARNING_LEVEL_*) that SETTING, `warning_minimum_level' or
    ! `warning_minimum_log_level', names (WARNING_MINIMUM_*).  Not a port.
    integer function minimum_level(setting)
        type(choice_setting_type), intent(in) :: setting

        select case (setting%choice)
        case (WARNING_MINIMUM_EMERGENCY)
            minimum_level = WARNING_LEVEL_EMERGENCY
        case (WARNING_MINIMUM_ERROR)
            minimum_level = WARNING_LEVEL_ERROR
        case (WARNING_MINIMUM_DEBUG)
            minimum_level = WARNING_LEVEL_DEBUG
        case default
            minimum_level = WARNING_LEVEL_WARNING
        end select
    end function minimum_level

    ! Upstream's `ding' as the procedure of WARNING_LEVEL_EMERGENCY.  Not a port.
    subroutine ding_adapter(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call ding(status=status)
    end subroutine ding_adapter

    ! Write ITEM with FORMAT, a Fortran format with an `a' edit
    ! descriptor for it (or none: then ITEM is left out), into TEXT.
    ! Not a port (upstream formats the type into the string).
    subroutine write_with_type(format, item, text, status)
        character(len=*), intent(in) :: format, item
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        ! The end of the written text, a character no format writes.
        character(len=*), parameter :: SENTINEL = achar(1)
        character(len=:), allocatable :: marked, buffer
        character(len=256) :: message
        integer :: last, k, written

        status = status_type()
        text = ""
        last = index(format, ")", back=.true.)

        if (last == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Not a Fortran format: "//format)
            return
        end if

        marked = format(1:last - 1)//", """//SENTINEL//""")"
        allocate (character(len=len(format) + len(item) + 256) :: buffer)
        write (buffer, marked, iostat=written, iomsg=message) item
        ! A format without an edit descriptor for ITEM.
        if (written /= 0) write (buffer, marked, iostat=written, iomsg=message)

        if (written /= 0) then
            call set_error(status, GENERIC_ERROR_CODE, trim(message))
            return
        end if

        k = index(buffer, SENTINEL)

        if (k == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Not a Fortran format: "//format)
            return
        end if

        text = buffer(1:k - 1)
    end subroutine write_with_type

    !> Display a warning message, MESSAGE (finished text).
    !> This is equivalent to `display_warning', using
    !> "fortmacs" as the type and WARNING_LEVEL_WARNING as the level.  Port of
    !> warnings.el:warn (the message is built by the caller, with an
    !> internal write, its literal parts passed through `substitute_quotes' as
    !> upstream's format-message does).
    recursive subroutine warn(message, status)
        character(len=*), intent(in) :: message
        type(status_type), intent(out), optional :: status
        type(status_type) :: st

        call display_warning("fortmacs", message, status=st)
        if (present(status)) status = st
    end subroutine warn

    ! ------------------------------------------------------------------
    ! Delayed warnings
    ! ------------------------------------------------------------------

    !> Display a delayed warning.
    !> Aside from going through `delayed_warnings', this is equivalent
    !> to `display_warning'.  Port of subr.el:delay-warning.
    subroutine delay_warning_of_name(type, message, level, buffer_name)
        character(len=*), intent(in) :: type, message
        integer, intent(in), optional :: level
        character(len=*), intent(in), optional :: buffer_name
        type(string_type), allocatable :: types(:)

        call append_string(types, type)
        call push_delayed_warning(types, .false., message, level, buffer_name)
    end subroutine delay_warning_of_name

    !> `delay_warning' with the warning type TYPES, a list of names.
    !> Port of subr.el:delay-warning.
    subroutine delay_warning_of_names(types, message, level, buffer_name)
        type(string_type), intent(in) :: types(:)
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: level
        character(len=*), intent(in), optional :: buffer_name

        call push_delayed_warning(types, .true., message, level, buffer_name)
    end subroutine delay_warning_of_names

    ! Put the warning at the front of `delayed_warnings'.  Port of
    ! subr.el:delay-warning.
    subroutine push_delayed_warning(types, is_list, message, level, buffer_name)
        type(string_type), intent(in) :: types(:)
        logical, intent(in) :: is_list
        character(len=*), intent(in) :: message
        integer, intent(in), optional :: level
        character(len=*), intent(in), optional :: buffer_name
        type(delayed_warning_type) :: warning

        warning%types = types
        warning%is_list = is_list
        warning%message = message
        if (present(level)) warning%level = level
        if (present(buffer_name)) warning%buffer_name = buffer_name
        if (.not. allocated(delayed_warnings)) allocate (delayed_warnings(0))
        delayed_warnings = [warning, delayed_warnings]
    end subroutine push_delayed_warning

    !> Display delayed warnings from `delayed_warnings'.
    !> Used from `delayed_warnings_hook' (which see).  Port of
    !> subr.el:display-delayed-warnings.
    recursive subroutine display_delayed_warnings(status)
        type(status_type), intent(out) :: status
        type(delayed_warning_type), allocatable :: pending(:)
        integer :: k

        status = status_type()
        if (.not. allocated(delayed_warnings)) allocate (delayed_warnings(0))
        pending = delayed_warnings

        ! The oldest first.
        do k = size(pending), 1, -1
            associate (w => pending(k))
                if (w%level /= 0 .and. allocated(w%buffer_name)) then
                    call display_warning_internal(w%types, w%is_list, w%message, w%level, w%buffer_name, status)
                else if (w%level /= 0) then
                    call display_warning_internal(w%types, w%is_list, w%message, w%level, status=status)
                else if (allocated(w%buffer_name)) then
                    call display_warning_internal(w%types, w%is_list, w%message, buffer_name=w%buffer_name, status=status)
                else
                    call display_warning_internal(w%types, w%is_list, w%message, status=status)
                end if
            end associate

            if (.not. status%ok()) return
        end do

        deallocate (delayed_warnings)
        allocate (delayed_warnings(0))
    end subroutine display_delayed_warnings

    !> Remove duplicates from `delayed_warnings'.
    !> Collapse identical adjacent warnings into one (plus count).
    !> Used from `delayed_warnings_hook' (which see).  Port of
    !> subr.el:collapse-delayed-warnings.
    subroutine collapse_delayed_warnings(status)
        type(status_type), intent(out) :: status
        type(delayed_warning_type), allocatable :: collapsed(:)
        type(delayed_warning_type) :: warning
        character(len=40) :: count_text
        integer :: k, count

        status = status_type()
        if (.not. allocated(delayed_warnings)) allocate (delayed_warnings(0))
        allocate (collapsed(0))
        count = 1

        do k = 1, size(delayed_warnings)
            warning = delayed_warnings(k)

            if (k < size(delayed_warnings)) then
                if (is_same_delayed_warning(warning, delayed_warnings(k + 1))) then
                    count = count + 1
                    cycle
                end if
            end if

            if (count > 1) then
                write (count_text, "(a, i0, a)") " [", count, " times]"
                warning%message = warning%message//trim(count_text)
                count = 1
            end if

            collapsed = [collapsed, warning]
        end do

        call move_alloc(collapsed, delayed_warnings)
    end subroutine collapse_delayed_warnings

    ! Whether the delayed warnings A and B are `equal'.  Not a port.
    logical function is_same_delayed_warning(a, b) result(same)
        type(delayed_warning_type), intent(in) :: a, b
        integer :: k

        same = .false.
        if (a%is_list .neqv. b%is_list) return
        if (a%level /= b%level) return
        if (size(a%types) /= size(b%types)) return
        if (.not. is_same_text(a%message, b%message)) return
        if (allocated(a%buffer_name) .neqv. allocated(b%buffer_name)) return

        if (allocated(a%buffer_name)) then
            if (.not. is_same_text(a%buffer_name, b%buffer_name)) return
        end if

        do k = 1, size(a%types)
            if (.not. is_same_text(a%types(k)%text, b%types(k)%text)) return
        end do

        same = .true.
    end function is_same_delayed_warning

    ! Whether the strings A and B are the same, trailing blanks included.
    ! Not a port.
    logical function is_same_text(a, b)
        character(len=*), intent(in) :: a, b

        is_same_text = len(a) == len(b)
        if (is_same_text) is_same_text = a == b
    end function is_same_text

    !> Set up warnings: the entries of `warning_levels', `warning_type_format', and
    !> `delayed_warnings_hook''s functions.  Not a port.
    subroutine initialize_warnings()
        warning_levels(1)%level = WARNING_LEVEL_EMERGENCY
        warning_levels(1)%string = "(""Emergency"", a, "": "")"
        warning_levels(1)%procedure => ding_adapter
        warning_levels(2)%level = WARNING_LEVEL_ERROR
        warning_levels(2)%string = "(""Error"", a, "": "")"
        warning_levels(3)%level = WARNING_LEVEL_WARNING
        warning_levels(3)%string = "(""Warning"", a, "": "")"
        warning_levels(4)%level = WARNING_LEVEL_DEBUG
        warning_levels(4)%string = "(""Debug"", a, "": "")"
        if (.not. allocated(warning_type_format)) warning_type_format = "("" ("", a, "")"")"
        call delayed_warnings_hook%append(collapse_delayed_warnings)
        call delayed_warnings_hook%append(display_delayed_warnings)
    end subroutine initialize_warnings
end module warnings_mod

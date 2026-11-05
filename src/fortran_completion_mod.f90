! fortran_completion_mod.f90 -- completion of components after `%' in Fortran buffers.
!
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

!> Fortran Completion mode, the minor mode `fortran_completion_mode': after
!> a designator followed by `%' (`faces%', `commands%upcase_region%',
!> `sessions(k)%'), `completion_at_point' offers the components and
!> type-bound procedures of its derived type.  Not a port: upstream Emacs
!> has no such mode; it is an added feature, an owner decision (PORTING.md,
!> "Added features").  The mode is off by default; init.f90 turns it on in
!> F90 mode buffers with
!>
!>     call f90_mode_hook%prepend(turn_on_fortran_completion_mode)
!>
!> The derived types come from three places, searched in this order: the
!> current buffer, the project's .f90 files (only when the first name of
!> the designator is not declared in the buffer) and the configuration
!> API's types, a table the build generates (fortran_completion_table_mod).
!> Each buffer and file is parsed once and again only when it changes
!> (fortran_declarations_mod).  What it does not do yet is listed in
!> docs/status/fortran_completion_mod.md.
module fortran_completion_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type
    use value_mod, only: value_type, make_list
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use modes_mod, only: minor_modes
    use buffer_mod, only: buffers, current, current_buffer, buffer_characters_modified_tick, buffer_serial_live
    use editing_functions_mod, only: point, point_minimum, point_maximum, buffer_substring
    use marker_mod, only: character_to_byte
    use minibuffer_reading_tables_mod, only: completion_at_point_function_data_type, completion_list_table
    use minibuffer_mod, only: completion_at_point_functions
    use file_input_output_mod, only: file_attributes
    use system_dependent_mod, only: file_status_type
    use project_mod, only: project_type, get_current_project
    use string_type_mod, only: string_type
    use character_mod, only: downcase_ascii
    use fortran_declarations_mod, only: declarations_type, type_definition_type, parse_declarations, NAME_LENGTH
    use fortran_completion_table_mod, only: load_configuration_declarations
    implicit none(type, external)
    private

    public :: initialize_fortran_completion, fortran_completion_mode, turn_on_fortran_completion_mode, &
              get_component_completion_data

    !> Hook run after entering or leaving `fortran_completion_mode'.  No
    !> problems result if this variable is not bound.  `prepend'
    !> automatically binds it.  (This is true for all hook variables.)
    !> Not a port.
    type(hook_type), target, public :: fortran_completion_mode_hook = hook_type(name="fortran_completion_mode_hook")

    !> The declarations of a buffer as last parsed: the buffer (by serial
    !> number), its text's modification count and accessible portion then.
    type :: buffer_cache_type
        integer(int64) :: serial = 0
        integer(int64) :: tick = -1
        integer(int64) :: beginning = 0, end = 0
        type(declarations_type) :: declarations
    end type buffer_cache_type

    !> The declarations of a file as last parsed, with the file's size and
    !> modification time then (size -1: not read).
    type :: file_cache_type
        character(len=:), allocatable :: file_name
        integer(int64) :: size = -1
        integer(int64) :: modification_seconds = 0
        integer(int32) :: modification_nanoseconds = 0
        type(declarations_type) :: declarations
    end type file_cache_type

    !> The buffers' and the files' declarations,
    !> `buffer_caches(1:buffer_cache_count)' and
    !> `file_caches(1:file_cache_count)'.
    type(buffer_cache_type), allocatable, target :: buffer_caches(:)
    integer :: buffer_cache_count = 0
    type(file_cache_type), allocatable, target :: file_caches(:)
    integer :: file_cache_count = 0

    !> The root of the project whose files were listed last ("" for none),
    !> and the indexes in `file_caches' of its .f90 files.
    character(len=:), allocatable :: listed_root
    integer, allocatable :: project_files(:)

    !> The configuration API's declarations, loaded when first needed.
    type(declarations_type), target :: configuration
    logical :: is_configuration_loaded = .false.

    !> The current buffer's declarations while a completion is computed.
    type(declarations_type), pointer :: buffer_declarations => null()

    !> How far back from point a designator is looked for, in characters.
    integer(int64), parameter :: CONTEXT = 2000
    !> The deepest chain of parent types followed (a cycle ends there).
    integer, parameter :: MAXIMUM_PARENTS = 64

    character(len=*), parameter :: LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    character(len=*), parameter :: NAME_CHARACTERS = LETTERS//"0123456789_"
    character(len=*), parameter :: BLANKS = " "//achar(9)
    character(len=*), parameter :: LF = achar(10)

contains

    !> Define Fortran Completion mode and its command.  Not a port.
    subroutine initialize_fortran_completion()
        call minor_modes%fortran_completion_mode%define("fortran_completion_mode", hook=fortran_completion_mode_hook, &
                                                        body=fortran_completion_mode_body)
        commands%fortran_completion_mode%execute => fortran_completion_mode
    end subroutine initialize_fortran_completion

    !> Toggle completion of components after `%' (Fortran Completion mode).
    !>
    !> When enabled, `completion_at_point' (M-TAB, C-M-i) offers, after a
    !> name followed by `%', the components and type-bound procedures of
    !> the derived type of what precedes the `%': `faces%' offers the
    !> faces, `commands%upcase_region%' the components of a command, and
    !> `sessions(k)%' those of the element type of the array `sessions'.
    !> The types are those defined in the buffer, in the project's .f90
    !> files and in the configuration API.
    !>
    !> To turn it on in every F90 mode buffer, init.f90 says
    !> `call f90_mode_hook%prepend(turn_on_fortran_completion_mode)'.
    !> Not a port.
    recursive subroutine fortran_completion_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%fortran_completion_mode%command(status)
    end subroutine fortran_completion_mode

    !> Turn on Fortran Completion mode (for `f90_mode_hook').  Not a port.
    recursive subroutine turn_on_fortran_completion_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%fortran_completion_mode%enable(status)
    end subroutine turn_on_fortran_completion_mode

    !> The body of Fortran Completion mode: add the completion of
    !> components to the buffer's `completion_at_point_functions' (ENABLE),
    !> or remove it.  Not a port.
    subroutine fortran_completion_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()

        if (enable) then
            call completion_at_point_functions%prepend(get_component_completion_data, local=.true.)
        else
            call completion_at_point_functions%remove(get_component_completion_data, local=.true.)
        end if
    end subroutine fortran_completion_mode_body

    ! ------------------------------------------------------------------
    ! Completion
    ! ------------------------------------------------------------------

    !> The completion data after `%' at point (for
    !> `completion_at_point_functions'): the name being typed after a
    !> designator and `%', completed with the components and type-bound
    !> procedures of the designator's type.  Nothing when point is not
    !> after `%' and a name, or the type is not known.  Not a port.
    recursive subroutine get_component_completion_data(data, status)
        type(completion_at_point_function_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: before, after, type_name
        character(len=NAME_LENGTH), allocatable :: chain(:), candidates(:)
        type(value_type), allocatable :: values(:)
        integer(int64) :: here, start, finish, offset
        integer :: tail, ahead, k

        status = status_type()
        here = point()
        call buffer_substring(max(point_minimum(), here - CONTEXT), here, before, status)
        if (.not. status%ok()) return
        tail = len(before) - verify(before, NAME_CHARACTERS, back=.true.)
        if (tail == len(before)) return
        call designator_names(before(1:len(before) - tail), chain)
        if (size(chain) == 0) return
        start = here - tail
        call buffer_substring(here, min(point_maximum(), here + NAME_LENGTH), after, status)
        if (.not. status%ok()) return
        ahead = verify(after//" ", NAME_CHARACTERS) - 1
        finish = here + ahead
        call read_buffer_declarations(status)
        if (.not. status%ok()) return
        offset = character_to_byte(start) - character_to_byte(point_minimum()) + 1
        call resolve_designator(chain, offset, type_name, status)
        if (.not. status%ok()) return
        if (len(type_name) == 0) return
        call collect_members(type_name, candidates)
        if (size(candidates) == 0) return
        allocate (values(size(candidates)))

        do k = 1, size(candidates)
            values(k) = value_type(text=trim(candidates(k)))
        end do

        data%found = .true.
        data%start = start
        data%end = finish
        data%table = completion_list_table(make_list(values))
    end subroutine get_component_completion_data

    !> The names of the designator TEXT ends with, followed by `%' (and
    !> blanks), first name first: `a%b(k)%' gives `a' and `b'.  Subscripts
    !> and arguments in parentheses are skipped; continuation lines are
    !> joined.  None if TEXT does not end so, or ends in a comment or a
    !> character literal.  Not a port.
    subroutine designator_names(text, chain)
        character(len=*), intent(in) :: text
        character(len=NAME_LENGTH), allocatable, intent(out) :: chain(:)
        character(len=NAME_LENGTH), allocatable :: grown(:)
        character(len=:), allocatable :: line
        integer :: k, first, n

        allocate (chain(0))
        call last_logical_line(text, line)
        if (.not. is_code(line)) return
        k = len_trim(line)
        if (k == 0) return
        if (line(k:k) /= "%") return
        n = 0

        do
            k = skip_blanks_backward(line, k - 1)
            if (k == 0) return

            if (line(k:k) == ")") then
                k = opening_parenthesis(line, k)
                k = skip_blanks_backward(line, k - 1)
                if (k == 0) return
            end if

            first = verify(line(1:k), NAME_CHARACTERS, back=.true.) + 1
            if (first > k) return
            if (verify(line(first:first), LETTERS) /= 0) return
            allocate (grown(n + 1))
            grown(1) = downcase_ascii(line(first:k))
            grown(2:n + 1) = chain(1:n)
            call move_alloc(grown, chain)
            n = n + 1
            k = skip_blanks_backward(line, first - 1)
            if (k == 0) return
            if (line(k:k) /= "%") return
        end do
    end subroutine designator_names

    !> The last line of TEXT, with the lines it continues (ending with
    !> `&') joined to it.  Not a port.
    subroutine last_logical_line(text, line)
        character(len=*), intent(in) :: text
        character(len=:), allocatable, intent(out) :: line
        character(len=:), allocatable :: previous
        integer :: k, j

        k = index(text, LF, back=.true.)
        line = text(k + 1:)

        do while (k > 0)
            j = index(text(1:k - 1), LF, back=.true.)
            previous = trim(text(j + 1:k - 1))
            if (len(previous) == 0) return
            if (previous(len(previous):len(previous)) /= "&") return
            line = adjustl(line)

            if (len(line) > 0) then
                if (line(1:1) == "&") line = line(2:)
            end if

            line = previous(1:len(previous) - 1)//line
            k = j
        end do
    end subroutine last_logical_line

    !> Whether the end of the source line LINE is code (not in a comment
    !> or a character literal).  Not a port.
    logical function is_code(line)
        character(len=*), intent(in) :: line
        character :: delimiter
        integer :: k

        is_code = .false.
        delimiter = " "

        do k = 1, len(line)
            if (delimiter /= " ") then
                if (line(k:k) == delimiter) delimiter = " "
            else if (line(k:k) == "'" .or. line(k:k) == '"') then
                delimiter = line(k:k)
            else if (line(k:k) == "!") then
                return
            end if
        end do

        is_code = delimiter == " "
    end function is_code

    !> The index of the last character of LINE(1:K) that is not a blank, 0
    !> if none.  Not a port.
    integer function skip_blanks_backward(line, k) result(found)
        character(len=*), intent(in) :: line
        integer, intent(in) :: k

        found = 0
        if (k < 1) return
        found = verify(line(1:k), BLANKS, back=.true.)
    end function skip_blanks_backward

    !> The index of the parenthesis that opens the one closing at K in
    !> LINE, 0 if none.  Not a port.
    integer function opening_parenthesis(line, k) result(found)
        character(len=*), intent(in) :: line
        integer, intent(in) :: k
        integer :: depth

        depth = 0

        do found = k, 1, -1
            if (line(found:found) == ")") then
                depth = depth + 1
            else if (line(found:found) == "(") then
                depth = depth - 1
                if (depth == 0) return
            end if
        end do

        found = 0
    end function opening_parenthesis

    !> The derived type of the designator whose names are CHAIN, as seen at
    !> byte offset OFFSET of the current buffer (its declarations read
    !> already): "" if not known.  The project's files are read only when
    !> needed, and listed again once when the type is still not found.
    !> Not a port.
    subroutine resolve_designator(chain, offset, type_name, status)
        character(len=NAME_LENGTH), intent(in) :: chain(:)
        integer(int64), intent(in) :: offset
        character(len=:), allocatable, intent(out) :: type_name
        type(status_type), intent(out) :: status

        status = status_type()
        type_name = ""

        if (buffer_declarations%find_variable(trim(chain(1)), offset) > 0) then
            call follow_chain(chain, offset, .false., type_name)
            if (len(type_name) > 0) return
        end if

        call read_project_declarations(.false., status)
        if (.not. status%ok()) return
        call follow_chain(chain, offset, .true., type_name)
        if (len(type_name) > 0) return
        call read_project_declarations(.true., status)
        if (.not. status%ok()) return
        call follow_chain(chain, offset, .true., type_name)
    end subroutine resolve_designator

    !> The type of the designator CHAIN at byte offset OFFSET: the first
    !> name's declared type, then each component's.  The project's files
    !> are searched when USE_PROJECT.  "" if not known.  Not a port.
    subroutine follow_chain(chain, offset, use_project, type_name)
        character(len=NAME_LENGTH), intent(in) :: chain(:)
        integer(int64), intent(in) :: offset
        logical, intent(in) :: use_project
        character(len=:), allocatable, intent(out) :: type_name
        character(len=:), allocatable :: outer
        integer :: k

        call variable_type(trim(chain(1)), offset, use_project, type_name)

        do k = 2, size(chain)
            if (len(type_name) == 0) return
            outer = type_name
            call component_type(outer, trim(chain(k)), use_project, type_name)
        end do
    end subroutine follow_chain

    !> The declared type of the variable NAME visible at byte offset
    !> OFFSET: in the buffer, then among the module variables of the
    !> project's files (when USE_PROJECT), then of the configuration API.
    !> Not a port.
    subroutine variable_type(name, offset, use_project, type_name)
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: offset
        logical, intent(in) :: use_project
        character(len=:), allocatable, intent(out) :: type_name
        integer :: k, f

        k = buffer_declarations%find_variable(name, offset)

        if (k > 0) then
            type_name = trim(buffer_declarations%variables(k)%type_name)
            return
        end if

        if (use_project .and. allocated(project_files)) then
            do f = 1, size(project_files)
                associate (declarations => file_caches(project_files(f))%declarations)
                    k = declarations%find_variable(name, 0_int64)

                    if (k > 0) then
                        type_name = trim(declarations%variables(k)%type_name)
                        return
                    end if
                end associate
            end do
        end if

        call load_configuration()
        k = configuration%find_variable(name, 0_int64)
        type_name = ""
        if (k > 0) type_name = trim(configuration%variables(k)%type_name)
    end subroutine variable_type

    !> The type of the component COMPONENT of type TYPE_NAME (or of its
    !> parent types), "" if not known or not of derived type.  Not a port.
    subroutine component_type(type_name, component, use_project, result)
        character(len=*), intent(in) :: type_name, component
        logical, intent(in) :: use_project
        character(len=:), allocatable, intent(out) :: result
        type(type_definition_type), pointer :: definition
        integer :: depth, k

        result = ""
        definition => find_definition(type_name, use_project)

        do depth = 1, MAXIMUM_PARENTS
            if (.not. associated(definition)) return

            do k = 1, definition%component_count
                if (definition%components(k)%name == component) then
                    result = trim(definition%components(k)%type_name)
                    return
                end if
            end do

            if (len_trim(definition%parent) == 0) return
            definition => find_definition(trim(definition%parent), use_project)
        end do
    end subroutine component_type

    !> The names of the components and type-bound procedures of type
    !> TYPE_NAME, its own first, then its parent types'.  Not a port.
    subroutine collect_members(type_name, names)
        character(len=*), intent(in) :: type_name
        character(len=NAME_LENGTH), allocatable, intent(out) :: names(:)
        type(type_definition_type), pointer :: definition
        integer :: depth, k, n

        allocate (names(0))
        n = 0
        definition => find_definition(type_name, .true.)

        do depth = 1, MAXIMUM_PARENTS
            if (.not. associated(definition)) exit

            do k = 1, definition%component_count
                call add_name(definition%components(k)%name)
            end do

            do k = 1, definition%binding_count
                call add_name(definition%bindings(k))
            end do

            if (len_trim(definition%parent) == 0) exit
            definition => find_definition(trim(definition%parent), .true.)
        end do

        names = names(1:n)

    contains

        ! Add NAME to NAMES unless it is there already.
        subroutine add_name(name)
            character(len=*), intent(in) :: name
            character(len=NAME_LENGTH), allocatable :: grown(:)

            if (any(names(1:n) == name)) return

            if (n == size(names)) then
                allocate (grown(max(16, 2*n)))
                grown(1:n) = names(1:n)
                call move_alloc(grown, names)
            end if

            n = n + 1
            names(n) = name
        end subroutine add_name
    end subroutine collect_members

    !> The definition of the type NAME: in the buffer, the project's files
    !> (when USE_PROJECT and they were read) or the configuration API;
    !> null if none.  Not a port.
    function find_definition(name, use_project) result(definition)
        character(len=*), intent(in) :: name
        logical, intent(in) :: use_project
        type(type_definition_type), pointer :: definition
        integer :: k, f

        definition => null()
        k = buffer_declarations%find_type(name)

        if (k > 0) then
            definition => buffer_declarations%types(k)
            return
        end if

        if (use_project .and. allocated(project_files)) then
            do f = 1, size(project_files)
                k = file_caches(project_files(f))%declarations%find_type(name)

                if (k > 0) then
                    definition => file_caches(project_files(f))%declarations%types(k)
                    return
                end if
            end do
        end if

        call load_configuration()
        k = configuration%find_type(name)
        if (k > 0) definition => configuration%types(k)
    end function find_definition

    !> Load the configuration API's declarations, once.  Not a port.
    subroutine load_configuration()
        if (is_configuration_loaded) return
        call load_configuration_declarations(configuration)
        is_configuration_loaded = .true.
    end subroutine load_configuration

    ! ------------------------------------------------------------------
    ! Caches
    ! ------------------------------------------------------------------

    !> Point `buffer_declarations' at the current buffer's declarations,
    !> parsing its accessible portion again if it changed.  Not a port.
    subroutine read_buffer_declarations(status)
        type(status_type), intent(out) :: status
        type(buffer_cache_type), allocatable :: grown(:)
        character(len=:), allocatable :: text
        integer(int64) :: serial, tick
        integer :: k, slot

        status = status_type()
        serial = buffers(current)%serial
        call buffer_characters_modified_tick(current_buffer(), tick, status)
        if (.not. status%ok()) return
        slot = 0

        do k = 1, buffer_cache_count
            if (buffer_caches(k)%serial == serial) slot = k
        end do

        if (slot == 0) then
            ! Reuse the entry of a buffer that is gone, else add one.
            do k = 1, buffer_cache_count
                if (.not. buffer_serial_live(buffer_caches(k)%serial)) slot = k
            end do
        end if

        if (slot == 0) then
            if (.not. allocated(buffer_caches)) allocate (buffer_caches(4))

            if (buffer_cache_count == size(buffer_caches)) then
                allocate (grown(2*buffer_cache_count))

                do k = 1, buffer_cache_count
                    grown(k)%serial = buffer_caches(k)%serial
                    grown(k)%tick = buffer_caches(k)%tick
                    grown(k)%beginning = buffer_caches(k)%beginning
                    grown(k)%end = buffer_caches(k)%end
                    call move_declarations(buffer_caches(k)%declarations, grown(k)%declarations)
                end do

                call move_alloc(grown, buffer_caches)
            end if

            buffer_cache_count = buffer_cache_count + 1
            slot = buffer_cache_count
        end if

        associate (entry => buffer_caches(slot))
            if (entry%serial /= serial .or. entry%tick /= tick .or. entry%beginning /= point_minimum() .or. &
                entry%end /= point_maximum()) then
                call buffer_substring(point_minimum(), point_maximum(), text, status)
                if (.not. status%ok()) return
                call parse_declarations(text, entry%declarations)
                entry%serial = serial
                entry%tick = tick
                entry%beginning = point_minimum()
                entry%end = point_maximum()
            end if
        end associate

        buffer_declarations => buffer_caches(slot)%declarations
    end subroutine read_buffer_declarations

    !> Move the arrays of FROM into TO.  Not a port.
    subroutine move_declarations(from, to)
        type(declarations_type), intent(inout) :: from, to

        to%type_count = from%type_count
        to%variable_count = from%variable_count
        if (allocated(from%types)) call move_alloc(from%types, to%types)
        if (allocated(from%variables)) call move_alloc(from%variables, to%variables)
    end subroutine move_declarations

    !> Read the declarations of the current project's .f90 files (other
    !> than the current buffer's own file), each parsed again only if it
    !> changed.  The project's files are listed when the project is not
    !> the one listed last, or when RELIST.  No project: no files.
    !> Not a port.
    subroutine read_project_declarations(relist, status)
        logical, intent(in) :: relist
        type(status_type), intent(out) :: status
        class(project_type), allocatable :: project
        type(string_type), allocatable :: files(:)
        character(len=:), allocatable :: root, own
        integer :: k, n

        call get_current_project(project, status)
        if (.not. status%ok()) return

        if (.not. allocated(project)) then
            listed_root = ""
            if (allocated(project_files)) deallocate (project_files)
            allocate (project_files(0))
            return
        end if

        root = project%root()
        if (.not. allocated(listed_root)) listed_root = ""

        if (relist .or. root /= listed_root .or. .not. allocated(project_files)) then
            call project%list_files(files, status)
            if (.not. status%ok()) return
            own = ""
            if (allocated(buffers(current)%filename)) own = buffers(current)%filename
            if (allocated(project_files)) deallocate (project_files)
            allocate (project_files(size(files)))
            n = 0

            do k = 1, size(files)
                if (.not. is_fortran_file(files(k)%text)) cycle
                if (files(k)%text == own) cycle
                n = n + 1
                call add_file_cache(files(k)%text, project_files(n))
            end do

            project_files = project_files(1:n)
            listed_root = root
        end if

        do k = 1, size(project_files)
            call read_file_declarations(file_caches(project_files(k)), status)
            if (.not. status%ok()) return
        end do
    end subroutine read_project_declarations

    !> Whether FILE_NAME names a free-form Fortran source file (.f90, in
    !> any case).  Not a port.
    logical function is_fortran_file(file_name)
        character(len=*), intent(in) :: file_name

        is_fortran_file = .false.
        if (len(file_name) < 4) return
        is_fortran_file = downcase_ascii(file_name(len(file_name) - 3:)) == ".f90"
    end function is_fortran_file

    !> Add an entry for FILE_NAME to `file_caches' (not read yet) unless
    !> there is one; SLOT is its index.  Not a port.
    subroutine add_file_cache(file_name, slot)
        character(len=*), intent(in) :: file_name
        integer, intent(out) :: slot
        type(file_cache_type), allocatable :: grown(:)
        integer :: k

        do slot = 1, file_cache_count
            if (file_caches(slot)%file_name == file_name) return
        end do

        if (.not. allocated(file_caches)) allocate (file_caches(16))

        if (file_cache_count == size(file_caches)) then
            allocate (grown(2*file_cache_count))

            do k = 1, file_cache_count
                call move_alloc(file_caches(k)%file_name, grown(k)%file_name)
                grown(k)%size = file_caches(k)%size
                grown(k)%modification_seconds = file_caches(k)%modification_seconds
                grown(k)%modification_nanoseconds = file_caches(k)%modification_nanoseconds
                call move_declarations(file_caches(k)%declarations, grown(k)%declarations)
            end do

            call move_alloc(grown, file_caches)
        end if

        file_cache_count = file_cache_count + 1
        slot = file_cache_count
        file_caches(slot)%file_name = file_name
        file_caches(slot)%size = -1
    end subroutine add_file_cache

    !> Parse the file of ENTRY again if its size or modification time
    !> changed since it was last read; a file that cannot be read has no
    !> declarations.  Not a port.
    subroutine read_file_declarations(entry, status)
        type(file_cache_type), intent(inout) :: entry
        type(status_type), intent(out) :: status
        type(file_status_type) :: attributes
        character(len=:), allocatable :: text
        logical :: found
        integer :: unit, iostat

        status = status_type()
        call file_attributes(entry%file_name, attributes, found)

        if (.not. found) then
            entry%size = -1
            entry%declarations = declarations_type()
            return
        end if

        if (entry%size == attributes%size .and. entry%modification_seconds == attributes%mtime_second .and. &
            entry%modification_nanoseconds == attributes%mtime_nsec) return
        entry%size = attributes%size
        entry%modification_seconds = attributes%mtime_second
        entry%modification_nanoseconds = attributes%mtime_nsec
        allocate (character(len=attributes%size) :: text)

        if (attributes%size > 0) then
            open (newunit=unit, file=entry%file_name, access="stream", form="unformatted", action="read", status="old", &
                  iostat=iostat)

            if (iostat /= 0) then
                entry%declarations = declarations_type()
                return
            end if

            read (unit, iostat=iostat) text
            close (unit)

            if (iostat /= 0) then
                entry%declarations = declarations_type()
                return
            end if
        end if

        call parse_declarations(text, entry%declarations)
    end subroutine read_file_declarations
end module fortran_completion_mod

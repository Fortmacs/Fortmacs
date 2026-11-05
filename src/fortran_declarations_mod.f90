! fortran_declarations_mod.f90 -- the derived types and typed variables of Fortran source.
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

!> The derived-type definitions (their components, type-bound procedures
!> and parent types) and the variables declared with a derived type, as
!> `fortran_completion_mode' reads them from Fortran source: the current
!> buffer, the project's .f90 files and the table of the configuration
!> API that the build generates (fortran_completion_table_mod).  Not a
!> port: an added feature (PORTING.md, "Added features").
!>
!> Names are compared in lower case, as Fortran does.  What the parser
!> does not read yet is listed in docs/status/fortran_completion_mod.md.
module fortran_declarations_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use character_mod, only: downcase_ascii
    implicit none(type, external)
    private

    public :: parse_declarations

    !> The longest name a declaration keeps (Fortran 2018 allows 63
    !> characters).
    integer, parameter, public :: NAME_LENGTH = 63

    !> A component of a derived type: a data component or a procedure
    !> pointer component.
    type, public :: type_component_type
        !> The component's name, in lower case.
        character(len=NAME_LENGTH) :: name = ""
        !> The declared type of the component when it is a derived type
        !> (`type(...)' or `class(...)'), in lower case; "" otherwise.
        character(len=NAME_LENGTH) :: type_name = ""
    end type type_component_type

    !> A derived-type definition.
    type, public :: type_definition_type
        !> The type's name, in lower case.
        character(len=NAME_LENGTH) :: name = ""
        !> The parent type (`extends(...)'), "" for none.
        character(len=NAME_LENGTH) :: parent = ""
        !> The components, `components(1:component_count)'.
        type(type_component_type), allocatable :: components(:)
        integer :: component_count = 0
        !> The names of the type-bound procedures and generic bindings,
        !> `bindings(1:binding_count)'.
        character(len=NAME_LENGTH), allocatable :: bindings(:)
        integer :: binding_count = 0
    end type type_definition_type

    !> A variable declared with a derived type.
    type, public :: variable_declaration_type
        !> The variable's name, in lower case.
        character(len=NAME_LENGTH) :: name = ""
        !> Its declared type, in lower case.
        character(len=NAME_LENGTH) :: type_name = ""
        !> The extent of the scoping unit that declares it, as byte
        !> offsets in the parsed text (1-based, `scope_end' past its end
        !> statement).
        integer(int64) :: scope_start = 0, scope_end = 0
        !> Whether it is declared in the specification part of a module,
        !> where other program units reach it by use association.
        logical :: is_module_variable = .false.
    end type variable_declaration_type

    !> The derived-type definitions and the variables declared with a
    !> derived type in a source.
    type, public :: declarations_type
        !> `types(1:type_count)'.
        type(type_definition_type), allocatable :: types(:)
        integer :: type_count = 0
        !> `variables(1:variable_count)'.
        type(variable_declaration_type), allocatable :: variables(:)
        integer :: variable_count = 0
    contains
        !> Add a derived type named NAME with parent type PARENT ("" for
        !> none); the components and bindings added next are its own.
        procedure :: append_type => declarations_append_type
        !> Add a component NAME of derived type TYPE_NAME ("" for another
        !> type) to the type added last.
        procedure :: append_component => declarations_append_component
        !> Add a type-bound procedure or generic binding NAME to the type
        !> added last.
        procedure :: append_binding => declarations_append_binding
        !> Add a module variable NAME declared with type TYPE_NAME.
        procedure :: append_variable => declarations_append_variable
        !> The index of the type named NAME, 0 if none.
        procedure :: find_type => declarations_find_type
        !> The index of the variable named NAME visible at byte offset
        !> POSITION (the innermost scope containing it), or with POSITION
        !> 0 of the module variable named NAME; 0 if none.
        procedure :: find_variable => declarations_find_variable
    end type declarations_type

    !> The scoping units open while parsing: where each began, whether
    !> it is a module, whether its `contains' was seen, and the first
    !> variable it declared.
    type :: scope_type
        integer(int64) :: start = 1
        logical :: is_module = .false.
        logical :: has_contains = .false.
        integer :: first_variable = 1
    end type scope_type

    character(len=*), parameter :: LETTERS = "abcdefghijklmnopqrstuvwxyz"
    character(len=*), parameter :: NAME_CHARACTERS = LETTERS//"0123456789_"
    character(len=*), parameter :: BLANKS = " "//achar(9)
    character(len=*), parameter :: LF = achar(10)

contains

    !> Add a derived type named NAME with parent type PARENT.  Not a port.
    subroutine declarations_append_type(self, name, parent)
        class(declarations_type), intent(inout) :: self
        character(len=*), intent(in) :: name, parent
        type(type_definition_type), allocatable :: grown(:)
        integer :: k

        if (.not. allocated(self%types)) allocate (self%types(16))

        if (self%type_count == size(self%types)) then
            allocate (grown(2*size(self%types)))

            do k = 1, self%type_count
                call move_type(self%types(k), grown(k))
            end do

            call move_alloc(grown, self%types)
        end if

        self%type_count = self%type_count + 1
        self%types(self%type_count)%name = name
        self%types(self%type_count)%parent = parent
        allocate (self%types(self%type_count)%components(8))
        allocate (self%types(self%type_count)%bindings(8))
    end subroutine declarations_append_type

    !> Move the definition FROM into TO (an element of a grown array),
    !> without copying its arrays.  Not a port.
    subroutine move_type(from, to)
        type(type_definition_type), intent(inout) :: from
        type(type_definition_type), intent(inout) :: to

        to%name = from%name
        to%parent = from%parent
        to%component_count = from%component_count
        to%binding_count = from%binding_count
        call move_alloc(from%components, to%components)
        call move_alloc(from%bindings, to%bindings)
    end subroutine move_type

    !> Add a component NAME of derived type TYPE_NAME to the type added
    !> last.  Not a port.
    subroutine declarations_append_component(self, name, type_name)
        class(declarations_type), intent(inout) :: self
        character(len=*), intent(in) :: name, type_name
        type(type_component_type), allocatable :: grown(:)
        integer :: n

        if (self%type_count == 0) return

        associate (definition => self%types(self%type_count))
            n = definition%component_count

            if (n == size(definition%components)) then
                allocate (grown(2*n))
                grown(1:n) = definition%components(1:n)
                call move_alloc(grown, definition%components)
            end if

            definition%component_count = n + 1
            definition%components(n + 1)%name = name
            definition%components(n + 1)%type_name = type_name
        end associate
    end subroutine declarations_append_component

    !> Add a binding NAME to the type added last.  Not a port.
    subroutine declarations_append_binding(self, name)
        class(declarations_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        character(len=NAME_LENGTH), allocatable :: grown(:)
        integer :: n

        if (self%type_count == 0) return

        associate (definition => self%types(self%type_count))
            n = definition%binding_count

            if (n == size(definition%bindings)) then
                allocate (grown(2*n))
                grown(1:n) = definition%bindings(1:n)
                call move_alloc(grown, definition%bindings)
            end if

            definition%binding_count = n + 1
            definition%bindings(n + 1) = name
        end associate
    end subroutine declarations_append_binding

    !> Add a module variable NAME of type TYPE_NAME.  Not a port.
    subroutine declarations_append_variable(self, name, type_name)
        class(declarations_type), intent(inout) :: self
        character(len=*), intent(in) :: name, type_name

        call add_variable(self, name, type_name, 1_int64, huge(1_int64), .true.)
    end subroutine declarations_append_variable

    !> Add the variable NAME of type TYPE_NAME declared in the scope from
    !> SCOPE_START to SCOPE_END.  Not a port.
    subroutine add_variable(self, name, type_name, scope_start, scope_end, is_module_variable)
        class(declarations_type), intent(inout) :: self
        character(len=*), intent(in) :: name, type_name
        integer(int64), intent(in) :: scope_start, scope_end
        logical, intent(in) :: is_module_variable
        type(variable_declaration_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(self%variables)) allocate (self%variables(16))
        n = self%variable_count

        if (n == size(self%variables)) then
            allocate (grown(2*n))
            grown(1:n) = self%variables(1:n)
            call move_alloc(grown, self%variables)
        end if

        self%variable_count = n + 1
        self%variables(n + 1)%name = name
        self%variables(n + 1)%type_name = type_name
        self%variables(n + 1)%scope_start = scope_start
        self%variables(n + 1)%scope_end = scope_end
        self%variables(n + 1)%is_module_variable = is_module_variable
    end subroutine add_variable

    !> The index of the type named NAME, 0 if none.  Not a port.
    integer function declarations_find_type(self, name) result(found)
        class(declarations_type), intent(in) :: self
        character(len=*), intent(in) :: name
        integer :: k

        found = 0

        do k = 1, self%type_count
            if (self%types(k)%name == name) then
                found = k
                return
            end if
        end do
    end function declarations_find_type

    !> The index of the variable named NAME visible at byte offset
    !> POSITION, or of the module variable NAME when POSITION is 0; 0 if
    !> none.  Not a port.
    integer function declarations_find_variable(self, name, position) result(found)
        class(declarations_type), intent(in) :: self
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: position
        integer(int64) :: innermost
        integer :: k

        found = 0
        innermost = 0

        do k = 1, self%variable_count
            if (self%variables(k)%name /= name) cycle

            if (position == 0) then
                if (self%variables(k)%is_module_variable) then
                    found = k
                    return
                end if
            else if (self%variables(k)%scope_start <= position .and. position < self%variables(k)%scope_end) then
                if (self%variables(k)%scope_start > innermost) then
                    innermost = self%variables(k)%scope_start
                    found = k
                end if
            end if
        end do
    end function declarations_find_variable

    ! ------------------------------------------------------------------
    ! Parsing
    ! ------------------------------------------------------------------

    !> Read the derived-type definitions and the variables declared with a
    !> derived type in the free-form Fortran source TEXT into
    !> DECLARATIONS.  Continuation lines are joined and comments dropped;
    !> program units, subprograms and interface bodies are the scopes of
    !> the variables they declare.  Not a port.
    subroutine parse_declarations(text, declarations)
        character(len=*), intent(in) :: text
        type(declarations_type), intent(out) :: declarations
        character(len=:), allocatable :: statement, line
        type(scope_type), allocatable :: scopes(:)
        integer(int64) :: line_start, line_end, statement_start
        integer :: depth, current_type, interface_depth, k
        logical :: in_bindings, continued

        allocate (scopes(32))
        depth = 0
        current_type = 0
        interface_depth = 0
        in_bindings = .false.
        statement = ""
        statement_start = 1
        continued = .false.
        line_start = 1

        do while (line_start <= len(text, int64))
            line_end = index(text(line_start:), LF, kind=int64)

            if (line_end == 0) then
                line_end = len(text, int64) + 1
            else
                line_end = line_start + line_end - 1
            end if

            line = downcase_ascii(text(line_start:line_end - 1))
            call strip_comment(line)
            line = trim(adjustl(line))

            if (continued) then
                if (len(line) > 0) then
                    if (line(1:1) == "&") line = line(2:)
                end if
            else
                statement = ""
                statement_start = line_start
            end if

            continued = .false.

            if (len(line) > 0) then
                if (line(len(line):len(line)) == "&") then
                    line = line(1:len(line) - 1)
                    continued = .true.
                end if
            end if

            statement = statement//line
            if (.not. continued) call split_statement(statement, statement_start)
            line_start = line_end + 1
        end do

        if (continued) call split_statement(statement, statement_start)

        ! Scopes still open at the end extend past it, so that point at the
        ! end of the text sees their variables.
        do while (depth > 0)
            call close_scope(len(text, int64) + 2)
        end do

        ! Variables declared outside any program unit (a fragment).
        do k = 1, declarations%variable_count
            if (declarations%variables(k)%scope_end == 0) declarations%variables(k)%scope_end = len(text, int64) + 2
        end do

    contains

        ! Process each statement of STATEMENTS (separated by semicolons),
        ! which begins at byte offset START.
        subroutine split_statement(statements, start)
            character(len=*), intent(in) :: statements
            integer(int64), intent(in) :: start
            integer :: first, k
            character :: delimiter

            first = 1
            delimiter = " "

            do k = 1, len(statements)
                if (delimiter /= " ") then
                    if (statements(k:k) == delimiter) delimiter = " "
                else if (statements(k:k) == "'" .or. statements(k:k) == '"') then
                    delimiter = statements(k:k)
                else if (statements(k:k) == ";") then
                    call process_statement(trim(adjustl(statements(first:k - 1))), start)
                    first = k + 1
                end if
            end do

            call process_statement(trim(adjustl(statements(first:))), start)
        end subroutine split_statement

        ! Read one statement S (in lower case, without comments) beginning
        ! at byte offset START.
        subroutine process_statement(s, start)
            character(len=*), intent(in) :: s
            integer(int64), intent(in) :: start
            character(len=:), allocatable :: word, second, rest

            if (len(s) == 0) return
            word = leading_name(s)
            rest = trim(adjustl(s(len(word) + 1:)))
            second = leading_name(rest)

            if (len(word) >= 3) then
                if (word(1:3) == "end") then
                    call process_end(s)
                    return
                end if
            end if

            if (s == "contains") then
                if (current_type > 0) then
                    in_bindings = .true.
                else if (depth > 0) then
                    scopes(depth)%has_contains = .true.
                end if

                return
            end if

            if (word == "interface" .or. (word == "abstract" .and. second == "interface")) then
                interface_depth = interface_depth + 1
                return
            end if

            if (current_type > 0) then
                call process_type_statement(s, word)
                return
            end if

            if (word == "type" .and. is_type_definition(rest)) then
                call begin_type(s)
                return
            end if

            if (word == "module" .and. len(second) > 0 .and. second /= "subroutine" .and. second /= "function" .and. &
                second /= "procedure" .and. is_prefix_free(rest, second)) then
                call open_scope(start, .true.)
                return
            end if

            if (word == "module" .and. second == "procedure") then
                ! A separate module subprogram (in a submodule) is a scope;
                ! a `module procedure' list of a generic interface is not.
                if (interface_depth == 0) call open_scope(start, .false.)
                return
            end if

            if (word == "submodule" .or. word == "program") then
                call open_scope(start, .false.)
                return
            end if

            if (is_subprogram_statement(s)) then
                call open_scope(start, .false.)
                return
            end if

            if (word == "type" .or. word == "class") call process_variables(s)
        end subroutine process_statement

        ! An `end' statement S.
        subroutine process_end(s)
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: rest, what

            rest = trim(adjustl(s(4:)))

            if (len(rest) > 0) then
                ! `endif', `end_position = 1': not an end statement of a
                ! scope.
                if (verify(rest(1:1), LETTERS) /= 0) return
            end if

            what = leading_name(rest)

            if (what == "type" .and. current_type > 0) then
                current_type = 0
                in_bindings = .false.
            else if (what == "interface") then
                interface_depth = max(0, interface_depth - 1)
            else if (any(what == [character(len=10) :: "", "subroutine", "function", "module", "program", "submodule", &
                                  "procedure"])) then
                ! The scope extends to the end of the line that ends it.
                call close_scope(line_end + 1)
            end if
        end subroutine process_end

        ! A statement S inside the definition of a derived type whose
        ! first word is WORD.
        subroutine process_type_statement(s, word)
            character(len=*), intent(in) :: s, word

            if (in_bindings) then
                if (word == "procedure") then
                    call read_bindings(s, .false.)
                else if (word == "generic") then
                    call read_bindings(s, .true.)
                end if
            else
                call process_components(s, word)
            end if
        end subroutine process_type_statement

        ! The derived-type definition statement S.
        subroutine begin_type(s)
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: attributes, type_name, parent
            integer :: colons, k, opening

            colons = top_level_index(s, "::")

            if (colons > 0) then
                attributes = s(5:colons - 1)
                type_name = leading_name(trim(adjustl(s(colons + 2:))))
            else
                attributes = ""
                type_name = leading_name(trim(adjustl(s(5:))))
            end if

            if (len(type_name) == 0) return
            parent = ""
            k = index(attributes, "extends")

            if (k > 0) then
                opening = index(attributes(k:), "(")
                if (opening > 0) parent = leading_name(trim(adjustl(attributes(k + opening:))))
            end if

            call declarations%append_type(type_name, parent)
            current_type = declarations%type_count
            in_bindings = .false.
        end subroutine begin_type

        ! The component definition statement S of the type being defined.
        subroutine process_components(s, word)
            character(len=*), intent(in) :: s, word
            character(len=:), allocatable :: type_name
            character(len=NAME_LENGTH), allocatable :: names(:)
            logical :: is_declaration
            integer :: k

            if (word == "procedure") then
                is_declaration = top_level_index(s, "::") > 0
                type_name = ""
            else
                call declared_type(s, type_name, is_declaration)
            end if

            if (.not. is_declaration) return
            call entity_names(s, names)

            do k = 1, size(names)
                call declarations%append_component(trim(names(k)), type_name)
            end do
        end subroutine process_components

        ! The type-bound procedure statement or (GENERIC) generic binding
        ! S of the type being defined.
        subroutine read_bindings(s, generic)
            character(len=*), intent(in) :: s
            logical, intent(in) :: generic
            character(len=NAME_LENGTH), allocatable :: names(:)
            character(len=:), allocatable :: list
            integer :: colons, k, arrow

            colons = top_level_index(s, "::")

            if (colons > 0) then
                list = s(colons + 2:)
            else
                ! `procedure name' or `procedure(interface) name'.
                list = trim(adjustl(s(10:)))

                if (len(list) > 0) then
                    if (list(1:1) == "(") then
                        k = matching_parenthesis(list, 1)
                        list = list(k + 1:)
                    end if
                end if
            end if

            if (generic) then
                ! `generic :: name => specific, ...': one binding.
                arrow = index(list, "=>")
                if (arrow > 0) list = list(1:arrow - 1)
                if (index(list, "(") > 0) return
                call declarations%append_binding(leading_name(trim(adjustl(list))))
                return
            end if

            call split_names(list, names)

            do k = 1, size(names)
                if (len_trim(names(k)) > 0) call declarations%append_binding(trim(names(k)))
            end do
        end subroutine read_bindings

        ! The type declaration statement S (outside a type definition):
        ! its variables of derived type.
        subroutine process_variables(s)
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: type_name
            character(len=NAME_LENGTH), allocatable :: names(:)
            logical :: is_declaration, is_module_variable
            integer(int64) :: scope_start
            integer :: k

            call declared_type(s, type_name, is_declaration)
            if (.not. is_declaration .or. len(type_name) == 0) return
            call entity_names(s, names)
            scope_start = 1
            is_module_variable = .false.

            if (depth > 0) then
                scope_start = scopes(depth)%start
                is_module_variable = scopes(depth)%is_module .and. .not. scopes(depth)%has_contains .and. &
                                     interface_depth == 0
            end if

            do k = 1, size(names)
                call add_variable(declarations, trim(names(k)), type_name, scope_start, 0_int64, is_module_variable)
            end do
        end subroutine process_variables

        ! Open a scope beginning at byte offset START.
        subroutine open_scope(start, is_module)
            integer(int64), intent(in) :: start
            logical, intent(in) :: is_module
            type(scope_type), allocatable :: grown(:)

            if (depth == size(scopes)) then
                allocate (grown(2*depth))
                grown(1:depth) = scopes(1:depth)
                call move_alloc(grown, scopes)
            end if

            depth = depth + 1
            scopes(depth)%start = start
            scopes(depth)%is_module = is_module
            scopes(depth)%has_contains = .false.
            scopes(depth)%first_variable = declarations%variable_count + 1
        end subroutine open_scope

        ! Close the innermost scope at byte offset FINISH: its variables
        ! are visible up to there.
        subroutine close_scope(finish)
            integer(int64), intent(in) :: finish
            integer :: k

            if (depth == 0) return

            do k = scopes(depth)%first_variable, declarations%variable_count
                if (declarations%variables(k)%scope_end == 0) declarations%variables(k)%scope_end = finish
            end do

            depth = depth - 1
        end subroutine close_scope
    end subroutine parse_declarations

    !> Drop the comment of the source line LINE (a `!' outside a
    !> character literal and what follows).  Not a port.
    subroutine strip_comment(line)
        character(len=:), allocatable, intent(inout) :: line
        character :: delimiter
        integer :: k

        delimiter = " "

        do k = 1, len(line)
            if (delimiter /= " ") then
                if (line(k:k) == delimiter) delimiter = " "
            else if (line(k:k) == "'" .or. line(k:k) == '"') then
                delimiter = line(k:k)
            else if (line(k:k) == "!") then
                line = line(1:k - 1)
                return
            end if
        end do
    end subroutine strip_comment

    !> The name S begins with ("" if it does not begin with a letter).
    !> Not a port.
    function leading_name(s) result(name)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: name
        integer :: k

        name = ""
        if (len(s) == 0) return
        if (verify(s(1:1), LETTERS) /= 0) return
        k = verify(s, NAME_CHARACTERS)

        if (k == 0) then
            name = s
        else
            name = s(1:k - 1)
        end if
    end function leading_name

    !> Whether REST, what follows `type', makes a derived-type definition
    !> statement (not `type(...)' or `type is (...)').  Not a port.
    logical function is_type_definition(rest)
        character(len=*), intent(in) :: rest

        is_type_definition = .false.
        if (len(rest) == 0) return
        if (rest(1:1) == "(") return
        if (leading_name(rest) == "is" .and. index(rest, "(") > 0) return
        is_type_definition = .true.
    end function is_type_definition

    !> Whether REST, what follows `module', is the name SECOND of a module
    !> alone (not a separate module subprogram's prefix).  Not a port.
    logical function is_prefix_free(rest, second)
        character(len=*), intent(in) :: rest, second

        is_prefix_free = len_trim(rest) == len(second)
    end function is_prefix_free

    !> Whether S is a subroutine or function statement: prefixes (`pure',
    !> `recursive', a type with its parameters...), then `subroutine' or
    !> `function'.  Not a port.
    logical function is_subprogram_statement(s)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: word
        integer :: k

        is_subprogram_statement = .false.
        k = 1

        do while (k <= len(s))
            word = leading_name(s(k:))

            if (word == "subroutine" .or. word == "function") then
                is_subprogram_statement = len(leading_name(adjustl(s(k + len(word):)))) > 0
                return
            end if

            if (.not. any(word == [character(len=15) :: "recursive", "pure", "elemental", "impure", "module", &
                                   "non_recursive", "integer", "real", "logical", "complex", "character", "double", &
                                   "precision", "doubleprecision", "type", "class"])) return
            k = k + len(word)
            k = k + verify(s(k:)//"x", BLANKS) - 1
            if (k > len(s)) return

            if (s(k:k) == "(") then
                k = matching_parenthesis(s, k) + 1
            else if (s(k:k) == "*") then
                k = k + verify(s(k + 1:)//"x", "0123456789 ")
            else if ((word == "type" .or. word == "class") .and. verify(s(k:k), LETTERS) == 0) then
                ! `type name': a derived-type definition, not a prefix.
                return
            end if

            if (k > len(s)) return
            k = k + verify(s(k:)//"x", BLANKS) - 1
        end do
    end function is_subprogram_statement

    !> The type of the type declaration statement S: TYPE_NAME is the
    !> derived type of `type(...)' or `class(...)' ("" for an intrinsic
    !> type or `class(*)'), IS_DECLARATION whether S declares entities at
    !> all (a type specification followed by `::').  Not a port.
    subroutine declared_type(s, type_name, is_declaration)
        character(len=*), intent(in) :: s
        character(len=:), allocatable, intent(out) :: type_name
        logical, intent(out) :: is_declaration
        character(len=:), allocatable :: word
        integer :: k, closing

        type_name = ""
        is_declaration = .false.
        if (top_level_index(s, "::") == 0) return
        word = leading_name(s)
        k = len(word) + 1
        if (k <= len(s)) k = k + verify(s(k:)//"x", BLANKS) - 1

        if (word == "type" .or. word == "class") then
            if (k > len(s)) return
            if (s(k:k) /= "(") return
            closing = matching_parenthesis(s, k)
            type_name = leading_name(trim(adjustl(s(k + 1:closing - 1))))
            is_declaration = .true.
        else if (any(word == [character(len=15) :: "integer", "real", "logical", "character", "complex", "double", &
                              "doubleprecision"])) then
            if (k <= len(s)) then
                is_declaration = scan(s(k:k), "(,:*") > 0 .or. word == "double"
            end if
        end if
    end subroutine declared_type

    !> The names of the entities the declaration statement S declares
    !> (after its `::'), without their array specifications and
    !> initializations.  Not a port.
    subroutine entity_names(s, names)
        character(len=*), intent(in) :: s
        character(len=NAME_LENGTH), allocatable, intent(out) :: names(:)
        integer :: colons

        colons = top_level_index(s, "::")

        if (colons == 0) then
            allocate (names(0))
            return
        end if

        call split_names(s(colons + 2:), names)
    end subroutine entity_names

    !> The names of the comma-separated items of LIST (each item's leading
    !> name; commas inside parentheses, brackets or character literals do
    !> not separate items).  Not a port.
    subroutine split_names(list, names)
        character(len=*), intent(in) :: list
        character(len=NAME_LENGTH), allocatable, intent(out) :: names(:)
        character(len=NAME_LENGTH), allocatable :: grown(:)
        character(len=:), allocatable :: item
        integer :: k, first, depth, n
        character :: delimiter

        allocate (names(8))
        n = 0
        first = 1
        depth = 0
        delimiter = " "

        do k = 1, len(list) + 1
            if (k <= len(list)) then
                if (delimiter /= " ") then
                    if (list(k:k) == delimiter) delimiter = " "
                    cycle
                end if

                select case (list(k:k))
                case ("'", '"')
                    delimiter = list(k:k)
                    cycle
                case ("(", "[")
                    depth = depth + 1
                    cycle
                case (")", "]")
                    depth = depth - 1
                    cycle
                case (",")
                    if (depth > 0) cycle
                case default
                    cycle
                end select
            end if

            item = leading_name(trim(adjustl(list(first:k - 1))))
            first = k + 1
            if (len(item) == 0) cycle

            if (n == size(names)) then
                allocate (grown(2*n))
                grown(1:n) = names(1:n)
                call move_alloc(grown, names)
            end if

            n = n + 1
            names(n) = item
        end do

        names = names(1:n)
    end subroutine split_names

    !> The index in S of PATTERN outside parentheses and character
    !> literals, 0 if none.  Not a port.
    integer function top_level_index(s, pattern) result(found)
        character(len=*), intent(in) :: s, pattern
        integer :: k, depth
        character :: delimiter

        found = 0
        depth = 0
        delimiter = " "

        do k = 1, len(s) - len(pattern) + 1
            if (delimiter /= " ") then
                if (s(k:k) == delimiter) delimiter = " "
            else if (s(k:k) == "'" .or. s(k:k) == '"') then
                delimiter = s(k:k)
            else if (s(k:k) == "(") then
                depth = depth + 1
            else if (s(k:k) == ")") then
                depth = depth - 1
            else if (depth == 0) then
                if (s(k:k + len(pattern) - 1) == pattern) then
                    found = k
                    return
                end if
            end if
        end do
    end function top_level_index

    !> The index of the parenthesis closing the one at OPENING in S (past the
    !> end of S if it is not closed).  Not a port.
    integer function matching_parenthesis(s, opening) result(closing)
        character(len=*), intent(in) :: s
        integer, intent(in) :: opening
        integer :: depth
        character :: delimiter

        depth = 0
        delimiter = " "

        do closing = opening, len(s)
            if (delimiter /= " ") then
                if (s(closing:closing) == delimiter) delimiter = " "
            else if (s(closing:closing) == "'" .or. s(closing:closing) == '"') then
                delimiter = s(closing:closing)
            else if (s(closing:closing) == "(") then
                depth = depth + 1
            else if (s(closing:closing) == ")") then
                depth = depth - 1
                if (depth == 0) return
            end if
        end do

        closing = len(s) + 1
    end function matching_parenthesis
end module fortran_declarations_mod

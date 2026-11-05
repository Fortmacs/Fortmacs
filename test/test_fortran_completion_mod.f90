! test_fortran_completion_mod.f90 -- tests for Fortran Completion mode (fortran_completion_mod).
! Own tests; fortran_completion_mode is an added feature with no upstream tests.
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

!> Tests of `fortran_completion_mode': off by default, and the components
!> it offers after `%' for the configuration API's objects, for types
!> defined in the buffer, through nested components, array elements and
!> parent types, and for variables of the same name in different scopes.
module test_fortran_completion_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, buffer_substring
    use bindings_mod, only: initialize_bindings
    use value_mod, only: value_type, list_elements, xstring
    use modes_mod, only: minor_modes
    use minibuffer_reading_mod, only: all_completions
    use minibuffer_reading_tables_mod, only: completion_at_point_function_data_type
    use minibuffer_mod, only: completion_at_point_functions
    use f90_mod, only: f90_mode
    use fortran_completion_mod, only: get_component_completion_data, turn_on_fortran_completion_mode, &
                                      fortran_completion_mode
    implicit none(type, external)
    private

    public :: collect_fortran_completion_mod

    character(len=*), parameter :: LF = achar(10)

    !> A module defining types, for the tests of types defined in the
    !> buffer.
    character(len=*), parameter :: SOURCE = &
                                   "module shapes_mod"//LF// &
                                   "    implicit none"//LF// &
                                   "    type, public :: point_type"//LF// &
                                   "        real :: x = 0, y = 0"//LF// &
                                   "    contains"//LF// &
                                   "        procedure :: norm => point_norm"//LF// &
                                   "    end type point_type"//LF// &
                                   "    type, extends(point_type) :: point_3d_type"//LF// &
                                   "        real :: z"//LF// &
                                   "    end type"//LF// &
                                   "    type :: session_type"//LF// &
                                   "        type(point_type) :: corner"//LF// &
                                   "        character(len=8) :: label = ""a, b"""//LF// &
                                   "        procedure(), pointer, nopass :: callback => null()"//LF// &
                                   "    end type session_type"//LF// &
                                   "    type(session_type), allocatable :: sessions(:)"//LF// &
                                   "contains"//LF// &
                                   "    subroutine first()"//LF// &
                                   "        type(point_3d_type) :: p"//LF// &
                                   "    end subroutine first"//LF// &
                                   "    subroutine second(p, &"//LF// &
                                   "                      k)"//LF// &
                                   "        type(session_type), intent(in) :: p ! a comment"//LF// &
                                   "        integer, intent(in) :: k"//LF

contains

    !> Collect all exported unit tests.
    subroutine collect_fortran_completion_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("fortran-completion-off-by-default", test_off_by_default), &
                    new_unittest("fortran-completion-configuration", test_configuration), &
                    new_unittest("fortran-completion-buffer-type", test_buffer_type), &
                    new_unittest("fortran-completion-nested", test_nested), &
                    new_unittest("fortran-completion-scopes", test_scopes), &
                    new_unittest("fortran-completion-not-after-percent", test_not_after_percent) &
                    ]
    end subroutine collect_fortran_completion_mod

    !> A fresh buffer in F90 mode holding TEXT, point at its end, in the
    !> root directory (in no project).
    subroutine f90_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call initialize_bindings()
        call get_or_make_buffer(" *fortran-completion-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        buffers(h%index)%directory = "/"
        call f90_mode(st)
        call insert(text, st)
    end subroutine f90_buffer

    !> The candidates completing the name before point, sorted as given.
    function completions() result(names)
        character(len=64), allocatable :: names(:)
        type(completion_at_point_function_data_type) :: data
        type(status_type) :: st
        type(value_type) :: v
        type(value_type), allocatable :: elements(:)
        character(len=:), allocatable :: typed
        integer :: k

        allocate (names(0))
        call get_component_completion_data(data, st)
        if (.not. st%ok() .or. .not. data%found) return
        call buffer_substring(data%start, point(), typed, st)
        call all_completions(typed, data%table, v, st)
        call list_elements(v, elements)
        deallocate (names)
        allocate (names(size(elements)))

        do k = 1, size(elements)
            names(k) = xstring(elements(k))
        end do
    end function completions

    !> The mode is off in a new F90 mode buffer, and adds its completion to
    !> the buffer's `completion_at_point_functions' only while it is on.
    subroutine test_off_by_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call f90_buffer("", h)
        call check(error,.not. minor_modes%fortran_completion_mode%enabled(), "off by default")
        if (allocated(error)) return
        call check(error,.not. completion_at_point_functions%member(get_component_completion_data), &
                   "no completion while off")
        if (allocated(error)) return
        call turn_on_fortran_completion_mode(st)
        call check(error, minor_modes%fortran_completion_mode%enabled(), "turned on")
        if (allocated(error)) return
        call check(error, completion_at_point_functions%member(get_component_completion_data), "completion while on")
        if (allocated(error)) return
        call fortran_completion_mode(st)
        call check(error,.not. minor_modes%fortran_completion_mode%enabled(), "toggled off")
        if (allocated(error)) return
        call check(error,.not. completion_at_point_functions%member(get_component_completion_data), &
                   "no completion once off")
        call close_buffer(h, st)
    end subroutine test_off_by_default

    !> The configuration API's objects: commands, faces, hooks and
    !> settings, and the components of a command.
    subroutine test_configuration(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=64), allocatable :: names(:)

        call f90_buffer("commands%upcase_reg", h)
        names = completions()
        call check(error, any(names == "upcase_region") .and. .not. any(names == "downcase_region"), &
                   "commands%upcase_region")
        if (allocated(error)) return
        call insert("ion%disab", st)
        names = completions()
        call check(error, any(names == "disabled"), "commands%upcase_region%disabled")
        if (allocated(error)) return
        call insert(LF//"faces%region%back", st)
        names = completions()
        call check(error, any(names == "background"), "faces%region%background")
        if (allocated(error)) return
        call insert(LF//"call text_mode_hook%", st)
        names = completions()
        call check(error, any(names == "prepend") .and. any(names == "append"), "text_mode_hook%prepend")
        if (allocated(error)) return
        call insert(LF//"x = COMMANDS % upcase_region %", st)
        names = completions()
        call check(error, any(names == "disabled"), "case and blanks")
        call close_buffer(h, st)
    end subroutine test_configuration

    !> A type defined in the buffer, and a type extending it.
    subroutine test_buffer_type(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=64), allocatable :: names(:)

        call f90_buffer(SOURCE//"    end subroutine second"//LF//"    subroutine third()"//LF// &
                        "        type(point_type) :: origin"//LF//"        origin%", h)
        names = completions()
        call check(error, size(names) == 3, "three members")
        if (allocated(error)) return
        call check(error, any(names == "x") .and. any(names == "y") .and. any(names == "norm"), &
                   "origin%x, y, norm")
        if (allocated(error)) return
        call insert(LF//"    end subroutine third"//LF//"    subroutine fourth()"//LF// &
                    "        type(point_3d_type) :: corner"//LF//"        corner%", st)
        names = completions()
        call check(error, size(names) == 4, "four members")
        if (allocated(error)) return
        call check(error, any(names == "z") .and. any(names == "x") .and. any(names == "norm"), &
                   "corner%z and the parent's")
        call close_buffer(h, st)
    end subroutine test_buffer_type

    !> Nested components and array elements.
    subroutine test_nested(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=64), allocatable :: names(:)

        call f90_buffer(SOURCE//"        sessions(k + 1)%", h)
        names = completions()
        call check(error, size(names) == 3, "three components")
        if (allocated(error)) return
        call check(error, any(names == "corner") .and. any(names == "label") .and. any(names == "callback"), &
                   "sessions(k + 1)%corner, label, callback")
        if (allocated(error)) return
        call insert("corner%n", st)
        names = completions()
        call check(error, size(names) == 1, "one match")
        if (allocated(error)) return
        call check(error, names(1) == "norm", "sessions(k + 1)%corner%norm")
        if (allocated(error)) return
        call insert(LF//"        call show(sessions(size(sessions))% &"//LF//"            & corner%", st)
        names = completions()
        call check(error, any(names == "x"), "across a continuation line")
        call close_buffer(h, st)
    end subroutine test_nested

    !> The variable visible at point: `p' is a point_3d_type in `first'
    !> and a session_type in `second'.
    subroutine test_scopes(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=64), allocatable :: names(:)

        call f90_buffer(SOURCE//"        p%", h)
        names = completions()
        call check(error, any(names == "corner") .and. .not. any(names == "z"), "p in second")
        call close_buffer(h, st)
    end subroutine test_scopes

    !> Nothing to complete without `%', in a comment, or for an unknown
    !> name.
    subroutine test_not_after_percent(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call f90_buffer("x = comm", h)
        call check(error, size(completions()) == 0, "no %")
        if (allocated(error)) return
        call insert(LF//"! commands%", st)
        call check(error, size(completions()) == 0, "in a comment")
        if (allocated(error)) return
        call insert(LF//"no_such_object%", st)
        call check(error, size(completions()) == 0, "unknown name")
        call close_buffer(h, st)
    end subroutine test_not_after_percent
end module test_fortran_completion_mod

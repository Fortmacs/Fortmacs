! test_conf_mode_fm.f90 -- tests for Conf mode (conf_mode_fm).
! Ports GNU Emacs 31.1 test/lisp/textmodes/conf-mode-tests.el.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/textmodes/conf-mode-tests.el (all its tests), plus
!> the guessing of `conf_mode' and auto-mode-alist entries.
module test_conf_mode_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, symbols, xsymbol, list_elements, vtag, V_NULL, V_NAME, V_ARRAY
    use lread_fm, only: sym
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, goto_char, buffer_string
    use cmds_fm, only: forward_line, forward_char, beginning_of_line
    use search_fm, only: search_forward, re_search_forward, SEARCH_NOERROR
    use textprop_fm, only: get_text_property
    use bindings_fm, only: init_bindings
    use modes_fm, only: minor_modes
    use font_lock_fm, only: font_lock_ensure
    use files_fm, only: set_auto_mode
    use conf_mode_fm, only: conf_mode, conf_unix_mode, conf_windows_mode, conf_javaprop_mode, conf_space_mode, &
                            conf_colon_mode, conf_ppd_mode, conf_xdefaults_mode, conf_toml_mode, conf_desktop_mode, &
                            conf_align_assignments
    implicit none(type, external)
    private

    public :: collect_conf_mode_fm

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

    abstract interface
        subroutine mode_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine mode_proc
    end interface

contains

    !> Collect all exported unit tests.
    subroutine collect_conf_mode_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("conf-test-align-assignments", test_align), &
                    new_unittest("conf-test-font-lock", test_font_lock), &
                    new_unittest("conf-test-windows-mode", test_windows), &
                    new_unittest("conf-test-javaprop-mode", test_javaprop), &
                    new_unittest("conf-test-space-mode", test_space), &
                    new_unittest("conf-test-colon-mode", test_colon), &
                    new_unittest("conf-test-ppd-mode", test_ppd), &
                    new_unittest("conf-test-xdefaults-mode", test_xdefaults), &
                    new_unittest("conf-test-toml-mode", test_toml), &
                    new_unittest("conf-test-toml-mode/boolean", test_toml_boolean), &
                    new_unittest("conf-test-desktop-mode", test_desktop), &
                    new_unittest("conf-mode-guess", test_guess), &
                    new_unittest("conf-auto-mode-alist", test_auto_mode) &
                    ]
    end subroutine collect_conf_mode_fm

    !> A fresh buffer holding TEXT (with-temp-buffer, insert).
    subroutine temp_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *conf-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
    end subroutine temp_buffer

    !> TEXT in a fresh buffer, point at its start, in MODE, fontified.
    subroutine fontified(text, mode, h)
        character(len=*), intent(in) :: text
        procedure(mode_proc) :: mode
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call temp_buffer(text, h)
        call goto_char(point_min())
        call mode(st)
        call minor_modes%font_lock_mode%enable(st)
        call font_lock_ensure(st)
    end subroutine fontified

    !> The face at point (face-at-point: the first of a list), "" if none.
    function face() result(name)
        character(len=:), allocatable :: name
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st

        name = ""
        call get_text_property(point(), sym("face"), v, st)

        if (vtag(v) == V_ARRAY) then
            call list_elements(v, elts)
            v = elts(1)
        end if

        if (.not. allocated(v%a)) return
        if ((vtag(v) == V_NAME .or. vtag(v) == V_NULL)) name = symbols(xsymbol(v))%name
    end function face

    !> (search-forward S)
    subroutine find(s)
        character(len=*), intent(in) :: s
        integer(int64) :: found
        type(status_type) :: st

        call search_forward(s, found, st)
    end subroutine find

    subroutine move(n)
        integer(int64), intent(in) :: n
        type(status_type) :: st

        call forward_char(st, n)
    end subroutine move

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call kill_buffer(h, st)
    end subroutine leave

    subroutine test_align(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("foo: bar"//LF//"bar: baz", h)
        call conf_colon_mode(st)
        call conf_align_assignments(st)
        call check(error, buffer_string(), "foo:"//TAB//TAB//TAB//"bar"//LF//"bar:"//TAB//TAB//TAB//"baz")
        call leave(h)
    end subroutine test_align

    subroutine test_font_lock(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call fontified("foo: bar"//LF//"bar: baz", conf_colon_mode, h)

        checks: block
            call check(error, face(), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            call find("bar")
            call check(error, face(), "")
        end block checks

        call leave(h)
    end subroutine test_font_lock

    subroutine test_windows(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: shortage
        character(len=*), parameter :: G = "{5984FFE0-28D4-11CF-AE66-08002B2E1262}"

        ! from `conf_windows_mode' docstring:
        call fontified("[ExtShellFolderViews]"//LF//"Default="//G//LF//G//"="//G//LF//LF//"["//G//"]"//LF// &
                       "PersistMoniker=file://Folder.htt", conf_windows_mode, h)

        checks: block
            call find("ExtShell")
            call check(error, face(), "font_lock_type_face")
            if (allocated(error)) exit checks
            call find("Defau")
            call check(error, face(), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            shortage = forward_line(1_int64)
            call beginning_of_line(1_int64)
            call check(error, face(), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            shortage = forward_line(2_int64)
            call check(error, face(), "")
            if (allocated(error)) exit checks
            call move(1_int64)
            call check(error, face(), "font_lock_type_face")
        end block checks

        call leave(h)
    end subroutine test_windows

    subroutine test_javaprop(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: found
        type(status_type) :: st

        ! From `conf_javaprop_mode' docstring
        call fontified("# comment"//LF//LF//"name:value"//LF//"name=value"//LF//"name value"//LF//"x.1 ="//LF// &
                       "x.2.y.1.z.1 ="//LF//"x.2.y.1.z.2.zz =", conf_javaprop_mode, h)

        checks: block
            call check(error, face(), "font_lock_comment_delimiter_face")
            if (allocated(error)) exit checks
            call move(3_int64)
            call check(error, face(), "font_lock_comment_face")
            if (allocated(error)) exit checks

            do
                call search_forward("nam", found, st, noerror=SEARCH_NOERROR)
                if (found <= 0) exit
                call check(error, face(), "font_lock_variable_name_face")
                if (allocated(error)) exit checks
                call find("val")
                call check(error, face(), "")
                if (allocated(error)) exit checks
            end do

            do
                call re_search_forward("[xyz]", found, st, noerror=SEARCH_NOERROR)
                if (found <= 0) exit
                call move(-1_int64)
                call check(error, face(), "font_lock_variable_name_face")
                if (allocated(error)) exit checks
                call move(1_int64)
                call re_search_forward("[0-9]", found, st, noerror=SEARCH_NOERROR)

                if (found > 0) then
                    call move(-1_int64)
                    call check(error, face(), "font_lock_constant_face")
                    if (allocated(error)) exit checks
                end if
            end do
        end block checks

        call leave(h)
    end subroutine test_javaprop

    subroutine test_space(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! From `conf_space_mode' docstring.
        call fontified("image/jpeg"//TAB//TAB//TAB//"jpeg jpg jpe"//LF//"image/png"//TAB//TAB//TAB//"png"//LF// &
                       "image/tiff"//TAB//TAB//TAB//"tiff tif"//LF, conf_space_mode, h)

        checks: block
            call check(error, face(), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            call move(15_int64)
            call check(error, face(), "")
        end block checks

        call leave(h)
    end subroutine test_space

    subroutine test_colon(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: shortage

        ! From `conf_colon_mode' docstring.
        call fontified("<Multi_key> <exclam> <exclam>"//TAB//TAB//": ""\241"""//TAB//"exclamdown"//LF// &
                       "<Multi_key> <c> <slash>"//TAB//TAB//TAB//": ""\242"""//TAB//"cent", conf_colon_mode, h)

        checks: block
            call check(error, face(), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            call find("24")
            call check(error, face(), "font_lock_string_face")
            if (allocated(error)) exit checks
            shortage = forward_line(1_int64)
            call check(error, face(), "font_lock_variable_name_face")
        end block checks

        call leave(h)
    end subroutine test_colon

    subroutine test_ppd(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! From `conf_ppd_mode' docstring.
        call fontified("*DefaultTransfer: Null"//LF//"*Transfer Null.Inverse: ""{ 1 exch sub }""", conf_ppd_mode, h)

        checks: block
            call check(error, face(), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            call find("Nul")
            call check(error, face(), "")
        end block checks

        call leave(h)
    end subroutine test_ppd

    subroutine test_xdefaults(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! From `conf_xdefaults_mode' docstring.
        call fontified("*background:"//TAB//TAB//TAB//"gray99"//LF//"*foreground:"//TAB//TAB//TAB//"black", &
                       conf_xdefaults_mode, h)

        checks: block
            call check(error, face(), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            call find("gray")
            call check(error, face(), "")
        end block checks

        call leave(h)
    end subroutine test_xdefaults

    subroutine test_toml(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: shortage

        call fontified("[entry]"//LF//"value = ""some string""", conf_toml_mode, h)

        checks: block
            call check(error, face(), "")
            if (allocated(error)) exit checks
            call move(1_int64)
            call check(error, face(), "font_lock_type_face")
            if (allocated(error)) exit checks
            shortage = forward_line(1_int64)
            call check(error, face(), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            call find("som")
            call check(error, face(), "font_lock_string_face")
        end block checks

        call leave(h)
    end subroutine test_toml

    subroutine test_toml_boolean(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! https://toml.io/en/v1.0.0#boolean
        call fontified("[entry]"//LF//"a = true"//LF//"b = True", conf_toml_mode, h)

        checks: block
            call find("tru")
            call check(error, face(), "font_lock_keyword_face")
            if (allocated(error)) exit checks
            ! Do not fontify upper-case "True".
            call find("Tru")
            call check(error, face(), "")
        end block checks

        call leave(h)
    end subroutine test_toml_boolean

    subroutine test_desktop(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! From `conf_desktop_mode' docstring.
        call fontified(TAB//"[Desktop Entry]"//LF//TAB//"Name=GNU Image Manipulation Program"//LF//TAB// &
                       "Name[oc]=Editor d'imatge GIMP"//LF//TAB//"Exec=gimp-2.8 %U"//LF//TAB//"Terminal=false", &
                       conf_desktop_mode, h)

        checks: block
            call find("Desk")
            call check(error, face(), "font_lock_type_face")
            if (allocated(error)) exit checks
            call find("Nam")
            call check(error, face(), "font_lock_variable_name_face")
            if (allocated(error)) exit checks
            call move(2_int64)
            call check(error, face(), "")
            if (allocated(error)) exit checks
            call find("[")
            call check(error, face(), "font_lock_constant_face")
        end block checks

        call leave(h)
    end subroutine test_desktop

    !> `conf_mode' called directly runs the sub-mode the contents suggest.
    subroutine test_guess(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("; comment"//LF//"a=b"//LF, h)
        call conf_mode(st)
        call check(error, buffers(cur)%major_mode, "conf_windows_mode")

        if (allocated(error)) then
            call leave(h)
            return
        end if

        call erase_buffer(st)
        call insert("a: b"//LF//"c: d"//LF, st)
        call conf_mode(st)
        call check(error, buffers(cur)%major_mode, "conf_colon_mode")

        if (allocated(error)) then
            call leave(h)
            return
        end if

        call erase_buffer(st)
        call conf_mode(st)
        call check(error, buffers(cur)%major_mode, "conf_unix_mode")
        call leave(h)
    end subroutine test_guess

    !> auto-mode-alist selects the Conf modes, stripping version suffixes.
    subroutine test_auto_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer("", h)

        checks: block
            buffers(cur)%filename = "/tmp/x/Cargo.toml"
            call set_auto_mode(st)
            call check(error, buffers(cur)%major_mode, "conf_toml_mode")
            if (allocated(error)) exit checks
            buffers(cur)%filename = "/tmp/x/app.properties.orig"
            call set_auto_mode(st)
            call check(error, buffers(cur)%major_mode, "conf_javaprop_mode")
            if (allocated(error)) exit checks
            buffers(cur)%filename = "/tmp/x/sshd.conf"
            call set_auto_mode(st)
            call check(error, buffers(cur)%major_mode, "conf_unix_mode")
        end block checks

        deallocate (buffers(cur)%filename)
        call leave(h)
    end subroutine test_auto_mode
end module test_conf_mode_fm

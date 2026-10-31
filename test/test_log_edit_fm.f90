! test_log_edit_fm.f90 -- tests for log_edit_fm.
! Ports GNU Emacs 31.1 test/lisp/vc/log-edit-tests.el.
!
! Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

!> The log-edit-tests.el tests: `log_edit_fill_entry' and
!> `log_edit_done_strip_cvs_lines' as a `log_edit_done_hook'.
module test_log_edit_fm
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, erase_buffer, fill_column
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string, goto_char, point_min
    use bindings_fm, only: init_bindings
    use vc_hooks_fm, only: register_vc_backend
    use log_edit_fm, only: log_edit_fill_entry, log_edit_done, log_edit_done_hook, log_edit_done_strip_cvs_lines, &
                           set_log_edit_callback, set_log_edit_vc_backend
    implicit none(type, external)
    private

    public :: collect_log_edit_fm

    character(len=*), parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_log_edit_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("log-edit-fill-entry", test_fill_entry), &
                    new_unittest("log-edit-fill-entry-indented-func-entries", test_indented), &
                    new_unittest("log-edit-fill-entry-trailing-prose", test_trailing_prose), &
                    new_unittest("log-edit-fill-entry-joining", test_joining), &
                    new_unittest("log-edit-fill-entry-confinement", test_confinement), &
                    new_unittest("log-edit-fill-entry-space-substitution", test_space_substitution), &
                    new_unittest("log-edit-fill-entry-initial-wrapping", test_initial_wrapping), &
                    new_unittest("log-edit-fill-entry-no-defun-list-wrapping", test_no_subprogram_list_wrapping), &
                    new_unittest("log-edit-done-strip-cvs-lines-cvs", test_strip_cvs), &
                    new_unittest("log-edit-done-strip-cvs-lines-non-cvs", test_strip_non_cvs), &
                    new_unittest("log-edit-done-strip-cvs-lines-only-cvs-colon-blank", test_strip_colon_blank), &
                    new_unittest("log-edit-done-strip-cvs-lines-only-cvs-colon", test_strip_colon) &
                    ]
    end subroutine collect_log_edit_fm

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> (with-temp-buffer (insert TEXT)).
    subroutine temp_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *log-edit-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
    end subroutine temp_buffer

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        fill_column = 70
        call kill_buffer(h, st)
    end subroutine leave

    !> (let ((fill-column COLUMN)) (log-edit-fill-entry)), then check the
    !> buffer is WANTED.
    subroutine fill_and_check(error, column, wanted)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: column
        character(len=*), intent(in) :: wanted
        type(status_type) :: st
        logical :: done

        fill_column = column
        call log_edit_fill_entry(st, done)
        fill_column = 70
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string() == wanted, "got:"//LF//buffer_string()//LF//"wanted:"//LF//wanted)
    end subroutine fill_and_check

    ! ------------------------------------------------------------------
    ! log-edit-fill-entry
    ! ------------------------------------------------------------------

    subroutine test_fill_entry(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call temp_buffer("* dir/file.ext (fun1):"//LF// &
                         "(fun2):"//LF// &
                         "(fun3):"//LF// &
                         "* file2.txt (fun4):"//LF// &
                         "(fun5):"//LF// &
                         "(fun6):"//LF// &
                         "(fun7): Some prose."//LF// &
                         "(fun8): A longer description of a complicated change."// &
                         "  Spread over a couple of sentences."// &
                         "  Long enough to be filled for several lines."//LF// &
                         "(fun9): Etc.", h)
        call goto_char(point_min())
        call fill_and_check(error, 72, &
                            "* dir/file.ext (fun1, fun2, fun3):"//LF// &
                            "* file2.txt (fun4, fun5, fun6, fun7): Some prose."//LF// &
                            "(fun8): A longer description of a complicated change.  Spread over a"//LF// &
                            "couple of sentences.  Long enough to be filled for several lines."//LF// &
                            "(fun9): Etc.")

        if (.not. allocated(error)) then
            call fill_and_check(error, 20, &
                                "* dir/file.ext (fun1)"//LF// &
                                "(fun2, fun3):"//LF// &
                                "* file2.txt (fun4)"//LF// &
                                "(fun5, fun6, fun7):"//LF// &
                                "Some prose."//LF// &
                                "(fun8): A longer"//LF// &
                                "description of a"//LF// &
                                "complicated change."//LF// &
                                "Spread over a couple"//LF// &
                                "of sentences.  Long"//LF// &
                                "enough to be filled"//LF// &
                                "for several lines."//LF// &
                                "(fun9): Etc.")
        end if

        if (.not. allocated(error)) then
            call fill_and_check(error, 40, &
                                "* dir/file.ext (fun1, fun2, fun3):"//LF// &
                                "* file2.txt (fun4, fun5, fun6, fun7):"//LF// &
                                "Some prose."//LF// &
                                "(fun8): A longer description of a"//LF// &
                                "complicated change.  Spread over a"//LF// &
                                "couple of sentences.  Long enough to be"//LF// &
                                "filled for several lines."//LF// &
                                "(fun9): Etc.")
        end if

        call leave(h)
    end subroutine test_fill_entry

    subroutine test_indented(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! Indenting function entries is a typical mistake caused by using a
        ! misconfigured or non-ChangeLog specific fill function.
        call temp_buffer("* dir/file.ext (fun1):"//LF// &
                         "  (fun2):"//LF// &
                         "  (fun3):"//LF// &
                         "* file2.txt (fun4):"//LF// &
                         "  (fun5):"//LF// &
                         "  (fun6):"//LF// &
                         "  (fun7): Some prose."//LF// &
                         "  (fun8): A longer description of a complicated change."// &
                         "  Spread over a couple of sentences."// &
                         "  Long enough to be filled for several lines."//LF// &
                         "  (fun9): Etc.", h)
        call goto_char(point_min())
        call fill_and_check(error, 72, &
                            "* dir/file.ext (fun1, fun2, fun3):"//LF// &
                            "* file2.txt (fun4, fun5, fun6, fun7): Some prose."//LF// &
                            "(fun8): A longer description of a complicated change.  Spread over a"//LF// &
                            "couple of sentences.  Long enough to be filled for several lines."//LF// &
                            "(fun9): Etc.")
        call leave(h)
    end subroutine test_indented

    subroutine test_trailing_prose(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call temp_buffer("* dir/file.ext (fun1): A longer description of a complicated change."// &
                         "  Spread over a couple of sentences."// &
                         "  Long enough to be filled for several lines.", h)
        call fill_and_check(error, 72, &
                            "* dir/file.ext (fun1): A longer description of a complicated change."//LF// &
                            "Spread over a couple of sentences.  Long enough to be filled for several"//LF// &
                            "lines.")
        call leave(h)
    end subroutine test_trailing_prose

    subroutine test_joining(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! Join short enough function names on the same line.
        call temp_buffer("* dir/file.ext (fun1):"//LF//"(fun2):", h)
        call fill_and_check(error, 72, "* dir/file.ext (fun1, fun2):")
        call leave(h)
        if (allocated(error)) return
        ! Don't combine them if they're too long.
        call temp_buffer("* dir/long-file-name.ext (a-really-long-function-name):"//LF// &
                         "(another-very-long-function-name):", h)
        call fill_and_check(error, 72, "* dir/long-file-name.ext (a-really-long-function-name)"//LF// &
                            "(another-very-long-function-name):")
        call leave(h)
        if (allocated(error)) return
        ! Put function name on next line, if the file name is too long.
        call temp_buffer("* a-very-long-directory-name/another-long-directory-name/and-a-long-file-name.ext"// &
                         " (a-really-long-function-name):", h)
        call fill_and_check(error, 72, &
                            "* a-very-long-directory-name/another-long-directory-name/and-a-long-file-name.ext"//LF// &
                            "(a-really-long-function-name):")
        call leave(h)
    end subroutine test_joining

    subroutine test_confinement(error)
        type(error_type), allocatable, intent(out) :: error
        ! This entry is precisely 65 columns in length;
        ! log-edit-fill-column should leave it unmodified.
        character(len=*), parameter :: string = &
                                       "* file2.txt (fun4, fun5, fun6, fun7, fun8, fun9, fun10, fun1134):"
        ! This entry is 66 columns in length, and must be filled.
        character(len=*), parameter :: string1 = &
                                       "* file2.txt (fun4, fun5, fun6, fun7, fun8, fun9, fun10, fun11345):"
        ! The first line of this entry totals 65 columns in length,
        ! and should be preserved intact.
        character(len=*), parameter :: string2 = &
                                       "* file2.txt (fun4, fun5, fun6, fun7, fun8, fun9, fun10, fun11345)"//LF// &
                                       "(fun11356):"
        ! The first subprogram in this entry is a file name that brings
        ! the total to 40 columns in length and should be preserved
        ! intact.
        character(len=*), parameter :: string3 = "* file2.txt (abcdefghijklmnopqrstuvwxyz)"//LF//"(ABC):"
        ! The first subprogram brings that total to 41, and should be
        ! placed on the next line.
        character(len=*), parameter :: string4 = "* file2.txt (abcdefghijklmnopqrstuvwxyz):"
        type(handle_type) :: h
        type(status_type) :: st

        call temp_buffer(string, h)
        call fill_and_check(error, 64, string)

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call insert(string1, st)
            call fill_and_check(error, 64, "* file2.txt (fun4, fun5, fun6, fun7, fun8, fun9, fun10)"//LF// &
                                "(fun11345):")
        end if

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call insert(string2, st)
            call fill_and_check(error, 64, string2)
        end if

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call insert(string3, st)
            call fill_and_check(error, 39, string3)
        end if

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call insert(string4, st)
            call fill_and_check(error, 39, "* file2.txt "//LF//"(abcdefghijklmnopqrstuvwxyz):")
        end if

        call leave(h)
    end subroutine test_confinement

    subroutine test_space_substitution(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! This test verifies that filling the paragraph surrounding the
        ! last line of subprograms does not break between subprogram lists with
        ! spaces in identifiers.
        call temp_buffer(LF// &
                         "* src/sfnt.c (xmalloc, xrealloc): Improve behavior upon allocation"//LF// &
                         "failures during test."//LF// &
                         "(sfnt_table_names): Add prep."//LF// &
                         "(sfnt_transform_coordinates): Allow applying offsets during"//LF// &
                         "coordinate transform."//LF// &
                         "(sfnt_decompose_compound_glyph): Defer offset computation until"//LF// &
                         "any component compound glyph is loaded, then apply it during the"//LF// &
                         "transform process."//LF// &
                         "(sfnt_multiply_divide): Make available everywhere.  Implement on"//LF// &
                         "64 bit systems."//LF// &
                         "(sfnt_multiply_divide_signed): New function."//LF// &
                         "(sfnt_mul_fixed): Fix division overflow."//LF// &
                         "(sfnt_curve_to_and_build_1, sfnt_build_glyph_outline): Remove"//LF// &
                         "outdated comment."//LF// &
                         "(sfnt_build_outline_edges): Fix coding style."//LF// &
                         "(sfnt_lookup_glyph_metrics): Allow looking up metrics without"//LF// &
                         "scaling."//LF// &
                         "(struct sfnt_cvt_table): Fix type of cvt values."//LF// &
                         "(struct sfnt_prep_table): New structure."//LF// &
                         "(sfnt_read_cvt_table): Read cvt values in terms of fwords, not"//LF// &
                         "longs (as Apple's doc seems to say)."//LF// &
                         "(sfnt_read_fpgm_table): Fix memory allocation for font program"//LF// &
                         "table."//LF// &
                         "(sfnt_read_prep_table): New function."//LF// &
                         "(struct sfnt_interpreter_zone): New structure."//LF// &
                         "(struct sfnt_interpreter_graphics_state): New fields `project',"//LF// &
                         "`move', `vector_dot_product'.  Rename to `sfnt_graphics_state'."//LF// &
                         "(struct sfnt_interpreter, sfnt_mul_f26dot6): Stop doing rounding"//LF// &
                         "division."//LF// &
                         "(sfnt_init_graphics_state, sfnt_make_interpreter, MOVE, SSW, RAW)"//LF// &
                         "(SDS, ADD, SUB, ABS, NEG, WCVTF, _MIN, S45ROUND, SVTCAx)"//LF// &
                         "(sfnt_set_srounding_state, sfnt_skip_code)"//LF// &
                         "(sfnt_interpret_unimplemented, sfnt_interpret_fdef)"//LF// &
                         "(sfnt_interpret_idef, sfnt_interpret_if, sfnt_interpret_else)"//LF// &
                         "(sfnt_round_none, sfnt_round_to_grid, sfnt_round_to_double_grid)"//LF, h)
        call fill_and_check(error, 20, LF// &
                            "* src/sfnt.c "//LF// &
                            "(xmalloc, xrealloc):"//LF// &
                            "Improve behavior"//LF// &
                            "upon allocation"//LF// &
                            "failures during"//LF// &
                            "test."//LF// &
                            "(sfnt_table_names):"//LF// &
                            "Add prep."//LF// &
                            "(sfnt_transform_coordinates):"//LF// &
                            "Allow applying"//LF// &
                            "offsets during"//LF// &
                            "coordinate"//LF// &
                            "transform."//LF// &
                            "(sfnt_decompose_compound_glyph):"//LF// &
                            "Defer offset"//LF// &
                            "computation until"//LF// &
                            "any component"//LF// &
                            "compound glyph is"//LF// &
                            "loaded, then apply"//LF// &
                            "it during the"//LF// &
                            "transform process."//LF// &
                            "(sfnt_multiply_divide):"//LF// &
                            "Make available"//LF// &
                            "everywhere."//LF// &
                            "Implement on 64 bit"//LF// &
                            "systems."//LF// &
                            "(sfnt_multiply_divide_signed):"//LF// &
                            "New function."//LF// &
                            "(sfnt_mul_fixed):"//LF// &
                            "Fix division"//LF// &
                            "overflow."//LF// &
                            "(sfnt_curve_to_and_build_1)"//LF// &
                            "(sfnt_build_glyph_outline):"//LF// &
                            "Remove outdated"//LF// &
                            "comment."//LF// &
                            "(sfnt_build_outline_edges):"//LF// &
                            "Fix coding style."//LF// &
                            "(sfnt_lookup_glyph_metrics):"//LF// &
                            "Allow looking up"//LF// &
                            "metrics without"//LF// &
                            "scaling."//LF// &
                            "(struct sfnt_cvt_table):"//LF// &
                            "Fix type of cvt"//LF// &
                            "values."//LF// &
                            "(struct sfnt_prep_table):"//LF// &
                            "New structure."//LF// &
                            "(sfnt_read_cvt_table):"//LF// &
                            "Read cvt values in"//LF// &
                            "terms of fwords, not"//LF// &
                            "longs (as Apple's"//LF// &
                            "doc seems to say)."//LF// &
                            "(sfnt_read_fpgm_table):"//LF// &
                            "Fix memory"//LF// &
                            "allocation for font"//LF// &
                            "program table."//LF// &
                            "(sfnt_read_prep_table):"//LF// &
                            "New function."//LF// &
                            "(struct sfnt_interpreter_zone):"//LF// &
                            "New structure."//LF// &
                            "(struct sfnt_interpreter_graphics_state):"//LF// &
                            "New fields"//LF// &
                            "`project', `move',"//LF// &
                            "`vector_dot_product'."//LF// &
                            "Rename to"//LF// &
                            "`sfnt_graphics_state'."//LF// &
                            "(struct sfnt_interpreter)"//LF// &
                            "(sfnt_mul_f26dot6):"//LF// &
                            "Stop doing rounding"//LF// &
                            "division."//LF// &
                            "(sfnt_init_graphics_state)"//LF// &
                            "(sfnt_make_interpreter)"//LF// &
                            "(MOVE, SSW, RAW, SDS)"//LF// &
                            "(ADD, SUB, ABS, NEG)"//LF// &
                            "(WCVTF, _MIN)"//LF// &
                            "(S45ROUND, SVTCAx)"//LF// &
                            "(sfnt_set_srounding_state)"//LF// &
                            "(sfnt_skip_code)"//LF// &
                            "(sfnt_interpret_unimplemented)"//LF// &
                            "(sfnt_interpret_fdef)"//LF// &
                            "(sfnt_interpret_idef)"//LF// &
                            "(sfnt_interpret_if)"//LF// &
                            "(sfnt_interpret_else)"//LF// &
                            "(sfnt_round_none)"//LF// &
                            "(sfnt_round_to_grid)"//LF// &
                            "(sfnt_round_to_double_grid):"//LF)
        call leave(h)
    end subroutine test_space_substitution

    subroutine test_initial_wrapping(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! This test verifies that a newline is inserted before a subprogram
        ! itself longer than the fill column when such a subprogram is being
        ! inserted after a file name, and not otherwise.
        call temp_buffer(LF// &
                         "* src/sfnt.c (long_entry_1): This entry should be placed on a"//LF// &
                         "new line."//LF// &
                         "(but_this_entry_should_not): With the prose displaced to the"//LF// &
                         "next line instead.", h)
        call fill_and_check(error, 20, LF// &
                            "* src/sfnt.c "//LF// &
                            "(long_entry_1): This"//LF// &
                            "entry should be"//LF// &
                            "placed on a new"//LF// &
                            "line."//LF// &
                            "(but_this_entry_should_not):"//LF// &
                            "With the prose"//LF// &
                            "displaced to the"//LF// &
                            "next line instead.")
        call leave(h)
    end subroutine test_initial_wrapping

    subroutine test_no_subprogram_list_wrapping(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! This test verifies that the opening subprogram list of an entry is never
        ! broken, even in the event its length in total exceeds the fill
        ! column.
        call temp_buffer(LF// &
                         "* src/androidfns.c (Fxw_display_color_p):"//LF// &
                         "(Fx_display_grayscale_p): Report color and/or grayscale properly."//LF, h)
        call fill_and_check(error, 64, LF// &
                            "* src/androidfns.c (Fxw_display_color_p, Fx_display_grayscale_p):"//LF// &
                            "Report color and/or grayscale properly."//LF)
        call leave(h)
    end subroutine test_no_subprogram_list_wrapping

    ! ------------------------------------------------------------------
    ! log-edit-done-strip-cvs-lines
    ! ------------------------------------------------------------------

    ! A no-op `log_edit_callback'.
    recursive subroutine no_callback(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine no_callback

    !> Tests that running log_edit_done_strip_cvs_lines as a log_edit_done_hook
    !> produces the WANTED string when run on INITIAL_TEXT with
    !> `log_edit_vc_backend' set to VC_BACKEND ("" for nil).
    subroutine strip_cvs_lines_helper(error, initial_text, wanted, vc_backend)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: initial_text, wanted, vc_backend
        type(handle_type) :: h
        type(status_type) :: st
        integer :: b

        call temp_buffer("", h)
        b = 0
        if (len(vc_backend) > 0) call register_vc_backend(vc_backend, b)
        call log_edit_done_hook%clear()
        call log_edit_done_hook%add(log_edit_done_strip_cvs_lines)
        call set_log_edit_vc_backend(b)
        call set_log_edit_callback(no_callback)
        call insert(initial_text, st)
        call log_edit_done(st)
        call log_edit_done_hook%clear()
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string() == wanted, "got: "//buffer_string())
        call leave(h)
    end subroutine strip_cvs_lines_helper

    !> Strip lines beginning with "CVS:" when using CVS as VC backend.
    subroutine test_strip_cvs(error)
        type(error_type), allocatable, intent(out) :: error

        call strip_cvs_lines_helper(error, "summary line"//LF//"first line"//LF// &
                                    "CVS: Please evaluate your changes and consider the following."//LF// &
                                    "CVS: Abort checkin if you answer no."//LF, &
                                    "summary line"//LF//"first line"//LF, "CVS")
    end subroutine test_strip_cvs

    !> Do not strip lines beginning with "CVS:" when not using CVS as VC backend.
    subroutine test_strip_non_cvs(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: string = "summary line"//LF//"first line"//LF// &
                                       "CVS: Please evaluate your changes and consider the following."//LF// &
                                       "CVS: Abort checkin if you answer no."//LF

        call strip_cvs_lines_helper(error, string, string, "")
    end subroutine test_strip_non_cvs

    !> Strip lines that contain solely "CVS: " when using CVS as VC backend.
    subroutine test_strip_colon_blank(error)
        type(error_type), allocatable, intent(out) :: error

        call strip_cvs_lines_helper(error, "CVS: "//LF, "", "CVS")
    end subroutine test_strip_colon_blank

    !> Strip lines that contain solely "CVS:" when using CVS as VC backend.
    subroutine test_strip_colon(error)
        type(error_type), allocatable, intent(out) :: error

        ! This test verifies that lines consisting only of "CVS:" (no blank
        ! after the colon) are stripped from the commit message.
        ! CVS does this to accommodate editors that delete trailing whitespace.
        call strip_cvs_lines_helper(error, "CVS:"//LF, "", "CVS")
    end subroutine test_strip_colon
end module test_log_edit_fm

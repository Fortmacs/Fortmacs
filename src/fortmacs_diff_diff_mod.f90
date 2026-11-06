! fortmacs_diff_diff_mod.f90 -- running diff and diff3 for Ediff.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-diff.el (in part: the
! difference regions of two and three buffers; fine differences and
! words are deferred).
!
! Copyright (C) 1994-2026 Free Software Foundation, Inc.
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

!> The difference regions of an Ediff session: `diff' (or `diff3' for
!> three buffers) runs on the files, its normal output is parsed into
!> line ranges, the ranges become positions in buffers A, B and C, and
!> each region gets an overlay (`make_difference_overlays' of the session).  Lines
!> diff prints that are no normal output are errors, shown in a buffer of
!> their own.  The procedures that act on one session are type-bound procedures of
!> `fortmacs_diff_session_type', in fortmacs_diff_settings_diff_mod.
module fortmacs_diff_diff_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use value_mod, only: value_type, list, make_list, command_type, list_elements, xstring, vtag, xfixnum, V_INTEGER
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, erase_buffer
    use search_mod, only: match_data, set_match_data
    use basic_subprograms_mod, only: string_matches, read_y_or_n
    use execute_process_mod, only: execute_process
    use coding_mod, only: coding_system_for_read
    use file_input_output_mod, only: is_file_directory, is_file_regular, expand_file_name, file_name_nondirectory
    use directory_editor_mod, only: directory_files
    use string_type_mod, only: string_type
    use files_mod, only: file_name_unquote
    use display_engine_mod, only: message
    use fortmacs_diff_settings_mod, only: fortmacs_diff_sessions, is_fortmacs_diff_buffer_job, &
                                          check_fortmacs_diff_control_buffer, fortmacs_diff_coding_system_for_read, &
                                          fortmacs_diff_coding_system_for_write
    use fortmacs_diff_utilities_mod, only: update_fortmacs_diff_diffs
    use commands_mod, only: commands
    implicit none(type, external)
    private

    public :: execute_fortmacs_diff_process, set_fortmacs_diff_actual_diff_options, &
              toggle_fortmacs_diff_ignore_case, initialize_fortmacs_diff_diff, get_fortmacs_diff_directory_files, &
              compare_fortmacs_diff_file_contents, compare_fortmacs_diff_contents

    character, parameter :: CR = achar(13), LF = achar(10)

    !> Program to use for generating the differential of the two files.
    !> Port of ediff-diff.el ediff-diff-program.
    character(len=:), allocatable, public :: fortmacs_diff_diff_program

    !> Program to be used for three-way comparison.
    !> Must produce output compatible with Unix's diff3 program.  Port of
    !> ediff-diff.el ediff-diff3-program.
    character(len=:), allocatable, public :: fortmacs_diff_diff3_program

    !> Program to use for generating custom diff output for saving it in a
    !> file.
    !> This output is not used by Ediff internally.  Port of ediff-diff.el
    !> ediff-custom-diff-program.
    character(len=:), allocatable, public :: fortmacs_diff_custom_diff_program

    !> Options to pass to `fortmacs_diff_custom_diff_program'.  Port of
    !> ediff-diff.el ediff-custom-diff-options.
    character(len=:), allocatable, public :: fortmacs_diff_custom_diff_options

    !> Utility to use to determine if two files are identical.
    !> It must return code 0, if its arguments are identical files.  Port
    !> of ediff-diff.el ediff-cmp-program.
    character(len=:), allocatable, public :: fortmacs_diff_cmp_program

    !> Options to pass to `fortmacs_diff_diff_program'.
    !> If Unix diff is used as `fortmacs_diff_diff_program',
    !> then a useful option is `-w', to ignore space.
    !> Options `-c', `-u', and `-i' are not allowed.  Case sensitivity
    !> can be toggled interactively using \[toggle_fortmacs_diff_ignore_case].
    !>
    !> Do not remove the default options.  If you need to change this
    !> variable, add new options after the default ones.  Port of
    !> ediff-diff.el ediff-diff-options (the options a session uses are
    !> computed when the first one starts, as upstream computes them when
    !> it loads Ediff).
    character(len=:), allocatable, public :: fortmacs_diff_diff_options

    !> Option that causes the diff program to ignore case of letters.
    !> Port of ediff-diff.el ediff-ignore-case-option.
    character(len=:), allocatable, public :: fortmacs_diff_ignore_case_option

    !> Option that causes the diff3 program to ignore case of letters.
    !> GNU diff3 doesn't have such an option.  Port of ediff-diff.el
    !> ediff-ignore-case-option3.
    character(len=:), allocatable, public :: fortmacs_diff_ignore_case_option3

    !> Options to pass to `fortmacs_diff_diff3_program'.  Port of ediff-diff.el
    !> ediff-diff3-options.
    character(len=:), allocatable, public :: fortmacs_diff_diff3_options

    !> Regexp that matches normal output lines from `fortmacs_diff_diff3_program'.
    !> Lines that do not match are assumed to be error messages.  Port of
    !> ediff-diff.el ediff-diff3-ok-lines-regexp.
    character(len=:), allocatable, public :: fortmacs_diff_diff3_ok_lines_regular_expression

    !> The actual options used in comparison.  Port of ediff-diff.el
    !> ediff-actual-diff-options.
    character(len=:), allocatable, public :: fortmacs_diff_actual_diff_options

    !> The actual options used in comparison.  Port of ediff-diff.el
    !> ediff-actual-diff3-options.
    character(len=:), allocatable, public :: fortmacs_diff_actual_diff3_options

    !> Pattern to match lines produced by diff3 that describe differences.
    !> Port of ediff-diff.el ediff-match-diff3-line.
    character(len=*), parameter, public :: FORTMACS_DIFF_MATCH_DIFF3_LINE = "^====\(.?\)"//CR//"?$"

    !> Regexp that matches normal output lines from `fortmacs_diff_diff_program'.
    !> This is mostly lifted from Emerge, except that Ediff also considers
    !> warnings and `Missing newline'-type messages to be normal output.
    !> Lines that do not match are assumed to be error messages.  Port of
    !> ediff-diff.el ediff-diff-ok-lines-regexp.
    character(len=*), parameter, public :: FORTMACS_DIFF_DIFF_OK_LINES_REGULAR_EXPRESSION = &
                                           "^\([0-9,]+[acd][0-9,]+"//CR//"?$\|[<>] \|---\|.*Warning *:\|.*No +newline"// &
                                           "\|.*missing +newline\|^"//CR//"?$\)"

    !> Pattern to match lines produced by diff that describe differences.
    !> Port of ediff-diff.el ediff-match-diff-line.
    character(len=*), parameter, public :: FORTMACS_DIFF_MATCH_DIFF_LINE = &
                                           "^\([0-9]+\)\(\|,\([0-9]+\)\)\([acd]\)\([0-9]+\)\(\|,\([0-9]+\)\)"//CR//"?$"

contains

    !> Execute the diff PROGRAM.
    !>
    !> The PROGRAM output is sent to BUFFER, which must be a live buffer
    !> object.
    !>
    !> OPTIONS is a string of space-separated options to pass to PROGRAM.
    !> It may be a blank string.
    !>
    !> FILE1, FILE2 and FILE3 are the filenames to pass to PROGRAM; absent
    !> and "" ones are ignored.  JOB_NAME is the kind of job
    !> (a command, as commands%fortmacs_diff_buffers; none if absent): for a buffer job the output is
    !> read with `fortmacs_diff_coding_system_for_write', and only then is
    !> "--binary" passed.  Port of ediff-diff.el:ediff-exec-process (always
    !> synchronous: SYNCH is only false for the custom diff of `D', which is
    !> deferred).
    recursive subroutine execute_fortmacs_diff_process(program, buffer, options, status, job_name, file1, file2, file3)
        character(len=*), intent(in) :: program, options
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(command_type), pointer, intent(in), optional :: job_name
        character(len=*), intent(in), optional :: file1, file2, file3
        type(value_type) :: result, item
        type(value_type), allocatable :: argv(:)
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64), allocatable :: data(:)
        character(len=:), allocatable :: saved_coding, olddir
        logical :: buffer_job, had_coding
        integer :: i, j

        data = match_data()
        buffer_job = .false.
        if (present(job_name)) buffer_job = is_fortmacs_diff_buffer_job(job_name)
        allocate (argv(0))
        i = 1

        ! The options, split at white space.
        do
            j = verify(options(i:), " "//achar(9)//LF)
            if (j == 0) exit
            i = i + j - 1
            j = scan(options(i:), " "//achar(9)//LF)

            if (j == 0) then
                j = len(options) + 1
            else
                j = i + j - 1
            end if

            ! The --binary option, if present, should be used only for
            ! buffer jobs or for refining the differences.
            if (options(i:j - 1) /= "--binary" .or. buffer_job) then
                item = value_type(text=options(i:j - 1))
                argv = [argv, item]
            end if

            i = j
        end do

        if (present(file1)) call append_file(file1)
        if (present(file2)) call append_file(file2)
        if (present(file3)) call append_file(file3)
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call erase_buffer(status)

        if (.not. status%ok()) then
            call set_buffer(old, st)
            return
        end if

        ! default-directory may be on some remote machine or a
        ! non-existing dir.
        olddir = "/"
        if (allocated(buffers(current)%directory)) olddir = buffers(current)%directory
        buffers(current)%directory = "/"
        had_coding = allocated(coding_system_for_read%value)
        if (had_coding) saved_coding = coding_system_for_read%value

        ! If this is a buffer job, we are diffing temporary files produced
        ! with fortmacs_diff_coding_system_for_write, so use the same encoding to
        ! read the results.
        if (buffer_job) then
            coding_system_for_read = fortmacs_diff_coding_system_for_write
        else
            coding_system_for_read = fortmacs_diff_coding_system_for_read
        end if

        ! Avoid localization of messages so we can parse the output.
        call execute_process(program, result, status, destination=value_type(.true.), arguments=make_list(argv), &
                             environment=list(value_type(text="LC_MESSAGES=C")))

        if (had_coding) then
            coding_system_for_read = saved_coding
        else
            call coding_system_for_read%clear()
        end if

        buffers(current)%directory = olddir
        ! Make the old buffer current again, keeping the first error.
        call set_buffer(old, st)
        if (status%ok()) status = st
        call set_match_data(data)

    contains

        ! Append FILE, unquoted, to ARGUMENTS unless it is "".
        subroutine append_file(file)
            character(len=*), intent(in) :: file

            if (len(file) == 0) return
            item = value_type(text=file_name_unquote(file))
            argv = [argv, item]
        end subroutine append_file
    end subroutine execute_fortmacs_diff_process

    !> Compute the options diff and diff3 run with: `fortmacs_diff_diff_options'
    !> and `fortmacs_diff_diff3_options', and with IGNORE_CASE the options that
    !> ignore case.  Port of ediff-diff.el:ediff-set-actual-diff-options
    !> (`fortmacs_diff_ignore_case' is the session's, given as IGNORE_CASE).
    subroutine set_fortmacs_diff_actual_diff_options(ignore_case)
        logical, intent(in) :: ignore_case

        if (ignore_case) then
            fortmacs_diff_actual_diff_options = fortmacs_diff_diff_options//" "//fortmacs_diff_ignore_case_option
            fortmacs_diff_actual_diff3_options = fortmacs_diff_diff3_options//" "//fortmacs_diff_ignore_case_option3
        else
            fortmacs_diff_actual_diff_options = fortmacs_diff_diff_options
            fortmacs_diff_actual_diff3_options = fortmacs_diff_diff3_options
        end if
    end subroutine set_fortmacs_diff_actual_diff_options

    !> Toggle whether difference regions that differ only in letter case
    !> are skipped (`fortmacs_diff_ignore_case'), and recompute the differences.
    !> Port of ediff-diff.el:ediff-toggle-ignore-case.
    recursive subroutine toggle_fortmacs_diff_ignore_case(status)
        type(status_type), intent(out) :: status
        integer :: session_index

        call check_fortmacs_diff_control_buffer(session_index, status)
        if (.not. status%ok()) return
        fortmacs_diff_sessions(session_index)%ignore_case = .not. fortmacs_diff_sessions(session_index)%ignore_case
        call set_fortmacs_diff_actual_diff_options(fortmacs_diff_sessions(session_index)%ignore_case)

        if (fortmacs_diff_sessions(session_index)%ignore_case) then
            call message("Ignoring regions that differ only in case")
        else
            call message("Ignoring case differences turned OFF")
        end if

        if (fortmacs_diff_sessions(session_index)%merge_job) then
            call message("Ignoring letter case is too dangerous in merge jobs")
        else if (fortmacs_diff_sessions(session_index)%diff3_job .and. len(fortmacs_diff_ignore_case_option3) == 0) then
            call message("Ignoring letter case is not supported by this diff3 program")
        else if (.not. fortmacs_diff_sessions(session_index)%three_way_job .and. len(fortmacs_diff_ignore_case_option) == 0) then
            call message("Ignoring letter case is not supported by this diff program")
        else
            call update_fortmacs_diff_diffs(status)
        end if
    end subroutine toggle_fortmacs_diff_ignore_case

    !> The files of DIRECTORY (full names with FULL, else relative) whose
    !> names match REGULAR_EXPRESSION ("" for all), sorted, in NAMES.  Not a port: what
    !> directory-files does with its MATCH argument.
    recursive subroutine get_fortmacs_diff_directory_files(directory, regular_expression, full, names, status)
        character(len=*), intent(in) :: directory, regular_expression
        logical, intent(in) :: full
        type(string_type), allocatable, intent(out) :: names(:)
        type(status_type), intent(out) :: status
        type(value_type) :: files
        type(value_type), allocatable :: all(:)
        type(string_type), allocatable :: grown(:)
        character(len=:), allocatable :: name
        logical :: matches
        integer :: j, n

        allocate (names(0))
        call directory_files(directory, files, status, full=full)
        if (.not. status%ok()) return
        call list_elements(files, all)
        allocate (grown(size(all)))
        n = 0

        do j = 1, size(all)
            name = xstring(all(j))

            if (len(regular_expression) > 0) then
                matches = string_matches(regular_expression, file_name_nondirectory(name), status)
                if (.not. status%ok()) return
                if (.not. matches) cycle
            end if

            n = n + 1
            grown(n)%text = name
        end do

        names = grown(1:n)
    end subroutine get_fortmacs_diff_directory_files

    !> SAME is .true. if files F1 and F2 have identical contents
    !> (`fortmacs_diff_cmp_program' exits with 0).  Port of
    !> ediff-diff.el:ediff-same-file-contents (`ediff-cmp-options' and
    !> remote files are Deferred: see docs/status/fortmacs_diff_diff_mod.md).
    recursive subroutine compare_fortmacs_diff_file_contents(f1, f2, same, status)
        character(len=*), intent(in) :: f1, f2
        logical, intent(out) :: same
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: expanded1, expanded2
        type(value_type) :: result, argument1, argument2

        same = .false.
        status = status_type()
        if (is_file_directory(f1) .or. is_file_directory(f2)) return
        call expand_file_name(f1, expanded1, status)
        if (.not. status%ok()) return
        call expand_file_name(f2, expanded2, status)
        if (.not. status%ok()) return
        argument1 = value_type(text=expanded1)
        argument2 = value_type(text=expanded2)
        call execute_process(fortmacs_diff_cmp_program, result, status, arguments=list(argument1, argument2))
        if (.not. status%ok()) return
        if (vtag(result) == V_INTEGER) same = xfixnum(result) == 0
    end subroutine compare_fortmacs_diff_file_contents

    !> SAME is .true. if D1 and D2 have the same content.
    !> D1 and D2 can either be both directories or both regular files.
    !> Symlinks and the likes are not handled.
    !> If FILTER_REGULAR_EXPRESSION is not "", recursive checking in directories
    !> affects only files whose names match the expression.  ASKED and
    !> RECURSE are the session group's `fortmacs_diff_recurse_to_subdirectories':
    !> whether the user was asked to compare subdirectories recursively
    !> (the first time two directories are compared), and the answer.  Port of ediff-diff.el:ediff-same-contents
    !> (with ediff-same-file-contents-lists and ediff-delete-all-matches).
    recursive subroutine compare_fortmacs_diff_contents(d1, d2, filter_regular_expression, asked, recurse, same, status)
        character(len=*), intent(in) :: d1, d2, filter_regular_expression
        logical, intent(inout) :: asked, recurse
        logical, intent(out) :: same
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: entries1(:), entries2(:)
        integer :: k

        same = .false.
        status = status_type()
        ! Indicate progress.
        call message("Comparing `"//d1//"' and `"//d2//"' modulo `"//filter_regular_expression//"'")

        if (is_file_directory(d1) .and. is_file_directory(d2)) then
            ! D1 & D2 directories => recurse.
            if (.not. asked) then
                call read_y_or_n("Compare subdirectories recursively? ", recurse, status)
                if (.not. status%ok()) return
                asked = .true.
            end if

            if (.not. recurse) return
            call entries(d1, entries1)
            if (.not. status%ok()) return
            call entries(d2, entries2)
            if (.not. status%ok()) return
            ! First, check only the names (works quickly and ensures a
            ! precondition for subsequent code).
            if (size(entries1) /= size(entries2)) return

            do k = 1, size(entries1)
                if (file_name_nondirectory(entries1(k)%text) /= file_name_nondirectory(entries2(k)%text)) return
            end do

            ! With name equality established, compare the entries through
            ! recursion.
            do k = 1, size(entries1)
                call compare_fortmacs_diff_contents(entries1(k)%text, entries2(k)%text, filter_regular_expression, asked, recurse, &
                                                    same, &
                                                    status)
                if (.not. status%ok()) return
                if (.not. same) return
            end do

            ! If reached the end then lists are equal.
            same = .true.
        else if (is_file_regular(d1) .and. is_file_regular(d2)) then
            ! D1 & D2 are both files => compare directly.
            call compare_fortmacs_diff_file_contents(d1, d2, same, status)
        end if

    contains

        ! The files of DIRECTORY (full names) whose names match FILTER_REGULAR_EXPRESSION,
        ! but . and ..
        recursive subroutine entries(directory, names)
            character(len=*), intent(in) :: directory
            type(string_type), allocatable, intent(out) :: names(:)
            type(string_type), allocatable :: all(:)
            logical, allocatable :: keep(:)
            integer :: j

            call get_fortmacs_diff_directory_files(directory, filter_regular_expression, .true., all, status)
            allocate (keep(size(all)))

            do j = 1, size(all)
                keep(j) = file_name_nondirectory(all(j)%text) /= "." .and. file_name_nondirectory(all(j)%text) /= ".."
            end do

            names = pack(all, keep)
        end subroutine entries
    end subroutine compare_fortmacs_diff_contents

    !> Register the command of ediff-diff.el and give the options their
    !> defaults.  Not a port.
    subroutine initialize_fortmacs_diff_diff()
        commands%toggle_fortmacs_diff_ignore_case%execute => toggle_fortmacs_diff_ignore_case
        if (.not. allocated(fortmacs_diff_diff_program)) fortmacs_diff_diff_program = "diff"
        if (.not. allocated(fortmacs_diff_diff3_program)) fortmacs_diff_diff3_program = "diff3"
        if (.not. allocated(fortmacs_diff_custom_diff_program)) fortmacs_diff_custom_diff_program = fortmacs_diff_diff_program
        if (.not. allocated(fortmacs_diff_custom_diff_options)) fortmacs_diff_custom_diff_options = "-c"
        if (.not. allocated(fortmacs_diff_cmp_program)) fortmacs_diff_cmp_program = "cmp"
        if (.not. allocated(fortmacs_diff_diff_options)) fortmacs_diff_diff_options = ""
        if (.not. allocated(fortmacs_diff_ignore_case_option)) fortmacs_diff_ignore_case_option = "-i"
        if (.not. allocated(fortmacs_diff_ignore_case_option3)) fortmacs_diff_ignore_case_option3 = ""
        if (.not. allocated(fortmacs_diff_diff3_options)) fortmacs_diff_diff3_options = ""

        if (.not. allocated(fortmacs_diff_diff3_ok_lines_regular_expression)) then
            fortmacs_diff_diff3_ok_lines_regular_expression = &
                "^\([1-3]:\|====\|  \|.*Warning *:\|.*No newline\|.*missing newline\|^"// &
                CR//"$\)"
        end if
    end subroutine initialize_fortmacs_diff_diff
end module fortmacs_diff_diff_mod

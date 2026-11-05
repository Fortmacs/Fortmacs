! test_version_control_annotate_mod.f90 -- tests for version_control_annotate_mod and the Git
! backend's annotate functions.
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

!> Own tests of VC-Annotate on a temporary Git repository (upstream has
!> no vc-annotate tests): C-x v g's buffer, the coloring of lines by age,
!> moving between revisions (p, n) and the diff (d) and log (l) of the
!> revision at a line.  They are skipped without git.
module test_version_control_annotate_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64, error_unit
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use test_support_mod, only: show_alone
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_list, vtag, xfixnum, V_INTEGER
    use buffer_mod, only: buffers, current, get_or_make_buffer, set_buffer, close_buffer, current_buffer, get_buffer, &
                          is_buffer_live, generate_new_buffer_name
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, point, point_min, goto_char, pos_bol, pos_eol
    use editing_commands_mod, only: forward_line
    use text_properties_mod, only: get_text_property
    use data_mod, only: value_eq
    use file_input_output_mod, only: init_fileio, fortmacs_wd, file_name_as_directory, is_file_directory, &
                                     create_directory_internal, write_region
    use files_mod, only: delete_directory, find_file_noselect, executable_find
    use execute_process_mod, only: execute_process
    use process_mod, only: get_buffer_process, is_process_live, accept_process_output
    use bindings_mod, only: init_bindings
    use version_control_hooks_mod, only: vc_handled_backends, vc_clear_context, vc_backend_named, vc_short_revision
    use version_control_mod, only: vc_create_repo, vc_buffer_revision
    use font_lock_mod, only: font_lock_ensure
    use version_control_annotate_mod, only: vc_annotate, previous_vc_annotate_revision, next_vc_annotate_revision, &
                                            show_vc_annotate_diff_revision_at_line, show_vc_annotate_log_revision_at_line, &
                                            vc_annotate_current_color_map, vc_annotate_color_type, &
                                       vc_annotate_display_mode_type, vc_annotate_very_old_color, VC_ANNOTATE_DISPLAY_MODE_FULLSCALE
    implicit none(type, external)
    private

    public :: collect_version_control_annotate_mod

    !> The test directory, under the build directory.
    character(len=*), parameter :: subdir = "build/test-vc-annotate/"

    character, parameter :: LF = achar(10)

contains

    !> Collect all exported unit tests.
    subroutine collect_version_control_annotate_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("vc-annotate-git", test_annotate), &
                    new_unittest("vc-annotate-git-prev-next", test_prev_next), &
                    new_unittest("vc-git-annotate-time", test_annotate_time), &
                    new_unittest("vc-annotate-git-diff-log", test_diff_log), &
                    new_unittest("log-view-annotate-version", test_log_view_annotate) &
                    ]
    end subroutine collect_version_control_annotate_mod

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> A fresh temporary buffer, current.
    subroutine temp_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(generate_new_buffer_name(" *vc-annotate-test*"), h, st)
        call set_buffer(h, st)
        buffers(current)%directory = file_name_as_directory(fortmacs_wd)
    end subroutine temp_buffer

    !> The test repository directory.
    function tdir() result(dir)
        character(len=:), allocatable :: dir

        dir = file_name_as_directory(file_name_as_directory(fortmacs_wd)//subdir)
    end function tdir

    !> Whether git is available (else the test is skipped).
    logical function has_git(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path

        call executable_find("git", path)
        has_git = len(path) > 0
        if (.not. has_git) call skip_test(error, "no git")
    end function has_git

    !> Write TEXT to FILE.
    subroutine write_file(file, text)
        character(len=*), intent(in) :: file, text
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: msg

        old = current_buffer()
        call temp_buffer(h)
        call insert(text, st)
        call write_region(file, .false., st, message=msg)
        call close_buffer(h, st)
        call set_buffer(old, st)
    end subroutine write_file

    !> Run git with ARGS in the test directory, reporting a failure on
    !> standard error.
    subroutine git(a1, a2, a3, a4, a5, a6)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4, a5, a6
        type(value_type) :: args(10)
        type(value_type) :: result
        type(handle_type) :: old, h
        type(status_type) :: st
        integer :: n

        ! One element at a time: function results in an array constructor
        ! of value_type leak.
        args(1) = value_type(text="-c")
        args(2) = value_type(text="user.name=t")
        args(3) = value_type(text="-c")
        args(4) = value_type(text="user.email=t@t")
        args(5) = value_type(text=a1)
        n = 5
        if (present(a2)) call append(a2)
        if (present(a3)) call append(a3)
        if (present(a4)) call append(a4)
        if (present(a5)) call append(a5)
        if (present(a6)) call append(a6)
        old = current_buffer()
        call temp_buffer(h)
        buffers(current)%directory = tdir()
        call execute_process("git", result, st, destination=value_type(.true.), args=make_list(args(1:n)))

        if (.not. st%ok()) then
            write (error_unit, "(a)") "git "//a1//": "//format_error(st)
        else if (vtag(result) == V_INTEGER) then
            if (xfixnum(result) /= 0) write (error_unit, "(a)") "git "//a1//" failed: "//buffer_string()
        end if

        call close_buffer(h, st)
        call set_buffer(old, st)

    contains

        subroutine append(a)
            character(len=*), intent(in) :: a

            n = n + 1
            args(n) = value_type(text=a)
        end subroutine append
    end subroutine git

    !> A Git repository whose a.txt has two commits: "init" in 2020
    !> ("one\ntwo\n") and "second" now ("one\nthree\n"), visited in BUFFER
    !> (current, shown alone); H is the setup buffer.
    subroutine two_commits(h, buffer, status)
        type(handle_type), intent(out) :: h, buffer
        type(status_type), intent(out) :: status
        type(status_type) :: st

        call init_bindings()
        call init_fileio()
        call vc_clear_context()
        vc_handled_backends = [character(len=8) :: "Git"]
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        call create_directory_internal(file_name_as_directory(fortmacs_wd)//"build", st)
        call create_directory_internal(tdir(), st)
        call temp_buffer(h)
        buffers(current)%directory = tdir()
        call vc_create_repo(vc_backend_named("Git"), status)
        if (.not. status%ok()) return
        call git("checkout", "-q", "-b", "main")
        call write_file(tdir()//"a.txt", "one"//LF//"two"//LF)
        call git("add", "a.txt")
        call git("commit", "-q", "--date=2020-01-01T12:00:00", "-m", "init")
        call write_file(tdir()//"a.txt", "one"//LF//"three"//LF)
        call git("commit", "-q", "-a", "-m", "second")
        call find_file_noselect(tdir()//"a.txt", buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        call show_alone()
    end subroutine two_commits

    !> Remove the test directory and the buffers; handle all backends again.
    subroutine teardown(h, buffer)
        type(handle_type), intent(in) :: h, buffer
        type(status_type) :: st
        type(handle_type) :: b
        integer :: k
        character(len=16), parameter :: names(3) = [character(len=16) :: "*vc*", "*vc-diff*", "*vc-change-log*"]

        do k = 1, size(names)
            b = get_buffer(trim(names(k)))
            if (is_buffer_live(b)) call close_buffer(b, st)
        end do

        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
        if (is_buffer_live(h)) call close_buffer(h, st)
        if (is_file_directory(tdir())) call delete_directory(tdir(), st, .true.)
        vc_handled_backends = [character(len=8) :: "RCS", "CVS", "SVN", "SCCS", "SRC", "Bzr", "Git", "Hg"]
        call vc_clear_context()
    end subroutine teardown

    !> Wait for the process of BUFFER to finish (its `vc_exec_after'
    !> actions run).
    subroutine wait_buffer_process(buffer)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: process
        type(status_type) :: st
        logical :: got
        integer :: k

        process = get_buffer_process(buffer)

        if (process%index /= 0) then
            do k = 1, 600
                if (.not. is_process_live(process)) exit
                call accept_process_output(got, st, process, 0.05)
            end do
        end if

        call accept_process_output(got, st, seconds=0.05)
    end subroutine wait_buffer_process

    !> Annotate the working revision of the visited file in BUFFER (C-x v
    !> g scaled from the oldest to the newest line) and wait for it; ANNOTATE
    !> is the *Annotate* buffer, current.
    subroutine annotate_head(buffer, annotate, status)
        type(handle_type), intent(in) :: buffer
        type(handle_type), intent(out) :: annotate
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rev
        logical :: found

        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call vc_short_revision(buffers(current)%filename, rev, found)
        call vc_annotate(buffers(current)%filename, rev, status, &
                         vc_annotate_display_mode_type(kind=VC_ANNOTATE_DISPLAY_MODE_FULLSCALE))
        if (.not. status%ok()) return
        annotate = get_buffer("*Annotate a.txt (rev "//rev//")*")
        if (.not. is_buffer_live(annotate)) return
        call wait_buffer_process(annotate)
        call set_buffer(annotate, status)
    end subroutine annotate_head

    !> Whether the `face' text property at POS is the face NAME.
    logical function has_face(pos, name)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: name
        type(value_type) :: v
        type(status_type) :: st
        character(len=*), parameter :: face_property = "face"

        call get_text_property(pos, value_type(name=face_property), v, st)
        has_face = st%ok()
        if (has_face) has_face = value_eq(v, value_type(name=name))
    end function has_face

    ! ------------------------------------------------------------------
    ! Tests
    ! ------------------------------------------------------------------

    !> C-x v g shows `git blame' in *Annotate a.txt (rev REV)*, in
    !> VC-Annotate mode, with the annotations invisible-able and each line
    !> colored by its age: the newest in the first color of the map, the
    !> oldest in the last.
    subroutine test_annotate(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, annotate
        type(status_type) :: st
        type(vc_annotate_color_type), allocatable :: map(:)
        type(value_type) :: v
        character(len=:), allocatable :: text, newest, oldest, very_old
        integer(int64) :: shortage
        character(len=*), parameter :: invisible_property = "invisible"

        if (.not. has_git(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "setup: "//format_error(st))
            if (allocated(error)) exit body
            call annotate_head(buffer, annotate, st)
            call check(error, st%ok() .and. is_buffer_live(annotate), "vc-annotate: "//format_error(st))
            if (allocated(error)) exit body
            call check(error, buffers(current)%major_mode, "vc_annotate_mode")
            if (allocated(error)) exit body
            text = buffer_string()
            call check(error, index(text, "(t 2020-01-01 1) one"//LF) > 0, text)
            if (allocated(error)) exit body
            call check(error, index(text, " 2) three"//LF) > 0, text)
            if (allocated(error)) exit body
            call font_lock_ensure(st)
            call check(error, st%ok(), "font-lock: "//format_error(st))
            if (allocated(error)) exit body
            call vc_annotate_current_color_map(map)
            newest = "vc_annotate_face_"//trim(map(1)%color(2:))
            oldest = "vc_annotate_face_"//trim(map(size(map))%color(2:))
            very_old = "vc_annotate_face_"//vc_annotate_very_old_color(2:)
            ! Line 1 is the oldest.
            call goto_char(point_min())
            call check(error, has_face(pos_eol() - 1, oldest) .or. has_face(pos_eol() - 1, very_old), "line 1: "//oldest)
            if (allocated(error)) exit body
            ! Its annotation is invisible with `vc_annotate_annotation'.
            call get_text_property(point_min(), value_type(name=invisible_property), v, st)
            call check(error, allocated(v%a), "annotation invisible")
            if (allocated(error)) exit body
            shortage = forward_line(1_int64)
            call check(error, has_face(pos_eol() - 1, newest), "line 2: "//newest)
        end block body

        if (is_buffer_live(annotate)) call close_buffer(annotate, st)
        call teardown(h, buffer)
    end subroutine test_annotate

    !> p annotates the previous revision of the file in the same buffer,
    !> renamed; n goes back to the newest one (named by its branch).
    subroutine test_prev_next(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, annotate
        type(status_type) :: st
        character(len=:), allocatable :: text, head, previous

        if (.not. has_git(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "setup: "//format_error(st))
            if (allocated(error)) exit body
            call annotate_head(buffer, annotate, st)
            call check(error, st%ok() .and. is_buffer_live(annotate), "vc-annotate: "//format_error(st))
            if (allocated(error)) exit body
            head = vc_buffer_revision()
            call previous_vc_annotate_revision(1_int64, st)
            call check(error, st%ok(), "p: "//format_error(st))
            if (allocated(error)) exit body
            call wait_buffer_process(annotate)
            call set_buffer(annotate, st)
            previous = vc_buffer_revision()
            call check(error, len(previous) > 0 .and. previous /= head, "previous revision: "//previous)
            if (allocated(error)) exit body
            call check(error, buffers(current)%name, "*Annotate a.txt (rev "//previous//")*")
            if (allocated(error)) exit body
            text = buffer_string()
            call check(error, index(text, ") two"//LF) > 0 .and. index(text, "three") == 0, text)
            if (allocated(error)) exit body
            call next_vc_annotate_revision(1_int64, st)
            call check(error, st%ok(), "n: "//format_error(st))
            if (allocated(error)) exit body
            call wait_buffer_process(annotate)
            call set_buffer(annotate, st)
            call check(error, vc_buffer_revision(), "main")
            if (allocated(error)) exit body
            text = buffer_string()
            call check(error, index(text, ") three"//LF) > 0, text)
        end block body

        if (is_buffer_live(annotate)) call close_buffer(annotate, st)
        call teardown(h, buffer)
    end subroutine test_prev_next

    !> d on a line shows the diff of its revision with the previous one; l
    !> shows the log entry of its revision.
    subroutine test_diff_log(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, annotate, d, log
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: shortage

        if (.not. has_git(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "setup: "//format_error(st))
            if (allocated(error)) exit body
            call annotate_head(buffer, annotate, st)
            call check(error, st%ok() .and. is_buffer_live(annotate), "vc-annotate: "//format_error(st))
            if (allocated(error)) exit body
            ! Line 2, changed by "second".
            call goto_char(point_min())
            shortage = forward_line(1_int64)
            call show_vc_annotate_diff_revision_at_line(st)
            call check(error, st%ok(), "d: "//format_error(st))
            if (allocated(error)) exit body
            d = get_buffer("*vc-diff*")
            call check(error, is_buffer_live(d), "*vc-diff*")
            if (allocated(error)) exit body
            call wait_buffer_process(d)
            call set_buffer(d, st)
            text = buffer_string()
            call check(error, index(text, "-two"//LF//"+three"//LF) > 0, text)
            if (allocated(error)) exit body
            call set_buffer(annotate, st)
            call goto_char(point_min())
            shortage = forward_line(1_int64)
            call show_vc_annotate_log_revision_at_line(st)
            call check(error, st%ok(), "l: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-change-log*")
            call check(error, is_buffer_live(log), "*vc-change-log*")
            if (allocated(error)) exit body
            call wait_buffer_process(log)
            call set_buffer(log, st)
            text = buffer_string()
            call check(error, index(text, "    second") > 0 .and. index(text, "    init") == 0, text)
        end block body

        if (is_buffer_live(annotate)) call close_buffer(annotate, st)
        call teardown(h, buffer)
    end subroutine test_diff_log

    !> `vc_git_annotate_time' reads the date of a `git blame' line, with a
    !> time and zone when it has them, and moves past the annotation; the
    !> revision and file name of a line are extracted.
    subroutine test_annotate_time(error)
        use version_control_git_mod, only: vc_git_annotate_time, extract_vc_git_annotate_revision_at_line
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: revision, file
        real(real64) :: days
        logical :: found

        call temp_buffer(h)
        call insert("^abc1234 (t 2026-11-03 12:00:00 +0100 1) x"//LF//"no annotation"//LF, st)
        call goto_char(point_min())
        call vc_git_annotate_time(days, found)
        call check(error, found, "found")
        if (allocated(error)) return
        call check(error, abs(days*86400.0_real64 - 1793703600.0_real64) < 1.0_real64, "the time")
        if (allocated(error)) return
        call check(error, point() == point_min() + len("^abc1234 (t 2026-11-03 12:00:00 +0100 1) "), "point")
        if (allocated(error)) return
        call goto_char(point_min())
        call extract_vc_git_annotate_revision_at_line(revision, file, found)
        call check(error, found .and. revision == "abc1234" .and. len(file) == 0, "revision: "//revision)
        if (allocated(error)) return
        call goto_char(pos_eol() + 1)
        call vc_git_annotate_time(days, found)
        call check(error,.not. found, "no annotation")
        call close_buffer(h, st)
    end subroutine test_annotate_time

    !> a in a log annotates the revision at point.
    subroutine test_log_view_annotate(error)
        use version_control_mod, only: vc_print_log
        use log_view_mod, only: log_view_annotate_version, log_view_current_tag
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buffer, log, annotate
        type(status_type) :: st
        character(len=:), allocatable :: tag, text
        logical :: has

        if (.not. has_git(error)) return
        call two_commits(h, buffer, st)

        body: block
            call check(error, st%ok(), "setup: "//format_error(st))
            if (allocated(error)) exit body
            call vc_print_log(st, "", 500_int64)
            call check(error, st%ok(), "vc-print-log: "//format_error(st))
            if (allocated(error)) exit body
            log = get_buffer("*vc-change-log*")
            call wait_buffer_process(log)
            call set_buffer(log, st)
            call goto_char(point_min())
            call log_view_current_tag(tag, has)
            call check(error, has, "tag")
            if (allocated(error)) exit body
            call log_view_annotate_version(point(), st)
            call check(error, st%ok(), "a: "//format_error(st))
            if (allocated(error)) exit body
            annotate = get_buffer("*Annotate a.txt (rev "//tag//")*")
            call check(error, is_buffer_live(annotate), "*Annotate a.txt (rev "//tag//")*")
            if (allocated(error)) exit body
            call wait_buffer_process(annotate)
            call set_buffer(annotate, st)
            text = buffer_string()
            call check(error, index(text, ") three"//LF) > 0, text)
        end block body

        if (is_buffer_live(annotate)) call close_buffer(annotate, st)
        call teardown(h, buffer)
    end subroutine test_log_view_annotate
end module test_version_control_annotate_mod

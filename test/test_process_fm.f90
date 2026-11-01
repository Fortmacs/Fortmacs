! test_process_fm.f90 -- tests for process_fm and sys_worker_fm.
! Ports GNU Emacs 31.1 test/src/process-tests.el in part; only the tests
! that apply to what is ported.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> The tests wait for subprocesses through `accept_process_output' (or
!> `sit_for') in loops bounded by a generous wall-clock limit, never by a
!> fixed delay, so they hold under valgrind and on slow machines.
module test_process_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type, null_handle
    use buffer_fm, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use editfns_fm, only: buffer_string
    use keyboard_fm, only: sit_for
    use sys_worker_fm, only: on_main_thread, wait_for_worker_messages
    use process_fm, only: process_arg_type, make_process, start_process, delete_process, destroy_process, &
                          process_status, process_exit_status, is_process_live, process_name, process_tty_name, &
                          set_process_sentinel, set_process_filter, accept_process_output, stop_process, &
                          process_send_string, process_send_eof, get_buffer_process, get_process, process_list, &
                          process_shutdown, is_process, PROCESS_CONNECTION_PIPE, PROCESS_CONNECTION_PTY
    implicit none(type, external)
    private

    public :: collect_process_fm

    !> Wall-clock limit of a wait, in seconds: generous, for valgrind on a
    !> loaded machine (a wait ends as soon as its condition holds).
    integer, parameter :: TIMEOUT = 600

    ! What the recording sentinel and filter saw.
    integer :: sentinel_calls = 0
    character(len=:), allocatable :: last_event, filtered
    type(handle_type) :: sentinel_process
    logical :: filter_off_main = .false.

contains

    !> Collect all exported unit tests.
    subroutine collect_process_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("process-test-sentinel-accept-process-output", test_sentinel_accept), &
                    new_unittest("process-test-sentinel-sit-for", test_sentinel_sit_for), &
                    new_unittest("start-process-should-not-modify-arguments", test_not_modify_arguments), &
                    new_unittest("process-tests/sentinel-called", test_sentinel_called), &
                    new_unittest("process-tests/sentinel-with-multiple-processes", test_sentinel_multiple), &
                    new_unittest("make-process/connection-type/pty", test_connection_pty), &
                    new_unittest("make-process/connection-type/pipe", test_connection_pipe), &
                    new_unittest("process-default-filter-and-sentinel", test_default_filter), &
                    new_unittest("process-exit-abnormally", test_exit_abnormally), &
                    new_unittest("process-kill", test_kill), &
                    new_unittest("process-delete", test_delete), &
                    new_unittest("process-output-on-main-thread", test_main_thread), &
                    new_unittest("process-utf-8-across-reads", test_utf8_split), &
                    new_unittest("process-send-string-and-eof", test_send), &
                    new_unittest("process-unique-names", test_unique_names), &
                    new_unittest("process-kill-buffer", test_close_buffer), &
                    new_unittest("process-missing-program", test_missing_program), &
                    new_unittest("process-workers-beside-main-thread", test_workers_beside_main), &
                    new_unittest("list-processes", test_list_processes) &
                    ]
    end subroutine collect_process_fm

    !> Records the sentinel's calls.
    subroutine record_sentinel(process, event, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: event
        type(status_type), intent(out) :: status

        status = status_type()
        sentinel_calls = sentinel_calls + 1
        last_event = event
        sentinel_process = process
    end subroutine record_sentinel

    !> Collects the output, noting whether it ran off the main thread.
    subroutine record_filter(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. is_process(process)) return
        if (.not. on_main_thread()) filter_off_main = .true.
        filtered = filtered//string
    end subroutine record_filter

    subroutine reset_records()
        sentinel_calls = 0
        last_event = ""
        filtered = ""
        sentinel_process = null_handle()
        filter_off_main = .false.
    end subroutine reset_records

    !> Seconds since `start'.
    integer function elapsed(start)
        integer(int64), intent(in) :: start
        integer(int64) :: now, rate

        call system_clock(now, rate)
        elapsed = int((now - start)/max(rate, 1_int64))
    end function elapsed

    !> Wait (accepting output) until the recording sentinel has been
    !> called, or the time limit.
    subroutine wait_for_sentinel(use_sit_for)
        logical, intent(in), optional :: use_sit_for
        integer(int64) :: start
        type(status_type) :: st
        logical :: got, sit

        sit = .false.
        if (present(use_sit_for)) sit = use_sit_for
        call system_clock(start)

        do while (sentinel_calls == 0 .and. elapsed(start) < TIMEOUT)
            if (sit) then
                call sit_for(10_int32)
            else
                call accept_process_output(got, st, seconds=1.0)
            end if
        end do
    end subroutine wait_for_sentinel

    !> Accept output from `process' until it stops sending (as upstream's
    !> `(while (accept-process-output process))'), or the time limit.
    subroutine drain(process)
        type(handle_type), intent(in) :: process
        integer(int64) :: start
        type(status_type) :: st
        logical :: got

        call system_clock(start)

        do while (elapsed(start) < TIMEOUT)
            call accept_process_output(got, st, process)
            if (.not. got) exit
        end do
    end subroutine drain

    !> The arguments `sh -c SCRIPT'.
    subroutine sh_args(script, args)
        character(len=*), intent(in) :: script
        type(process_arg_type), allocatable, intent(out) :: args(:)

        allocate (args(2))
        args(1)%s = "-c"
        args(2)%s = script
    end subroutine sh_args

    !> Upstream's process-test-wait-for-sentinel on `bash -c "exit 20"`
    !> (sh here), waiting with accept-process-output.
    subroutine test_sentinel_accept(error)
        type(error_type), allocatable, intent(out) :: error

        call sentinel_exit_20(error, .false.)
    end subroutine test_sentinel_accept

    !> The same, waiting with sit-for.
    subroutine test_sentinel_sit_for(error)
        type(error_type), allocatable, intent(out) :: error

        call sentinel_exit_20(error, .true.)
    end subroutine test_sentinel_sit_for

    subroutine sentinel_exit_20(error, sit)
        type(error_type), allocatable, intent(out) :: error
        logical, intent(in) :: sit
        type(handle_type) :: process
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st

        call reset_records()
        call sh_args("exit 20", args)
        call start_process("test", null_handle(), "sh", process, st, args)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_process_sentinel(process, st, record_sentinel)
        call wait_for_sentinel(sit)
        call check(error, sentinel_calls == 1, "sentinel called")
        if (.not. allocated(error)) call check(error, process_status(process), "exit")
        if (.not. allocated(error)) call check(error, process_exit_status(process), 20)
        if (.not. allocated(error)) call check(error, last_event, "exited abnormally with code 20"//new_line("a"))
        call destroy_process(process, st)
    end subroutine sentinel_exit_20

    !> `start_process' must not modify its arguments in-place (Bug#21831):
    !> "/bin//sh" is run as given.
    subroutine test_not_modify_arguments(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process
        type(status_type) :: st
        character(len=:), allocatable :: path, samepath

        path = "/bin//sh"
        samepath = path
        call start_process("", null_handle(), path, process, st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, is_process_live(process), "live")
        if (.not. allocated(error)) call check(error, path, samepath)
        call destroy_process(process, st)
        call process_shutdown()
    end subroutine test_not_modify_arguments

    !> Check that sentinels are called after processes finish (with a pipe
    !> and with a pty).
    subroutine test_sentinel_called(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: k
        type(handle_type) :: process
        type(process_arg_type), allocatable :: command(:)
        type(status_type) :: st
        integer, parameter :: types(2) = [PROCESS_CONNECTION_PIPE, PROCESS_CONNECTION_PTY]

        allocate (command(2))
        command(1)%s = "echo"
        command(2)%s = "first"

        do k = 1, 2
            call reset_records()
            call make_process("echo", command, process, st, noquery=.true., connection_type=types(k), &
                              sentinel=record_sentinel)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) return
            call drain(process)
            call check(error, sentinel_calls == 1, "one call")
            if (.not. allocated(error)) call check(error, last_event, "finished"//new_line("a"))

            if (.not. allocated(error)) then
                call check(error, sentinel_process%index == process%index .and. &
                           sentinel_process%generation == process%generation, "the process")
            end if

            call destroy_process(process, st)
            if (allocated(error)) return
        end do
    end subroutine test_sentinel_called

    !> Check that sentinels are called in time even when other processes
    !> have written output.
    subroutine test_sentinel_multiple(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: k
        type(handle_type) :: process, other
        type(process_arg_type), allocatable :: command(:), slow(:)
        type(status_type) :: st
        integer, parameter :: types(2) = [PROCESS_CONNECTION_PIPE, PROCESS_CONNECTION_PTY]

        allocate (command(2), slow(3))
        command(1)%s = "echo"
        command(2)%s = "first"
        slow(1)%s = "sh"
        slow(2)%s = "-c"
        slow(3)%s = "sleep 10; echo second"

        do k = 1, 2
            call reset_records()
            call make_process("echo", command, process, st, noquery=.true., connection_type=types(k), &
                              sentinel=record_sentinel)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) return
            call make_process("bash", slow, other, st, noquery=.true., connection_type=types(k))
            call drain(process)
            call check(error, sentinel_calls == 1, "one call")
            if (.not. allocated(error)) call check(error, last_event, "finished"//new_line("a"))
            call destroy_process(process, st)
            call destroy_process(other, st)
            if (allocated(error)) return
        end do

        call process_shutdown()
    end subroutine test_sentinel_multiple

    !> The child's standard streams are terminals with a pty, pipes with
    !> a pipe (make-process/test-connection-type).
    subroutine connection_type(error, ctype, expected, tty)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: ctype
        character(len=*), intent(in) :: expected
        logical, intent(in) :: tty
        type(handle_type) :: process, buffer
        type(process_arg_type), allocatable :: command(:)
        type(status_type) :: st

        call get_or_make_buffer("*process-test*", buffer, st)
        call set_buffer(buffer, st)
        call erase_buffer(st)
        allocate (command(3))
        command(1)%s = "sh"
        command(2)%s = "-c"
        command(3)%s = "for fd in 0 1 2; do if [ -t $fd ]; then printf y; else printf n; fi; done"
        call reset_records()
        call make_process("test", command, process, st, buffer=buffer, noquery=.true., connection_type=ctype, &
                          sentinel=record_sentinel)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, len(process_tty_name(process)) > 0 .eqv. tty, "tty name")

        if (.not. allocated(error)) then
            call wait_for_sentinel()
            call check(error, process_exit_status(process), 0)
        end if

        if (.not. allocated(error)) call check(error, buffer_string(), expected)
        call destroy_process(process, st)
        call close_buffer(buffer, st)
    end subroutine connection_type

    subroutine test_connection_pty(error)
        type(error_type), allocatable, intent(out) :: error

        call connection_type(error, PROCESS_CONNECTION_PTY, "yyy", .true.)
    end subroutine test_connection_pty

    subroutine test_connection_pipe(error)
        type(error_type), allocatable, intent(out) :: error

        call connection_type(error, PROCESS_CONNECTION_PIPE, "nnn", .false.)
    end subroutine test_connection_pipe

    !> The default filter inserts the output at the process mark (keeping
    !> point where it was, before it); the default sentinel inserts the
    !> status message.
    subroutine test_default_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process, buffer
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st

        call get_or_make_buffer("*process-test*", buffer, st)
        call set_buffer(buffer, st)
        call erase_buffer(st)
        call sh_args("echo one; echo two", args)
        call start_process("printer", buffer, "sh", process, st, args)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call drain(process)
        call check(error, buffer_string(), "one"//new_line("a")//"two"//new_line("a")//new_line("a")// &
                   "Process printer finished"//new_line("a"))
        if (.not. allocated(error)) call check(error, process_status(process), "exit")
        ! An exited process is deleted (delete-exited-processes).
        if (.not. allocated(error)) call check(error,.not. is_process(get_process("printer")), "removed")
        call destroy_process(process, st)
        call close_buffer(buffer, st)
    end subroutine test_default_filter

    subroutine test_exit_abnormally(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process, buffer
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st

        call get_or_make_buffer("*process-test*", buffer, st)
        call set_buffer(buffer, st)
        call erase_buffer(st)
        call sh_args("exit 3", args)
        call start_process("failing", buffer, "sh", process, st, args)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call drain(process)
        call check(error, buffer_string(), new_line("a")//"Process failing exited abnormally with code 3"//new_line("a"))
        call destroy_process(process, st)
        call close_buffer(buffer, st)
    end subroutine test_exit_abnormally

    !> kill-process sends SIGKILL; the sentinel reports "killed".
    subroutine test_kill(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st

        call reset_records()
        call sh_args("sleep 100", args)
        call start_process("sleeper", null_handle(), "sh", process, st, args)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_process_sentinel(process, st, record_sentinel)
        call check(error, process_status(process), "run")
        if (allocated(error)) return
        call stop_process(st, process)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_for_sentinel()
        call check(error, last_event, "killed"//new_line("a"))
        if (.not. allocated(error)) call check(error, process_status(process), "signal")
        if (.not. allocated(error)) call check(error, process_exit_status(process), 9)
        call destroy_process(process, st)
    end subroutine test_kill

    !> delete-process runs the sentinel at once and forgets the process.
    subroutine test_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st

        call reset_records()
        call sh_args("sleep 100", args)
        call start_process("doomed", null_handle(), "sh", process, st, args)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_process_sentinel(process, st, record_sentinel)
        call delete_process(process, st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, sentinel_calls == 1, "sentinel run")
        if (.not. allocated(error)) call check(error, last_event, "killed"//new_line("a"))
        if (.not. allocated(error)) call check(error,.not. is_process(get_process("doomed")), "forgotten")
        if (.not. allocated(error)) call check(error, size(process_list()) == 0, "no processes")
        if (.not. allocated(error)) call check(error, process_status(process), "signal")
        call destroy_process(process, st)
        ! The worker still reaps the killed child.
        call process_shutdown()
    end subroutine test_delete

    !> The output reaches the buffer only when the main thread takes it,
    !> and the filter runs on the main thread.
    subroutine test_main_thread(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process, buffer
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st
        logical :: ready

        call get_or_make_buffer("*process-test*", buffer, st)
        call set_buffer(buffer, st)
        call erase_buffer(st)
        call reset_records()
        call sh_args("printf hello", args)
        call start_process("hello", buffer, "sh", process, st, args)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        ! Wait until a worker has reported something, without taking it.
        ready = wait_for_worker_messages(TIMEOUT*1000)
        call check(error, ready, "a message")
        if (.not. allocated(error)) call check(error, buffer_string(), "")

        if (.not. allocated(error)) then
            call set_process_filter(process, st, record_filter)
            call drain(process)
            call check(error, filtered, "hello")
        end if

        if (.not. allocated(error)) call check(error,.not. filter_off_main, "on the main thread")
        ! The filter replaced the default one, so the buffer only has the
        ! sentinel's message.
        if (.not. allocated(error)) call check(error, buffer_string(), new_line("a")//"Process hello finished"//new_line("a"))
        call destroy_process(process, st)
        call close_buffer(buffer, st)
    end subroutine test_main_thread

    !> A character split between two writes arrives whole.
    subroutine test_utf8_split(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st

        call reset_records()
        call sh_args("printf '\303'; sleep 1; printf '\251'", args)
        call start_process("split", null_handle(), "sh", process, st, args)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_process_filter(process, st, record_filter)
        call drain(process)
        call check(error, filtered, char(195)//char(169))
        call destroy_process(process, st)
    end subroutine test_utf8_split

    !> Input sent to the process, then end of file.
    subroutine test_send(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process
        type(status_type) :: st
        integer :: k
        integer, parameter :: types(2) = [PROCESS_CONNECTION_PIPE, PROCESS_CONNECTION_PTY]
        type(process_arg_type), allocatable :: command(:)

        allocate (command(1))
        command(1)%s = "cat"

        do k = 1, 2
            call reset_records()
            call make_process("cat", command, process, st, connection_type=types(k), filter=record_filter, &
                              sentinel=record_sentinel)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) return
            call process_send_string("abc"//new_line("a"), st, process)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) return
            call process_send_eof(st, process)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) return
            call wait_for_sentinel()
            call check(error, last_event, "finished"//new_line("a"))
            if (.not. allocated(error)) call check(error, index(filtered, "abc") == 1, filtered)
            call destroy_process(process, st)
            if (allocated(error)) return
        end do
    end subroutine test_send

    !> A name in use gets a <N> suffix.
    subroutine test_unique_names(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: p1, p2
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st

        call sh_args("sleep 100", args)
        call start_process("twin", null_handle(), "sh", p1, st, args)
        call start_process("twin", null_handle(), "sh", p2, st, args)
        call check(error, process_name(p1), "twin")
        if (.not. allocated(error)) call check(error, process_name(p2), "twin<1>")
        call destroy_process(p1, st)
        call destroy_process(p2, st)
        call process_shutdown()
    end subroutine test_unique_names

    !> Closing a buffer sends SIGHUP to its process.
    subroutine test_close_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process, buffer, other
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st

        call get_or_make_buffer("*process-test*", buffer, st)
        call set_buffer(buffer, st)
        call reset_records()
        call sh_args("sleep 100", args)
        call start_process("hup", buffer, "sh", process, st, args)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        other = get_buffer_process(buffer)
        call check(error, other%index == process%index, "the buffer's process")
        if (allocated(error)) return
        call set_process_sentinel(process, st, record_sentinel)
        call close_buffer(buffer, st)
        call wait_for_sentinel()
        call check(error, last_event, "hangup"//new_line("a"))
        if (.not. allocated(error)) call check(error, process_exit_status(process), 1)
        call destroy_process(process, st)
    end subroutine test_close_buffer

    subroutine test_missing_program(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process
        type(status_type) :: st

        call start_process("none", null_handle(), "fortmacs-no-such-program", process, st)
        call check(error,.not. st%ok(), "an error")

        if (.not. allocated(error)) then
            call check(error, format_error(st), "Searching for program: No such file or directory, fortmacs-no-such-program")
        end if

        if (.not. allocated(error)) call check(error, size(process_list()) == 0, "not listed")
    end subroutine test_missing_program

    !> Workers and the main thread run the same src/sys procedures at once
    !> (errno while polling): many short-lived processes, the main thread
    !> polling for their messages without waiting.  (With -fcheck=all a
    !> non-recursive procedure has a static recursion guard, which two
    !> threads in it at once trip.)
    subroutine test_workers_beside_main(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st
        integer(int64) :: start
        integer :: k
        logical :: ready

        call sh_args("exit 0", args)

        do k = 1, 100
            call start_process("short", null_handle(), "sh", process, st, args)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) return
            call system_clock(start)
            ready = .false.

            do while (.not. ready .and. elapsed(start) < TIMEOUT)
                ready = wait_for_worker_messages(0)
            end do

            call process_shutdown()
            call destroy_process(process, st)
        end do
    end subroutine test_workers_beside_main

    !> `list_processes' lists the processes in *Process List*; d deletes
    !> the process at point.
    subroutine test_list_processes(error)
        use buffer_fm, only: get_buffer, current_buffer
        use editfns_fm, only: goto_char, point_min
        use search_fm, only: search_forward, SEARCH_NOERROR
        use bindings_fm, only: init_bindings
        use simple_fm, only: list_processes, process_menu_delete_process
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: process, b, old
        type(process_arg_type), allocatable :: args(:)
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: pos

        call init_bindings()
        call sh_args("sleep 30", args)
        call start_process("fm-listed", null_handle(), "sh", process, st, args)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call list_processes(st)
        call check(error, st%ok(), format_error(st))
        b = get_buffer("*Process List*")
        if (.not. allocated(error)) call check(error, b%index /= 0, "the buffer")

        if (allocated(error)) then
            call delete_process(process, st)
            return
        end if

        old = current_buffer()
        call set_buffer(b, st)
        text = buffer_string()
        call check(error, index(text, "fm-listed") > 0 .and. index(text, "run") > 0 .and. &
                   index(text, "sh -c sleep 30") > 0, "the listing: "//text)

        if (.not. allocated(error)) then
            call goto_char(point_min())
            call search_forward("fm-listed", pos, st, noerror=SEARCH_NOERROR)
            call process_menu_delete_process(st)
            call check(error, st%ok(), format_error(st))
        end if

        if (.not. allocated(error)) call check(error, index(buffer_string(), "fm-listed") == 0, "deleted")
        call set_buffer(old, st)
        call close_buffer(b, st)
        if (process_status(process) == "run") call delete_process(process, st)
    end subroutine test_list_processes
end module test_process_fm

! sys_worker_fm.f90 -- worker threads reading subprocess output.
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

!> Worker threads and the queue they report through.
!>
!> Not a port: upstream reads subprocess output on the main thread
!> (select in process.c:wait_reading_process_output, SIGCHLD and waitpid
!> in handle_child_signal).  Here each asynchronous subprocess has a POSIX
!> worker thread that owns the descriptor of its output and a pidfd for
!> its exit: it reads what the child writes and, once the child has
!> exited, whatever output is left, then reaps it.  Workers see only
!> their own copied descriptors and pid, and touch no editor state: they
!> append messages (output bytes, then the raw wait status) to a queue
!> under a mutex and write a byte to a self-pipe.  The main thread polls
!> the self-pipe's read end (`worker_wake_fd') together with the terminal
!> while waiting for input, takes the messages (`take_worker_messages')
!> and runs the filters and sentinels itself.  A worker's last message is
!> its exit, after which taking it joins the thread.
!>
!> Every procedure a worker runs is `recursive' (so its locals are on the
!> thread's stack, never static) and saves nothing.  Workers block every
!> signal, so the handlers that set flags for the command loop run on the
!> main thread.  If pidfd_open is unavailable (old kernels, valgrind), a
!> worker polls with a 50 ms timeout and checks waitpid(WNOHANG).
module sys_worker_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use, intrinsic :: iso_c_binding, only: c_int, c_long, c_size_t, c_char, c_ptr, c_int32_t, c_int64_t, &
                                                                              c_null_ptr, c_funloc, c_loc, c_f_pointer, c_short
    use sys_linux_fm, only: pollfd_type, POLLIN, POLLHUP, POLLERR, c_poll, c_read, c_write, c_close, &
                            c_pipe2, c_waitpid, c_pthread_create, c_pthread_join, c_pthread_self, &
                            c_pthread_mutex_lock, c_pthread_mutex_unlock, c_pthread_sigmask, c_pidfd_open, &
                            errno, EINTR, EAGAIN, O_CLOEXEC, O_NONBLOCK, SIG_BLOCK, SIG_SETMASK, WNOHANG
    implicit none(type, external)
    private

    public :: worker_message_type, start_reader, take_worker_messages, worker_wake_fd, &
              wait_for_worker_messages, worker_count, on_main_thread, note_main_thread
    public :: WORKER_OUTPUT, WORKER_EXIT

    !> Message kinds: output read from the child, and its exit (the last
    !> message of a worker).
    integer, parameter :: WORKER_OUTPUT = 1, WORKER_EXIT = 2

    !> The most a worker reads at once (process.c read-process-output-max).
    integer, parameter :: READMAX = 65536

    !> The most workers at once (upstream is limited by FD_SETSIZE too).
    integer, parameter :: MAX_WORKERS = 1024
    !> errno EMFILE (asm-generic/errno-base.h).
    integer(int32), parameter :: EMFILE = 24

    !> A message from a worker.
    type :: worker_message_type
        !> The worker it comes from (as `start_reader' returned it).
        integer(int32) :: worker = 0
        integer :: kind = 0
        !> The output (WORKER_OUTPUT).
        character(len=:), allocatable :: bytes
        !> The raw wait status (WORKER_EXIT).
        integer(int32) :: wait_status = 0
    end type worker_message_type

    !> What a worker is given: copies, read by its thread only.
    type, bind(C) :: reader_job_type
        integer(c_int32_t) :: slot = 0
        integer(c_int32_t) :: pid = 0
        integer(c_int32_t) :: fd = -1
        integer(c_int32_t) :: pidfd = -1
    end type reader_job_type

    type(reader_job_type), target :: jobs(MAX_WORKERS)
    integer(c_long) :: threads(MAX_WORKERS) = 0
    logical :: in_use(MAX_WORKERS) = .false.
    integer :: active = 0

    !> The queue, shared with the workers under `queue_mutex' (a
    !> pthread_mutex_t; all zeros is PTHREAD_MUTEX_INITIALIZER).
    integer(c_int64_t), target :: queue_mutex(8) = 0_c_int64_t
    type(worker_message_type), allocatable :: queue(:)
    integer :: queue_count = 0

    !> The self-pipe: workers write a byte to wake_pipe(2) after queuing.
    integer(c_int) :: wake_pipe(2) = -1

    !> The main thread (pthread_self), once noted.
    integer(c_long) :: main_thread = 0
    logical :: main_noted = .false.

contains

    !> Remember the calling thread as the main thread (done when the
    !> first worker starts).
    subroutine note_main_thread()
        if (main_noted) return
        main_thread = c_pthread_self()
        main_noted = .true.
    end subroutine note_main_thread

    !> Whether the caller runs on the main thread (true before any worker
    !> has started).
    recursive logical function on_main_thread()
        on_main_thread = .true.
        if (main_noted) on_main_thread = c_pthread_self() == main_thread
    end function on_main_thread

    !> Start a worker for child `pid' reading descriptor `fd', which it
    !> owns from now on (it closes it).  `worker' identifies its messages;
    !> `err' is 0 or errno (the descriptor is then closed).
    subroutine start_reader(pid, fd, worker, err)
        integer(int32), intent(in) :: pid, fd
        integer(int32), intent(out) :: worker, err
        integer(c_int64_t) :: all(16), old(16), ignored(16)
        integer(c_int) :: r, r2, slot

        call note_main_thread()
        worker = 0
        err = 0

        if (wake_pipe(1) < 0) then
            if (c_pipe2(wake_pipe, ior(O_CLOEXEC, O_NONBLOCK)) /= 0) then
                err = errno()
                r = c_close(fd)
                return
            end if
        end if

        slot = findloc(in_use, .false., dim=1)

        if (slot == 0) then
            ! As upstream when FD_SETSIZE is reached.
            err = EMFILE
            r = c_close(fd)
            return
        end if

        jobs(slot)%slot = slot
        jobs(slot)%pid = pid
        jobs(slot)%fd = fd
        jobs(slot)%pidfd = c_pidfd_open(pid, 0_c_int)
        ! The thread inherits the signal mask: block everything in it.
        all = -1_c_int64_t
        r = c_pthread_sigmask(SIG_BLOCK, all, old)
        r = c_pthread_create(threads(slot), c_null_ptr, c_funloc(reader_main), c_loc(jobs(slot)))
        r2 = c_pthread_sigmask(SIG_SETMASK, old, ignored)

        if (r /= 0) then
            err = r
            r = c_close(fd)
            if (jobs(slot)%pidfd >= 0) r = c_close(jobs(slot)%pidfd)
            return
        end if

        in_use(slot) = .true.
        active = active + 1
        worker = slot
    end subroutine start_reader

    !> The number of workers not yet joined.
    integer function worker_count()
        worker_count = active
    end function worker_count

    !> The descriptor the main thread polls for messages (-1 before the
    !> first worker).
    integer(int32) function worker_wake_fd()
        worker_wake_fd = wake_pipe(1)
    end function worker_wake_fd

    !> Wait up to `timeout_ms' milliseconds (negative: no limit) for a
    !> message; true if one may be waiting.  Doesn't take it.
    logical function wait_for_worker_messages(timeout_ms)
        integer(int32), intent(in) :: timeout_ms
        type(pollfd_type) :: pfd(1)
        integer(c_int) :: r

        wait_for_worker_messages = .false.
        if (wake_pipe(1) < 0) return
        pfd(1)%fd = wake_pipe(1)
        pfd(1)%events = POLLIN

        do
            pfd(1)%revents = 0
            r = c_poll(pfd, 1_c_long, timeout_ms)
            if (r >= 0 .or. errno() /= EINTR) exit
        end do

        wait_for_worker_messages = r > 0
    end function wait_for_worker_messages

    !> Take the queued messages, oldest first, and join the workers whose
    !> exit is among them.  Main thread only.
    subroutine take_worker_messages(msgs)
        type(worker_message_type), allocatable, intent(out) :: msgs(:)
        character(kind=c_char) :: junk(64)
        integer(c_long) :: n
        integer(c_int) :: r
        integer :: k, count, slot

        if (wake_pipe(1) >= 0) then
            do
                n = c_read(wake_pipe(1), junk, 64_c_size_t)

                if (n <= 0) then
                    if (n < 0) then
                        if (errno() == EINTR) cycle
                    end if

                    exit
                end if
            end do
        end if

        r = c_pthread_mutex_lock(queue_mutex)
        count = queue_count
        allocate (msgs(count))

        do k = 1, count
            msgs(k)%worker = queue(k)%worker
            msgs(k)%kind = queue(k)%kind
            msgs(k)%wait_status = queue(k)%wait_status
            if (allocated(queue(k)%bytes)) call move_alloc(queue(k)%bytes, msgs(k)%bytes)
        end do

        if (allocated(queue)) deallocate (queue)
        queue_count = 0
        r = c_pthread_mutex_unlock(queue_mutex)

        do k = 1, count
            if (msgs(k)%kind == WORKER_EXIT) then
                slot = msgs(k)%worker
                r = c_pthread_join(threads(slot), c_null_ptr)
                in_use(slot) = .false.
                active = active - 1
            end if
        end do
    end subroutine take_worker_messages

    ! ------------------------------------------------------------------
    ! The worker side.  Reentrant: no saved locals, shared state only
    ! under the mutex.
    ! ------------------------------------------------------------------
    !> The body of a worker thread: read the child's output until it
    !> exits, then the rest of it, then reap it.
    recursive function reader_main(arg) result(ret) bind(C)
        type(c_ptr), value :: arg
        type(c_ptr) :: ret
        type(reader_job_type), pointer :: job
        type(pollfd_type) :: pfd(2)
        character(kind=c_char), allocatable :: buf(:)
        integer(c_int) :: r, wstatus, timeout
        integer(c_int32_t) :: w
        logical :: exited

        ret = c_null_ptr
        call c_f_pointer(arg, job)
        allocate (buf(READMAX))
        pfd(1)%fd = job%fd
        pfd(1)%events = POLLIN
        pfd(2)%fd = job%pidfd
        pfd(2)%events = POLLIN
        timeout = -1
        if (job%pidfd < 0) timeout = 50
        exited = .false.
        wstatus = 0

        do
            pfd(1)%revents = 0
            pfd(2)%revents = 0
            r = c_poll(pfd, 2_c_long, timeout)

            if (r < 0) then
                if (errno() == EINTR) cycle
                exit
            end if

            if (pfd(1)%fd >= 0 .and. pfd(1)%revents /= 0) then
                if (.not. read_some(job%slot, pfd(1)%fd, buf)) pfd(1)%fd = -1
            end if

            if (job%pidfd >= 0) then
                exited = pfd(2)%revents /= 0
            else
                w = c_waitpid(job%pid, wstatus, WNOHANG)
                exited = w == job%pid
            end if

            if (exited) exit
        end do

        ! The child has exited: read what output is left, without waiting.
        do while (pfd(1)%fd >= 0)
            pfd(1)%revents = 0
            r = c_poll(pfd(1:1), 1_c_long, 0_c_int)

            if (r < 0) then
                if (errno() == EINTR) cycle
            end if

            if (r <= 0) exit
            if (.not. read_some(job%slot, pfd(1)%fd, buf)) exit
        end do

        if (job%pidfd >= 0 .or. .not. exited) then
            do
                w = c_waitpid(job%pid, wstatus, 0_c_int)
                if (w >= 0) exit
                if (errno() /= EINTR) exit
            end do
        end if

        r = c_close(job%fd)
        if (job%pidfd >= 0) r = c_close(job%pidfd)
        call post(job%slot, WORKER_EXIT, buf, 0, wstatus)
    end function reader_main

    !> Read once from `fd' and post what was read; false at end of file
    !> (or on an error such as a pty's EIO).
    recursive logical function read_some(slot, fd, buf) result(more)
        integer(c_int32_t), intent(in) :: slot, fd
        character(kind=c_char), intent(inout) :: buf(:)
        integer(c_long) :: n
        integer(c_int) :: e

        more = .true.

        do
            n = c_read(fd, buf, int(size(buf), c_size_t))
            if (n >= 0) exit
            e = errno()
            if (e == EINTR) cycle
            more = e == EAGAIN
            return
        end do

        if (n == 0) then
            more = .false.
            return
        end if

        call post(slot, WORKER_OUTPUT, buf, int(n), 0_c_int)
    end function read_some

    !> Queue a message and wake the main thread.
    recursive subroutine post(slot, kind, buf, n, wait_status)
        integer(c_int32_t), intent(in) :: slot
        integer, intent(in) :: kind, n
        character(kind=c_char), intent(in) :: buf(:)
        integer(c_int), intent(in) :: wait_status
        type(worker_message_type), allocatable :: grown(:)
        character(kind=c_char) :: one(1)
        integer(c_int) :: r
        integer(c_long) :: written
        integer :: k

        r = c_pthread_mutex_lock(queue_mutex)
        if (.not. allocated(queue)) allocate (queue(16))

        if (queue_count == size(queue)) then
            allocate (grown(2*queue_count))

            do k = 1, queue_count
                grown(k)%worker = queue(k)%worker
                grown(k)%kind = queue(k)%kind
                grown(k)%wait_status = queue(k)%wait_status
                if (allocated(queue(k)%bytes)) call move_alloc(queue(k)%bytes, grown(k)%bytes)
            end do

            call move_alloc(grown, queue)
        end if

        queue_count = queue_count + 1
        queue(queue_count)%worker = slot
        queue(queue_count)%kind = kind
        queue(queue_count)%wait_status = wait_status
        allocate (character(len=n) :: queue(queue_count)%bytes)

        do k = 1, n
            queue(queue_count)%bytes(k:k) = buf(k)
        end do

        r = c_pthread_mutex_unlock(queue_mutex)
        one(1) = char(1)
        ! A full pipe already wakes the main thread.
        written = c_write(wake_pipe(2), one, 1_c_size_t)
    end subroutine post
end module sys_worker_fm

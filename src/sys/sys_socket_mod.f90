! sys_socket_mod.f90 -- TCP connections through the Linux socket calls.
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

!> Stream sockets: `fortmacs_connect' opens a TCP connection to a host
!> and service (getaddrinfo, socket, connect), `fortmacs_send' writes to
!> it without raising SIGPIPE; reading uses sysdep_mod's
!> `input_available' and `fortmacs_read', closing its `close_fd'.
!> `fortmacs_listen_loopback', `fortmacs_accept', `fortmacs_fork' and
!> `fortmacs_exit_now' let a test run a server on 127.0.0.1 in a child
!> process.
!>
!> Not a port: the blocking part of process.c's make-network-process
!> (network_lookup_address_info_1 and connect_network_socket), without
!> :nowait, servers or other families.  Layouts and constants, the same
!> on x86-64 and aarch64:
!>
!> - struct addrinfo: glibc netdb.h (four ints, socklen_t, three pointers)
!> - AF_INET, AF_UNSPEC: glibc bits/socket.h;  SOCK_STREAM,
!>   SOCK_CLOEXEC (= O_CLOEXEC): bits/socket_type.h;  MSG_NOSIGNAL:
!>   bits/socket.h
!> - struct sockaddr_in: netinet/in.h (16 bytes: family in host order,
!>   port and address in network order); both targets are little-endian
module sys_socket_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use, intrinsic :: iso_c_binding, only: c_int, c_int32_t, c_long, c_size_t, c_char, c_ptr, c_null_ptr, &
                                                                              c_null_char, c_associated, c_f_pointer
    use sys_linux_mod, only: errno, c_string_to_fortran, c_close, c_fork, c_exit, EINTR, O_CLOEXEC
    implicit none(type, external)
    private

    public :: fortmacs_connect, fortmacs_send, fortmacs_listen_loopback, fortmacs_accept, fortmacs_fork, fortmacs_exit_now

    !> Address families (bits/socket.h).
    integer(c_int), parameter :: AF_UNSPEC = 0, AF_INET = 2
    !> Socket type and its close-on-exec flag (bits/socket_type.h).
    integer(c_int), parameter :: SOCK_STREAM = 1, SOCK_CLOEXEC = O_CLOEXEC
    !> send flag: no SIGPIPE when the peer has gone (bits/socket.h).
    integer(c_int), parameter :: MSG_NOSIGNAL = int(z'4000', c_int)

    !> struct addrinfo (glibc netdb.h).
    type, bind(C) :: addrinfo_type
        integer(c_int) :: ai_flags = 0, ai_family = 0, ai_socktype = 0, ai_protocol = 0
        integer(c_int32_t) :: ai_addrlen = 0
        type(c_ptr) :: ai_addr = c_null_ptr, ai_canonname = c_null_ptr, ai_next = c_null_ptr
    end type addrinfo_type

    interface
        integer(c_int) function c_getaddrinfo(node, service, hints, res) bind(C, name="getaddrinfo")
            import :: c_int, c_char, c_ptr, addrinfo_type
            implicit none(type, external)
            character(kind=c_char), intent(in) :: node(*), service(*)
            type(addrinfo_type), intent(in) :: hints
            type(c_ptr), intent(out) :: res
        end function c_getaddrinfo

        subroutine c_freeaddrinfo(res) bind(C, name="freeaddrinfo")
            import :: c_ptr
            implicit none(type, external)
            type(c_ptr), value :: res
        end subroutine c_freeaddrinfo

        type(c_ptr) function c_gai_strerror(code) bind(C, name="gai_strerror")
            import :: c_int, c_ptr
            implicit none(type, external)
            integer(c_int), value :: code
        end function c_gai_strerror

        integer(c_int) function c_socket(domain, type, protocol) bind(C, name="socket")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: domain, type, protocol
        end function c_socket

        integer(c_int) function c_connect(fd, addr, addrlen) bind(C, name="connect")
            import :: c_int, c_int32_t, c_ptr
            implicit none(type, external)
            integer(c_int), value :: fd
            type(c_ptr), value :: addr
            integer(c_int32_t), value :: addrlen
        end function c_connect

        integer(c_int) function c_bind(fd, addr, addrlen) bind(C, name="bind")
            import :: c_int, c_int32_t, c_char
            implicit none(type, external)
            integer(c_int), value :: fd
            character(kind=c_char), intent(in) :: addr(*)
            integer(c_int32_t), value :: addrlen
        end function c_bind

        integer(c_int) function c_listen(fd, backlog) bind(C, name="listen")
            import :: c_int
            implicit none(type, external)
            integer(c_int), value :: fd, backlog
        end function c_listen

        integer(c_int) function c_accept4(fd, addr, addrlen, flags) bind(C, name="accept4")
            import :: c_int, c_ptr
            implicit none(type, external)
            integer(c_int), value :: fd
            type(c_ptr), value :: addr, addrlen
            integer(c_int), value :: flags
        end function c_accept4

        integer(c_int) function c_getsockname(fd, addr, addrlen) bind(C, name="getsockname")
            import :: c_int, c_int32_t, c_char
            implicit none(type, external)
            integer(c_int), value :: fd
            character(kind=c_char), intent(out) :: addr(*)
            integer(c_int32_t), intent(inout) :: addrlen
        end function c_getsockname

        integer(c_long) function c_send(fd, buffer, n, flags) bind(C, name="send")
            import :: c_int, c_long, c_size_t, c_char
            implicit none(type, external)
            integer(c_int), value :: fd
            character(kind=c_char), intent(in) :: buffer(*)
            integer(c_size_t), value :: n
            integer(c_int), value :: flags
        end function c_send
    end interface

contains

    !> Open a TCP connection to SERVICE (a port number or service name) on
    !> HOST; FD is the connected socket (close-on-exec), or -1.  When the
    !> lookup fails, LOOKUP_ERROR is getaddrinfo's message ("Name or service
    !> not known"); when no address could be connected to, ERR is the errno
    !> of the last attempt.  Both are empty and 0 on success.  Not a port:
    !> cf. process.c:network_lookup_address_info_1 and
    !> connect_network_socket (blocking).
    subroutine fortmacs_connect(host, service, fd, err, lookup_error)
        character(len=*), intent(in) :: host, service
        integer(int32), intent(out) :: fd, err
        character(len=:), allocatable, intent(out) :: lookup_error
        type(addrinfo_type) :: hints
        type(addrinfo_type), pointer :: ai
        type(c_ptr) :: res, p
        integer(c_int) :: r, s

        fd = -1
        err = 0
        lookup_error = ""
        hints%ai_family = AF_UNSPEC
        hints%ai_socktype = SOCK_STREAM
        r = c_getaddrinfo(host//c_null_char, service//c_null_char, hints, res)

        if (r /= 0) then
            lookup_error = c_string_to_fortran(c_gai_strerror(r))
            return
        end if

        p = res

        do while (c_associated(p))
            call c_f_pointer(p, ai)
            p = ai%ai_next
            s = c_socket(ai%ai_family, ior(ai%ai_socktype, SOCK_CLOEXEC), ai%ai_protocol)

            if (s < 0) then
                err = errno()
                cycle
            end if

            if (c_connect(s, ai%ai_addr, ai%ai_addrlen) == 0) then
                fd = s
                err = 0
                exit
            end if

            err = errno()
            r = c_close(s)
        end do

        call c_freeaddrinfo(res)
    end subroutine fortmacs_connect

    !> Write all of DATA to the socket FD; ERR is 0, or the errno of the
    !> failure (EPIPE when the peer has closed: no SIGPIPE is raised).
    !> Not a port.
    subroutine fortmacs_send(fd, data, err)
        integer(int32), intent(in) :: fd
        character(len=*), intent(in) :: data
        integer(int32), intent(out) :: err
        integer(c_long) :: r
        integer(int64) :: done

        err = 0
        done = 0

        do while (done < len(data, kind=int64))
            r = c_send(fd, data(done + 1:), int(len(data, kind=int64) - done, c_size_t), MSG_NOSIGNAL)

            if (r < 0) then
                err = errno()
                if (err == EINTR) cycle
                return
            end if

            done = done + r
        end do

        err = 0
    end subroutine fortmacs_send

    !> A listening TCP socket FD on 127.0.0.1 at a port the system picks,
    !> PORT; ERR is 0 or the errno of the failure (FD is then -1).
    !> Not a port.
    subroutine fortmacs_listen_loopback(fd, port, err)
        integer(int32), intent(out) :: fd, port, err
        character(kind=c_char) :: address(16)
        integer(c_int32_t) :: length
        integer(c_int) :: s, r

        fd = -1
        port = 0
        err = 0
        ! struct sockaddr_in: AF_INET (host order), port 0, 127.0.0.1.
        address = achar(0)
        address(1) = achar(AF_INET)
        address(5) = achar(127)
        address(8) = achar(1)
        s = c_socket(AF_INET, ior(SOCK_STREAM, SOCK_CLOEXEC), 0_c_int)

        if (s < 0) then
            err = errno()
            return
        end if

        if (c_bind(s, address, 16_c_int32_t) /= 0) then
            err = errno()
            r = c_close(s)
            return
        end if

        if (c_listen(s, 1_c_int) /= 0) then
            err = errno()
            r = c_close(s)
            return
        end if

        length = 16

        if (c_getsockname(s, address, length) /= 0) then
            err = errno()
            r = c_close(s)
            return
        end if

        fd = s
        port = iachar(address(3))*256 + iachar(address(4))
    end subroutine fortmacs_listen_loopback

    !> Accept a connection on the listening socket FD: CONNECTION is its
    !> socket (close-on-exec), or -1 with ERR the errno.  Not a port.
    subroutine fortmacs_accept(fd, connection, err)
        integer(int32), intent(in) :: fd
        integer(int32), intent(out) :: connection, err

        err = 0

        do
            connection = c_accept4(fd, c_null_ptr, c_null_ptr, SOCK_CLOEXEC)
            if (connection >= 0) return
            err = errno()
            if (err /= EINTR) return
        end do
    end subroutine fortmacs_accept

    !> Fork the process: 0 in the child, the child's pid in the parent, -1
    !> on failure.  The child must end with `fortmacs_exit_now'.  Not a
    !> port.
    integer(int32) function fortmacs_fork() result(pid)
        pid = c_fork()
    end function fortmacs_fork

    !> End the process at once with exit status CODE, without flushing
    !> Fortran units (the end of a forked child).  Not a port.
    subroutine fortmacs_exit_now(code)
        integer(int32), intent(in) :: code

        call c_exit(int(code, c_int))
    end subroutine fortmacs_exit_now
end module sys_socket_mod

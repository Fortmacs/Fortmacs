! test_comint_fm.f90 -- tests for comint_fm.
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

!> Own tests: upstream's comint-tests.el tests password prompts, which
!> are not ported.  The processes are `cat' and `sleep' over pipes; the
!> waits are bounded by a wall-clock limit, not fixed delays.
module test_comint_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: nil
    use buffer_fm, only: buffers, cur, set_buffer, kill_buffer, buffer_live_p
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_max, goto_char, buffer_string
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    use process_fm, only: accept_process_output, get_buffer_process, process_status, delete_process, &
                          process_connection_type, process_shutdown, process_arg_type
    use comint_fm, only: make_comint, comint_send_input, comint_arguments, comint_input_ring_item, &
                         comint_input_ring_length, comint_previous_input, comint_interrupt_subjob, comint_bol, &
                         comint_replace_by_expanded_history, comint_insert_previous_argument, comint_input_autoexpand, &
                         COMINT_AUTOEXPAND_T, COMINT_AUTOEXPAND_INPUT, comint_within_quotes, &
                         comint_password_prompt_regexp, comint_send_invisible, set_comint_password_function, &
                         comint_password_proc
    use search_fm, only: string_match
    implicit none(type, external)
    private

    public :: collect_comint_fm

    integer, parameter :: TIMEOUT = 60

contains

    subroutine collect_comint_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("comint-arguments", test_arguments), &
                    new_unittest("comint-send-input", test_send_input), &
                    new_unittest("comint-command-loop", test_command_loop), &
                    new_unittest("comint-interrupt-subjob", test_interrupt), &
                    new_unittest("comint-history-expansion", test_history_expansion), &
                    new_unittest("comint-within-quotes", test_within_quotes), &
                    new_unittest("comint-test-password-regexp", test_password_regexp), &
                    new_unittest("comint-test-no-password-function", test_no_password_function), &
                    new_unittest("comint-test-password-function-with-value", test_password_function_with_value), &
                    new_unittest("comint-test-password-function-with-nil", test_password_function_with_nil) &
                    ]
    end subroutine collect_comint_fm

    !> Wait until the current buffer contains TEXT, or its process has
    !> exited if TEXT is empty.
    subroutine wait_for(text)
        character(len=*), intent(in) :: text
        type(handle_type) :: proc
        type(status_type) :: st
        integer(int64) :: start, now, rate
        logical :: got

        call system_clock(start, rate)

        do
            if (len(text) > 0) then
                if (index(buffer_string(), text) > 0) exit
            end if

            proc = get_buffer_process(current())
            if (proc%index == 0) exit

            if (len(text) == 0) then
                if (process_status(proc) /= "run") exit
            end if

            call system_clock(now)
            if ((now - start)/max(rate, 1_int64) > TIMEOUT) exit
            call accept_process_output(got, st, proc, 1.0)
        end do
    end subroutine wait_for

    function current() result(h)
        use buffer_fm, only: current_buffer
        type(handle_type) :: h

        h = current_buffer()
    end function current

    !> End the process of buffer H and kill it.
    subroutine finish(h)
        type(handle_type), intent(in) :: h
        type(handle_type) :: proc
        type(status_type) :: st

        if (.not. buffer_live_p(h)) return
        proc = get_buffer_process(h)
        if (proc%index /= 0) call delete_process(proc, st)
        call kill_buffer(h, st)
        call process_shutdown()
    end subroutine finish

    subroutine test_arguments(error)
        type(error_type), allocatable, intent(out) :: error

        call init_bindings()
        call check(error, comint_arguments("ls -l /tmp", 0, 0), "ls")
        if (.not. allocated(error)) call check(error, comint_arguments("ls -l /tmp", 1, 2), "-l /tmp")
        if (.not. allocated(error)) call check(error, comint_arguments("ls -l /tmp", -1, -1), "/tmp")
        if (.not. allocated(error)) call check(error, comint_arguments("echo 'a b' c", 1, 1), "'a b'")
        if (.not. allocated(error)) call check(error, comint_arguments("cat a\ b", 1, 1), "a\ b")
    end subroutine test_arguments

    !> RET sends the input after the process mark, which goes into the
    !> input ring; M-p brings it back.
    subroutine test_send_input(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: saved

        call init_bindings()
        saved = process_connection_type%value
        process_connection_type = .false.
        call make_comint("fm-cat", "cat", h, st)
        process_connection_type = saved
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_buffer(h, st)
        call show_alone()
        call check(error, buffers(cur)%name, "*fm-cat*")
        if (.not. allocated(error)) call check(error, buffers(cur)%major_mode, "comint_mode")
        if (allocated(error)) return
        call goto_char(point_max())
        call insert("hello", st)
        call comint_send_input(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_for("hello"//new_line("a")//"hello"//new_line("a"))
        call check(error, buffer_string(), "hello"//new_line("a")//"hello"//new_line("a"))
        if (.not. allocated(error)) call check(error, comint_input_ring_length(), 1)
        if (.not. allocated(error)) call check(error, comint_input_ring_item(0), "hello")
        if (allocated(error)) return
        call comint_previous_input(1, st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "hello"//new_line("a")//"hello"//new_line("a")// &
                                               "hello")
        call finish(h)
    end subroutine test_send_input

    !> Typing input and RET through the command loop; C-a goes to the
    !> start of the input, after the output.
    subroutine test_command_loop(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: saved
        integer(int64) :: input_start

        call init_bindings()
        saved = process_connection_type%value
        process_connection_type = .false.
        call make_comint("fm-cat-2", "cat", h, st)
        process_connection_type = saved
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_buffer(h, st)
        call show_alone()
        call discard_input()
        prefix_arg = nil()
        call kbd_store_bytes("one"//achar(13))
        call command_loop(st)
        call wait_for("one"//new_line("a")//"one"//new_line("a"))
        call check(error, buffer_string(), "one"//new_line("a")//"one"//new_line("a"))
        if (allocated(error)) return
        input_start = point_max()
        call goto_char(point_max())
        call insert("two", st)
        call comint_bol(.false.)
        call check(error, point(), input_start)
        call finish(h)
    end subroutine test_command_loop

    !> C-c C-c interrupts the process.
    subroutine test_interrupt(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, proc
        type(status_type) :: st
        type(process_arg_type) :: switches(1)
        logical :: saved

        call init_bindings()
        saved = process_connection_type%value
        process_connection_type = .false.
        switches(1)%s = "100"
        call make_comint("fm-sleep", "sleep", h, st, switches=switches)
        process_connection_type = saved
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_buffer(h, st)
        call comint_interrupt_subjob(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call wait_for("Process fm-sleep interrupt")
        proc = get_buffer_process(h)
        if (proc%index /= 0) call check(error, process_status(proc) /= "run", "still running")
        if (.not. allocated(error)) call check(error, index(buffer_string(), "Process fm-sleep interrupt") > 0, &
                                               buffer_string())
        call finish(h)
    end subroutine test_interrupt

    !> History references expand before point: !!, !!:N, !-N, !STRING,
    !> ^OLD^NEW; C-c . inserts the last argument of the previous input;
    !> RET expands the input with `comint_input_autoexpand' set to
    !> COMINT_AUTOEXPAND_INPUT.
    subroutine test_history_expansion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: saved, done
        integer(int64) :: pm

        call init_bindings()
        saved = process_connection_type%value
        process_connection_type = .false.
        call make_comint("fm-cat-history", "cat", h, st)
        process_connection_type = saved
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_buffer(h, st)
        call show_alone()
        call goto_char(point_max())
        call insert("echo one two three", st)
        call comint_send_input(st)
        call wait_for("three"//new_line("a")//"echo one two three"//new_line("a"))
        call goto_char(point_max())
        pm = point_max()
        call comint_input_autoexpand%set_local(COMINT_AUTOEXPAND_T)
        ! !!:2 is the second argument of the previous input.
        call insert("!!:2", st)
        done = comint_replace_by_expanded_history(st)
        call check(error, done, "expanded: "//format_error(st))
        if (.not. allocated(error)) call check(error, input_text(pm), "two")

        ! !! is the whole previous input.
        if (.not. allocated(error)) then
            call clear_input(pm)
            call insert("!!", st)
            done = comint_replace_by_expanded_history(st)
            call check(error, input_text(pm), "echo one two three")
        end if

        ! !ec is the most recent input starting with "ec".
        if (.not. allocated(error)) then
            call clear_input(pm)
            call insert("x !ec:$", st)
            done = comint_replace_by_expanded_history(st)
            call check(error, input_text(pm), "x three")
        end if

        ! ^two^2 substitutes in the previous input.
        if (.not. allocated(error)) then
            call clear_input(pm)
            call insert("^two^2", st)
            done = comint_replace_by_expanded_history(st)
            call check(error, input_text(pm), "echo one 2 three")
        end if

        ! Quoted references stay.
        if (.not. allocated(error)) then
            call clear_input(pm)
            call insert("echo '!!'", st)
            done = comint_replace_by_expanded_history(st)
            call check(error, input_text(pm), "echo '!!'")
        end if

        ! C-c . inserts the last argument.
        if (.not. allocated(error)) then
            call clear_input(pm)
            call insert("ls", st)
            call comint_insert_previous_argument(0, .false., st)
            call check(error, input_text(pm), "ls three")
        end if

        ! RET expands the input.
        if (.not. allocated(error)) then
            call clear_input(pm)
            call comint_input_autoexpand%set_local(COMINT_AUTOEXPAND_INPUT)
            call insert("!!:1", st)
            call comint_send_input(st)
            call wait_for("one"//new_line("a")//"one"//new_line("a"))
            call check(error, comint_input_ring_item(0), "one")
        end if

        call finish(h)

    contains

        ! The text from PM to the end of the buffer.
        function input_text(from) result(text)
            use editfns_fm, only: buffer_substring
            integer(int64), intent(in) :: from
            character(len=:), allocatable :: text
            type(status_type) :: st2

            call buffer_substring(from, point_max(), text, st2)
        end function input_text

        ! Delete the text from PM to the end of the buffer.
        subroutine clear_input(from)
            use editfns_fm, only: delete_region
            integer(int64), intent(in) :: from
            type(status_type) :: st2

            call delete_region(from, point_max(), st2)
            call goto_char(point_max())
        end subroutine clear_input
    end subroutine test_history_expansion

    !> Whether a position is inside quotes.
    subroutine test_within_quotes(error)
        use buffer_fm, only: get_buffer_create
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call get_buffer_create(" comint quotes", h, st)
        call set_buffer(h, st)
        call insert("echo 'a b' ""c", st)
        call check(error, comint_within_quotes(1_int64, 9_int64), "inside single quotes")
        if (.not. allocated(error)) call check(error,.not. comint_within_quotes(1_int64, 11_int64), "after them")
        if (.not. allocated(error)) call check(error, comint_within_quotes(1_int64, 14_int64), "after a double quote")
        call kill_buffer(h, st)
    end subroutine test_within_quotes

    !> Test `comint_password_prompt_regexp' against common password
    !> strings.  Port of comint-tests.el comint-test-password-regexp
    !> (with comint-testsuite-password-strings).
    subroutine test_password_regexp(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: strings(32) = [character(len=56) :: &
                                                      "foo@example.net's password: ", & ! ssh
                                                      "Password for foo@example.org: ", & ! kinit
                                                      "Password for 'https://foo@example.org':", & ! git push Bug#20910
                                                      "Please enter the password for foo@example.org: ", & ! kinit
                                                      "Kerberos password for devnull/root <at> GNU.ORG: ", & ! ksu
                                                      "Enter passphrase: ", & ! ssh-add
                                                      "Enter passphrase (empty for no passphrase): ", & ! ssh-keygen
                                                      "Enter same passphrase again: ", & ! ssh-keygen
                                                      "Enter your password: ", & ! python3 -m twine ... Bug#37636
                                                      "Passphrase for key root@GNU.ORG: ", & ! plink
                                                      "[sudo] password for user:", & ! Ubuntu sudo
                                                      "[sudo: authenticate] Password:", & ! sudo-rs
                                                      "[su: authenticate] Password:", & ! su-rs
                                                      "[sudo] user 的密码：", & ! localized
                                                      "doas (user@host) password:", & ! OpenBSD doas
                                                      "PIN for user:", & ! Bug#35523
                                                      "Password (again):", &
                                                      "Enter password:", &
                                                      "(user@host) Password: ", & ! openssh-8.6p1
                                                      "(user@host) Password for user@host:", & ! (Bug#79424)
                                                      "Current password:", & ! "passwd" (to change password) in Debian.
                                                      "Enter encryption key: ", & ! ccrypt
                                                      "Enter decryption key: ", & ! ccrypt
                                                      "Enter encryption key: (repeat) ", & ! ccrypt
                                                      "Enter Auth Password:", & ! OpenVPN (Bug#35724)
                                                      "Verify password: ", & ! zip -e zipfile.zip ... (Bug#47209)
                                                      "Vault password: ", & ! ansible-playbook --ask-vault-pass
                                                      "Vault password (dev): ", & ! ansible-playbook --vault-id
                                                      "SSH password: ", & ! ansible-playbook --ask-pass
                                                      "BECOME password: ", & ! ansible-playbook --ask-become-pass
                                                      "Mot de Passe :", & ! localized (Bug#29729)
                                                      "Passwort:"] ! localized
        ! The lengths of the strings (their trailing spaces count).
        integer, parameter :: lengths(32) = [28, 30, 39, 47, 49, 18, 44, 29, 21, 33, 25, 30, 28, 24, 26, 13, 17, 15, &
                                             22, 35, 17, 22, 22, 31, 20, 17, 16, 22, 14, 17, 14, 9]
        type(status_type) :: st
        integer :: k

        call init_bindings()

        do k = 1, size(strings)
            call check(error, string_match(comint_password_prompt_regexp, strings(k) (1:lengths(k)), st) > 0, &
                       "matches: "//strings(k) (1:lengths(k)))
            if (allocated(error)) return
        end do
    end subroutine test_password_regexp

    !> PASSWORD_FUNCTION can return nothing or a string: the password
    !> typed in the minibuffer (here "non-nil") or its value is sent.
    !> Port of comint-tests.el comint-tests/test-password-function.
    subroutine test_password_function(error, password_function, expected)
        type(error_type), allocatable, intent(out) :: error
        procedure(comint_password_proc), optional :: password_function
        character(len=*), intent(in) :: expected
        type(handle_type) :: h
        type(status_type) :: st
        logical :: saved

        call init_bindings()
        saved = process_connection_type%value
        process_connection_type = .false.
        call make_comint("test-comint-password", "cat", h, st)
        process_connection_type = saved
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set_buffer(h, st)
        call show_alone()
        call set_comint_password_function(password_function)
        call discard_input()
        call type_text("non-nil"//achar(13))
        call comint_send_invisible(st, "Password: ")
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            call wait_for(expected//new_line("a"))
            call check(error, buffer_string(), expected//new_line("a"))
        end if

        call discard_input()
        call finish(h)
    end subroutine test_password_function

    !> Test that `comint_password_function' not being set does not alter
    !> normal password flow.  Port of comint-tests.el
    !> comint-test-no-password-function.
    subroutine test_no_password_function(error)
        type(error_type), allocatable, intent(out) :: error

        call test_password_function(error, expected="non-nil")
    end subroutine test_no_password_function

    !> Test that `comint_password_function' alters normal password flow.
    !> Hook function returns alternative password.  Port of
    !> comint-tests.el comint-test-password-function-with-value.
    subroutine test_password_function_with_value(error)
        type(error_type), allocatable, intent(out) :: error

        call test_password_function(error, magic_password, "MaGiC-PaSsWoRd789")
    end subroutine test_password_function_with_value

    !> Test that `comint_password_function' does not alter the normal
    !> password flow if it returns a nil value.  Port of comint-tests.el
    !> comint-test-password-function-with-nil.
    subroutine test_password_function_with_nil(error)
        type(error_type), allocatable, intent(out) :: error

        call test_password_function(error, ignore_password, "non-nil")
    end subroutine test_password_function_with_nil

    recursive subroutine magic_password(prompt, password, found)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: password
        logical, intent(out) :: found

        password = "MaGiC-PaSsWoRd789"
        found = len(prompt) >= 0
    end subroutine magic_password

    recursive subroutine ignore_password(prompt, password, found)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: password
        logical, intent(out) :: found

        password = ""
        found = len(prompt) < 0
    end subroutine ignore_password

    !> Queue TEXT as input events (for the minibuffer).
    subroutine type_text(text)
        use lisp_fm, only: value_type, make_fixnum
        use keyboard_fm, only: unread_command_events
        character(len=*), intent(in) :: text
        type(value_type), allocatable :: evs(:)
        integer :: i

        allocate (evs(len(text)))

        do i = 1, len(text)
            evs(i) = make_fixnum(int(iachar(text(i:i)), int64))
        end do

        call unread_command_events(evs)
    end subroutine type_text
end module test_comint_fm

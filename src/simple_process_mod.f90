! simple_process_mod.f90 -- the list of processes (M-x list-processes).
! Ported from GNU Emacs 31.1 lisp/simple.el (the process menu part).
!
! Copyright (C) 1985-1987, 1993-2026 Free Software Foundation, Inc.
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

!> `list_processes' and Process Menu mode (d deletes the process at
!> point).  A submodule of simple_mod, as it uses the processes and
!> tabulated lists, which sit above simple_mod.
!>
!> Deferred: the buffer column's button (`process_menu_visit_buffer',
!> with its help-echo; tabulated lists have no buttons yet), threads (the
!> Thread column shows "--"), and network, serial and pipe connections
!> in the Command column.
submodule(simple_mod) simple_process_mod
    use buffer_mod, only: get_or_make_buffer, set_buffer, current_buffer, is_buffer_live
    use editfns_mod, only: point_max, is_eob
    use cmds_mod, only: forward_line, beginning_of_line
    use process_mod, only: process_list, process_status, process_id, process_name, process_tty_name, &
                           process_buffer, process_command, process_arg_type, process_query_on_exit_flag, &
                           delete_process, get_process
    use tabulated_list_mod, only: tabulated_list_column_type, tabulated_list_entry_type, set_tabulated_list_format, &
                                  set_tabulated_list_entries, set_tabulated_list_sort_key, tabulated_list_init_header, &
                                  tabulated_list_print, tabulated_list_get_id, tabulated_list_revert_hook
    use files_mod, only: revert_buffer
    use window_mod, only: display_buffer
    use modes_mod, only: major_modes
    use commands_mod, only: commands
    use keymap_mod, only: keymap_set
    implicit none(type, external)

    ! A buffer's `process_menu_query_only' (process-menu-query-only).
    type :: query_only_type
        integer(int64) :: serial = 0
        logical :: value = .false.
    end type query_only_type

    type(query_only_type), allocatable :: query_only(:)

contains

    ! The current buffer's process-menu-query-only.  Not a port.
    logical function menu_query_only()
        integer :: k

        menu_query_only = .false.
        if (.not. allocated(query_only)) return

        do k = 1, size(query_only)
            if (query_only(k)%serial == buffers(current)%serial) menu_query_only = query_only(k)%value
        end do
    end function menu_query_only

    ! Set process-menu-query-only to VALUE in the current buffer.
    ! Not a port.
    subroutine set_menu_query_only(value)
        logical, intent(in) :: value
        type(query_only_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(query_only)) allocate (query_only(0))

        do k = 1, size(query_only)
            if (query_only(k)%serial == buffers(current)%serial) then
                query_only(k)%value = value
                return
            end if
        end do

        n = size(query_only)
        allocate (grown(n + 1))
        grown(1:n) = query_only
        grown(n + 1)%serial = buffers(current)%serial
        grown(n + 1)%value = value
        call move_alloc(grown, query_only)
    end subroutine set_menu_query_only

    !> Major mode for listing the processes called by Emacs.  Port of
    !> simple.el:process-menu-mode.
    recursive module subroutine process_menu_mode(status)
        type(status_type), intent(out) :: status
        type(tabulated_list_column_type) :: format(7)
        logical :: saved

        call major_modes%process_menu_mode%begin(saved, status)
        if (.not. status%ok()) return
        format(1) = column("Process", 15)
        format(2) = column("PID", 7)
        format(3) = column("Status", 7)
        ! 25 is the length of the long standard buffer name "*Async Shell
        ! Command*<10>" (bug#30016)
        format(4) = column("Buffer", 25)
        format(5) = column("TTY", 12)
        format(6) = column("Thread", 12)
        format(7) = column("Command", 0)
        call set_tabulated_list_format(format)
        call set_menu_query_only(menu_query_only())
        call set_tabulated_list_sort_key("Process", .false.)
        call tabulated_list_revert_hook%append(refresh_hook, local=.true.)
        call major_modes%process_menu_mode%end(saved, status)

    contains

        function column(name, width) result(c)
            character(len=*), intent(in) :: name
            integer, intent(in) :: width
            type(tabulated_list_column_type) :: c

            c%name = name
            c%width = width
            c%sort = .true.
        end function column
    end subroutine process_menu_mode

    ! Process Menu mode's local tabulated-list-revert-hook entry, which
    ! runs list_processes_refresh.  Not a port.
    recursive subroutine refresh_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call list_processes_refresh()
    end subroutine refresh_hook

    !> Stop process at point in a `list_processes' buffer.  Port of
    !> simple.el:process-menu-delete-process.
    recursive module subroutine process_menu_delete_process(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: id
        integer(int64) :: pos, shortage

        pos = point()
        id = tabulated_list_get_id()
        call delete_process(get_process(id), status)
        if (.not. status%ok()) return
        call revert_buffer(status)
        if (.not. status%ok()) return
        call goto_char(min(pos, point_max()))

        if (is_eob()) then
            shortage = forward_line(-1_int64)
        else
            call beginning_of_line()
        end if
    end subroutine process_menu_delete_process

    ! Recompute the list of processes for the Process List buffer.
    ! Also, delete any process that is exited or signaled.  Port of
    ! simple.el:list-processes--refresh.
    subroutine list_processes_refresh()
        type(handle_type), allocatable :: processes(:)
        type(tabulated_list_entry_type), allocatable :: entries(:)
        type(process_arg_type), allocatable :: command(:)
        type(handle_type) :: buffer
        type(status_type) :: st
        character(len=:), allocatable :: status_name, pid, cmd
        character(len=16) :: s
        integer :: k, n, j

        processes = process_list()
        allocate (entries(size(processes)))
        n = 0

        do k = 1, size(processes)
            status_name = process_status(processes(k))

            if (status_name == "exit" .or. status_name == "signal" .or. status_name == "closed") then
                call delete_process(processes(k), st)
            else if (.not. menu_query_only() .or. process_query_on_exit_flag(processes(k))) then
                n = n + 1

                if (process_id(processes(k)) /= 0) then
                    write (s, "(i0)") process_id(processes(k))
                    pid = trim(s)
                else
                    pid = "--"
                end if

                buffer = process_buffer(processes(k))
                call process_command(processes(k), command)
                cmd = ""

                do j = 1, size(command)
                    if (j > 1) cmd = cmd//" "
                    cmd = cmd//command(j)%s
                end do

                associate (e => entries(n))
                    e%id = process_name(processes(k))
                    allocate (e%cols(7))
                    e%cols(1)%text = process_name(processes(k))
                    e%cols(2)%text = pid
                    e%cols(3)%text = status_name

                    if (is_buffer_live(buffer)) then
                        e%cols(4)%text = buffers(buffer%index)%name
                        e%cols(4)%face = "link"
                    else
                        e%cols(4)%text = "--"
                    end if

                    e%cols(5)%text = process_tty_name(processes(k))
                    if (len(e%cols(5)%text) == 0) e%cols(5)%text = "--"
                    e%cols(6)%text = "--"
                    e%cols(7)%text = cmd

                    do j = 1, 7
                        if (.not. allocated(e%cols(j)%face)) e%cols(j)%face = ""
                    end do
                end associate
            end if
        end do

        ! (push ... tabulated-list-entries): the newest first.
        call set_tabulated_list_entries(entries(n:1:-1))
        call tabulated_list_init_header()
    end subroutine list_processes_refresh

    !> Display a list of all processes that are Emacs sub-processes.
    !> If optional argument QUERY_ONLY is true, only processes with the
    !> query-on-exit flag set are listed.  Any process listed as exited or
    !> signaled is actually eliminated after the listing is made.
    !> Optional argument BUFFER specifies a buffer to use, instead of
    !> "*Process List*".
    !>
    !> This function lists only processes that were launched by Emacs.
    !> To see other processes running on the system, use
    !> `list_system_processes'.  Port of simple.el:list-processes.
    recursive module subroutine list_processes(status, query_only_arg, buffer)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: query_only_arg
        type(handle_type), intent(in), optional :: buffer
        type(handle_type) :: buffer_handle, old, window
        type(status_type) :: st
        logical :: q

        q = .false.
        if (present(query_only_arg)) q = query_only_arg

        if (present(buffer)) then
            buffer_handle = buffer
        else
            call get_or_make_buffer("*Process List*", buffer_handle, status)
            if (.not. status%ok()) return
        end if

        old = current_buffer()
        call set_buffer(buffer_handle, status)
        if (.not. status%ok()) return
        call process_menu_mode(status)

        if (status%ok()) then
            call set_menu_query_only(q)
            call list_processes_refresh()
            call tabulated_list_print(status)
        end if

        if (is_buffer_live(old)) call set_buffer(old, st)
        if (.not. status%ok()) return
        call display_buffer(buffer_handle, window, status)
    end subroutine list_processes

    recursive subroutine cmd_list_processes(status)
        type(status_type), intent(out) :: status

        call list_processes(status)
    end subroutine cmd_list_processes

    !> Define Process Menu mode and register its commands.  (In
    !> simple_process_mod.)
    module subroutine init_simple_process()
        type(status_type) :: st

        call major_modes%process_menu_mode%define("process_menu_mode", "Process Menu", process_menu_mode, &
                                                  parent=major_modes%tabulated_list_mode)
        call keymap_set(major_modes%process_menu_mode%map, "d", "process_menu_delete_process", st)
        commands%process_menu_mode%execute => process_menu_mode
        commands%process_menu_delete_process%execute => process_menu_delete_process
        commands%list_processes%execute => cmd_list_processes
    end subroutine init_simple_process
end submodule simple_process_mod

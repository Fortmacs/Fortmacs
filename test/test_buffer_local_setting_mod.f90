! test_buffer_local_setting_mod.f90 -- tests for buffer_local_setting_mod.
! Own tests; buffer_local_setting_mod ports nothing.
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

!> Tests of the buffer-local setting objects: `set_local', `get_current',
!> `is_local' and `clear_local' in two buffers, on a procedure setting
!> (`blink_matching_check_function') and on an array setting
!> (`electric_indent_characters'), and the reuse of a closed buffer's
!> entry; also `local_abbreviation_table', `command_interpreter_completion_fignore',
!> `syntax_parser_state_table' and the per-buffer variables of vc-dir.el,
!> vc-dispatcher.el, log-edit.el and diff-mode.el.  Not a port.
module test_buffer_local_setting_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, current_buffer
    use simple_mod, only: blink_matching_check_function, blink_matching_check_procedure
    use electric_mod, only: electric_indent_characters
    use abbreviation_mod, only: local_abbreviation_table, global_abbreviation_table, fundamental_mode_abbreviation_table, &
                                abbreviation_table_type
    use command_interpreter_mod, only: command_interpreter_completion_fignore
    use syntax_mod, only: syntax_table
    use syntax_propertize_mod, only: syntax_parser_state_table
    use data_mod, only: clear_all_local_variables
    use value_mod, only: command_procedure
    use string_type_mod, only: string_type, append_string
    use modes_mod, only: major_modes
    use version_control_directory_mod, only: version_control_directory_backend, initialize_version_control_directory
    use version_control_dispatcher_mod, only: version_control_parent_buffer, version_control_parent_buffer_name, &
                                              version_control_log_fileset, initialize_version_control_dispatcher
    use log_edit_mod, only: log_edit_callback, log_edit_diff_function, log_edit_version_control_backend, initialize_log_edit
    use diff_mode_mod, only: diff_default_directory, diff_version_control_backend, initialize_diff_mode
    implicit none(type, external)
    private

    public :: collect_buffer_local_setting_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_buffer_local_setting_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("set-local-in-one-buffer", test_set_local), &
                    new_unittest("set-local-without-procedure", test_set_local_null), &
                    new_unittest("clear-local", test_clear_local), &
                    new_unittest("closed-buffer-entry-reused", test_entry_reused), &
                    new_unittest("array-setting-per-buffer", test_array_setting), &
                    new_unittest("abbreviation-table-per-buffer", test_abbreviation_table), &
                    new_unittest("completion-fignore-per-buffer", test_completion_fignore), &
                    new_unittest("parser-state-table-per-buffer", test_parser_state_table), &
                    new_unittest("version-control-settings-per-buffer", test_version_control_settings), &
                    new_unittest("log-edit-settings-per-buffer", test_log_edit_settings), &
                    new_unittest("diff-settings-per-buffer", test_diff_settings) &
                    ]
    end subroutine collect_buffer_local_setting_mod

    ! A check that always reports a mismatch.
    logical function always_mismatch(start, end)
        integer(int64), intent(in) :: start, end

        always_mismatch = start >= 0 .or. end >= 0
    end function always_mismatch

    ! A check that never reports a mismatch.
    logical function never_mismatch(start, end)
        integer(int64), intent(in) :: start, end

        never_mismatch = start < 0 .and. end < 0
    end function never_mismatch

    ! Whether A and B have the same elements in the same order.
    logical function is_same(a, b)
        integer, intent(in) :: a(:), b(:)

        is_same = .false.
        if (size(a) == size(b)) is_same = all(a == b)
    end function is_same

    ! Make the buffers NAME1 and NAME2, remembering the current buffer in
    ! SAVED.
    subroutine make_buffers(name1, name2, b1, b2, saved)
        character(len=*), intent(in) :: name1, name2
        type(handle_type), intent(out) :: b1, b2, saved
        type(status_type) :: st

        saved = current_buffer()
        call get_or_make_buffer(name1, b1, st)
        call get_or_make_buffer(name2, b2, st)
    end subroutine make_buffers

    ! Close B1 and B2 and make SAVED current again.
    subroutine close_buffers(b1, b2, saved)
        type(handle_type), intent(in) :: b1, b2, saved
        type(status_type) :: st

        call set_buffer(saved, st)
        call close_buffer(b1, st)
        call close_buffer(b2, st)
    end subroutine close_buffers

    !> A buffer's own value is seen only in that buffer; the others see
    !> the global value.
    subroutine test_set_local(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved
        type(status_type) :: st
        procedure(blink_matching_check_procedure), pointer :: global, check_now

        global => blink_matching_check_function%procedure
        blink_matching_check_function%procedure => always_mismatch
        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call blink_matching_check_function%set_local(never_mismatch)
        check_now => blink_matching_check_function%get_current()
        call check(error, associated(check_now, never_mismatch) .and. blink_matching_check_function%is_local(), "b1")

        if (.not. allocated(error)) then
            call set_buffer(b2, st)
            check_now => blink_matching_check_function%get_current()
            call check(error, associated(check_now, always_mismatch) .and. .not. blink_matching_check_function%is_local(), &
                       "b2")
        end if

        call set_buffer(b1, st)
        call blink_matching_check_function%clear_local()
        blink_matching_check_function%procedure => global
        call close_buffers(b1, b2, saved)
    end subroutine test_set_local

    !> `set_local' without a procedure gives the buffer the value null,
    !> not the global value.
    subroutine test_set_local_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved
        type(status_type) :: st
        procedure(blink_matching_check_procedure), pointer :: global, check_now

        global => blink_matching_check_function%procedure
        blink_matching_check_function%procedure => always_mismatch
        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call blink_matching_check_function%set_local()
        check_now => blink_matching_check_function%get_current()
        call check(error,.not. associated(check_now) .and. blink_matching_check_function%is_local())
        call blink_matching_check_function%clear_local()
        blink_matching_check_function%procedure => global
        call close_buffers(b1, b2, saved)
    end subroutine test_set_local_null

    !> `clear_local' makes the buffer use the global value again, and
    !> leaves the other buffers' own values alone.
    subroutine test_clear_local(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved
        type(status_type) :: st
        procedure(blink_matching_check_procedure), pointer :: global, check_now

        global => blink_matching_check_function%procedure
        blink_matching_check_function%procedure => always_mismatch
        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call blink_matching_check_function%set_local(never_mismatch)
        call set_buffer(b2, st)
        call blink_matching_check_function%set_local(never_mismatch)
        call set_buffer(b1, st)
        call blink_matching_check_function%clear_local()
        check_now => blink_matching_check_function%get_current()
        call check(error, associated(check_now, always_mismatch) .and. .not. blink_matching_check_function%is_local(), "b1")

        if (.not. allocated(error)) then
            call set_buffer(b2, st)
            check_now => blink_matching_check_function%get_current()
            call check(error, associated(check_now, never_mismatch), "b2")
        end if

        call set_buffer(b2, st)
        call blink_matching_check_function%clear_local()
        blink_matching_check_function%procedure => global
        call close_buffers(b1, b2, saved)
    end subroutine test_clear_local

    !> The entry of a closed buffer is reused, and the new buffer does
    !> not inherit its value.
    subroutine test_entry_reused(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, b3, saved
        type(status_type) :: st
        procedure(blink_matching_check_procedure), pointer :: global, check_now
        integer :: entries

        global => blink_matching_check_function%procedure
        blink_matching_check_function%procedure => always_mismatch
        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call blink_matching_check_function%set_local(never_mismatch)
        entries = size(blink_matching_check_function%serials)
        call set_buffer(saved, st)
        call close_buffer(b1, st)
        call get_or_make_buffer(" *local-3*", b3, st)
        call set_buffer(b3, st)
        check_now => blink_matching_check_function%get_current()
        call check(error, associated(check_now, always_mismatch), "inherited")

        if (.not. allocated(error)) then
            call blink_matching_check_function%set_local(never_mismatch)
            call check(error, size(blink_matching_check_function%serials), entries, "reused")
        end if

        call blink_matching_check_function%clear_local()
        blink_matching_check_function%procedure => global
        call set_buffer(saved, st)
        call close_buffer(b3, st)
        call close_buffer(b2, st)
    end subroutine test_entry_reused

    !> `electric_indent_characters': `=' sets the global value, which a
    !> buffer's own value hides only in that buffer.
    subroutine test_array_setting(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved
        type(status_type) :: st
        integer, allocatable :: global(:)
        logical :: had_global

        had_global = allocated(electric_indent_characters%characters)
        if (had_global) global = electric_indent_characters%characters
        electric_indent_characters = [10]
        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call electric_indent_characters%set_local([10, iachar("}")])
        call check(error, is_same(electric_indent_characters%get_current(), [10, iachar("}")]), "b1")

        if (.not. allocated(error)) then
            call set_buffer(b2, st)
            call check(error, is_same(electric_indent_characters%get_current(), [10]), "b2")
        end if

        if (.not. allocated(error)) then
            call set_buffer(b1, st)
            call electric_indent_characters%clear_local()
            call check(error, is_same(electric_indent_characters%get_current(), [10]), "cleared")
        end if

        deallocate (electric_indent_characters%characters)
        if (had_global) electric_indent_characters = global
        call close_buffers(b1, b2, saved)
    end subroutine test_array_setting

    !> `local_abbreviation_table': a buffer's own table is seen only in
    !> that buffer; the others use `fundamental_mode_abbreviation_table'.
    subroutine test_abbreviation_table(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved
        type(status_type) :: st
        type(abbreviation_table_type), pointer :: table

        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call local_abbreviation_table%set_local(global_abbreviation_table)
        table => local_abbreviation_table%get_current()
        call check(error, associated(table, global_abbreviation_table), "b1")

        if (.not. allocated(error)) then
            call set_buffer(b2, st)
            table => local_abbreviation_table%get_current()
            call check(error, associated(table, fundamental_mode_abbreviation_table), "b2")
        end if

        if (.not. allocated(error)) then
            call set_buffer(b1, st)
            call local_abbreviation_table%clear_local()
            table => local_abbreviation_table%get_current()
            call check(error, associated(table, fundamental_mode_abbreviation_table), "cleared")
        end if

        call close_buffers(b1, b2, saved)
    end subroutine test_abbreviation_table

    !> `command_interpreter_completion_fignore': `=' sets the global
    !> value, which a buffer's own value hides only in that buffer.
    subroutine test_completion_fignore(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved
        type(status_type) :: st
        character(len=8), allocatable :: global(:), suffixes(:)

        global = command_interpreter_completion_fignore%get_current()
        command_interpreter_completion_fignore = [character(len=8) :: ".o"]
        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call command_interpreter_completion_fignore%set_local([character(len=8) :: ".a", ".b"])
        suffixes = command_interpreter_completion_fignore%get_current()
        call check(error, size(suffixes), 2, "b1")

        if (.not. allocated(error)) then
            call set_buffer(b2, st)
            suffixes = command_interpreter_completion_fignore%get_current()
            call check(error, size(suffixes), 1, "b2 size")
            if (.not. allocated(error)) call check(error, suffixes(1) == ".o", "b2")
        end if

        call set_buffer(saved, st)
        command_interpreter_completion_fignore = global
        call close_buffers(b1, b2, saved)
    end subroutine test_completion_fignore

    !> `syntax_parser_state_table': no table unless the buffer has its own.
    subroutine test_parser_state_table(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved, table
        type(status_type) :: st
        logical :: found

        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call syntax_parser_state_table%set_local(syntax_table())
        call syntax_parser_state_table%get_current(table, found)
        call check(error, found, "b1")

        if (.not. allocated(error)) then
            call set_buffer(b2, st)
            call syntax_parser_state_table%get_current(table, found)
            call check(error,.not. found, "b2")
        end if

        call close_buffers(b1, b2, saved)
    end subroutine test_parser_state_table

    !> The per-buffer variables of vc-dir.el and vc-dispatcher.el: a
    !> buffer's own values are seen only there; `clear_all_local_variables'
    !> clears `version_control_directory_backend' and
    !> `version_control_log_fileset' but keeps the permanent-local
    !> `version_control_parent_buffer' and `version_control_parent_buffer_name'.
    subroutine test_version_control_settings(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved, parent
        type(status_type) :: st
        type(string_type), allocatable :: files(:), got(:)

        if (.not. associated(major_modes%version_control_directory_mode%execute)) then
            call initialize_version_control_directory()
        end if

        call initialize_version_control_dispatcher()
        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call version_control_directory_backend%set_local(2)
        call version_control_parent_buffer%set_local(b2)
        call version_control_parent_buffer_name%set_local(" from b2")
        call append_string(files, "a.f90", "b.f90")
        call version_control_log_fileset%set_local(files)
        call check(error, int(version_control_directory_backend%value), 2, "b1 backend")
        parent = version_control_parent_buffer%get_current()
        if (.not. allocated(error)) call check(error, parent%index == b2%index, "b1 parent")
        if (.not. allocated(error)) call check(error, version_control_parent_buffer_name%value == " from b2", "b1 name")

        if (.not. allocated(error)) then
            call version_control_log_fileset%get_current(got)
            call check(error, size(got), 2, "b1 fileset")
            if (.not. allocated(error)) call check(error, got(2)%text == "b.f90", "b1 fileset file")
        end if

        if (.not. allocated(error)) then
            call set_buffer(b2, st)
            call check(error, int(version_control_directory_backend%value), 0, "b2 backend")
            parent = version_control_parent_buffer%get_current()
            if (.not. allocated(error)) call check(error, int(parent%index), 0, "b2 parent")
            if (.not. allocated(error)) call check(error,.not. allocated(version_control_parent_buffer_name%value), "b2 name")
            if (.not. allocated(error)) call check(error,.not. version_control_log_fileset%is_local(), "b2 fileset local")

            if (.not. allocated(error)) then
                call version_control_log_fileset%get_current(got)
                call check(error, size(got), 0, "b2 fileset")
            end if
        end if

        if (.not. allocated(error)) then
            call set_buffer(b1, st)
            call clear_all_local_variables()
            call check(error, int(version_control_directory_backend%value), 0, "cleared backend")
            if (.not. allocated(error)) call check(error,.not. version_control_log_fileset%is_local(), "cleared fileset")
            parent = version_control_parent_buffer%get_current()
            if (.not. allocated(error)) call check(error, parent%index == b2%index, "kept parent")

            if (.not. allocated(error)) then
                call check(error, allocated(version_control_parent_buffer_name%value), "kept name")
                if (.not. allocated(error)) call check(error, version_control_parent_buffer_name%value == " from b2", "kept name")
            end if
        end if

        call close_buffers(b1, b2, saved)
    end subroutine test_version_control_settings

    ! A `log_edit_callback' and `log_edit_diff_function' that does nothing.
    recursive subroutine no_operation(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine no_operation

    !> The per-buffer variables of log-edit.el: a Log Edit buffer's own
    !> values are seen only there, and `clear_all_local_variables' clears
    !> them.
    subroutine test_log_edit_settings(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved
        type(status_type) :: st
        procedure(command_procedure), pointer :: callback_now, diff_now

        if (.not. associated(major_modes%log_edit_mode%execute)) call initialize_log_edit()
        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call log_edit_callback%set_local(no_operation)
        call log_edit_diff_function%set_local(no_operation)
        call log_edit_version_control_backend%set_local(4)
        callback_now => log_edit_callback%get_current()
        diff_now => log_edit_diff_function%get_current()
        call check(error, associated(callback_now, no_operation), "b1 callback")
        if (.not. allocated(error)) call check(error, associated(diff_now, no_operation), "b1 diff")
        if (.not. allocated(error)) call check(error, int(log_edit_version_control_backend%value), 4, "b1 backend")

        if (.not. allocated(error)) then
            call set_buffer(b2, st)
            callback_now => log_edit_callback%get_current()
            diff_now => log_edit_diff_function%get_current()
            call check(error,.not. associated(callback_now), "b2 callback")
            if (.not. allocated(error)) call check(error,.not. associated(diff_now), "b2 diff")
            if (.not. allocated(error)) call check(error, int(log_edit_version_control_backend%value), 0, "b2 backend")
        end if

        if (.not. allocated(error)) then
            call set_buffer(b1, st)
            call clear_all_local_variables()
            call check(error,.not. log_edit_callback%is_local(), "cleared callback")
            if (.not. allocated(error)) call check(error,.not. log_edit_diff_function%is_local(), "cleared diff")
            if (.not. allocated(error)) call check(error, int(log_edit_version_control_backend%value), 0, "cleared backend")
        end if

        call close_buffers(b1, b2, saved)
    end subroutine test_log_edit_settings

    !> The per-buffer variables of diff-mode.el: a Diff buffer's own
    !> `diff_default_directory' and `diff_version_control_backend' are
    !> seen only there, and `clear_all_local_variables' clears them.
    subroutine test_diff_settings(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: b1, b2, saved
        type(status_type) :: st

        if (.not. associated(major_modes%diff_mode%execute)) call initialize_diff_mode()
        call make_buffers(" *local-1*", " *local-2*", b1, b2, saved)
        call set_buffer(b1, st)
        call diff_default_directory%set_local("/tmp/")
        call diff_version_control_backend%set_local("Git")
        call check(error, diff_default_directory%value == "/tmp/", "b1 directory")
        if (.not. allocated(error)) call check(error, diff_version_control_backend%value == "Git", "b1 backend")

        if (.not. allocated(error)) then
            call set_buffer(b2, st)
            call check(error,.not. allocated(diff_default_directory%value), "b2 directory")
            if (.not. allocated(error)) call check(error,.not. allocated(diff_version_control_backend%value), "b2 backend")
        end if

        if (.not. allocated(error)) then
            call set_buffer(b1, st)
            call clear_all_local_variables()
            call check(error,.not. allocated(diff_default_directory%value), "cleared directory")
            if (.not. allocated(error)) call check(error,.not. allocated(diff_version_control_backend%value), "cleared backend")
        end if

        call close_buffers(b1, b2, saved)
    end subroutine test_diff_settings
end module test_buffer_local_setting_mod

! calendar_movement_mod.f90 -- scrolling and moving the date cursor in the calendar.
! Ported from GNU Emacs 31.1 lisp/calendar/cal-move.el.
!
! Copyright (C) 1995, 2001-2026 Free Software Foundation, Inc.
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

!> Part of calendar_mod: the movement commands of cal-move.el, which use
!> the calendar's buffer-local state and are used by calendar.el's own
!> commands, so they live in a submodule.  Deferred: the EVENT arguments
!> of the scrolling commands and `calendar_scroll_toolkit_scroll' (the
!> mouse and scroll bars, with cal-menu.el); the obsolete aliases
!> `calendar_scroll_left_three_months' and
!> `calendar_scroll_right_three_months' are Not applicable (obsolete).
submodule(calendar_mod) calendar_movement_mod
    use keyboard_mod, only: this_command, last_command
    implicit none(type, external)

contains

    ! Note that this is not really the "closest" date.
    ! In most cases, it just searches forwards for the next day.
    recursive module subroutine calendar_cursor_to_nearest_date(date, status)
        type(calendar_date_type), intent(out) :: date
        type(status_type), intent(out) :: status
        integer :: column, k, e, last, right
        integer(int64) :: reached, shortage, found
        logical :: on_date, backward
        type(status_type) :: st

        call calendar_cursor_to_date(date, status)
        if (.not. status%ok()) return
        if (date%month /= 0) return
        column = int(current_column())
        k = calendar_local_index()
        e = 0
        if (allocated(locals(k)%month_edges)) e = findloc(locals(k)%month_edges%segment, calendar_column_to_segment(), dim=1)

        if (count_lines(point_minimum(), point()) < calendar_first_date_row) then
            call goto_character(point_minimum())
            shortage = forward_line(int(calendar_first_date_row - 1, int64))
            call move_to_column(int(column, int64), reached)
        end if

        ! The date positions are fixed and computable, but searching
        ! is probably more flexible.  Need to consider blank days at
        ! start and end of month if computing positions.
        ! 'date text-property is used to exclude intermonth text.
        on_date = looking_at("[0-9]", st)
        if (on_date) on_date = has_calendar_date_property(point())

        if (.not. on_date) then
            ! We search forwards for a number, except close to the RH
            ! margin of a month, where we search backwards.
            ! Note that the searches can go to other lines.
            backward = looking_at(" *$", st)

            if (.not. backward) then
                if (e == 0) then
                    call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "number_or_marker_p, no month edges")
                    return
                end if

                last = locals(k)%month_edges(e)%last
                right = locals(k)%month_edges(e)%right
                backward = column > last .and. column < right
            end if

            if (backward) then
                do
                    call regular_expression_search_backward("[0-9]", found, st, noerror=1)
                    if (found == 0) exit
                    if (has_calendar_date_property(point())) exit
                end do
            else
                do
                    call regular_expression_search_forward("[0-9]", found, st, noerror=1)
                    if (found == 0) exit
                    if (has_calendar_date_property(point() - 1)) exit
                end do

                if (point() > point_minimum()) call goto_character(point() - 1)
            end if
        end if

        call calendar_cursor_to_date(date, status)
    end subroutine calendar_cursor_to_nearest_date

    recursive module subroutine calendar_cursor_to_visible_date(date)
        type(calendar_date_type), intent(in) :: date
        integer :: k, line, column
        integer(int64) :: reached, shortage

        k = calendar_local_index()
        call goto_character(point_minimum())
        line = calendar_first_date_row - 1 &
               + (date%day - 1 + modulo(calendar_day_of_week(calendar_date_type(date%month, 1, date%year)) &
                                        - calendar_week_start_day, 7))/7
        shortage = forward_line(int(line, int64))
        column = calendar_left_margin + calendar_day_digit_width - 1 &
                 + calendar_month_width*(1 + calendar_interval(locals(k)%displayed_month, locals(k)%displayed_year, &
                                                               date%month, date%year)) &
                 + calendar_column_width*modulo(calendar_day_of_week(date) - calendar_week_start_day, 7)
        call move_to_column(int(column, int64), reached)
    end subroutine calendar_cursor_to_visible_date

    recursive module subroutine calendar_goto_today(status)
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: today

        status = status_type()
        ! the date might have changed
        today = calendar_current_date()

        if (.not. is_calendar_date_visible(today)) then
            call calendar_generate_window(status)
            if (.not. status%ok()) return
        end if

        call calendar_cursor_to_visible_date(today)
        call calendar_move_hook%execute(status)
    end subroutine calendar_goto_today

    ! Regenerate the calendar with MONTH, YEAR at the left or right edge.
    ! This function is mainly used when MONTH, YEAR is invisible.  The new
    ! month is placed at the right edge if ARGUMENT is positive, and the left edge
    ! otherwise.  Port of cal-move.el:calendar--show-month-at-edge.
    recursive subroutine show_month_at_edge(month, year, argument, status)
        integer, intent(in) :: month, year, argument
        type(status_type), intent(out) :: status
        integer :: m1, y1, m2, y2, offset

        call calendar_get_month_range(m1, y1, m2, y2)

        if (argument > 0) then
            offset = calendar_interval(m2, y2, month, year)
        else
            offset = calendar_interval(m1, y1, month, year)
        end if

        call calendar_increment_month(m1, y1, 1 + offset)
        call calendar_generate_window(status, m1, y1)
    end subroutine show_month_at_edge

    recursive module subroutine calendar_forward_month(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: cursor_date, new_cursor_date
        integer :: month, year

        call calendar_cursor_to_nearest_date(cursor_date, status)
        if (.not. status%ok()) return
        call calendar_cursor_to_date(cursor_date, status, .true.)
        if (.not. status%ok()) return
        month = cursor_date%month
        year = cursor_date%year
        call calendar_increment_month(month, year, argument)
        ! Put the new month on the screen, if needed, and go to the new date.
        new_cursor_date = calendar_date_type(month, min(calendar_last_day_of_month(month, year), cursor_date%day), year)

        if (.not. is_calendar_date_visible(new_cursor_date)) then
            call show_month_at_edge(month, year, argument, status)
            if (.not. status%ok()) return
        end if

        call calendar_cursor_to_visible_date(new_cursor_date)
        call calendar_move_hook%execute(status)
    end subroutine calendar_forward_month

    recursive module subroutine calendar_forward_year(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call calendar_forward_month(12*argument, status)
    end subroutine calendar_forward_year

    recursive module subroutine calendar_backward_month(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call calendar_forward_month(-argument, status)
    end subroutine calendar_backward_month

    recursive module subroutine calendar_backward_year(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call calendar_forward_month(-12*argument, status)
    end subroutine calendar_backward_year

    recursive module subroutine calendar_scroll_left(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: old_date, today, date
        integer :: month, year, offset

        call calendar_cursor_to_nearest_date(old_date, status)
        if (.not. status%ok()) return

        if (argument /= 0) then
            call calendar_cursor_to_date(old_date, status)
            if (.not. status%ok()) return
            today = calendar_current_date()
            month = calendar_displayed_month()
            year = calendar_displayed_year()
            offset = calendar_interval(month, year, old_date%month, old_date%year)
            call calendar_increment_month(month, year, argument)
            call calendar_generate_window(status, month, year)
            if (.not. status%ok()) return

            if (is_calendar_date_visible(old_date)) then
                date = old_date
            else if (is_calendar_date_visible(today)) then
                date = today
            else
                call calendar_increment_month(month, year, offset)
                date = calendar_date_type(month, 1, year)
            end if

            call calendar_cursor_to_visible_date(date)
        end if

        call calendar_move_hook%execute(status)
    end subroutine calendar_scroll_left

    recursive module subroutine calendar_scroll_right(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call calendar_scroll_left(-argument, status)
    end subroutine calendar_scroll_right

    recursive module subroutine calendar_scroll_calendar_left(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call calendar_scroll_left(calendar_total_months_local()*argument, status)
    end subroutine calendar_scroll_calendar_left

    recursive module subroutine calendar_scroll_calendar_right(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call calendar_scroll_left(-calendar_total_months_local()*argument, status)
    end subroutine calendar_scroll_calendar_right

    recursive module subroutine calendar_recenter(status)
        type(status_type), intent(out) :: status
        integer, parameter :: POSITIONS(3) = [CALENDAR_RECENTER_LAST_OP_CENTER, CALENDAR_RECENTER_LAST_OP_FIRST, &
                                              CALENDAR_RECENTER_LAST_OP_LAST]
        type(calendar_date_type) :: cursor_date
        integer :: m1, y1, m2, y2, k
        logical :: repeated

        call calendar_cursor_to_nearest_date(cursor_date, status)
        if (.not. status%ok()) return
        call calendar_get_month_range(m1, y1, m2, y2)
        ! (eq this-command last-command)
        repeated = .not. associated(this_command) .and. .not. associated(last_command)

        if (associated(this_command) .and. associated(last_command)) then
            repeated = associated(this_command, last_command)
        end if

        ! Update global last position upon repeat.
        if (repeated) then
            k = findloc(POSITIONS, calendar_recenter_last_op, dim=1)

            if (k == 0 .or. k == size(POSITIONS)) then
                calendar_recenter_last_op = POSITIONS(1)
            else
                calendar_recenter_last_op = POSITIONS(k + 1)
            end if
        else
            calendar_recenter_last_op = POSITIONS(1)
        end if

        select case (calendar_recenter_last_op)
        case (CALENDAR_RECENTER_LAST_OP_CENTER)
            call calendar_increment_month(m1, y1, (calendar_total_months_local() - 1)/2)
        case (CALENDAR_RECENTER_LAST_OP_LAST)
            ! Other sub-cases should not happen as we should be centered
            ! from here.
            m1 = m2
            y1 = y2
        case default
            ! CALENDAR_RECENTER_LAST_OP_FIRST: the first month stays.
        end select

        call calendar_scroll_left(calendar_interval(m1, y1, cursor_date%month, cursor_date%year), status)
    end subroutine calendar_recenter

    recursive module subroutine calendar_forward_day(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: cursor_date, new_cursor_date
        integer :: n

        status = status_type()
        n = argument

        if (n /= 0) then
            call calendar_cursor_to_date(cursor_date, status)
            if (.not. status%ok()) return

            if (cursor_date%month == 0) then
                if (n > 0) n = n - 1
                call calendar_cursor_to_nearest_date(cursor_date, status)
                if (.not. status%ok()) return
            end if

            new_cursor_date = calendar_gregorian_from_absolute(calendar_absolute_from_gregorian(cursor_date) + n)

            ! Put the new month on the screen, if needed.
            if (.not. is_calendar_date_visible(new_cursor_date)) then
                ! The next line gives smoother scrolling (i.e. making the new
                ! month appear at the edge).
                call show_month_at_edge(new_cursor_date%month, new_cursor_date%year, n, status)
                if (.not. status%ok()) return
            end if

            ! Go to the new date.
            call calendar_cursor_to_visible_date(new_cursor_date)
        end if

        call calendar_move_hook%execute(status)
    end subroutine calendar_forward_day

    recursive module subroutine calendar_backward_day(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call calendar_forward_day(-argument, status)
    end subroutine calendar_backward_day

    recursive module subroutine calendar_forward_week(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call calendar_forward_day(7*argument, status)
    end subroutine calendar_forward_week

    recursive module subroutine calendar_backward_week(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call calendar_forward_day(-7*argument, status)
    end subroutine calendar_backward_week

    recursive module subroutine calendar_beginning_of_week(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date
        integer :: day

        call calendar_cursor_to_nearest_date(date, status)
        if (.not. status%ok()) return
        call calendar_cursor_to_date(date, status)
        if (.not. status%ok()) return
        day = calendar_day_of_week(date)

        if (day == calendar_week_start_day) then
            call calendar_backward_day(7*argument, status)
        else
            call calendar_backward_day(modulo(day - calendar_week_start_day, 7) + 7*(argument - 1), status)
        end if
    end subroutine calendar_beginning_of_week

    recursive module subroutine calendar_end_of_week(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date
        integer :: day

        call calendar_cursor_to_nearest_date(date, status)
        if (.not. status%ok()) return
        call calendar_cursor_to_date(date, status)
        if (.not. status%ok()) return
        day = calendar_day_of_week(date)

        if (day == modulo(calendar_week_start_day - 1, 7)) then
            call calendar_forward_day(7*argument, status)
        else
            call calendar_forward_day(6 - modulo(day - calendar_week_start_day, 7) + 7*(argument - 1), status)
        end if
    end subroutine calendar_end_of_week

    recursive module subroutine calendar_beginning_of_month(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date

        call calendar_cursor_to_nearest_date(date, status)
        if (.not. status%ok()) return
        call calendar_cursor_to_date(date, status)
        if (.not. status%ok()) return

        if (date%day == 1) then
            call calendar_backward_month(argument, status)
        else
            call calendar_cursor_to_visible_date(calendar_date_type(date%month, 1, date%year))
            call calendar_backward_month(argument - 1, status)
        end if
    end subroutine calendar_beginning_of_month

    recursive module subroutine calendar_end_of_month(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date, last_day
        integer :: month, year, n

        call calendar_cursor_to_nearest_date(date, status)
        if (.not. status%ok()) return
        call calendar_cursor_to_date(date, status)
        if (.not. status%ok()) return
        month = date%month
        year = date%year
        n = argument

        if (date%day /= calendar_last_day_of_month(month, year)) then
            call calendar_cursor_to_visible_date(calendar_date_type(month, calendar_last_day_of_month(month, year), year))
            n = n - 1
        end if

        call calendar_increment_month(month, year, n)
        last_day = calendar_date_type(month, calendar_last_day_of_month(month, year), year)

        if (.not. is_calendar_date_visible(last_day)) then
            call show_month_at_edge(month, year, n, status)
            if (.not. status%ok()) return
        end if

        call calendar_cursor_to_visible_date(last_day)
        call calendar_move_hook%execute(status)
    end subroutine calendar_end_of_month

    recursive module subroutine calendar_beginning_of_year(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date, jan_first
        type(hook_type) :: saved_hook
        integer :: year

        call calendar_cursor_to_nearest_date(date, status)
        if (.not. status%ok()) return
        call calendar_cursor_to_date(date, status)
        if (.not. status%ok()) return
        year = date%year
        jan_first = calendar_date_type(1, 1, year)
        ! (calendar-move-hook nil)
        saved_hook = calendar_move_hook
        call calendar_move_hook%clear()

        if (date%day == 1 .and. date%month == 1) then
            call calendar_backward_month(12*argument, status)
        else if (argument == 1 .and. is_calendar_date_visible(jan_first)) then
            call calendar_cursor_to_visible_date(jan_first)
        else
            year = year - (argument - 1)
            call show_month_at_edge(1, year, -argument, status)
            if (status%ok()) call calendar_cursor_to_visible_date(calendar_date_type(1, 1, year))
        end if

        calendar_move_hook = saved_hook
        if (.not. status%ok()) return
        call calendar_move_hook%execute(status)
    end subroutine calendar_beginning_of_year

    recursive module subroutine calendar_end_of_year(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date, dec_31
        type(hook_type) :: saved_hook
        integer :: year

        call calendar_cursor_to_nearest_date(date, status)
        if (.not. status%ok()) return
        call calendar_cursor_to_date(date, status)
        if (.not. status%ok()) return
        year = date%year
        dec_31 = calendar_date_type(12, 31, year)
        ! (calendar-move-hook nil)
        saved_hook = calendar_move_hook
        call calendar_move_hook%clear()

        if (date%day == 31 .and. date%month == 12) then
            call calendar_forward_month(12*argument, status)
        else if (argument == 1 .and. is_calendar_date_visible(dec_31)) then
            call calendar_cursor_to_visible_date(dec_31)
        else
            year = year + (argument - 1)
            call show_month_at_edge(12, year, argument, status)
            if (status%ok()) call calendar_cursor_to_visible_date(calendar_date_type(12, 31, year))
        end if

        calendar_move_hook = saved_hook
        if (.not. status%ok()) return
        call calendar_move_hook%execute(status)
    end subroutine calendar_end_of_year

    recursive module subroutine calendar_goto_date(date, status)
        type(calendar_date_type), intent(in) :: date
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. is_calendar_date_visible(date)) then
            if (date%month == 1 .and. date%year == 1) then
                call calendar_other_month(2, date%year, status)
            else
                call calendar_other_month(date%month, date%year, status)
            end if

            if (.not. status%ok()) return
        end if

        call calendar_cursor_to_visible_date(date)
        call calendar_move_hook%execute(status)
    end subroutine calendar_goto_date

    recursive module subroutine calendar_goto_day_of_year(year, day, status, noecho)
        integer, intent(in) :: year, day
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noecho

        call calendar_goto_date(calendar_date_from_day_of_year(year, day), status)
        if (.not. status%ok()) return

        if (present(noecho)) then
            if (noecho) return
        end if

        call calendar_print_day_of_year(status)
    end subroutine calendar_goto_day_of_year

    recursive module subroutine calendar_show_more_months(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: cursor_date
        integer :: avail, k, m1, y1, m2, y2, offset
        type(status_type) :: st

        status = status_type()
        k = calendar_local_index()

        if (argument > 0) then
            if (locals(k)%total_months >= 12) then
                call set_error(status, GENERIC_ERROR_CODE, "The calendar shows at most 12 months.")
                return
            end if

            avail = (window_body_width(selected_window(), st) - locals(k)%right_margin) &
                    /(calendar_month_width + calendar_intermonth_spacing)

            if (avail < 1) then
                call message("No space left to display more months.")
            else
                locals(k)%total_months = locals(k)%total_months + min(avail, argument)
                call calendar_recompute_layout_variables()
                call calendar_redraw(status)
            end if
        else if (argument < 0) then
            if (locals(k)%total_months <= 3) then
                call set_error(status, GENERIC_ERROR_CODE, "The calendar shows at least 3 months.")
                return
            end if

            call calendar_cursor_to_nearest_date(cursor_date, status)
            if (.not. status%ok()) return
            call calendar_cursor_to_date(cursor_date, status, .true.)
            if (.not. status%ok()) return
            k = calendar_local_index()
            locals(k)%total_months = max(3, locals(k)%total_months + argument)
            call calendar_recompute_layout_variables()
            call calendar_get_month_range(m1, y1, m2, y2)
            offset = calendar_interval(m1, y1, cursor_date%month, cursor_date%year) - locals(k)%total_months

            if (offset < 0) then
                call calendar_redraw(status)
            else
                call calendar_increment_month(m1, y1, 2 + offset)
                call calendar_generate_window(status, m1, y1)
                if (.not. status%ok()) return
                call calendar_cursor_to_visible_date(cursor_date)
                call calendar_update_mode_line()
            end if
        end if
    end subroutine calendar_show_more_months

    recursive module subroutine calendar_show_fewer_months(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call calendar_show_more_months(-argument, status)
    end subroutine calendar_show_fewer_months

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The numeric prefix argument.  Not a port.
    integer function prefix_count()
        prefix_count = int(prefix_numeric_value(current_prefix_argument))
    end function prefix_count

    ! The command `calendar_forward_month'.  Not a port.
    recursive subroutine execute_calendar_forward_month(status)
        type(status_type), intent(out) :: status

        call calendar_forward_month(prefix_count(), status)
    end subroutine execute_calendar_forward_month

    ! The command `calendar_forward_year'.  Not a port.
    recursive subroutine execute_calendar_forward_year(status)
        type(status_type), intent(out) :: status

        call calendar_forward_year(prefix_count(), status)
    end subroutine execute_calendar_forward_year

    ! The command `calendar_backward_month'.  Not a port.
    recursive subroutine execute_calendar_backward_month(status)
        type(status_type), intent(out) :: status

        call calendar_backward_month(prefix_count(), status)
    end subroutine execute_calendar_backward_month

    ! The command `calendar_backward_year'.  Not a port.
    recursive subroutine execute_calendar_backward_year(status)
        type(status_type), intent(out) :: status

        call calendar_backward_year(prefix_count(), status)
    end subroutine execute_calendar_backward_year

    ! The command `calendar_scroll_left'.  Not a port.
    recursive subroutine execute_calendar_scroll_left(status)
        type(status_type), intent(out) :: status

        call calendar_scroll_left(prefix_count(), status)
    end subroutine execute_calendar_scroll_left

    ! The command `calendar_scroll_right'.  Not a port.
    recursive subroutine execute_calendar_scroll_right(status)
        type(status_type), intent(out) :: status

        call calendar_scroll_right(prefix_count(), status)
    end subroutine execute_calendar_scroll_right

    ! The command `calendar_scroll_calendar_left'.  Not a port.
    recursive subroutine execute_calendar_scroll_calendar_left(status)
        type(status_type), intent(out) :: status

        call calendar_scroll_calendar_left(prefix_count(), status)
    end subroutine execute_calendar_scroll_calendar_left

    ! The command `calendar_scroll_calendar_right'.  Not a port.
    recursive subroutine execute_calendar_scroll_calendar_right(status)
        type(status_type), intent(out) :: status

        call calendar_scroll_calendar_right(prefix_count(), status)
    end subroutine execute_calendar_scroll_calendar_right

    ! The command `calendar_forward_day'.  Not a port.
    recursive subroutine execute_calendar_forward_day(status)
        type(status_type), intent(out) :: status

        call calendar_forward_day(prefix_count(), status)
    end subroutine execute_calendar_forward_day

    ! The command `calendar_backward_day'.  Not a port.
    recursive subroutine execute_calendar_backward_day(status)
        type(status_type), intent(out) :: status

        call calendar_backward_day(prefix_count(), status)
    end subroutine execute_calendar_backward_day

    ! The command `calendar_forward_week'.  Not a port.
    recursive subroutine execute_calendar_forward_week(status)
        type(status_type), intent(out) :: status

        call calendar_forward_week(prefix_count(), status)
    end subroutine execute_calendar_forward_week

    ! The command `calendar_backward_week'.  Not a port.
    recursive subroutine execute_calendar_backward_week(status)
        type(status_type), intent(out) :: status

        call calendar_backward_week(prefix_count(), status)
    end subroutine execute_calendar_backward_week

    ! The command `calendar_beginning_of_week'.  Not a port.
    recursive subroutine execute_calendar_beginning_of_week(status)
        type(status_type), intent(out) :: status

        call calendar_beginning_of_week(prefix_count(), status)
    end subroutine execute_calendar_beginning_of_week

    ! The command `calendar_end_of_week'.  Not a port.
    recursive subroutine execute_calendar_end_of_week(status)
        type(status_type), intent(out) :: status

        call calendar_end_of_week(prefix_count(), status)
    end subroutine execute_calendar_end_of_week

    ! The command `calendar_beginning_of_month'.  Not a port.
    recursive subroutine execute_calendar_beginning_of_month(status)
        type(status_type), intent(out) :: status

        call calendar_beginning_of_month(prefix_count(), status)
    end subroutine execute_calendar_beginning_of_month

    ! The command `calendar_end_of_month'.  Not a port.
    recursive subroutine execute_calendar_end_of_month(status)
        type(status_type), intent(out) :: status

        call calendar_end_of_month(prefix_count(), status)
    end subroutine execute_calendar_end_of_month

    ! The command `calendar_beginning_of_year'.  Not a port.
    recursive subroutine execute_calendar_beginning_of_year(status)
        type(status_type), intent(out) :: status

        call calendar_beginning_of_year(prefix_count(), status)
    end subroutine execute_calendar_beginning_of_year

    ! The command `calendar_end_of_year'.  Not a port.
    recursive subroutine execute_calendar_end_of_year(status)
        type(status_type), intent(out) :: status

        call calendar_end_of_year(prefix_count(), status)
    end subroutine execute_calendar_end_of_year

    ! The command `calendar_goto_date': reads the date.  Not a port.
    recursive subroutine execute_calendar_goto_date(status)
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: date

        call calendar_read_date(date, status)
        if (.not. status%ok()) return
        call calendar_goto_date(date, status)
    end subroutine execute_calendar_goto_date

    ! The command `calendar_goto_day_of_year': reads the year and the day
    ! number.  Not a port.
    recursive subroutine execute_calendar_goto_day_of_year(status)
        type(status_type), intent(out) :: status
        type(calendar_date_type) :: today
        integer :: year, day, last
        character(len=32) :: prompt

        today = calendar_current_date()
        call calendar_read_expression("Year (>0)", is_positive, year, status, today%year)
        if (.not. status%ok()) return
        last = 365
        if (is_calendar_leap_year(year)) last = 366
        write (prompt, '("Day number (+/- 1-", i0, ")")') last
        call calendar_read_expression(trim(prompt), is_day_number, day, status, calendar_day_number(today))
        if (.not. status%ok()) return
        call calendar_goto_day_of_year(year, day, status)

    contains

        logical function is_positive(x)
            integer, intent(in) :: x

            is_positive = x > 0
        end function is_positive

        logical function is_day_number(x)
            integer, intent(in) :: x

            is_day_number = 1 <= abs(x) .and. abs(x) <= last
        end function is_day_number
    end subroutine execute_calendar_goto_day_of_year

    ! The command `calendar_show_more_months'.  Not a port.
    recursive subroutine execute_calendar_show_more_months(status)
        type(status_type), intent(out) :: status

        call calendar_show_more_months(prefix_count(), status)
    end subroutine execute_calendar_show_more_months

    ! The command `calendar_show_fewer_months'.  Not a port.
    recursive subroutine execute_calendar_show_fewer_months(status)
        type(status_type), intent(out) :: status

        call calendar_show_fewer_months(prefix_count(), status)
    end subroutine execute_calendar_show_fewer_months

    module subroutine initialize_cal_move()
        commands%calendar_goto_today%execute => calendar_goto_today
        commands%calendar_forward_month%execute => execute_calendar_forward_month
        commands%calendar_forward_year%execute => execute_calendar_forward_year
        commands%calendar_backward_month%execute => execute_calendar_backward_month
        commands%calendar_backward_year%execute => execute_calendar_backward_year
        commands%calendar_scroll_left%execute => execute_calendar_scroll_left
        commands%calendar_scroll_right%execute => execute_calendar_scroll_right
        commands%calendar_scroll_calendar_left%execute => execute_calendar_scroll_calendar_left
        commands%calendar_scroll_calendar_right%execute => execute_calendar_scroll_calendar_right
        commands%calendar_recenter%execute => calendar_recenter
        commands%calendar_forward_day%execute => execute_calendar_forward_day
        commands%calendar_backward_day%execute => execute_calendar_backward_day
        commands%calendar_forward_week%execute => execute_calendar_forward_week
        commands%calendar_backward_week%execute => execute_calendar_backward_week
        commands%calendar_beginning_of_week%execute => execute_calendar_beginning_of_week
        commands%calendar_end_of_week%execute => execute_calendar_end_of_week
        commands%calendar_beginning_of_month%execute => execute_calendar_beginning_of_month
        commands%calendar_end_of_month%execute => execute_calendar_end_of_month
        commands%calendar_beginning_of_year%execute => execute_calendar_beginning_of_year
        commands%calendar_end_of_year%execute => execute_calendar_end_of_year
        commands%calendar_goto_date%execute => execute_calendar_goto_date
        commands%calendar_goto_day_of_year%execute => execute_calendar_goto_day_of_year
        commands%calendar_show_more_months%execute => execute_calendar_show_more_months
        commands%calendar_show_fewer_months%execute => execute_calendar_show_fewer_months
    end subroutine initialize_cal_move
end submodule calendar_movement_mod

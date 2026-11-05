! just_in_time_lock_mod.f90 -- just-in-time fontification.
! Ported from GNU Emacs 31.1 lisp/jit-lock.el.
!
! Copyright (C) 1998-2026 Free Software Foundation, Inc.
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

!> Just-in-time Lock mode: text is fontified when redisplay is about to
!> show it.  Redisplay calls `fontification_functions' at text whose
!> `fontified' property is false; `jit_lock_function' fontifies a chunk
!> from there with the functions registered in `jit_lock_functions' (Font
!> Lock's `font_lock_fontify_region') and marks it fontified.  A change
!> marks the changed text as not fontified (`jit_lock_after_change').
!>
!> Deferred: stealth fontification (`jit-lock-stealth-time' and its
!> companions), deferred fontification (`jit-lock-defer-time'), contextual
!> refontification after `jit-lock-context-time' (`jit-lock-contextually',
!> `jit-lock-context-fontify', the antiblink grace): all need idle timers;
!> `jit-lock-debug-mode'; the forced redisplay of text fontified before
!> the start of a chunk (here the next redisplay shows it); indirect
!> buffers.
module just_in_time_lock_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use value_mod, only: value_type
    use error_mod, only: status_type
    use value_mod, only: value_type
    use data_mod, only: append_locals_clearer
    use buffer_mod, only: buffers, current
    use hooks_mod, only: region_hook_type, region_entry_type, after_change_hook_type
    use insert_delete_mod, only: after_change_functions
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, widen, narrow_to_region
    use search_mod, only: match_data, set_match_data
    use text_properties_mod, only: put_text_property, next_single_property_change, text_property_any
    use basic_subprograms_mod, only: silent_modifications_type, silent_modifications_begin, silent_modifications_end
    use display_engine_mod, only: fontification_functions
    implicit none(type, external)
    private

    public :: initialize_jit_lock, jit_lock_mode, jit_lock_register, jit_lock_unregister, jit_lock_refontify, jit_lock_function, &
              jit_lock_fontify_now, jit_lock_after_change, is_jit_lock_mode

    !> Jit-lock asks to fontify chunks of at most this many characters at
    !> a time.
    !>
    !> The actual size of the fontified chunk of text can be different,
    !> depending on what the `fontification_functions' actually decide to
    !> do.
    !>
    !> This variable controls both display-time and stealth
    !> fontifications.
    !>
    !> The optimum value is a little over the typical number of buffer
    !> characters which fit in a typical window.  Port of
    !> jit-lock.el:jit-lock-chunk-size.
    integer(int64), public :: jit_lock_chunk_size = 1500

    !> Functions to do the actual fontification.
    !> They are called with two arguments: the START and END of the region
    !> to fontify, and give the part they actually fontified.  Port of
    !> jit-lock.el:jit-lock-functions.
    type(region_hook_type), target, public :: jit_lock_functions = region_hook_type(name="jit_lock_functions")

    !> Hook that can extend the text to refontify after a change.
    !> This is run after every buffer change.  The functions are called
    !> with the three arguments of `after_change_functions': START END
    !> OLD-LEN.  The extended region to refontify is returned indirectly
    !> by modifying the variables `jit_lock_start' and `jit_lock_end'.
    !>
    !> Note that extending the region this way is not strictly necessary,
    !> except that the nature of the redisplay code tends to otherwise
    !> leave some of the rehighlighted text displayed with the old
    !> highlight until the next redisplay.  Port of
    !> jit-lock.el:jit-lock-after-change-extend-region-functions.
    type(after_change_hook_type), target, public :: jit_lock_after_change_extend_region_functions = &
                                                    after_change_hook_type(name="jit_lock_after_change_extend_region_functions")

    !> The region to refontify after a change, as the functions of
    !> `jit_lock_after_change_extend_region_functions' extend it.  Port of
    !> jit-lock.el's jit-lock-start and jit-lock-end.
    integer(int64), public :: jit_lock_start = 0, jit_lock_end = 0

    !> The buffers in Just-in-time Lock mode (the buffer-local
    !> `jit_lock_mode'), by serial number.
    integer(int64), allocatable :: on(:)

contains

    !> Set up Just-in-time Lock (as loading jit-lock.el does).  Not a port.
    subroutine initialize_jit_lock()
        call append_locals_clearer(clear_jit_lock_locals)
    end subroutine initialize_jit_lock

    !> kill-all-local-variables: `jit_lock_mode' is buffer-local.
    !> Not a port.
    subroutine clear_jit_lock_locals()
        call set_jit_lock_mode(.false.)
    end subroutine clear_jit_lock_locals

    !> Whether Just-in-time Lock mode is on in the current buffer (the
    !> variable `jit_lock_mode').  Not a port.
    logical function is_jit_lock_mode()
        is_jit_lock_mode = .false.
        if (current == 0) return
        if (.not. allocated(on)) return
        is_jit_lock_mode = any(on == buffers(current)%serial)
    end function is_jit_lock_mode

    !> Record whether Just-in-time Lock mode is on in the current buffer.
    !> Not a port.
    subroutine set_jit_lock_mode(argument)
        logical, intent(in) :: argument
        integer(int64), allocatable :: kept(:)

        if (current == 0) return
        if (.not. allocated(on)) allocate (on(0))

        if (argument) then
            if (.not. any(on == buffers(current)%serial)) on = [on, buffers(current)%serial]
        else
            kept = pack(on, on /= buffers(current)%serial)
            call move_alloc(kept, on)
        end if
    end subroutine set_jit_lock_mode

    !> Toggle Just-in-time Lock mode.
    !> Turn Just-in-time Lock mode on if and only if ARGUMENT is true.
    !> Enable it automatically by customizing group `font-lock'.
    !>
    !> When Just-in-time Lock mode is enabled, fontification is different
    !> in the following ways:
    !>
    !> - Demand-driven buffer fontification triggered by redisplay.
    !>   This means initial fontification of the whole buffer does not
    !>   occur.  Instead, fontification occurs when necessary, such as when
    !>   scrolling through the buffer would otherwise reveal unfontified
    !>   areas.  This is useful if buffer fontification is too slow for
    !>   large buffers.
    !>
    !> `jit_lock_mode' is not a regular minor mode, and it doesn't
    !> follow the regular conventions to switch the functionality on or
    !> off.  Instead, an ARGUMENT of false will switch it off, and true will
    !> switch it on.  Port of jit-lock.el:jit-lock-mode (stealth, deferred
    !> and contextual fontification need timers and are deferred).
    subroutine jit_lock_mode(argument, status)
        logical, intent(in) :: argument
        type(status_type), intent(out) :: status

        status = status_type()
        call set_jit_lock_mode(argument)

        if (argument) then
            ! Turn Just-in-time Lock mode on.
            ! Mark the buffer for refontification.
            call jit_lock_refontify(status)
            if (.not. status%ok()) return
            ! Setup our hooks.
            call after_change_functions%prepend(jit_lock_after_change, local=.true.)
            call fontification_functions%prepend(jit_lock_function, local=.true.)
        else
            ! Turn Just-in-time Lock mode off: remove hooks.
            call after_change_functions%remove(jit_lock_after_change, local=.true.)
            call fontification_functions%remove(jit_lock_function)
        end if
    end subroutine jit_lock_mode

    !> Register FUNCTION as a fontification function to be called in this
    !> buffer.  FUNCTION will be called with two arguments START and END
    !> indicating the region that needs to be (re)fontified.  If true,
    !> Port of jit-lock.el:jit-lock-register (without CONTEXTUAL:
    !> contextual refontification needs timers).
    subroutine jit_lock_register(function, status)
        interface
            subroutine function(beginning, end, done_beginning, done_end, status)
                import :: int64, status_type
                implicit none(type, external)
                integer(int64), intent(in) :: beginning, end
                integer(int64), intent(out) :: done_beginning, done_end
                type(status_type), intent(out) :: status
            end subroutine function
        end interface
        type(status_type), intent(out) :: status

        call jit_lock_functions%prepend(function, local=.true.)
        call jit_lock_mode(.true., status)
    end subroutine jit_lock_register

    !> Unregister FUNCTION as a fontification function.
    !> Only applies to the current buffer.  Port of
    !> jit-lock.el:jit-lock-unregister.
    subroutine jit_lock_unregister(function, status)
        interface
            subroutine function(beginning, end, done_beginning, done_end, status)
                import :: int64, status_type
                implicit none(type, external)
                integer(int64), intent(in) :: beginning, end
                integer(int64), intent(out) :: done_beginning, done_end
                type(status_type), intent(out) :: status
            end subroutine function
        end interface
        type(status_type), intent(out) :: status
        type(region_entry_type), allocatable :: functions(:)

        status = status_type()
        call jit_lock_functions%remove(function, local=.true.)
        functions = jit_lock_functions%functions()
        if (size(functions) == 0) call jit_lock_mode(.false., status)
    end subroutine jit_lock_unregister

    !> Force refontification of the region BEGINNING..END (default whole buffer).
    !> Port of jit-lock.el:jit-lock-refontify.
    subroutine jit_lock_refontify(status, beginning, end)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beginning, end
        type(silent_modifications_type) :: silent
        integer(int64) :: beginning_of_accessible_portion, end_of_accessible_portion, b, e

        call silent_modifications_begin(silent)
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion = buffers(current)%end_of_accessible_portion
        call widen()
        b = point_minimum()
        if (present(beginning)) b = beginning
        e = point_maximum()
        if (present(end)) e = end
        call put_text_property(b, e, value_type(name="fontified"), value_type(), status)
        call narrow_to_region(beginning_of_accessible_portion, end_of_accessible_portion, status)
        call silent_modifications_end(silent)
    end subroutine jit_lock_refontify

    !> Fontify current buffer starting at position START.
    !> This function is added to `fontification_functions' when
    !> `jit_lock_mode' is active.  Port of jit-lock.el:jit-lock-function
    !> (without deferral, which needs a timer).
    subroutine jit_lock_function(start, status)
        integer(int64), intent(in) :: start
        type(status_type), intent(out) :: status
        integer(int64) :: cend, vend
        logical :: found

        status = status_type()
        if (.not. is_jit_lock_mode()) return
        ! No deferral.
        cend = min(point_maximum(), start + jit_lock_chunk_size)
        call next_single_property_change(start, value_type(name="invisible"), vend, found, status, cend)
        if (.not. found) vend = cend
        ! Presumably if we're called it means START is not at the end of
        ! the buffer (nor invisible) and hence VEND > START.
        call jit_lock_fontify_now(status, start, vend)
    end subroutine jit_lock_function

    !> Execute `jit_lock_functions' on BEGINNING..END: TIGHT_BEGINNING and TIGHT_END give
    !> the part all of them fontified, LOOSE_BEGINNING and LOOSE_END the part some
    !> of them did.  Port of jit-lock.el:jit-lock--run-functions.
    subroutine jit_lock_execute_functions(beginning, end, tight_beginning, tight_end, loose_beginning, loose_end, status)
        integer(int64), intent(in) :: beginning, end
        integer(int64), intent(out) :: tight_beginning, tight_end, loose_beginning, loose_end
        type(status_type), intent(out) :: status
        type(region_entry_type), allocatable :: functions(:)
        integer(int64) :: this_beginning, this_end
        logical :: has_tight
        integer :: i

        status = status_type()
        has_tight = .false.
        tight_beginning = beginning
        tight_end = end
        loose_beginning = beginning
        loose_end = end
        functions = jit_lock_functions%functions()

        do i = 1, size(functions)
            this_beginning = beginning
            this_end = end
            call functions(i)%fn(beginning, end, this_beginning, this_end, status)
            if (.not. status%ok()) return

            ! If all functions don't fontify the same region, we currently
            ! just try to "still be correct".
            if (.not. has_tight) then
                tight_beginning = max(point_minimum(), this_beginning)
                tight_end = min(point_maximum(), this_end)
                has_tight = .true.
            else
                tight_beginning = max(tight_beginning, this_beginning)
                tight_end = min(tight_end, this_end)
            end if

            loose_beginning = min(loose_beginning, this_beginning)
            loose_end = max(loose_end, this_end)
        end do

        if (.not. has_tight) then
            tight_beginning = beginning
            tight_end = end
        else
            tight_beginning = min(tight_beginning, beginning)
            tight_end = max(tight_end, end)
        end if
    end subroutine jit_lock_execute_functions

    !> Fontify current buffer from START to END.
    !> Defaults to the whole buffer.  END can be out of bounds.  Port of
    !> jit-lock.el:jit-lock-fontify-now.
    subroutine jit_lock_fontify_now(status, start_in, end_in)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start_in, end_in
        type(silent_modifications_type) :: silent
        integer(int64), allocatable :: saved_match(:)
        integer(int64) :: start, end, next, point_value, tight_beginning, tight_end, loose_beginning, loose_end, position
        logical :: found
        type(status_type) :: st

        status = status_type()
        call silent_modifications_begin(silent)
        point_value = point()
        start = point_minimum()
        if (present(start_in)) start = start_in
        end = point_maximum()
        if (present(end_in)) end = min(end_in, point_maximum())
        saved_match = match_data()
        ! Fontify chunks beginning at START.  The end of a chunk is either
        ! END, or the start of a region before END that has already been
        ! fontified.
        found = .true.

        do while (found .and. start < end)
            ! Determine the end of this chunk.
            call text_property_any(start, end, value_type(name="fontified"), value_type(.true.), next, found, status)
            if (.not. found) next = end

            ! Avoid unnecessary work if the chunk is empty.
            if (next > start) then
                ! Fontify the chunk, and mark it as fontified.  We mark it
                ! first, to make sure that we don't indefinitely re-execute
                ! this fontification if an error occurs.
                call put_text_property(start, next, value_type(name="fontified"), value_type(.true.), status)
                call jit_lock_execute_functions(start, next, tight_beginning, tight_end, loose_beginning, loose_end, status)
                if (.not. status%ok()) exit

                ! In case we fontified more than requested, take advantage
                ! of the good news.
                if (tight_beginning < start .or. tight_end > next) then
                    call put_text_property(tight_beginning, tight_end, value_type(name="fontified"), value_type(.true.), st)
                end if

                ! Skip to the end of the fully refontified part.
                start = tight_end
            end if

            ! Find the start of the next chunk, if any.
            call text_property_any(start, end, value_type(name="fontified"), value_type(), position, found, st)
            start = position
        end do

        call set_match_data(saved_match)
        call goto_character(point_value)
        call silent_modifications_end(silent)
    end subroutine jit_lock_fontify_now

    !> Mark the rest of the buffer as not fontified after a change.
    !> Installed on `after_change_functions'.
    !> START and END are the start and end of the changed text.  OLD_LENGTH
    !> is the pre-change length.
    !> This function ensures that lines following the change will be
    !> refontified in case the syntax of those lines has changed.  Port of
    !> jit-lock.el:jit-lock-after-change (the deferred contextual
    !> refontification is not ported).
    subroutine jit_lock_after_change(start, end, old_length, status)
        integer(int64), intent(in) :: start, end, old_length
        type(status_type), intent(out) :: status
        type(silent_modifications_type) :: silent
        integer(int64) :: beginning_of_accessible_portion, end_of_accessible_portion, saved_start, saved_end

        status = status_type()
        if (.not. is_jit_lock_mode()) return
        saved_start = jit_lock_start
        saved_end = jit_lock_end
        jit_lock_start = start
        jit_lock_end = end
        call silent_modifications_begin(silent)
        call jit_lock_after_change_extend_region_functions%execute(start, end, old_length, status)

        if (status%ok()) then
            ! Make sure we change at least one char (in case of deletions).
            jit_lock_end = min(max(jit_lock_end, start + 1), point_maximum())
            ! Request refontification.
            beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
            end_of_accessible_portion = buffers(current)%end_of_accessible_portion
            call widen()
            call put_text_property(jit_lock_start, jit_lock_end, value_type(name="fontified"), value_type(), status)
            call narrow_to_region(beginning_of_accessible_portion, end_of_accessible_portion, status)
        end if

        call silent_modifications_end(silent)
        jit_lock_start = saved_start
        jit_lock_end = saved_end
    end subroutine jit_lock_after_change
end module just_in_time_lock_mod

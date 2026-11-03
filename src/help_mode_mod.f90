! help_mode_mod.f90 -- Help mode: the *Help* buffer's keymap and cross-references.
! Ported from GNU Emacs 31.1 lisp/help-mode.el.
!
! Copyright (C) 1985-1986, 1993-1994, 1998-2026 Free Software Foundation, Inc.
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

!> Help mode, the major mode of *Help*, derived from Special mode
!> (simple.el): a read-only buffer where `q' quits the window, SPC and
!> DEL scroll, digits and `-' make prefix arguments.
!>
!> The keys of button_mod's `button_buffer_map' (TAB, ESC TAB and
!> <backtab>, moving between buttons) are bound in `help_mode_map'
!> itself: upstream composes the two parents with make-composed-keymap,
!> which is not ported.
!>
!> The cross-references (help-mode.el's xref buttons and stacks): the
!> help commands call `help_setup_xref' with the procedure and argument
!> that show their page again, `help_make_xrefs' makes the quoted names
!> of a page into buttons that describe them, and `l' and `r'
!> (`help_go_back', `help_go_forward') move through the pages shown in
!> *Help*.  The procedures that use buttons and the help commands are in
!> the submodule help_mode_xref_mod (button_mod and help_fns_mod use this
!> module through help_mod).
!>
!> Deferred: `s' (help-view-source), `i'/`I' (Info), `c' (customize),
!> `g' (revert), `h'/`?' (describe-mode); the Info node, man page,
!> customization group, URL and multilingual references and the face,
!> `source of' (help-function-def) and type buttons of
!> `help_make_xrefs'; `help_clean_buttons'; describe-key's stack item;
!> the stacks of a help buffer other than *Help* (one set, not
!> buffer-local).
module help_mode_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_mod, only: commands
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use keymap_mod, only: is_keymap_live, keymap_set
    use modes_mod, only: major_modes
    use hooks_mod, only: hook_type
    use abbrev_mod, only: abbrev_table_type
    use simple_mod, only: init_simple_modes
    implicit none(type, external)
    private

    public :: init_help_mode, help_mode, help_xref_item_type, help_xref_procedure, help_setup_xref, help_make_xrefs, &
              help_xref_go_back, help_xref_go_forward, help_go_back, help_go_forward, help_follow_name, &
              help_goto_next_page, help_goto_previous_page

    !> Keymap for Help mode.  Port of help-mode.el help-mode-map (its
    !> parent is special-mode-map).
    type(handle_type), public :: help_mode_map

    !> Hook run by `help_mode'.  Port of help-mode.el help-mode-hook.
    type(hook_type), target, public :: help_mode_hook = hook_type(name="help_mode_hook")

    !> Abbrev table for `help_mode'.  Port of help-mode.el
    !> help-mode-abbrev-table (made by define-derived-mode).
    type(abbrev_table_type), target, public :: help_mode_abbrev_table

    abstract interface
        !> Show the help page about ARGUMENT again.  INTERACTIVE is true
        !> when a command shows it (`help_setup_xref' then truncates the
        !> stack).
        recursive subroutine help_xref_procedure(argument, status, interactive)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: argument
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: interactive
        end subroutine help_xref_procedure
    end interface

    !> A way to show a help page again: `describe' called with `argument',
    !> then point moved to `position'.  Port of help-mode.el's elements of
    !> help-xref-stack, (POSITION FUNCTION ARGS...).
    type :: help_xref_item_type
        !> Where point was on the page.
        integer(int64) :: position = 0
        !> The procedure that shows the page; null for none.
        procedure(help_xref_procedure), pointer, nopass :: describe => null()
        !> Its argument.
        character(len=:), allocatable :: argument
    end type help_xref_item_type

    !> A stack of ways by which to return to help buffers after following
    !> xrefs.  Used by `help_follow_name' and `help_xref_go_back'.  The
    !> top of the stack is its last element.  Port of help-mode.el
    !> help-xref-stack.
    type(help_xref_item_type), allocatable, public :: help_xref_stack(:)

    !> A stack used to navigate help forwards after using the back button.
    !> Used by `help_follow_name' and `help_xref_go_forward'.  The top
    !> of the stack is its last element.  Port of help-mode.el
    !> help-xref-forward-stack.
    type(help_xref_item_type), allocatable, public :: help_xref_forward_stack(:)

    !> An item for `help_follow_name' to append to `help_xref_stack'.
    !> Port of help-mode.el help-xref-stack-item.
    type(help_xref_item_type), public :: help_xref_stack_item

    !> True when following a help cross-reference.  Port of help-mode.el
    !> help-xref-following.
    logical, public :: help_xref_following = .false.

    !> Label to use by `help_make_xrefs' for the go-back reference.  Port
    !> of help-mode.el help-back-label.
    character(len=32), public :: help_back_label = "[back]"

    !> Label to use by `help_make_xrefs' for the go-forward reference.
    !> Port of help-mode.el help-forward-label.
    character(len=32), public :: help_forward_label = "[forward]"

    interface
        !> Invoked from commands using the "*Help*" buffer to install some
        !> xref info.
        !>
        !> DESCRIBE and ARGUMENT are appropriate for recreating the help
        !> buffer after following a reference (absent for none).
        !> INTERACTIVE is true if the calling command was invoked
        !> interactively.  In this case the stack of items for help buffer
        !> "back" buttons is truncated.
        !>
        !> This function also re-enables the major mode of the buffer, thus
        !> resetting local variables to the values set by the mode and
        !> running the mode hooks.
        !>
        !> So this should be called very early, before the output buffer is
        !> cleared, also because we want to record the "previous" position
        !> of point so we can restore it properly when going back.  Port of
        !> help-mode.el:help-setup-xref.  (In help_mode_xref_mod.)
        recursive module subroutine help_setup_xref(interactive, status, describe, argument)
            implicit none(type, external)
            logical, intent(in) :: interactive
            type(status_type), intent(out) :: status
            procedure(help_xref_procedure), optional :: describe
            character(len=*), intent(in), optional :: argument
        end subroutine help_setup_xref

        !> Parse and hyperlink documentation cross-references in the
        !> current buffer.
        !>
        !> Find cross-reference information in a buffer and activate such
        !> cross references for selection with `help_follow_name'.
        !> Cross-references have the canonical form `...'  and the type of
        !> reference may be disambiguated by the preceding word(s) used in
        !> `help_xref_name_regexp'.  Variables without variable
        !> documentation do not get cross-referenced.
        !>
        !> A special reference `back' is made to return back through a
        !> stack of help buffers.  Variable `help_back_label' specifies the
        !> text for that.  Port of help-mode.el:help-make-xrefs (the
        !> current buffer; quoted names and M-x commands).  (In
        !> help_mode_xref_mod.)
        recursive module subroutine help_make_xrefs(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine help_make_xrefs

        !> From BUFFER, go back to previous help buffer text using
        !> `help_xref_stack'.  Port of help-mode.el:help-xref-go-back.
        !> (In help_mode_xref_mod.)
        recursive module subroutine help_xref_go_back(buffer, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
        end subroutine help_xref_go_back

        !> From BUFFER, go forward to next help buffer.  Port of
        !> help-mode.el:help-xref-go-forward.  (In help_mode_xref_mod.)
        recursive module subroutine help_xref_go_forward(buffer, status)
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
        end subroutine help_xref_go_forward

        !> Go back to previous topic in this help buffer.  Port of
        !> help-mode.el:help-go-back.  (In help_mode_xref_mod.)
        recursive module subroutine help_go_back(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine help_go_back

        !> Go to the next topic in this help buffer.  Port of
        !> help-mode.el:help-go-forward.  (In help_mode_xref_mod.)
        recursive module subroutine help_go_forward(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine help_go_forward

        !> In help buffer, show docs for the name at POSITION, defaulting
        !> to point.  Show all docs for that name as either a variable or
        !> function.  Port of help-mode.el:help-follow-symbol (no faces).
        !> (In help_mode_xref_mod.)
        recursive module subroutine help_follow_name(status, position)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: position
        end subroutine help_follow_name

        !> Go to the next page (if any) in the current buffer.
        !> The help buffers are divided into "pages" by the ^L character.
        !> Port of help-mode.el:help-goto-next-page.  (In
        !> help_mode_xref_mod.)
        recursive module subroutine help_goto_next_page(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine help_goto_next_page

        !> Go to the previous page (if any) in the current buffer.
        !> (If not at the start of a page, go to the start of the current
        !> page.)
        !>
        !> The help buffers are divided into "pages" by the ^L character.
        !> Port of help-mode.el:help-goto-previous-page.  (In
        !> help_mode_xref_mod.)
        recursive module subroutine help_goto_previous_page(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine help_goto_previous_page

        !> Define the button types of the cross-references.  Not a port.
        !> (In help_mode_xref_mod.)
        module subroutine init_help_xref()
            implicit none(type, external)
        end subroutine init_help_xref
    end interface

contains

    !> Define Help mode and its parent, Special mode.  Not a port.
    subroutine init_help_mode()
        type(status_type) :: st

        call init_simple_modes()
        call major_modes%help_mode%define("help_mode", "Help", help_mode, help_mode_hook, &
                                          parent=major_modes%special_mode, abbrev_table=help_mode_abbrev_table)
        help_mode_map = major_modes%help_mode%map
        ! The keys of help-mode-map's other parent, button-buffer-map.
        call keymap_set(help_mode_map, "TAB", "forward_button", st)
        call keymap_set(help_mode_map, "ESC TAB", "backward_button", st)
        call keymap_set(help_mode_map, "<backtab>", "backward_button", st)
        call keymap_set(help_mode_map, "n", "help_goto_next_page", st)
        call keymap_set(help_mode_map, "p", "help_goto_previous_page", st)
        call keymap_set(help_mode_map, "l", "help_go_back", st)
        call keymap_set(help_mode_map, "r", "help_go_forward", st)
        call keymap_set(help_mode_map, "C-c C-b", "help_go_back", st)
        call keymap_set(help_mode_map, "C-c C-f", "help_go_forward", st)
        call keymap_set(help_mode_map, "<XF86Back>", "help_go_back", st)
        call keymap_set(help_mode_map, "<XF86Forward>", "help_go_forward", st)
        call keymap_set(help_mode_map, "C-c C-c", "help_follow_name", st)
        call init_help_xref()
        commands%help_mode%execute => help_mode
    end subroutine init_help_mode

    !> Major mode for viewing help text and navigating references in it.
    !> Port of help-mode.el:help-mode.
    recursive subroutine help_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. is_keymap_live(help_mode_map)) call init_help_mode()
        call major_modes%help_mode%begin(saved, status)
        if (.not. status%ok()) return
        call major_modes%help_mode%end(saved, status)
    end subroutine help_mode
end module help_mode_mod

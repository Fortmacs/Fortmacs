! button_mod.f90 -- clickable buttons.
! Ported from GNU Emacs 31.1 lisp/button.el.
!
! Copyright (C) 2001-2026 Free Software Foundation, Inc.
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

!> Buttons: a region of text, marked by an overlay (`make_button',
!> `insert_button') or by text properties (`make_text_button',
!> `insert_text_button'), that does something when pushed with RET
!> (`push_button'), and that TAB and S-TAB move between
!> (`forward_button', `backward_button').
!>
!> A button type is a `button_kind_type' object, named after the
!> symbol upstream keeps its properties in (`default_button' for the type
!> `button', `help_xref_button' for `help_xref'): its `define' copies
!> the components of its supertype, and plain assignment changes them
!> afterwards, as button-type-put does:
!>
!>     type(button_kind_type), target :: my_button
!>     call my_button%define("my", supertype=default_button)
!>     my_button%action => my_action
!>     my_button%help_echo = "RET: do it"
!>
!> A button's own properties are the optional arguments of `make_button',
!> `make_text_button', `insert_button', `insert_text_button' and
!> `put_button_properties'; `get_button_properties' returns them, each falling back to the
!> button's type as upstream's `category' symbol does.  A button found
!> by `button_at', `next_button' or `previous_button' is a `button_type':
!> its overlay, or a position in a text button.
!>
!> The display properties of the type (`face', `mouse_face', `keymap',
!> `rear_nonsticky', `evaporate') are written on the button's text or
!> overlay where it has none of its own, since redisplay and the keymap
!> lookup read properties, not types; a later change to the type's
!> components reaches the buttons made after it.
!>
!> Deferred: mouse events (`push_button''s event argument, the mouse-2,
!> follow-link, mode-line, header-line and touch screen bindings of
!> `button_map'), area buttons (buttons in the mode line or header line,
!> strings), a marker as an `action', `make_text_button' on a string,
!> `buttonize' (a string), the Action and Mouse-action sections of
!> `describe_button' (a procedure pointer carries no name) and its xref
!> (help-setup-xref), and widgets (bug#34506's `button_at' test).
!> Not applicable: a form as `help_echo' (no run-time Lisp).
module button_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, format_message, FM_USER_ERROR
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, vtag, xfixnum, xstring, make_list, make_keymap_ref, V_INTEGER, V_STRING
    use data_mod, only: value_eq
    use buffer_mod, only: current_buffer, buffers, current
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use modes_mod, only: minor_modes
    use callint_mod, only: current_prefix_arg, prefix_numeric_value
    use insdel_mod, only: insert
    use editfns_mod, only: point, goto_char, point_min, point_max, buffer_substring_no_properties
    use textprop_mod, only: get_text_property, put_text_property, add_text_properties, &
                            next_single_property_change, previous_single_property_change, &
                            next_single_char_property_change, previous_single_char_property_change, &
                            get_char_property, get_char_property_and_overlay, remove_list_of_text_properties, &
                            add_face_text_property
    use itree_mod, only: make_overlay, overlay_put, overlay_get, overlay_start, overlay_end, overlays_in, &
                         is_overlay, destroy_overlay
    use keymap_mod, only: make_sparse_keymap, set_keymap_parent, keymap_set
    use faces_mod, only: named_face_type, face_attrs, display_when, highlight
    use frame_mod, only: selected_window
    use subr_mod, only: silent_modifications_type, silent_modifications_begin, silent_modifications_end
    use xdisp_mod, only: message
    use help_mod, only: substitute_command_keys, with_help_window, register_keymap_name
    implicit none(type, external)
    private

    public :: button_kind_type, button_type, button_properties_type, button_action_procedure, &
              button_help_echo_procedure
    public :: init_button, is_button_subtype, button_start, button_end, get_button_properties, &
              put_button_properties, activate_button, button_label, has_button_type, make_button, insert_button, &
              make_text_button, insert_text_button, button_at, next_button, previous_button, push_button, &
              button_help_echo, forward_button, backward_button, describe_button, buttonize_region, &
              unbuttonize_region, button_mode

    !> The longest name of a button type.
    integer, parameter, public :: BUTTON_NAME_MAX = 64

    !> A button found in the current buffer: the overlay of an overlay
    !> button, or a position in a text button (upstream returns a marker
    !> there).  Not a port.
    type :: button_type
        !> The button's overlay; the null handle for a text button.
        type(handle_type) :: overlay
        !> A position in a text button (0 for an overlay button).
        integer(int64) :: position = 0
    end type button_type

    abstract interface
        !> What pushing BUTTON does.  DATA is the button's `button_data'
        !> (unallocated if none), which upstream passes in place of BUTTON.
        recursive subroutine button_action_procedure(button, data, status)
            import :: button_type, value_type, status_type
            implicit none(type, external)
            type(button_type), intent(in) :: button
            type(value_type), intent(in) :: data
            type(status_type), intent(out) :: status
        end subroutine button_action_procedure

        !> The help text of a button that starts at POSITION, shown in
        !> WINDOW; OBJECT is the button's overlay, or the buffer of a text
        !> button.  HELP is unallocated for none.
        subroutine button_help_echo_procedure(window, object, position, help)
            import :: handle_type, int64
            implicit none(type, external)
            type(handle_type), intent(in) :: window, object
            integer(int64), intent(in) :: position
            character(len=:), allocatable, intent(out) :: help
        end subroutine button_help_echo_procedure
    end interface

    !> A button type: the properties its buttons have unless they give
    !> their own.  Port of what button.el:define-button-type puts on a
    !> type's category symbol.
    type :: button_kind_type
        !> The type's name, as `describe_button' shows it.
        character(len=BUTTON_NAME_MAX) :: name = ""
        !> The type the components were copied from when the type was
        !> defined (null for `default_button').
        type(button_kind_type), pointer :: supertype => null()
        !> What pushing the button does; null does nothing (`ignore').
        procedure(button_action_procedure), pointer, nopass :: action => null()
        !> What clicking the button does; null uses `action'.
        procedure(button_action_procedure), pointer, nopass :: mouse_action => null()
        !> The button's face.
        type(named_face_type), pointer :: face => null()
        !> The face of the button under the mouse.
        type(named_face_type), pointer :: mouse_face => null()
        !> The keymap active on the button.
        type(handle_type) :: keymap
        !> The help text (with \\[COMMAND] substitutions); unallocated for
        !> none.
        character(len=:), allocatable :: help_echo
        !> A procedure giving the help text, used instead of `help_echo'.
        procedure(button_help_echo_procedure), pointer, nopass :: help_echo_function => null()
        !> Whether a mouse-1 click acts as mouse-2 (`follow_link').
        logical :: follow_link = .false.
        !> Whether `forward_button' and `backward_button' skip the button.
        logical :: skip = .false.
        !> The index in the registry of defined types; 0 if not defined.
        integer :: index = 0
    contains
        !> Define the type, inheriting from a supertype.
        procedure :: define => button_kind_define
    end type button_kind_type

    !> A button's properties, its own or else its type's (button-get).
    !> Not a port.
    type :: button_properties_type
        !> The button's type; null for a button made by `buttonize_region'.
        type(button_kind_type), pointer :: kind => null()
        !> The button's `action'.
        procedure(button_action_procedure), pointer, nopass :: action => null()
        !> The button's `mouse_action'.
        procedure(button_action_procedure), pointer, nopass :: mouse_action => null()
        !> The button's help text; unallocated for none.
        character(len=:), allocatable :: help_echo
        !> The button's help text procedure.
        procedure(button_help_echo_procedure), pointer, nopass :: help_echo_function => null()
        !> Whether a mouse-1 click acts as mouse-2.
        logical :: follow_link = .false.
        !> Whether motion skips the button.
        logical :: skip = .false.
        !> The value passed to the action (`button_data'); unallocated for
        !> none.
        type(value_type) :: button_data
    end type button_properties_type

    !> Default face used for buttons.  Port of button.el button.
    type(named_face_type), target, public :: button_face

    !> Keymap useful for buffers containing buttons.
    !> Mode-specific keymaps may want to use this as their parent keymap.
    !> Port of button.el button-buffer-map.
    type(handle_type), public :: button_buffer_map

    !> Keymap used by buttons.  Port of button.el button-map.
    type(handle_type), public :: button_map

    !> Hook run after entering or leaving `button_mode'.  Port of
    !> button.el button-mode-hook (made by define-minor-mode).
    type(hook_type), target, public :: button_mode_hook = hook_type(name="button_mode_hook")

    !> The properties of the default button type, `button'.  Port of
    !> button.el's properties of default-button.
    type(button_kind_type), target, public :: default_button

    !> A pointer to a defined button type, for the registry.
    type :: kind_ref_type
        type(button_kind_type), pointer :: p => null()
    end type kind_ref_type

    !> A procedure a button names as its own `action', `mouse_action' or
    !> `help_echo', for the registry.
    type :: procedure_ref_type
        procedure(button_action_procedure), pointer, nopass :: action => null()
        procedure(button_help_echo_procedure), pointer, nopass :: help_echo => null()
    end type procedure_ref_type

    !> The defined button types: a button's `category' property is an
    !> index into it.
    type(kind_ref_type), allocatable :: kinds(:)

    !> The procedures buttons name: a button's own `action',
    !> `mouse_action' or `help_echo' procedure is an index into it.
    type(procedure_ref_type), allocatable :: procedures(:)

    !> The last `button' property value given: each button gets its own,
    !> so that the property changes between two adjacent buttons.
    integer(int64) :: last_serial = 0

    !> The text and overlay properties of buttons.
    character(len=*), parameter :: P_BUTTON = "button", P_CATEGORY = "category", P_ACTION = "action", &
                                   P_MOUSE_ACTION = "mouse_action", P_HELP_ECHO = "help_echo", P_SKIP = "skip", &
                                   P_FOLLOW_LINK = "follow_link", P_BUTTON_DATA = "button_data", P_FACE = "face", &
                                   P_MOUSE_FACE = "mouse_face", P_KEYMAP = "keymap", P_EVAPORATE = "evaporate", &
                                   P_REAR_NONSTICKY = "rear_nonsticky", P_FONT_LOCK_FACE = "font_lock_face", &
                                   P_HELP_ECHO_BUTTON = "help_echo_button"

contains

    ! ------------------------------------------------------------------
    ! Button types
    ! ------------------------------------------------------------------

    !> Define a `button type' called NAME.  The type's components are
    !> copied from SUPERTYPE (`default_button' if absent); assign the ones
    !> that differ afterwards.  The inheritance happens only when the type
    !> is defined; subsequent changes to a supertype are not reflected in
    !> its subtypes.  Port of button.el:define-button-type.
    subroutine button_kind_define(self, name, supertype)
        class(button_kind_type), intent(inout), target :: self
        character(len=*), intent(in) :: name
        type(button_kind_type), intent(in), target, optional :: supertype
        type(button_kind_type), pointer :: super
        type(kind_ref_type) :: ref

        super => default_button
        if (present(supertype)) super => supertype

        select type (self)
        type is (button_kind_type)
            ref%p => self
        end select

        if (.not. associated(ref%p, super)) then
            ! Initialize NAME's properties using the supertype's.
            self%action => super%action
            self%mouse_action => super%mouse_action
            self%face => super%face
            self%mouse_face => super%mouse_face
            self%keymap = super%keymap
            if (allocated(super%help_echo)) self%help_echo = super%help_echo
            self%help_echo_function => super%help_echo_function
            self%follow_link = super%follow_link
            self%skip = super%skip
            self%supertype => super
        end if

        self%name = name
        if (self%index /= 0) return
        if (.not. allocated(kinds)) allocate (kinds(0))
        kinds = [kinds, ref]
        self%index = size(kinds)
    end subroutine button_kind_define

    !> Return true if `button type' KIND is a subtype of SUPERTYPE.
    !> Port of button.el:button-type-subtype-p.
    recursive logical function is_button_subtype(kind, supertype) result(is_subtype)
        type(button_kind_type), intent(in), target :: kind
        type(button_kind_type), intent(in), target :: supertype
        type(button_kind_type), pointer :: p

        p => kind
        is_subtype = associated(kind%supertype)

        if (associated(p, supertype)) then
            is_subtype = .true.
        else if (is_subtype) then
            is_subtype = is_button_subtype(kind%supertype, supertype)
        end if
    end function is_button_subtype

    ! ------------------------------------------------------------------
    ! Button properties
    ! ------------------------------------------------------------------

    !> Return the position at which BUTTON starts.
    !>
    !> This function only works when BUTTON is in the current buffer.
    !> Port of button.el:button-start.
    integer(int64) function button_start(button) result(start)
        type(button_type), intent(in) :: button
        logical :: found
        type(status_type) :: st

        if (is_overlay(button%overlay)) then
            start = overlay_start(button%overlay)
            return
        end if

        ! Must be a text-property button.
        call previous_single_property_change(button%position + 1, value_type(name=P_BUTTON), start, found, st)
        if (.not. found) start = point_min()
    end function button_start

    !> Return the position at which BUTTON ends.
    !>
    !> This function only works when BUTTON is in the current buffer.
    !> Port of button.el:button-end.
    integer(int64) function button_end(button) result(end)
        type(button_type), intent(in) :: button
        logical :: found
        type(status_type) :: st

        if (is_overlay(button%overlay)) then
            end = overlay_end(button%overlay)
            return
        end if

        ! Must be a text-property button.
        call next_single_property_change(button%position, value_type(name=P_BUTTON), end, found, st)
        if (.not. found) end = point_max()
    end function button_end

    !> The property NAME of BUTTON, its own (unallocated if none).
    !> Not a port.
    function own_property(button, name) result(val)
        type(button_type), intent(in) :: button
        character(len=*), intent(in) :: name
        type(value_type) :: val
        type(status_type) :: st

        if (is_overlay(button%overlay)) then
            val = overlay_get(button%overlay, name)
        else
            call get_text_property(button%position, value_type(name=name), val, st)
        end if
    end function own_property

    !> The type a `category' property value names, or null.  Not a port.
    function kind_of(val) result(kind)
        type(value_type), intent(in) :: val
        type(button_kind_type), pointer :: kind
        integer(int64) :: k

        kind => null()
        if (vtag(val) /= V_INTEGER) return
        if (.not. allocated(kinds)) return
        k = xfixnum(val)
        if (k >= 1 .and. k <= size(kinds, kind=int64)) kind => kinds(k)%p
    end function kind_of

    !> The registered procedure a property value names: its index, or 0.
    !> Not a port.
    integer function procedure_index(val) result(k)
        type(value_type), intent(in) :: val

        k = 0
        if (vtag(val) /= V_INTEGER) return
        if (.not. allocated(procedures)) return
        if (xfixnum(val) >= 1 .and. xfixnum(val) <= size(procedures, kind=int64)) k = int(xfixnum(val))
    end function procedure_index

    !> Get the properties of button BUTTON: its own, or else those of its
    !> type.
    !>
    !> This function only works when BUTTON is in the current buffer.
    !> Port of button.el:button-get (all the fixed properties at once).
    function get_button_properties(button) result(props)
        type(button_type), intent(in) :: button
        type(button_properties_type) :: props
        type(button_kind_type), pointer :: kind
        type(value_type) :: val
        integer :: k

        kind => kind_of(own_property(button, P_CATEGORY))
        props%kind => kind

        if (associated(kind)) then
            props%action => kind%action
            props%mouse_action => kind%mouse_action
            if (allocated(kind%help_echo)) props%help_echo = kind%help_echo
            props%help_echo_function => kind%help_echo_function
            props%follow_link = kind%follow_link
            props%skip = kind%skip
        end if

        k = procedure_index(own_property(button, P_ACTION))
        if (k > 0) props%action => procedures(k)%action
        k = procedure_index(own_property(button, P_MOUSE_ACTION))
        if (k > 0) props%mouse_action => procedures(k)%action
        val = own_property(button, P_HELP_ECHO)

        if (vtag(val) == V_STRING) then
            props%help_echo = xstring(val)
            props%help_echo_function => null()
        else
            k = procedure_index(val)

            if (k > 0) then
                props%help_echo_function => procedures(k)%help_echo
                if (allocated(props%help_echo)) deallocate (props%help_echo)
            end if
        end if

        val = own_property(button, P_FOLLOW_LINK)
        if (allocated(val%a)) props%follow_link = .true.
        val = own_property(button, P_SKIP)
        if (allocated(val%a)) props%skip = .true.
        props%button_data = own_property(button, P_BUTTON_DATA)
    end function get_button_properties

    !> The registry index of ACTION or HELP_ECHO (one of them present),
    !> added if new.  Not a port.
    integer(int64) function register_procedure(action, help_echo) result(k)
        procedure(button_action_procedure), optional :: action
        procedure(button_help_echo_procedure), optional :: help_echo
        type(procedure_ref_type) :: ref
        integer :: i

        if (.not. allocated(procedures)) allocate (procedures(0))

        do i = 1, size(procedures)
            if (present(action)) then
                if (associated(procedures(i)%action, action)) then
                    k = i
                    return
                end if
            else if (present(help_echo)) then
                if (associated(procedures(i)%help_echo, help_echo)) then
                    k = i
                    return
                end if
            end if
        end do

        if (present(action)) ref%action => action
        if (present(help_echo)) ref%help_echo => help_echo
        procedures = [procedures, ref]
        k = size(procedures, kind=int64)
    end function register_procedure

    !> Set BUTTON's property NAME to VAL: on its overlay, or on the whole
    !> text button.  Not a port.
    subroutine put_property(button, name, val, status)
        type(button_type), intent(in) :: button
        character(len=*), intent(in) :: name
        type(value_type), intent(in) :: val
        type(status_type), intent(out) :: status

        if (is_overlay(button%overlay)) then
            call overlay_put(button%overlay, name, val, status)
        else
            call put_text_property(button_start(button), button_end(button), value_type(name=name), val, status)
        end if
    end subroutine put_property

    !> Give the text from START to END the property NAME with value VAL
    !> where it has none, or where it has the value OLD.  Not a port.
    subroutine put_text_property_where_absent(start, end, name, val, status, old)
        integer(int64), intent(in) :: start, end
        character(len=*), intent(in) :: name
        type(value_type), intent(in) :: val
        type(status_type), intent(out) :: status
        type(value_type), intent(in), optional :: old
        type(value_type) :: prop, here
        integer(int64) :: pos, next
        logical :: found, replace

        status = status_type()
        prop = value_type(name=name)
        pos = start

        do while (pos < end)
            call get_text_property(pos, prop, here, status)
            if (.not. status%ok()) return
            call next_single_property_change(pos, prop, next, found, status, limit=end)
            if (.not. status%ok()) return
            if (.not. found) next = end
            replace = .not. allocated(here%a)

            if (.not. replace .and. present(old)) then
                replace = value_eq(here, old)
            end if

            if (replace) then
                call put_text_property(pos, next, prop, val, status)
                if (.not. status%ok()) return
            end if

            pos = next
        end do
    end subroutine put_text_property_where_absent

    !> The value of a display property PROPERTY of KIND: `face',
    !> `mouse_face' or `keymap' (unallocated for none).  Not a port.
    function kind_display_value(kind, property) result(val)
        type(button_kind_type), intent(in) :: kind
        character(len=*), intent(in) :: property
        type(value_type) :: val

        select case (property)
        case (P_FACE)
            if (associated(kind%face)) val = value_type(name=trim(kind%face%name))
        case (P_MOUSE_FACE)
            if (associated(kind%mouse_face)) val = value_type(name=trim(kind%mouse_face%name))
        case default
            if (kind%keymap%index /= 0) val = make_keymap_ref(kind%keymap)
        end select
    end function kind_display_value

    !> Give BUTTON the type KIND: its `category' property, and the
    !> type's display properties where the button has none of its own
    !> (or has OLD's).  Not a port.
    subroutine put_kind(button, kind, status, old)
        type(button_type), intent(in) :: button
        type(button_kind_type), intent(in) :: kind
        type(status_type), intent(out) :: status
        type(button_kind_type), intent(in), optional :: old
        character(len=*), parameter :: DISPLAY(3) = [character(len=10) :: P_FACE, P_MOUSE_FACE, P_KEYMAP]
        type(value_type) :: val, here
        integer :: k

        call put_property(button, P_CATEGORY, value_type(int(kind%index, int64)), status)
        if (.not. status%ok()) return

        do k = 1, size(DISPLAY)
            val = kind_display_value(kind, trim(DISPLAY(k)))

            if (is_overlay(button%overlay)) then
                here = overlay_get(button%overlay, trim(DISPLAY(k)))
                if (allocated(here%a) .and. .not. present(old)) cycle

                if (allocated(here%a) .and. present(old)) then
                    if (.not. value_eq(here, kind_display_value(old, trim(DISPLAY(k))))) cycle
                end if

                call overlay_put(button%overlay, trim(DISPLAY(k)), val, status)
            else if (present(old)) then
                call put_text_property_where_absent(button_start(button), button_end(button), trim(DISPLAY(k)), val, &
                                                    status, old=kind_display_value(old, trim(DISPLAY(k))))
            else
                call put_text_property_where_absent(button_start(button), button_end(button), trim(DISPLAY(k)), val, &
                                                    status)
            end if

            if (.not. status%ok()) return
        end do
    end subroutine put_kind

    !> Set BUTTON's properties to the arguments present: its type
    !> KIND, ACTION, MOUSE_ACTION, the help text HELP_ECHO or the
    !> procedure HELP_ECHO_FUNCTION giving it, SKIP and BUTTON_DATA.
    !>
    !> This function only works when BUTTON is in the current buffer.
    !> Port of button.el:button-put (a `type' given as KIND becomes
    !> the `category' property; the `category' property itself is never
    !> set directly).
    recursive subroutine put_button_properties(button, status, kind, action, mouse_action, help_echo, &
                                               help_echo_function, skip, button_data)
        type(button_type), intent(in) :: button
        type(status_type), intent(out) :: status
        type(button_kind_type), intent(in), target, optional :: kind
        procedure(button_action_procedure), optional :: action, mouse_action
        character(len=*), intent(in), optional :: help_echo
        procedure(button_help_echo_procedure), optional :: help_echo_function
        logical, intent(in), optional :: skip
        type(value_type), intent(in), optional :: button_data
        type(button_kind_type), pointer :: old

        status = status_type()

        if (present(kind)) then
            old => kind_of(own_property(button, P_CATEGORY))

            if (associated(old)) then
                call put_kind(button, kind, status, old=old)
            else
                call put_kind(button, kind, status)
            end if

            if (.not. status%ok()) return
        end if

        if (present(action)) then
            call put_property(button, P_ACTION, value_type(register_procedure(action=action)), status)
            if (.not. status%ok()) return
        end if

        if (present(mouse_action)) then
            call put_property(button, P_MOUSE_ACTION, value_type(register_procedure(action=mouse_action)), status)
            if (.not. status%ok()) return
        end if

        if (present(help_echo)) then
            call put_property(button, P_HELP_ECHO, value_type(text=help_echo), status)
            if (.not. status%ok()) return
        end if

        if (present(help_echo_function)) then
            call put_property(button, P_HELP_ECHO, value_type(register_procedure(help_echo=help_echo_function)), status)
            if (.not. status%ok()) return
        end if

        if (present(skip)) then
            call put_property(button, P_SKIP, value_type(skip), status)
            if (.not. status%ok()) return
        end if

        if (present(button_data)) call put_property(button, P_BUTTON_DATA, button_data, status)
    end subroutine put_button_properties

    !> Call BUTTON's `action' property.
    !> If USE_MOUSE_ACTION is true, invoke the button's `mouse_action'
    !> property instead of `action'; if the button has no `mouse_action',
    !> the value of `action' is used instead.
    !>
    !> The action is called with BUTTON and the button's `button_data'
    !> value (unallocated if it has none).
    !>
    !> This function only works when BUTTON is in the current buffer.
    !> Port of button.el:button-activate (a marker as the action is
    !> Deferred).
    recursive subroutine activate_button(button, status, use_mouse_action)
        type(button_type), intent(in) :: button
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: use_mouse_action
        type(button_properties_type) :: props
        procedure(button_action_procedure), pointer :: action

        status = status_type()
        props = get_button_properties(button)
        action => props%action

        if (present(use_mouse_action)) then
            if (use_mouse_action .and. associated(props%mouse_action)) action => props%mouse_action
        end if

        if (associated(action)) call action(button, props%button_data, status)
    end subroutine activate_button

    !> Return BUTTON's text label.
    !>
    !> This function only works when BUTTON is in the current buffer.
    !> Port of button.el:button-label.
    function button_label(button) result(label)
        type(button_type), intent(in) :: button
        character(len=:), allocatable :: label
        type(status_type) :: st

        call buffer_substring_no_properties(button_start(button), button_end(button), label, st)
    end function button_label

    !> Return true if BUTTON has `button type' KIND, or one of its
    !> subtypes.  Port of button.el:button-has-type-p.
    logical function has_button_type(button, kind)
        type(button_type), intent(in) :: button
        type(button_kind_type), intent(in) :: kind
        type(button_properties_type) :: props

        props = get_button_properties(button)
        has_button_type = associated(props%kind)
        if (has_button_type) has_button_type = is_button_subtype(props%kind, kind)
    end function has_button_type

    ! ------------------------------------------------------------------
    ! Creating overlay buttons
    ! ------------------------------------------------------------------

    !> The next `button' property value.  Not a port.
    function next_serial() result(val)
        type(value_type) :: val

        last_serial = last_serial + 1
        val = value_type(last_serial)
    end function next_serial

    !> Make a button from BEGINNING to END in the current buffer.
    !> The optional arguments are properties to add to the button (see
    !> `put_button_properties').  In addition, KIND may be used to specify a
    !> `button type' from which to inherit other properties; see
    !> `button_kind_type'.
    !>
    !> Also see `make_text_button', `insert_button'.  Port of
    !> button.el:make-button.
    recursive subroutine make_button(beginning, end, button, status, kind, action, mouse_action, help_echo, &
                                     help_echo_function, skip, button_data)
        integer(int64), intent(in) :: beginning, end
        type(button_type), intent(out) :: button
        type(status_type), intent(out) :: status
        type(button_kind_type), intent(in), target, optional :: kind
        procedure(button_action_procedure), optional :: action, mouse_action
        character(len=*), intent(in), optional :: help_echo
        procedure(button_help_echo_procedure), optional :: help_echo_function
        logical, intent(in), optional :: skip
        type(value_type), intent(in), optional :: button_data

        call make_overlay(button%overlay, beginning, end, status, front_advance=.true., rear_advance=.false.)
        if (.not. status%ok()) return
        call put_button_properties(button, status, kind, action, mouse_action, help_echo, help_echo_function, skip, button_data)
        if (.not. status%ok()) return
        ! Put a value in the overlay that tells it is a button.
        call overlay_put(button%overlay, P_BUTTON, next_serial(), status)
        if (.not. status%ok()) return
        ! If the user didn't specify a type, use the default.
        if (.not. present(kind)) call put_kind(button, default_button, status)
        if (.not. status%ok()) return
        call overlay_put(button%overlay, P_EVAPORATE, value_type(.true.), status)
    end subroutine make_button

    !> Insert a button with the label LABEL.
    !> The optional arguments are properties to add to the button, and
    !> KIND its `button type'; see `make_button'.
    !>
    !> Also see `insert_text_button', `make_button'.  Port of
    !> button.el:insert-button.
    recursive subroutine insert_button(label, button, status, kind, action, mouse_action, help_echo, &
                                       help_echo_function, skip, button_data)
        character(len=*), intent(in) :: label
        type(button_type), intent(out) :: button
        type(status_type), intent(out) :: status
        type(button_kind_type), intent(in), target, optional :: kind
        procedure(button_action_procedure), optional :: action, mouse_action
        character(len=*), intent(in), optional :: help_echo
        procedure(button_help_echo_procedure), optional :: help_echo_function
        logical, intent(in), optional :: skip
        type(value_type), intent(in), optional :: button_data
        integer(int64) :: start

        start = point()
        call insert(label, status)
        if (.not. status%ok()) return
        call make_button(start, point(), button, status, kind, action, mouse_action, help_echo, help_echo_function, &
                         skip, button_data)
    end subroutine insert_button

    ! ------------------------------------------------------------------
    ! Creating text-property buttons
    ! ------------------------------------------------------------------

    !> Make a button from BEGINNING to END in the current buffer.
    !> The optional arguments are properties to add to the button, and
    !> KIND its `button type'; see `make_button'.
    !>
    !> This function is like `make_button', except that the button is
    !> actually part of the text instead of being a property of the buffer.
    !> That is, this function uses text properties, the other uses overlays.
    !> Creating large numbers of buttons can also be somewhat faster
    !> using `make_text_button'.  Note, however, that if there is an existing
    !> face property at the site of the button, the button face may not be visible.
    !> You may want to use `make_button' in that case.
    !>
    !> If BUTTON_DATA is present, it will later be passed to the `action'
    !> procedure.
    !>
    !> Also see `insert_text_button'.  Port of button.el:make-text-button
    !> (a string as BEGINNING is Deferred).
    recursive subroutine make_text_button(beginning, end, button, status, kind, action, mouse_action, help_echo, &
                                          help_echo_function, skip, button_data)
        integer(int64), intent(in) :: beginning, end
        type(button_type), intent(out) :: button
        type(status_type), intent(out) :: status
        type(button_kind_type), intent(in), target, optional :: kind
        procedure(button_action_procedure), optional :: action, mouse_action
        character(len=*), intent(in), optional :: help_echo
        procedure(button_help_echo_procedure), optional :: help_echo_function
        logical, intent(in), optional :: skip
        type(value_type), intent(in), optional :: button_data
        type(value_type), allocatable :: props(:)
        logical :: changed

        button%overlay = null_handle()
        button%position = beginning
        ! Each button has its own `button' property so that
        ! next_single_property_change can detect boundaries reliably.
        allocate (props(0))
        call append_property(props, P_BUTTON, next_serial())

        if (present(kind)) then
            call append_property(props, P_CATEGORY, value_type(int(kind%index, int64)))
        else
            ! The user didn't specify a type, use the default.
            call append_property(props, P_CATEGORY, value_type(int(default_button%index, int64)))
        end if

        call append_property(props, P_REAR_NONSTICKY, value_type(.true.))
        call add_text_properties(beginning, end, make_list(props), changed, status)
        if (.not. status%ok()) return

        if (present(kind)) then
            call put_kind(button, kind, status)
        else
            call put_kind(button, default_button, status)
        end if

        if (.not. status%ok()) return
        call put_button_properties(button, status, action=action, mouse_action=mouse_action, help_echo=help_echo, &
                                   help_echo_function=help_echo_function, skip=skip, button_data=button_data)
    end subroutine make_text_button

    !> Append the text property NAME with value VAL to PROPS, the names and
    !> values of the text properties to add.  Not a port.
    subroutine append_property(props, name, val)
        type(value_type), allocatable, intent(inout) :: props(:)
        character(len=*), intent(in) :: name
        type(value_type), intent(in) :: val
        type(value_type) :: key

        key = value_type(name=name)
        props = [props, key, val]
    end subroutine append_property

    !> Insert a button with the label LABEL.
    !> The optional arguments are properties to add to the button, and
    !> KIND its `button type'; see `make_button'.
    !>
    !> This function is like `insert_button', except that the button is
    !> actually part of the text instead of being a property of the buffer.
    !> Creating large numbers of buttons can also be somewhat faster using
    !> `insert_text_button'.
    !>
    !> Also see `make_text_button'.  Port of button.el:insert-text-button.
    recursive subroutine insert_text_button(label, button, status, kind, action, mouse_action, help_echo, &
                                            help_echo_function, skip, button_data)
        character(len=*), intent(in) :: label
        type(button_type), intent(out) :: button
        type(status_type), intent(out) :: status
        type(button_kind_type), intent(in), target, optional :: kind
        procedure(button_action_procedure), optional :: action, mouse_action
        character(len=*), intent(in), optional :: help_echo
        procedure(button_help_echo_procedure), optional :: help_echo_function
        logical, intent(in), optional :: skip
        type(value_type), intent(in), optional :: button_data
        integer(int64) :: start

        start = point()
        call insert(label, status)
        if (.not. status%ok()) return
        call make_text_button(start, point(), button, status, kind, action, mouse_action, help_echo, &
                              help_echo_function, skip, button_data)
    end subroutine insert_text_button

    ! ------------------------------------------------------------------
    ! Finding buttons in a buffer
    ! ------------------------------------------------------------------

    !> Return the button at position POSITION in the current buffer, or
    !> FOUND false.  The button of a text property button is POSITION.
    !> Port of button.el:button-at.
    subroutine button_at(position, button, found)
        integer(int64), intent(in) :: position
        type(button_type), intent(out) :: button
        logical, intent(out) :: found
        type(value_type) :: val, kind
        type(handle_type) :: overlay

        button%overlay = null_handle()
        button%position = 0
        call get_char_property_and_overlay(position, value_type(name=P_BUTTON), val, overlay)
        kind = get_char_property(position, value_type(name=P_CATEGORY))
        found = allocated(val%a) .and. allocated(kind%a)
        if (.not. found) return

        if (is_overlay(overlay)) then
            button%overlay = overlay
        else
            ! Must be a text-property button.
            button%position = position
        end if
    end subroutine button_at

    !> Return the next button after position POSITION in the current
    !> buffer, or FOUND false.
    !> If COUNT_CURRENT is true, count any button at POSITION in the search,
    !> instead of starting at the next button.  Port of
    !> button.el:next-button.
    subroutine next_button(position, button, found, count_current)
        integer(int64), intent(in) :: position
        type(button_type), intent(out) :: button
        logical, intent(out) :: found
        logical, intent(in), optional :: count_current
        integer(int64) :: pos
        logical :: count

        pos = position
        count = .false.
        if (present(count_current)) count = count_current
        ! Search for the next button boundary.
        if (.not. count) pos = next_single_char_property_change(pos, value_type(name=P_BUTTON))

        do
            found = .false.
            button%overlay = null_handle()
            button%position = 0
            if (pos >= point_max()) return
            call button_at(pos, button, found)
            if (found) return
            ! We must have originally been on a button, and are now in the
            ! inter-button space: look further.
            pos = next_single_char_property_change(pos, value_type(name=P_BUTTON))
        end do
    end subroutine next_button

    !> Return the previous button before position POSITION in the current
    !> buffer, or FOUND false.
    !> If COUNT_CURRENT is true, count any button at POSITION in the search,
    !> instead of starting at the next button.  Port of
    !> button.el:previous-button.
    recursive subroutine previous_button(position, button, found, count_current)
        integer(int64), intent(in) :: position
        type(button_type), intent(out) :: button
        logical, intent(out) :: found
        logical, intent(in), optional :: count_current
        type(button_type) :: new_button
        integer(int64) :: pos
        logical :: count, new_found

        count = .false.
        if (present(count_current)) count = count_current
        call button_at(position, button, found)

        if (found) then
            if (count) return
            ! We started out on a button, so move to its start and look
            ! for the previous button boundary.
            pos = previous_single_char_property_change(button_start(button), value_type(name=P_BUTTON))
            call button_at(pos, new_button, new_found)

            if (new_found) then
                ! We are in a button again; this can happen if there
                ! are adjacent buttons (or at bob).
                found = pos /= button_start(button)
                button = new_button
            else
                ! We are now in the space between buttons.
                call previous_button(pos, button, found)
            end if

            return
        end if

        ! We started out in the space between buttons.
        pos = previous_single_char_property_change(position, value_type(name=P_BUTTON))
        call button_at(pos, button, found)
        if (found) return
        if (pos > point_min()) call button_at(pos - 1, button, found)
    end subroutine previous_button

    ! ------------------------------------------------------------------
    ! User commands
    ! ------------------------------------------------------------------

    !> Perform the action specified by a button at location POSITION.
    !> If USE_MOUSE_ACTION is true, invoke the button's `mouse_action'
    !> property instead of its `action' property; if the button has no
    !> `mouse_action', the value of `action' is used instead.
    !>
    !> The action is invoked using `activate_button' (which see).
    !>
    !> POSITION defaults to point.
    !>
    !> If there's no button at POSITION, do nothing and return PUSHED
    !> false, otherwise return it true.
    !>
    !> To get a description of the function that will be invoked when
    !> pushing a button, use the `describe_button' command.  Port of
    !> button.el:push-button (mouse and touch screen events are Deferred).
    recursive subroutine push_button(status, position, use_mouse_action, pushed)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: position
        logical, intent(in), optional :: use_mouse_action
        logical, intent(out), optional :: pushed
        type(button_type) :: button
        integer(int64) :: pos
        logical :: found

        status = status_type()
        pos = point()
        if (present(position)) pos = position
        call button_at(pos, button, found)
        if (present(pushed)) pushed = found
        if (found) call activate_button(button, status, use_mouse_action)
    end subroutine push_button

    recursive subroutine cmd_push_button(status)
        type(status_type), intent(out) :: status

        call push_button(status)
    end subroutine cmd_push_button

    !> Compute BUTTON's `help_echo' property and return its value
    !> (unallocated for none).  If it is present, pass it through
    !> `substitute_command_keys' before returning it, as is done for
    !> `show_help_function'.  Port of button.el:button--help-echo.
    function button_help_echo(button) result(help)
        type(button_type), intent(in) :: button
        character(len=:), allocatable :: help
        type(button_properties_type) :: props
        type(handle_type) :: object

        props = get_button_properties(button)

        if (associated(props%help_echo_function)) then
            object = current_buffer()
            if (is_overlay(button%overlay)) object = button%overlay
            call props%help_echo_function(selected_window(), object, button_start(button), help)
        else if (allocated(props%help_echo)) then
            help = props%help_echo
        end if

        if (allocated(help)) help = substitute_command_keys(help)
    end function button_help_echo

    !> Move to the Nth next button, or Nth previous button if N is negative.
    !> If N is 0, move to the start of any button at point.
    !> If WRAP is true, moving past either end of the buffer continues from the
    !> other end.
    !> If DISPLAY_MESSAGE is true, the button's `help_echo' property
    !> is displayed.  Any button with a true `skip' property is
    !> skipped over.
    !>
    !> If NO_ERROR, return FOUND false if no further buttons could be found
    !> instead of erroring out.
    !>
    !> Returns the button found.  Port of button.el:forward-button.
    recursive subroutine forward_button(n, status, wrap, display_message, no_error, button, found)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: wrap, display_message, no_error
        type(button_type), intent(out), optional :: button
        logical, intent(out), optional :: found
        type(button_type) :: b
        type(button_properties_type) :: props
        integer(int64) :: count, wrap_start, opoint
        logical :: is_found, do_wrap, fail, have_opoint
        character(len=:), allocatable :: msg

        status = status_type()
        do_wrap = .false.
        if (present(wrap)) do_wrap = wrap

        if (n == 0) then
            ! Move to start of current button.
            call button_at(point(), b, is_found)
            if (is_found) call goto_char(button_start(b))
        else
            ! Move to Nth next button.
            wrap_start = point_min()
            if (n < 0) wrap_start = point_max()
            have_opoint = .false.
            opoint = 0
            fail = .false.
            count = abs(n)
            ! Just to start the loop.
            is_found = .true.

            do while (.not. fail .and. count > 0 .and. is_found)
                call iterate(point(), .false.)
                if (.not. is_found .and. do_wrap) call iterate(wrap_start, .true.)

                if (is_found) then
                    call goto_char(button_start(b))

                    ! Avoid looping forever (e.g., if all the buttons have
                    ! the `skip' property).
                    if (.not. have_opoint) then
                        opoint = point()
                        have_opoint = .true.
                    else if (opoint == point()) then
                        fail = .true.
                    end if

                    props = get_button_properties(b)
                    if (.not. props%skip) count = count - 1
                end if
            end do
        end if

        if (present(found)) found = is_found
        if (present(button)) button = b

        if (.not. is_found) then
            if (present(no_error)) then
                if (no_error) return
            end if

            if (do_wrap) then
                call set_error(status, FM_USER_ERROR, "No buttons!")
            else
                call set_error(status, FM_USER_ERROR, "No more buttons")
            end if

            return
        end if

        if (present(display_message)) then
            if (display_message) msg = button_help_echo(b)
        end if

        if (allocated(msg)) call message(msg)

    contains

        !> next_button or previous_button, as the sign of N says.
        recursive subroutine iterate(position, count_current)
            integer(int64), intent(in) :: position
            logical, intent(in) :: count_current

            if (n > 0) then
                call next_button(position, b, is_found, count_current)
            else
                call previous_button(position, b, is_found, count_current)
            end if
        end subroutine iterate
    end subroutine forward_button

    recursive subroutine cmd_forward_button(status)
        type(status_type), intent(out) :: status

        call forward_button(prefix_numeric_value(current_prefix_arg), status, wrap=.true., display_message=.true.)
    end subroutine cmd_forward_button

    !> Move to the Nth previous button, or Nth next button if N is negative.
    !> If N is 0, move to the start of any button at point.
    !> If WRAP is true, moving past either end of the buffer continues from the
    !> other end.
    !> If DISPLAY_MESSAGE is true, the button's `help_echo' property
    !> is displayed.  Any button with a true `skip' property is
    !> skipped over.
    !>
    !> If NO_ERROR, return FOUND false if no further buttons could be found
    !> instead of erroring out.
    !>
    !> Returns the button found.  Port of button.el:backward-button.
    recursive subroutine backward_button(n, status, wrap, display_message, no_error, button, found)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: wrap, display_message, no_error
        type(button_type), intent(out), optional :: button
        logical, intent(out), optional :: found

        call forward_button(-n, status, wrap, display_message, no_error, button, found)
    end subroutine backward_button

    recursive subroutine cmd_backward_button(status)
        type(status_type), intent(out) :: status

        call backward_button(prefix_numeric_value(current_prefix_arg), status, wrap=.true., display_message=.true.)
    end subroutine cmd_backward_button

    !> Describe a button's type in a *Help* buffer.  Port of
    !> button.el:button--describe (the action's description is Deferred).
    subroutine describe_properties(props, status)
        type(button_properties_type), intent(in) :: props
        type(status_type), intent(out) :: status

        if (associated(props%kind)) then
            call with_help_window(format_message("This button's type is `"//trim(props%kind%name)//"'."), status)
        else
            call with_help_window(format_message("This button's type is `none'."), status)
        end if
    end subroutine describe_properties

    !> Display a buffer with information about the button at point.
    !>
    !> When called from Fortran, pass POSITION as a buffer position where a
    !> button is present, or BUTTON as the button to describe.  If both are
    !> absent, the button at point is the button to describe.  DESCRIBED
    !> is true if there was a button.  Port of button.el:button-describe.
    subroutine describe_button(status, position, button, described)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: position
        type(button_type), intent(in), optional :: button
        logical, intent(out), optional :: described
        type(button_type) :: b
        logical :: found

        status = status_type()

        if (present(button)) then
            b = button
            found = .true.
        else if (present(position)) then
            call button_at(position, b, found)
        else
            call button_at(point(), b, found)
        end if

        if (present(described)) described = found
        if (found) call describe_properties(get_button_properties(b), status)
    end subroutine describe_button

    recursive subroutine cmd_describe_button(status)
        type(status_type), intent(out) :: status

        call describe_button(status, point())
    end subroutine cmd_describe_button

    ! ------------------------------------------------------------------
    ! Buttonizing text
    ! ------------------------------------------------------------------

    !> The text properties of a button made by `buttonize_region', with
    !> CALLBACK as its action, DATA and HELP_ECHO.  Port of
    !> button.el:button--properties.
    subroutine buttonize_properties(props, callback, data, help_echo)
        type(value_type), allocatable, intent(out) :: props(:)
        procedure(button_action_procedure), optional :: callback
        type(value_type), intent(in), optional :: data
        character(len=*), intent(in), optional :: help_echo
        type(value_type) :: val

        allocate (props(0))
        call append_property(props, P_FONT_LOCK_FACE, value_type(name=trim(button_face%name)))
        call append_property(props, P_MOUSE_FACE, value_type(name=trim(highlight%name)))
        call append_property(props, P_BUTTON, value_type(.true.))
        call append_property(props, P_FOLLOW_LINK, value_type(.true.))
        call append_property(props, P_CATEGORY, value_type(.true.))
        if (present(data)) val = data
        call append_property(props, P_BUTTON_DATA, val)
        call append_property(props, P_KEYMAP, make_keymap_ref(button_map))
        val = value_type()
        if (present(callback)) val = value_type(register_procedure(action=callback))
        call append_property(props, P_ACTION, val)

        if (present(help_echo)) then
            call append_property(props, P_HELP_ECHO, value_type(text=help_echo))
            ! Record that button.el is responsible for this property.
            call append_property(props, P_HELP_ECHO_BUTTON, value_type(.true.))
        end if
    end subroutine buttonize_properties

    !> Make the region between START and END into a button.
    !> When clicked, CALLBACK will be called with the DATA as the
    !> function argument.
    !>
    !> If HELP_ECHO, use that as the `help_echo' property.
    !>
    !> Also see `unbuttonize_region'.  Port of
    !> button.el:buttonize-region.
    subroutine buttonize_region(start, end, callback, status, data, help_echo)
        integer(int64), intent(in) :: start, end
        procedure(button_action_procedure) :: callback
        type(status_type), intent(out) :: status
        type(value_type), intent(in), optional :: data
        character(len=*), intent(in), optional :: help_echo
        type(value_type), allocatable :: props(:)
        logical :: changed

        call buttonize_properties(props, callback, data, help_echo)
        call add_text_properties(start, end, make_list(props), changed, status)
        if (.not. status%ok()) return
        call add_face_text_property(start, end, value_type(name=trim(button_face%name)), status, append=.true.)
    end subroutine buttonize_region

    !> Remove all the buttons between START and END.
    !> This removes both text-property and overlay based buttons.
    !> Port of button.el:unbuttonize-region.
    subroutine unbuttonize_region(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: overlays(:)
        type(value_type), allocatable :: props(:), names(:)
        type(value_type) :: val
        type(silent_modifications_type) :: silent
        logical :: changed
        integer :: k

        call overlays_in(start, end, overlays)

        do k = 1, size(overlays)
            val = overlay_get(overlays(k), P_BUTTON)
            if (allocated(val%a)) call destroy_overlay(overlays(k))
        end do

        call silent_modifications_begin(silent)
        call buttonize_properties(props)
        ! Only remove help_echo if it was added by button.el.
        call get_text_property(start, value_type(name=P_HELP_ECHO_BUTTON), val, status)

        if (allocated(val%a)) then
            call append_property(props, P_HELP_ECHO, value_type())
            call append_property(props, P_HELP_ECHO_BUTTON, value_type())
        end if

        ! The property names alone.
        names = props(1:size(props):2)
        call remove_list_of_text_properties(start, end, make_list(names), changed, status)
        call silent_modifications_end(silent)
        if (.not. status%ok()) return
        call add_face_text_property(start, end, value_type(name=trim(button_face%name)), status)
    end subroutine unbuttonize_region

    ! ------------------------------------------------------------------
    ! Button mode and setup
    ! ------------------------------------------------------------------

    !> A minor mode for navigating to buttons with the TAB key.
    !>
    !> Disabling the mode will remove all buttons in the current buffer.
    !> Port of button.el:button-mode.
    recursive subroutine button_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%button_mode%command(status)
    end subroutine button_mode

    !> The body of `button_mode'.  Port of button.el:button-mode, in part
    !> (its body).
    recursive subroutine button_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()
        if (enable) return
        ! (save-excursion (save-restriction (widen) ...)): the whole buffer.
        call unbuttonize_buffer(status)

    contains

        !> unbuttonize-region over the whole buffer, point and narrowing
        !> kept.
        subroutine unbuttonize_buffer(status)
            type(status_type), intent(out) :: status
            integer(int64) :: saved_point, saved_begv, saved_zv, saved_begv_byte, saved_zv_byte

            saved_point = point()
            saved_begv = buffers(current)%begv
            saved_zv = buffers(current)%zv
            saved_begv_byte = buffers(current)%begv_byte
            saved_zv_byte = buffers(current)%zv_byte
            buffers(current)%begv = 1
            buffers(current)%begv_byte = 1
            buffers(current)%zv = buffers(current)%text%z
            buffers(current)%zv_byte = buffers(current)%text%z_byte
            call unbuttonize_region(point_min(), point_max(), status)
            buffers(current)%begv = saved_begv
            buffers(current)%begv_byte = saved_begv_byte
            buffers(current)%zv = saved_zv
            buffers(current)%zv_byte = saved_zv_byte
            call goto_char(saved_point)
        end subroutine unbuttonize_buffer
    end subroutine button_mode_body

    !> Define the face, keymaps, default button type, mode and commands of
    !> button.el.  Not a port.
    subroutine init_button()
        type(named_face_type), pointer :: f
        type(status_type) :: st

        if (default_button%index /= 0) return
        f => button_face
        call f%define("button", "Default face used for buttons.")
        call f%spec_entry(display_when(), face_attrs(inherit="link"))
        call minor_modes%button_mode%define("button_mode", hook=button_mode_hook, keymap=.true., body=button_mode_body)
        button_buffer_map = minor_modes%button_mode%map
        call keymap_set(button_buffer_map, "TAB", "forward_button", st)
        call keymap_set(button_buffer_map, "ESC TAB", "backward_button", st)
        call keymap_set(button_buffer_map, "<backtab>", "backward_button", st)
        call register_keymap_name("button_buffer_map", button_buffer_map)
        call make_sparse_keymap(button_map)
        call set_keymap_parent(button_map, button_buffer_map, st)
        call keymap_set(button_map, "RET", "push_button", st)
        call register_keymap_name("button_map", button_map)
        default_button%face => button_face
        default_button%mouse_face => highlight
        default_button%keymap = button_map
        default_button%action => null()
        default_button%help_echo = "mouse-2, RET: Push this button"
        call default_button%define("button")
        commands%push_button%execute => cmd_push_button
        commands%forward_button%execute => cmd_forward_button
        commands%backward_button%execute => cmd_backward_button
        commands%describe_button%execute => cmd_describe_button
        commands%button_mode%execute => button_mode
    end subroutine init_button
end module button_mod

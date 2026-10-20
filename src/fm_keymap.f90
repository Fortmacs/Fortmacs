! fm_keymap.f90 -- keymaps: binding key sequences to commands.
! Ported from GNU Emacs 31.1 src/keymap.c, plus key-parse from
! lisp/keymap.el.
!
! Copyright (C) 1985-1988, 1993-1995, 1998-2026 Free Software
! Foundation, Inc.
! Copyright (C) 2021-2026 Free Software Foundation, Inc. (keymap.el)
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

!> Keymaps are registry objects referenced by handle (values of tag
!> V_KEYMAP).  Upstream a keymap is the list (keymap ENTRIES... . PARENT);
!> here each keymap holds parallel arrays of events and bindings plus a
!> parent handle.  An entry whose binding is nil is an explicit "unbound"
!> that shadows the parent; `remove` deletes the entry instead.
!>
!> A key is a vector of events: fixnums (characters with modifier bits)
!> and symbols (function keys such as `f1` or `C-home`).  Strings as keys
!> are not supported; `key_parse` (kbd) builds vectors.
!>
!> Composite keymaps, which upstream builds as (keymap MAP1 MAP2 ...)
!> when several active maps define the same prefix key, are lists of
!> keymap values.  The symbol `keymap` inside such a list marks the
!> start of an inherited part, exactly as upstream.  A list of keymaps
!> given to `lookup_key` is the same thing, so `keymapp` accepts it too
!> (upstream only accepts the (keymap ...) form).
!>
!> Lifetimes: keymaps made by `define_key` for new prefix keys, and the
!> copies of sub-keymaps made by `copy_keymap`, are owned by the keymap
!> they were made for, and `free_keymap` frees them along with it.
!> Freeing a keymap that is still bound or used as a parent leaves stale
!> handles, which are then treated as "not a keymap".
!>
!> Not ported yet: char-table and vector entries (a full keymap is a
!> flag), menu items, `remap`, symbols whose function definition is a
!> keymap, event ranges, minor-mode and text-property maps, where-is.
module fm_keymap
   use, intrinsic :: iso_fortran_env, only: int32, int64
   use fm_error, only: status_type, set_error, FM_GENERIC_ERROR, &
                       FM_WRONG_TYPE_ARGUMENT
   use fm_handle, only: handle_type, slot_table_type, null_handle
   use fm_lisp, only: value_type, V_INT, V_SYMBOL, V_STRING, V_LIST, &
                      V_KEYMAP, SYM_T, symbols, nil, make_fixnum, &
                      make_symbol_ref, make_keymap_ref, make_list, list, vtag, &
                      nilp, fixnump, symbolp, xfixnum, xsymbol, xstring, &
                      xhandle, list_length, list_elements, list_tail
   use fm_lread, only: intern
   use fm_data, only: value_eq
   use fm_character, only: CHAR_ALT, CHAR_SUPER, CHAR_HYPER, CHAR_SHIFT, &
                           CHAR_CTL, CHAR_META, MAX_CHAR, &
                           MAX_MULTIBYTE_LENGTH, char_string, &
                           string_char_and_length
   use fm_buffer, only: buffers, cur
   implicit none(type, external)
   private

   public :: make_keymap, make_sparse_keymap, free_keymap, keymap_live_p
   public :: keymapp, keymap_parent, set_keymap_parent, copy_keymap
   public :: keymap_prompt, keymap_full_p
   public :: access_keymap, define_key, lookup_key, key_binding
   public :: current_global_map, use_global_map
   public :: current_local_map, use_local_map
   public :: key_parse, key_description, single_key_description, &
             push_key_description

   !> Modifier bits as int64 (termhooks.h).
   integer(int64), parameter :: M_ALT = int(CHAR_ALT, int64), &
                                M_SUPER = int(CHAR_SUPER, int64), &
                                M_HYPER = int(CHAR_HYPER, int64), &
                                M_SHIFT = int(CHAR_SHIFT, int64), &
                                M_CTL = int(CHAR_CTL, int64), &
                                M_META = int(CHAR_META, int64)
   integer(int64), parameter :: M_ALL = M_ALT + M_SUPER + M_HYPER + &
                                M_SHIFT + M_CTL + M_META
   !> keyboard.c:meta_prefix_char (ESC).
   integer(int64), parameter :: META_PREFIX_CHAR = 27
   !> keymap.c:copy_keymap_1 recursion limit.
   integer, parameter :: MAX_COPY_DEPTH = 100

   !> One keymap.  Entries are stored oldest first; upstream's list order
   !> (newest first) is the reverse.
   type :: keymap_type
      type(value_type), allocatable :: events(:), bindings(:)
      integer(int64) :: n = 0
      type(handle_type) :: parent
      !> The keymap this one was made for (see the module note).
      type(handle_type) :: owner
      !> Overall prompt string (make-keymap's STRING argument).
      type(value_type) :: prompt
      !> Made by make-keymap rather than make-sparse-keymap.
      logical :: full = .false.
   end type keymap_type

   type(keymap_type), allocatable :: keymaps(:)
   type(slot_table_type) :: table
   !> keymap.c:current_global_map; made on first use.
   type(handle_type) :: global_map

   !> State of one access_keymap_1 scan.
   type :: access_state
      !> The binding found so far; meaningful when `found` (else Qunbound).
      type(value_type) :: retval
      logical :: found = .false.
      !> `retval` is a composite list built by this scan (retval_tail).
      logical :: built = .false.
      type(value_type) :: t_binding
      logical :: t_found = .false.
      logical :: t_ok = .false.
   end type access_state

contains

   ! ------------------------------------------------------------------
   ! Registry
   ! ------------------------------------------------------------------

   subroutine new_keymap(km, full, prompt)
      type(handle_type), intent(out) :: km
      logical, intent(in) :: full
      type(value_type), intent(in), optional :: prompt
      type(keymap_type), allocatable :: grown(:)

      call table%acquire(km)
      if (.not. allocated(keymaps)) then
         allocate (keymaps(table%capacity()))
      else if (size(keymaps, kind=int64) < table%capacity()) then
         allocate (grown(table%capacity()))
         grown(1:size(keymaps)) = keymaps
         call move_alloc(grown, keymaps)
      end if
      keymaps(km%index) = keymap_type()
      keymaps(km%index)%full = full
      if (present(prompt)) keymaps(km%index)%prompt = prompt
   end subroutine new_keymap

   !> A new full keymap.  Port of keymap.c:Fmake_keymap.
   subroutine make_keymap(km, prompt)
      type(handle_type), intent(out) :: km
      type(value_type), intent(in), optional :: prompt

      call new_keymap(km, .true., prompt)
   end subroutine make_keymap

   !> A new sparse keymap.  Port of keymap.c:Fmake_sparse_keymap.
   subroutine make_sparse_keymap(km, prompt)
      type(handle_type), intent(out) :: km
      type(value_type), intent(in), optional :: prompt

      call new_keymap(km, .false., prompt)
   end subroutine make_sparse_keymap

   !> Slot of keymap `km`, or a wrong-type-argument error.
   subroutine resolve_keymap(km, i, status)
      type(handle_type), intent(in) :: km
      integer(int64), intent(out) :: i
      type(status_type), intent(out) :: status

      call table%resolve(km, i, status)
      if (.not. status%ok()) &
         call set_error(status, FM_WRONG_TYPE_ARGUMENT, "keymapp")
   end subroutine resolve_keymap

   logical function keymap_live_p(km)
      type(handle_type), intent(in) :: km

      keymap_live_p = table%is_live(km)
   end function keymap_live_p

   logical function same_handle(a, b)
      type(handle_type), intent(in) :: a, b

      same_handle = a%index == b%index .and. a%generation == b%generation
   end function same_handle

   !> Free keymap `km` and the keymaps it owns.  Not a port: upstream
   !> relies on GC.
   recursive subroutine free_keymap(km, status)
      type(handle_type), intent(in) :: km
      type(status_type), intent(out) :: status
      type(handle_type) :: h
      type(status_type) :: ignored
      integer(int64) :: i, k

      call resolve_keymap(km, i, status)
      if (.not. status%ok()) return
      keymaps(i) = keymap_type()
      call table%release(km, status)
      if (.not. status%ok()) return
      do k = 1, table%capacity()
         h = table%handle_of(k)
         if (h%index == 0) cycle
         if (same_handle(keymaps(k)%owner, km)) call free_keymap(h, ignored)
      end do
   end subroutine free_keymap

   !> The prompt string of `km` (nil if none).
   function keymap_prompt(km) result(v)
      type(handle_type), intent(in) :: km
      type(value_type) :: v

      if (table%is_live(km)) v = keymaps(km%index)%prompt
   end function keymap_prompt

   !> Whether `km` was made by make-keymap (upstream: has a char-table).
   logical function keymap_full_p(km)
      type(handle_type), intent(in) :: km

      keymap_full_p = .false.
      if (table%is_live(km)) keymap_full_p = keymaps(km%index)%full
   end function keymap_full_p

   ! ------------------------------------------------------------------
   ! Predicates and parents
   ! ------------------------------------------------------------------

   !> The symbol `keymap`, which marks inherited parts of composites.
   function keymap_marker() result(v)
      type(value_type) :: v

      v = make_symbol_ref(intern("keymap"))
   end function keymap_marker

   logical function is_marker(v)
      type(value_type), intent(in) :: v

      is_marker = .false.
      if (symbolp(v)) is_marker = xsymbol(v) == intern("keymap")
   end function is_marker

   logical function is_t(v)
      type(value_type), intent(in) :: v

      is_t = .false.
      if (symbolp(v)) is_t = xsymbol(v) == SYM_T
   end function is_t

   !> Whether `v` is a live keymap or a composite (non-empty list of
   !> keymaps and `keymap` markers).  Port of keymap.c:Fkeymapp.
   recursive logical function keymapp(v) result(yes)
      type(value_type), intent(in) :: v
      type(value_type), allocatable :: elems(:)
      integer(int64) :: k

      yes = .false.
      select case (vtag(v))
      case (V_KEYMAP)
         yes = table%is_live(xhandle(v))
      case (V_LIST)
         call list_elements(v, elems)
         do k = 1, size(elems, kind=int64)
            if (is_marker(elems(k))) cycle
            if (.not. keymapp(elems(k))) return
            yes = .true.
         end do
      end select
   end function keymapp

   !> Parent of `km`, or the null handle.  Port of keymap.c:Fkeymap_parent.
   function keymap_parent(km) result(parent)
      type(handle_type), intent(in) :: km
      type(handle_type) :: parent

      parent = null_handle()
      if (.not. table%is_live(km)) return
      if (table%is_live(keymaps(km%index)%parent)) &
         parent = keymaps(km%index)%parent
   end function keymap_parent

   !> Make `parent` (or the null handle for none) the parent of `km`.
   !> Port of keymap.c:Fset_keymap_parent.
   subroutine set_keymap_parent(km, parent, status)
      type(handle_type), intent(in) :: km, parent
      type(status_type), intent(out) :: status
      type(handle_type) :: p
      integer(int64) :: i

      call resolve_keymap(km, i, status)
      if (.not. status%ok()) return
      if (parent%index /= 0) then
         if (.not. table%is_live(parent)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "keymapp")
            return
         end if
         ! keymap.c:keymap_memberp
         p = parent
         do while (table%is_live(p))
            if (same_handle(p, km)) then
               call set_error(status, FM_GENERIC_ERROR, &
                              "Cyclic keymap inheritance")
               return
            end if
            p = keymaps(p%index)%parent
         end do
      end if
      keymaps(i)%parent = parent
   end subroutine set_keymap_parent

   ! ------------------------------------------------------------------
   ! Events
   ! ------------------------------------------------------------------

   !> "A-C-H-M-S-s-" restricted to the modifiers in `bits`.  The order of
   !> keyboard.c:apply_modifiers_uncached.
   function modifier_prefix(bits) result(s)
      integer(int64), intent(in) :: bits
      character(len=:), allocatable :: s

      s = ""
      if (iand(bits, M_ALT) /= 0) s = s//"A-"
      if (iand(bits, M_CTL) /= 0) s = s//"C-"
      if (iand(bits, M_HYPER) /= 0) s = s//"H-"
      if (iand(bits, M_META) /= 0) s = s//"M-"
      if (iand(bits, M_SHIFT) /= 0) s = s//"S-"
      if (iand(bits, M_SUPER) /= 0) s = s//"s-"
   end function modifier_prefix

   !> Modifier bit for prefix letter `ch`, or 0.
   integer(int64) function modifier_bit(ch)
      character, intent(in) :: ch

      select case (ch)
      case ('A')
         modifier_bit = M_ALT
      case ('C')
         modifier_bit = M_CTL
      case ('H')
         modifier_bit = M_HYPER
      case ('M')
         modifier_bit = M_META
      case ('S')
         modifier_bit = M_SHIFT
      case ('s')
         modifier_bit = M_SUPER
      case default
         modifier_bit = 0
      end select
   end function modifier_bit

   !> Symbol event `v` with its modifier prefixes in canonical order, so
   !> `M-C-x` and `C-M-x` are the same event.  Port of the modifier part
   !> of keyboard.c:reorder_modifiers (click modifiers are not handled).
   function reorder_modifiers(v) result(e)
      type(value_type), intent(in) :: v
      type(value_type) :: e
      character(len=:), allocatable :: name, canon
      integer(int64) :: bits, i

      e = v
      name = symbols(xsymbol(v))%name
      bits = 0
      i = 1
      do while (i + 2 <= len(name, kind=int64))
         if (name(i + 1:i + 1) /= '-') exit
         if (modifier_bit(name(i:i)) == 0) exit
         bits = ior(bits, modifier_bit(name(i:i)))
         i = i + 2
      end do
      if (bits == 0) return
      canon = modifier_prefix(bits)//name(i:)
      if (canon /= name) e = make_symbol_ref(intern(canon))
   end function reorder_modifiers

   !> Event `c` in the form keymaps store: high bits of characters
   !> cleared, symbol modifiers reordered.
   function normalize_event(c) result(e)
      type(value_type), intent(in) :: c
      type(value_type) :: e

      select case (vtag(c))
      case (V_INT)
         e = make_fixnum(iand(xfixnum(c), ior(M_META, M_META - 1)))
      case (V_SYMBOL)
         e = reorder_modifiers(c)
      case default
         e = c
      end select
   end function normalize_event

   ! ------------------------------------------------------------------
   ! Lookup
   ! ------------------------------------------------------------------

   !> Record binding `val` found during a scan.  The "If we found a
   !> binding" part of keymap.c:access_keymap_1.  `stop` when it shadows
   !> everything after it.
   subroutine take_binding(st, val_in, stop)
      type(access_state), intent(inout) :: st
      type(value_type), intent(in) :: val_in
      logical, intent(out) :: stop
      type(value_type) :: val
      type(value_type) :: parts(1)

      stop = .false.
      val = val_in
      ! A t binding is like an explicit nil.
      if (is_t(val)) val = nil()
      if (.not. keymapp(val)) then
         if (.not. st%found .or. nilp(st%retval)) then
            st%retval = val
            st%found = .true.
         end if
         if (.not. nilp(val)) stop = .true.
      else if (.not. st%found .or. nilp(st%retval)) then
         st%retval = val
         st%found = .true.
      else
         parts(1) = val
         call add_parts(st, parts)
      end if
   end subroutine take_binding

   !> Append `parts` to the composite being built, starting one if needed.
   subroutine add_parts(st, parts)
      type(access_state), intent(inout) :: st
      type(value_type), intent(in) :: parts(:)
      type(value_type), allocatable :: elems(:), all(:)
      integer :: n

      if (st%built) then
         call list_elements(st%retval, elems)
      else
         allocate (elems(1))
         elems(1) = st%retval
      end if
      n = size(elems)
      allocate (all(n + size(parts)))
      all(1:n) = elems
      all(n + 1:) = parts
      st%retval = make_list(all)
      st%built = .true.
   end subroutine add_parts

   !> Reached the start of an inherited part, whose bindings are `tail`.
   !> The `EQ (binding, Qkeymap)` case of keymap.c:access_keymap_1.
   recursive subroutine at_parent(st, tail, idx, noinherit, stop)
      type(access_state), intent(inout) :: st
      type(value_type), intent(in) :: tail, idx
      logical, intent(in) :: noinherit
      logical, intent(out) :: stop
      type(value_type) :: entry
      type(value_type), allocatable :: members(:), parts(:)
      logical :: found

      stop = .true.
      if (noinherit) return
      if (st%found) then
         if (nilp(st%retval)) return
         ! A prefix keymap was found: compose it with the parent's.
         call access_1(tail, idx, st%t_ok, .false., entry, found)
         if (found .and. keymapp(entry)) then
            if (vtag(entry) == V_LIST) then
               call list_elements(entry, members)
            else
               allocate (members(1))
               members(1) = entry
            end if
            allocate (parts(size(members) + 1))
            parts(1) = keymap_marker()
            parts(2:) = members
            call add_parts(st, parts)
         end if
         return
      end if
      stop = .false.
   end subroutine at_parent

   !> Look up event `idx_in` in `map` (a keymap or composite).  `found`
   !> is false for Qunbound.  Port of keymap.c:access_keymap_1.
   recursive subroutine access_1(map, idx_in, t_ok, noinherit, val, found)
      type(value_type), intent(in) :: map, idx_in
      logical, intent(in) :: t_ok, noinherit
      type(value_type), intent(out) :: val
      logical, intent(out) :: found
      type(access_state) :: st
      type(value_type) :: idx, m, meta_binding, b, tail
      type(value_type), allocatable :: elems(:)
      type(status_type) :: status
      integer(int64) :: i, j, k, c
      logical :: f, stop

      found = .false.
      st%t_ok = t_ok
      idx = normalize_event(idx_in)
      m = map

      ! The meta -> ESC mapping.
      if (fixnump(idx)) then
         c = xfixnum(idx)
         if (iand(c, M_META) /= 0) then
            call access_1(map, make_fixnum(META_PREFIX_CHAR), t_ok, &
                          noinherit, meta_binding, f)
            if (f .and. keymapp(meta_binding)) then
               m = meta_binding
               idx = make_fixnum(iand(c, not(M_META)))
            else if (t_ok) then
               ! Only a default binding can apply.
               idx = make_symbol_ref(SYM_T)
            else
               found = f .and. nilp(meta_binding)
               return
            end if
         end if
      end if

      select case (vtag(m))
      case (V_KEYMAP)
         call table%resolve(xhandle(m), i, status)
         if (status%ok()) then
            scan: do
               do j = keymaps(i)%n, 1, -1
                  if (value_eq(keymaps(i)%events(j), idx)) then
                     call take_binding(st, keymaps(i)%bindings(j), stop)
                     if (stop) exit scan
                  else if (st%t_ok .and. is_t(keymaps(i)%events(j))) then
                     st%t_binding = keymaps(i)%bindings(j)
                     st%t_found = .true.
                     st%t_ok = .false.
                  end if
               end do
               if (.not. table%is_live(keymaps(i)%parent)) exit scan
               tail = make_keymap_ref(keymaps(i)%parent)
               call at_parent(st, tail, idx, noinherit, stop)
               if (stop) exit scan
               i = keymaps(i)%parent%index
            end do scan
         end if
      case (V_LIST)
         call list_elements(m, elems)
         do k = 1, size(elems, kind=int64)
            if (is_marker(elems(k))) then
               tail = list_tail(m, k)
               call at_parent(st, tail, idx, noinherit, stop)
               if (stop) exit
            else if (keymapp(elems(k))) then
               call access_1(elems(k), idx, st%t_ok, noinherit, b, f)
               if (f) then
                  call take_binding(st, b, stop)
                  if (stop) exit
               end if
            end if
         end do
      end select

      if (st%found) then
         val = st%retval
         found = .true.
      else if (st%t_found) then
         val = st%t_binding
         if (is_t(val)) val = nil()
         found = .true.
      end if
   end subroutine access_1

   !> Binding of the single event `idx` in `map`; nil if none.  With
   !> `t_ok`, bindings of t are defaults.  With `noinherit`, inherited
   !> bindings are ignored.  Port of keymap.c:access_keymap.
   function access_keymap(map, idx, t_ok, noinherit) result(val)
      type(value_type), intent(in) :: map, idx
      logical, intent(in) :: t_ok, noinherit
      type(value_type) :: val
      logical :: found

      call access_1(map, idx, t_ok, noinherit, val, found)
   end function access_keymap

   !> Binding of key sequence `key` in `map` (a keymap or composite).
   !> `val` is nil if undefined, or the fixnum N if the first N events
   !> already reach a non-prefix binding ("too long").  Port of
   !> keymap.c:Flookup_key (lookup_key_1; menu-bar case folding is not
   !> ported).
   subroutine lookup_key(map, key, val, status, accept_default)
      type(value_type), intent(in) :: map
      type(value_type), intent(in) :: key(:)
      type(value_type), intent(out) :: val
      type(status_type), intent(out) :: status
      logical, intent(in), optional :: accept_default
      type(value_type) :: keymap, cmd
      logical :: t_ok
      integer(int64) :: k, n

      t_ok = .false.
      if (present(accept_default)) t_ok = accept_default
      if (.not. (nilp(map) .or. keymapp(map))) then
         call set_error(status, FM_WRONG_TYPE_ARGUMENT, "keymapp")
         return
      end if
      n = size(key, kind=int64)
      if (n == 0) then
         val = map
         return
      end if
      keymap = map
      do k = 1, n
         cmd = access_keymap(keymap, key(k), t_ok, .false.)
         if (k == n) then
            val = cmd
            return
         end if
         if (.not. keymapp(cmd)) then
            val = make_fixnum(k)
            return
         end if
         keymap = cmd
      end do
   end subroutine lookup_key

   ! ------------------------------------------------------------------
   ! Definition
   ! ------------------------------------------------------------------

   !> Append entry (ev, def) to keymap slot `i`.
   subroutine append_entry(i, ev, def)
      integer(int64), intent(in) :: i
      type(value_type), intent(in) :: ev, def
      type(value_type), allocatable :: grown(:)
      integer(int64) :: cap

      if (.not. allocated(keymaps(i)%events)) then
         allocate (keymaps(i)%events(8), keymaps(i)%bindings(8))
      else if (keymaps(i)%n == size(keymaps(i)%events, kind=int64)) then
         cap = 2*keymaps(i)%n
         allocate (grown(cap))
         grown(1:keymaps(i)%n) = keymaps(i)%events
         call move_alloc(grown, keymaps(i)%events)
         allocate (grown(cap))
         grown(1:keymaps(i)%n) = keymaps(i)%bindings
         call move_alloc(grown, keymaps(i)%bindings)
      end if
      keymaps(i)%n = keymaps(i)%n + 1
      keymaps(i)%events(keymaps(i)%n) = ev
      keymaps(i)%bindings(keymaps(i)%n) = def
   end subroutine append_entry

   !> Bind event `c` to `def` in keymap slot `i`, or with `remove` delete
   !> its entry.  Port of keymap.c:store_in_keymap.
   subroutine store_in_keymap(i, c, def, remove, status)
      integer(int64), intent(in) :: i
      type(value_type), intent(in) :: c, def
      logical, intent(in) :: remove
      type(status_type), intent(out) :: status
      type(value_type) :: idx
      integer(int64) :: j, n

      if (is_marker(c)) then
         call set_error(status, FM_GENERIC_ERROR, &
                        "`keymap' is reserved for embedded parent maps")
         return
      end if
      idx = normalize_event(c)
      n = keymaps(i)%n
      do j = 1, n
         if (.not. value_eq(keymaps(i)%events(j), idx)) cycle
         if (remove) then
            keymaps(i)%events(j:n - 1) = keymaps(i)%events(j + 1:n)
            keymaps(i)%bindings(j:n - 1) = keymaps(i)%bindings(j + 1:n)
            keymaps(i)%events(n) = nil()
            keymaps(i)%bindings(n) = nil()
            keymaps(i)%n = n - 1
         else
            keymaps(i)%bindings(j) = def
         end if
         return
      end do
      if (.not. remove) call append_entry(i, idx, def)
   end subroutine store_in_keymap

   !> The keymap to define into when the prefix binding is `cmd`: a
   !> keymap, or the first member of a composite (as store_in_keymap
   !> descends into a leading sub-keymap).
   recursive subroutine prefix_target(cmd, km, ok)
      type(value_type), intent(in) :: cmd
      type(handle_type), intent(out) :: km
      logical, intent(out) :: ok
      type(value_type), allocatable :: elems(:)
      integer(int64) :: k

      ok = .false.
      km = null_handle()
      if (.not. keymapp(cmd)) return
      if (vtag(cmd) == V_KEYMAP) then
         km = xhandle(cmd)
         ok = .true.
         return
      end if
      call list_elements(cmd, elems)
      do k = 1, size(elems, kind=int64)
         if (is_marker(elems(k))) cycle
         call prefix_target(elems(k), km, ok)
         return
      end do
   end subroutine prefix_target

   !> In keymap `km`, bind key sequence `key` to `def`; with `remove`,
   !> delete the binding instead (so an inherited one shows through).
   !> Missing prefix keys get new sparse keymaps.  Port of
   !> keymap.c:Fdefine_key (with define_as_prefix).
   subroutine define_key(km, key, def, status, remove)
      type(handle_type), intent(in) :: km
      type(value_type), intent(in) :: key(:), def
      type(status_type), intent(out) :: status
      logical, intent(in), optional :: remove
      type(handle_type) :: target, prefix
      type(value_type) :: c, cmd
      integer(int64) :: i, pos, n
      logical :: metized, rm, ok
      character(len=:), allocatable :: trailing

      rm = .false.
      if (present(remove)) rm = remove
      call resolve_keymap(km, i, status)
      if (.not. status%ok()) return
      n = size(key, kind=int64)
      if (n == 0) return
      target = km
      metized = .false.
      pos = 0
      do
         c = key(pos + 1)
         if (.not. (fixnump(c) .or. symbolp(c))) then
            call set_error(status, FM_GENERIC_ERROR, &
                           "Key sequence contains invalid event")
            return
         end if
         if (fixnump(c)) then
            if (iand(xfixnum(c), M_META) /= 0 .and. .not. metized) then
               c = make_fixnum(META_PREFIX_CHAR)
               metized = .true.
            else
               c = make_fixnum(iand(xfixnum(c), not(M_META)))
               metized = .false.
               pos = pos + 1
            end if
         else
            metized = .false.
            pos = pos + 1
         end if
         i = target%index
         if (pos == n) then
            call store_in_keymap(i, c, def, rm, status)
            return
         end if
         cmd = access_keymap(make_keymap_ref(target), c, .false., .true.)
         if (nilp(cmd)) then
            ! keymap.c:define_as_prefix
            call make_sparse_keymap(prefix)
            keymaps(prefix%index)%owner = target
            cmd = make_keymap_ref(prefix)
            call store_in_keymap(i, c, cmd, .false., status)
            if (.not. status%ok()) return
         end if
         call prefix_target(cmd, target, ok)
         if (.not. ok) then
            trailing = ""
            if (metized) then
               trailing = " ESC"
               if (pos == 0) trailing = "ESC"
            end if
            call set_error(status, FM_GENERIC_ERROR, "Key sequence "// &
                           key_description(key)// &
                           " starts with non-prefix key "// &
                           key_description(key(1:pos))//trailing)
            return
         end if
      end do
   end subroutine define_key

   !> A copy of `km` whose sub-keymaps (bindings that are keymaps) are
   !> copied too; the parent is shared.  Port of keymap.c:Fcopy_keymap.
   subroutine copy_keymap(km, copy, status)
      type(handle_type), intent(in) :: km
      type(handle_type), intent(out) :: copy
      type(status_type), intent(out) :: status

      call copy_keymap_1(km, 0, copy, status)
   end subroutine copy_keymap

   !> Port of keymap.c:copy_keymap_1.
   recursive subroutine copy_keymap_1(km, depth, copy, status)
      type(handle_type), intent(in) :: km
      integer, intent(in) :: depth
      type(handle_type), intent(out) :: copy
      type(status_type), intent(out) :: status
      type(handle_type) :: sub
      type(value_type) :: b, ev, prompt
      type(status_type) :: ignored
      integer(int64) :: i, k
      logical :: full

      copy = null_handle()
      if (depth > MAX_COPY_DEPTH) then
         call set_error(status, FM_GENERIC_ERROR, &
                        "Possible infinite recursion when copying keymap")
         return
      end if
      call resolve_keymap(km, i, status)
      if (.not. status%ok()) return
      ! Copy fields first: new_keymap may reallocate the registry.
      prompt = keymaps(i)%prompt
      full = keymaps(i)%full
      call new_keymap(copy, full, prompt)
      keymaps(copy%index)%parent = keymaps(i)%parent
      do k = 1, keymaps(i)%n
         ev = keymaps(i)%events(k)
         b = keymaps(i)%bindings(k)
         if (vtag(b) == V_KEYMAP) then
            if (table%is_live(xhandle(b))) then
               call copy_keymap_1(xhandle(b), depth + 1, sub, status)
               if (.not. status%ok()) then
                  call free_keymap(copy, ignored)
                  copy = null_handle()
                  return
               end if
               keymaps(sub%index)%owner = copy
               b = make_keymap_ref(sub)
            end if
         end if
         call append_entry(copy%index, ev, b)
      end do
   end subroutine copy_keymap_1

   ! ------------------------------------------------------------------
   ! Active maps
   ! ------------------------------------------------------------------

   !> The global keymap, made (full) on first use.
   !> Port of keymap.c:Fcurrent_global_map.
   function current_global_map() result(km)
      type(handle_type) :: km

      if (.not. table%is_live(global_map)) call make_keymap(global_map)
      km = global_map
   end function current_global_map

   !> Port of keymap.c:Fuse_global_map.
   subroutine use_global_map(km, status)
      type(handle_type), intent(in) :: km
      type(status_type), intent(out) :: status
      integer(int64) :: i

      call resolve_keymap(km, i, status)
      if (status%ok()) global_map = km
   end subroutine use_global_map

   !> The current buffer's local keymap, or the null handle.
   !> Port of keymap.c:Fcurrent_local_map.
   function current_local_map() result(km)
      type(handle_type) :: km

      km = null_handle()
      if (cur == 0) return
      if (table%is_live(buffers(cur)%keymap)) km = buffers(cur)%keymap
   end function current_local_map

   !> Make `km` (or the null handle for none) the current buffer's local
   !> keymap.  Port of keymap.c:Fuse_local_map.
   subroutine use_local_map(km, status)
      type(handle_type), intent(in) :: km
      type(status_type), intent(out) :: status
      integer(int64) :: i

      if (km%index /= 0) then
         call resolve_keymap(km, i, status)
         if (.not. status%ok()) return
      end if
      if (cur == 0) then
         call set_error(status, FM_GENERIC_ERROR, "No current buffer")
         return
      end if
      buffers(cur)%keymap = km
   end subroutine use_local_map

   !> Binding of `key` in the active maps (local, then global); nil if
   !> none.  Port of keymap.c:Fkey_binding without remapping or
   !> minor-mode and text-property maps.
   subroutine key_binding(key, val, status, accept_default)
      type(value_type), intent(in) :: key(:)
      type(value_type), intent(out) :: val
      type(status_type), intent(out) :: status
      logical, intent(in), optional :: accept_default
      type(value_type) :: maps
      type(handle_type) :: local

      local = current_local_map()
      if (local%index /= 0) then
         maps = list(make_keymap_ref(local), &
                     make_keymap_ref(current_global_map()))
      else
         maps = make_keymap_ref(current_global_map())
      end if
      call lookup_key(maps, key, val, status, accept_default)
      if (fixnump(val)) val = nil()
   end subroutine key_binding

   ! ------------------------------------------------------------------
   ! Descriptions
   ! ------------------------------------------------------------------

   function int_text(n) result(s)
      integer(int64), intent(in) :: n
      character(len=:), allocatable :: s
      character(len=24) :: buf

      write (buf, '(i0)') n
      s = trim(buf)
   end function int_text

   !> Description of character event `ch`, such as "C-x" or "M-<".
   !> Port of keymap.c:push_key_description.
   function push_key_description(ch) result(s)
      integer(int64), intent(in) :: ch
      character(len=:), allocatable :: s
      integer(int64) :: c, c2
      logical :: tab_as_ci
      character(len=MAX_MULTIBYTE_LENGTH) :: buf
      integer(int32) :: nbytes
      type(status_type) :: status

      c = iand(ch, ior(M_META, M_META - 1))
      c2 = iand(c, not(M_ALL))
      if (c2 < 0 .or. c2 > MAX_CHAR) then
         s = "["//int_text(c)//"]"
         return
      end if
      tab_as_ci = c2 == 9 .and. iand(c, M_META) /= 0
      s = ""
      if (iand(c, M_ALT) /= 0) then
         s = s//"A-"
         c = c - M_ALT
      end if
      if (iand(c, M_CTL) /= 0 .or. (c2 < 32 .and. c2 /= 27 .and. c2 /= 9 &
                                    .and. c2 /= 13) .or. tab_as_ci) then
         s = s//"C-"
         c = iand(c, not(M_CTL))
      end if
      if (iand(c, M_HYPER) /= 0) then
         s = s//"H-"
         c = c - M_HYPER
      end if
      if (iand(c, M_META) /= 0) then
         s = s//"M-"
         c = c - M_META
      end if
      if (iand(c, M_SHIFT) /= 0) then
         s = s//"S-"
         c = c - M_SHIFT
      end if
      if (iand(c, M_SUPER) /= 0) then
         s = s//"s-"
         c = c - M_SUPER
      end if
      if (c < 32) then
         if (c == 27) then
            s = s//"ESC"
         else if (tab_as_ci) then
            s = s//"i"
         else if (c == 9) then
            s = s//"TAB"
         else if (c == 13) then
            s = s//"RET"
         else if (c > 0 .and. c <= 26) then
            s = s//achar(c + 96)
         else
            s = s//achar(c + 64)
         end if
      else if (c == 127) then
         s = s//"DEL"
      else if (c == 32) then
         s = s//"SPC"
      else if (c < 128) then
         s = s//achar(c)
      else
         call char_string(int(c, int32), buf, nbytes, status)
         s = s//buf(1:nbytes)
      end if
   end function push_key_description

   !> Description of one event: "C-x", "<f1>", "C-s-<home>".  With
   !> `no_angles`, symbols are given by name only.  Port of
   !> keymap.c:Fsingle_key_description; other values give "".
   function single_key_description(key, no_angles) result(s)
      type(value_type), intent(in) :: key
      logical, intent(in), optional :: no_angles
      character(len=:), allocatable :: s
      character(len=:), allocatable :: name
      integer :: i, n
      logical :: bare

      bare = .false.
      if (present(no_angles)) bare = no_angles
      select case (vtag(key))
      case (V_INT)
         s = push_key_description(xfixnum(key))
      case (V_SYMBOL)
         name = symbols(xsymbol(key))%name
         if (bare) then
            s = name
            return
         end if
         ! Find the modifier prefix, like "C-M-".
         n = len(name)
         i = 1
         do while (i <= n - 3)
            if (name(i + 1:i + 1) /= '-') exit
            if (index("CMSsHA", name(i:i)) == 0) exit
            i = i + 2
         end do
         s = name(1:i - 1)//"<"//name(i:)//">"
      case (V_STRING)
         s = xstring(key)
      case default
         s = ""
      end select
   end function single_key_description

   !> Description of key sequence `keys`, preceded by `prefix`, such as
   !> "C-x <right>"; ESC followed by a character reads as M-.  Port of
   !> keymap.c:Fkey_description.
   function key_description(keys, prefix) result(s)
      type(value_type), intent(in) :: keys(:)
      type(value_type), intent(in), optional :: prefix(:)
      character(len=:), allocatable :: s
      type(value_type), allocatable :: all(:)
      type(value_type) :: key
      integer :: k, np
      logical :: add_meta

      np = 0
      if (present(prefix)) np = size(prefix)
      allocate (all(np + size(keys)))
      if (np > 0) all(1:np) = prefix
      all(np + 1:) = keys
      s = ""
      add_meta = .false.
      do k = 1, size(all)
         key = all(k)
         if (add_meta) then
            if (.not. fixnump(key)) then
               call add_word(s, "ESC")
            else if (xfixnum(key) == META_PREFIX_CHAR .or. &
                     iand(xfixnum(key), M_META) /= 0) then
               call add_word(s, "ESC")
               if (xfixnum(key) == META_PREFIX_CHAR) cycle
            else
               key = make_fixnum(ior(xfixnum(key), M_META))
            end if
            add_meta = .false.
         else if (fixnump(key)) then
            if (xfixnum(key) == META_PREFIX_CHAR) then
               add_meta = .true.
               cycle
            end if
         end if
         call add_word(s, single_key_description(key))
      end do
      if (add_meta) call add_word(s, "ESC")
   contains
      subroutine add_word(s, word)
         character(len=:), allocatable, intent(inout) :: s
         character(len=*), intent(in) :: word

         if (len(s) > 0) then
            s = s//" "//word
         else
            s = word
         end if
      end subroutine add_word
   end function key_description

   ! ------------------------------------------------------------------
   ! kbd
   ! ------------------------------------------------------------------

   logical function is_space(ch)
      character, intent(in) :: ch

      is_space = ch == ' ' .or. ch == achar(9) .or. ch == achar(10) .or. &
                 ch == achar(12)
   end function is_space

   !> Code points of UTF-8 string `s`.
   subroutine decode(s, codes)
      character(len=*), intent(in) :: s
      integer(int64), allocatable, intent(out) :: codes(:)
      integer(int64) :: pos, n
      integer(int32) :: ch, length

      allocate (codes(len(s)))
      n = 0
      pos = 1
      do while (pos <= len(s, kind=int64))
         call string_char_and_length(s, pos, ch, length)
         n = n + 1
         codes(n) = ch
         pos = pos + length
      end do
      codes = codes(1:n)
   end subroutine decode

   !> End of a "<as df>" word starting at `wb` in `keys`, or 0.
   integer(int64) function angle_end(keys, wb) result(q)
      character(len=*), intent(in) :: keys
      integer(int64), intent(in) :: wb
      character :: ch

      q = 0
      if (keys(wb:wb) /= '<' .or. wb + 1 > len(keys, kind=int64)) return
      ch = keys(wb + 1:wb + 1)
      if (ch == '<' .or. ch == '>' .or. is_space(ch)) return
      do q = wb + 2, len(keys, kind=int64)
         ch = keys(q:q)
         if (ch == '>') return
         if (ch /= ' ' .and. is_space(ch)) exit
      end do
      q = 0
   end function angle_end

   !> Whether `word` ends in one of the names NUL RET LFD TAB ESC SPC DEL
   !> at a word boundary.
   logical function ends_in_name(word)
      character(len=*), intent(in) :: word
      character(len=3), parameter :: names(7) = &
                                     ["NUL", "RET", "LFD", "TAB", "ESC", "SPC", "DEL"]
      integer :: n
      character :: before

      ends_in_name = .false.
      n = len(word)
      if (n < 3) return
      if (all(names /= word(n - 2:n))) return
      if (n == 3) then
         ends_in_name = .true.
      else
         before = word(n - 3:n - 3)
         ends_in_name = .not. (('A' <= before .and. before <= 'Z') .or. &
                               ('a' <= before .and. before <= 'z') .or. &
                               ('0' <= before .and. before <= '9') .or. &
                               iachar(before) >= 128)
      end if
   end function ends_in_name

   !> Append `n` copies of `key` to `events(1:nev)`.
   subroutine push_events(events, nev, key, n)
      type(value_type), allocatable, intent(inout) :: events(:)
      integer(int64), intent(inout) :: nev
      type(value_type), intent(in) :: key(:)
      integer(int64), intent(in) :: n
      type(value_type), allocatable :: grown(:)
      integer(int64) :: k, need

      need = nev + n*size(key, kind=int64)
      if (need > size(events, kind=int64)) then
         allocate (grown(max(need, 2*size(events, kind=int64))))
         grown(1:nev) = events(1:nev)
         call move_alloc(grown, events)
      end if
      do k = 1, n
         events(nev + 1:nev + size(key)) = key
         nev = nev + size(key)
      end do
   end subroutine push_events

   !> Fixnum events for `codes`.
   subroutine char_events(codes, key)
      integer(int64), intent(in) :: codes(:)
      type(value_type), allocatable, intent(out) :: key(:)
      integer :: k

      allocate (key(size(codes)))
      do k = 1, size(codes)
         key(k) = make_fixnum(codes(k))
      end do
   end subroutine char_events

   !> The key sequence described by `keys`, such as "C-x C-f" or
   !> "C-M-<return>".  Port of keymap.el:key-parse (the kbd parser).
   subroutine key_parse(keys, events, status)
      character(len=*), intent(in) :: keys
      type(value_type), allocatable, intent(out) :: events(:)
      type(status_type), intent(out) :: status
      character(len=:), allocatable :: word, orig, mods
      type(value_type), allocatable :: key(:), trimmed(:)
      integer(int64), allocatable :: codes(:)
      integer(int64) :: pos, n, wb, we, q, times, nev, bits, prefix, k, c
      logical :: is_vector

      allocate (events(8))
      nev = 0
      n = len(keys, kind=int64)
      pos = 1
      words: do
         do while (pos <= n)
            if (.not. is_space(keys(pos:pos))) exit
            pos = pos + 1
         end do
         if (pos > n) exit words
         wb = pos
         we = wb
         do while (we < n)
            if (is_space(keys(we + 1:we + 1))) exit
            we = we + 1
         end do
         q = angle_end(keys, wb)
         if (q > 0) then
            word = keys(wb:q)
            pos = q + 1
         else
            word = keys(wb:we)
            pos = we + 1
         end if

         ! "N*KEY" repeats KEY.
         times = 1
         k = verify(word, "0123456789")
         if (k > 1 .and. k < len(word)) then
            if (word(k:k) == '*') then
               read (word(1:k - 1), *) times
               word = word(k + 1:)
            end if
         end if

         if (allocated(key)) deallocate (key)
         if (len(word) >= 5) then
            if (word(1:2) == '<<' .and. word(len(word) - 1:) == '>>') then
               ! <<command>>: M-x command RET.
               call decode(word(3:len(word) - 2), codes)
               codes = [M_META + iachar('x'), codes, 13_int64]
               call char_events(codes, key)
               call push_events(events, nev, key, times)
               cycle words
            end if
         end if

         ! [MODS]<NAME>: a function key, unless NAME is RET etc.
         k = 1
         do while (k + 1 <= len(word))
            if (modifier_bit(word(k:k)) == 0 .or. word(k + 1:k + 1) /= '-') exit
            k = k + 2
         end do
         if (len(word) >= k + 2) then
            if (word(k:k) == '<' .and. word(len(word):) == '>') then
               mods = word(1:k - 1)
               word = mods//word(k + 1:len(word) - 1)
               if (.not. ends_in_name(word)) then
                  allocate (key(1))
                  key(1) = make_symbol_ref(intern(word))
                  call push_events(events, nev, key, times)
                  cycle words
               end if
            end if
         end if

         ! REM or ;; comments out the rest of the line.
         if (word == "REM" .or. index(word, ";;") == 1) then
            q = index(keys(pos:), achar(10))
            if (q == 0) exit words
            pos = pos + q - 1
            cycle words
         end if

         orig = word
         prefix = 0
         bits = 0
         do while (len(word) >= 3)
            if (modifier_bit(word(1:1)) == 0 .or. word(2:2) /= '-') exit
            bits = bits + modifier_bit(word(1:1))
            prefix = prefix + 2
            word = word(3:)
         end do
         if (len(word) >= 2) then
            if (word(1:1) == '^') then
               call decode(word(2:), codes)
               if (size(codes) == 1) then
                  bits = bits + M_CTL
                  prefix = prefix + 1
                  word = word(2:)
               end if
            end if
         end if
         select case (word)
         case ("NUL")
            word = achar(0)
         case ("RET")
            word = achar(13)
         case ("LFD")
            word = achar(10)
         case ("TAB")
            word = achar(9)
         case ("ESC")
            word = achar(27)
         case ("SPC")
            word = " "
         case ("DEL")
            word = achar(127)
         end select
         is_vector = .false.
         if (len(word) >= 2) then
            if (word(1:1) == '\' .and. verify(word(2:), "01234567") == 0) then
               is_vector = .true.
               c = 0
               do k = 2, len(word)
                  c = 8*c + iachar(word(k:k)) - 48
               end do
               codes = [c]
            end if
         end if
         if (.not. is_vector) call decode(word, codes)

         if (bits == 0) then
            call char_events(codes, key)
         else if (bits == M_META .and. .not. is_vector .and. &
                  numeric_word(word)) then
            call char_events(codes + bits, key)
         else if (size(codes) /= 1) then
            call set_error(status, FM_GENERIC_ERROR, &
                           orig(1:prefix)//" must prefix a single character, not "// &
                           word)
            return
         else if (iand(bits, M_CTL) /= 0 .and. .not. is_vector .and. &
                  ((codes(1) >= 64 .and. codes(1) <= 95) .or. &
                   (codes(1) >= 97 .and. codes(1) <= 122))) then
            allocate (key(1))
            key(1) = make_fixnum(bits - M_CTL + iand(codes(1), 31_int64))
         else
            allocate (key(1))
            key(1) = make_fixnum(bits + codes(1))
         end if
         call push_events(events, nev, key, times)
      end do words
      allocate (trimmed(nev))
      trimmed = events(1:nev)
      call move_alloc(trimmed, events)
   contains
      !> Whether `w` matches ^-?[0-9]+$.
      logical function numeric_word(w)
         character(len=*), intent(in) :: w
         integer :: s

         s = 1
         if (len(w) > 0) then
            if (w(1:1) == '-') s = 2
         end if
         numeric_word = len(w) >= s .and. verify(w(s:), "0123456789") == 0
      end function numeric_word
   end subroutine key_parse

end module fm_keymap

! test_fm_term_xterm.f90 -- tests for fm_term_xterm: xterm escape
! sequences arriving as function keys.  Own tests (upstream's xterm-tests.el
! covers selection and color queries, not ported).
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

module test_fm_term_xterm
   use, intrinsic :: iso_fortran_env, only: int64
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error, only: status_type
   use fm_lisp, only: value_type, nilp, fixnump, xfixnum
   use fm_lread, only: sym
   use fm_data, only: value_eq
   use fm_keymap, only: key_description
   use fm_keyboard, only: kbd_store_bytes, read_key_sequence, discard_input
   use fm_bindings, only: init_bindings
   use fm_term_xterm, only: terminal_init_xterm
   implicit none(type, external)
   private

   public :: collect_fm_term_xterm

   character, parameter :: ESC = achar(27)

contains

   subroutine collect_fm_term_xterm(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("xterm-function-keys", test_function_keys), &
                  new_unittest("xterm-bound-keys", test_bound_keys) &
                  ]
   end subroutine collect_fm_term_xterm

   !> The key sequence read after typing `bytes`, as kbd text.
   function decoded(bytes) result(desc)
      character(len=*), intent(in) :: bytes
      character(len=:), allocatable :: desc
      type(value_type), allocatable :: keys(:)
      type(value_type) :: binding
      type(status_type) :: st
      logical :: eof

      call discard_input()
      call kbd_store_bytes(bytes)
      call read_key_sequence(keys, binding, eof, st)
      desc = key_description(keys)
   end function decoded

   subroutine test_function_keys(error)
      type(error_type), allocatable, intent(out) :: error

      call init_bindings()
      call terminal_init_xterm()
      call check(error, decoded(ESC//"[A"), "<up>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"OH"), "<home>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"[1;5C"), "C-<right>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"[1;8D"), "C-M-S-<left>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"OP"), "<f1>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"O5S"), "C-<f4>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"[15;2~"), "S-<f5>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"[24;3~"), "M-<f12>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"[6;5~"), "C-<next>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"Ok"), "<kp-add>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"[27;5;9~"), "C-<tab>")
      if (allocated(error)) return
      call check(error, decoded(ESC//"[44;5u"), "C-,")
      if (allocated(error)) return
      call check(error, decoded(ESC//"[27;13;49~"), "C-M-1")
      if (allocated(error)) return
      call check(error, decoded(ESC//"[27;6;33~"), "C-!")
      if (allocated(error)) return
      call check(error, decoded(ESC//"[13;2u"), "S-<return>")
   end subroutine test_function_keys

   !> Decoded keys reach their bindings, and ESC still prefixes Meta.
   subroutine test_bound_keys(error)
      type(error_type), allocatable, intent(out) :: error
      type(value_type), allocatable :: keys(:)
      type(value_type) :: binding
      type(status_type) :: st
      logical :: eof

      call init_bindings()
      call terminal_init_xterm()
      call discard_input()
      call kbd_store_bytes(ESC//"[C"//ESC//"[3~"//ESC//"5")
      call read_key_sequence(keys, binding, eof, st)
      call check(error, value_eq(binding, sym("right-char")), "<right>")
      if (allocated(error)) return
      ! <delete> becomes DEL through function-key-map.
      call read_key_sequence(keys, binding, eof, st)
      call check(error, value_eq(binding, sym("delete-backward-char")) .and. &
                 size(keys) == 1, "<delete>")
      if (allocated(error)) return
      call check(error, fixnump(keys(1)), "DEL event")
      if (allocated(error)) return
      call check(error, xfixnum(keys(1)), 127_int64)
      if (allocated(error)) return
      call read_key_sequence(keys, binding, eof, st)
      call check(error, value_eq(binding, sym("digit-argument")), "M-5")
      if (allocated(error)) return
      call check(error,.not. eof .and. .not. nilp(binding), "bound")
   end subroutine test_bound_keys

end module test_fm_term_xterm

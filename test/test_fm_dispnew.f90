! test_fm_dispnew.f90 -- tests for fm_dispnew and fm_frame: the bytes an
! xterm receives when the desired screen changes.  Own tests: upstream has
! no tests for frame updates.
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

module test_fm_dispnew
   use, intrinsic :: iso_fortran_env, only: int32
   use testdrive, only: new_unittest, unittest_type, error_type, check
   use fm_error, only: status_type
   use fm_terminfo, only: terminfo_type, builtin_xterm
   use fm_termchar, only: tty_take_output
   use fm_term, only: init_tty_entry
   use fm_dispextern, only: glyph_type
   use fm_frame, only: frame_type
   use fm_dispnew
   implicit none(type, external)
   private

   public :: collect_fm_dispnew

   character, parameter :: ESC = achar(27)
   !> What tty_update_end sends to show a hidden cursor.
   character(len=*), parameter :: SHOW = ESC//"[?12l"//ESC//"[?25h"//ESC//"[?12;25h", &
                                  HIDE = ESC//"[?25l"

contains

   subroutine collect_fm_dispnew(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("first-update-redraws", test_first_update), &
                  new_unittest("unchanged-sends-nothing", test_unchanged), &
                  new_unittest("changed-tail", test_changed_tail), &
                  new_unittest("insert-character", test_insert), &
                  new_unittest("delete-character", test_delete), &
                  new_unittest("short-suffix-is-rewritten", test_short_suffix), &
                  new_unittest("shorter-line", test_shorter), &
                  new_unittest("no-char-ins-del", test_no_ins_del), &
                  new_unittest("last-row-first-and-cursor", test_order_cursor), &
                  new_unittest("bottom-right-cell", test_bottom_right) &
                  ]
   end subroutine collect_fm_dispnew

   function g(text) result(glyphs)
      character(len=*), intent(in) :: text
      type(glyph_type) :: glyphs(len(text))
      integer :: k

      do k = 1, len(text)
         glyphs(k)%ch = iachar(text(k:k))
      end do
   end function g

   !> A built-in xterm 80x24 frame already showing `line` in row 0 (the
   !> first update's output is discarded).
   subroutine frame_showing(f, line)
      type(frame_type), intent(out) :: f
      character(len=*), intent(in) :: line
      type(terminfo_type) :: ti
      type(status_type) :: st
      character(len=:), allocatable :: discarded

      call builtin_xterm(ti)
      call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
      call adjust_frame_glyphs(f)
      call set_desired_row(f, 0, g(line))
      call update_frame(f, st)
      discarded = tty_take_output(f%tty)
   end subroutine frame_showing

   !> Output of updating `f` with row 0 changed to `line`.
   function update_row0(f, line) result(out)
      type(frame_type), intent(inout) :: f
      character(len=*), intent(in) :: line
      character(len=:), allocatable :: out
      type(status_type) :: st

      call set_desired_row(f, 0, g(line))
      call update_frame(f, st)
      out = tty_take_output(f%tty)
   end function update_row0

   subroutine test_first_update(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_type) :: f
      type(terminfo_type) :: ti
      type(status_type) :: st

      call builtin_xterm(ti)
      call init_tty_entry(f%tty, ti, "xterm-256color", -1_int32, st)
      call adjust_frame_glyphs(f)
      call check(error, f%garbaged .and. f%current_matrix%nrows == 24, "garbaged")
      if (allocated(error)) return
      call set_desired_row(f, 0, g("hello"))
      call update_frame(f, st)
      call check(error, tty_take_output(f%tty), ESC//"[H"//ESC//"[2J"//HIDE//"hello"// &
                 ESC//"[K"//SHOW)
      if (allocated(error)) return
      call check(error,.not. f%garbaged .and. f%current_matrix%rows(0)%used == 5, "current")
   end subroutine test_first_update

   subroutine test_unchanged(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_type) :: f

      call frame_showing(f, "hello")
      call check(error, update_row0(f, "hello"), "")
   end subroutine test_unchanged

   subroutine test_changed_tail(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_type) :: f

      call frame_showing(f, "hello")
      ! Only "p!" is new; the cursor backs up two columns to write it.
      call check(error, update_row0(f, "help!"), achar(8)//achar(8)//HIDE//"p!"//SHOW)
   end subroutine test_changed_tail

   subroutine test_insert(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_type) :: f
      character(len=*), parameter :: tail = "0123456789abcdefghijklmnopqrst"

      call frame_showing(f, "ab"//tail)
      ! A long unchanged suffix: insert one character instead.
      call check(error, update_row0(f, "abX"//tail), ESC//"[1;3H"//HIDE//ESC//"[1@X"//SHOW)
   end subroutine test_insert

   subroutine test_delete(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_type) :: f
      character(len=*), parameter :: tail = "0123456789abcdefghijklmnopqrst"

      call frame_showing(f, "abX"//tail)
      call check(error, update_row0(f, "ab"//tail), ESC//"[1;3H"//HIDE//ESC//"[1P"//SHOW)
   end subroutine test_delete

   !> A suffix shorter than the cost of insertion is simply rewritten.
   subroutine test_short_suffix(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_type) :: f

      call frame_showing(f, "abcdefgh")
      call check(error, update_row0(f, "abXcdefgh"), ESC//"[1;3H"//HIDE//"Xcdefgh"//SHOW)
   end subroutine test_short_suffix

   subroutine test_shorter(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_type) :: f

      call frame_showing(f, "hello")
      call check(error, update_row0(f, "he"), achar(8)//achar(8)//achar(8)//ESC//"[K")
   end subroutine test_shorter

   !> Without character insertion, each differing run is rewritten.
   subroutine test_no_ins_del(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_type) :: f

      call frame_showing(f, "abcdef")
      f%tty%char_ins_del_ok = .false.
      ! From column 6: CR and one right is cheapest to column 1.
      call check(error, update_row0(f, "aXcdYf"), achar(13)//ESC//"[C"//HIDE//"X"// &
                 ESC//"[1;5HY"//SHOW)
   end subroutine test_no_ins_del

   subroutine test_order_cursor(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_type) :: f
      type(status_type) :: st

      call frame_showing(f, "")
      call set_desired_row(f, 1, g("top"))
      call set_desired_row(f, 23, g("echo"))
      f%cursor_vpos = 1
      f%cursor_hpos = 1
      call update_frame(f, st)
      ! The echo area (last row) first, then the rest; the cursor ends up
      ! at row 1, column 1.
      call check(error, tty_take_output(f%tty), ESC//"[24d"//HIDE//"echo"//ESC//"[K"// &
                 ESC//"[H"//achar(10)//"top"//ESC//"[K"//SHOW//achar(8)//achar(8))
   end subroutine test_order_cursor

   !> A full last row: its bottom-right cell is inserted, not written.
   subroutine test_bottom_right(error)
      type(error_type), allocatable, intent(out) :: error
      type(frame_type) :: f
      type(status_type) :: st
      character(len=80) :: line
      integer :: k

      call frame_showing(f, "")
      do k = 1, 80
         line(k:k) = achar(iachar('a') + mod(k - 1, 26))
      end do
      call set_desired_row(f, 23, g(line))
      call update_frame(f, st)
      call check(error, tty_take_output(f%tty), ESC//"[24d"//HIDE//line(2:80)//achar(13)// &
                 ESC//"[1@"//line(1:1)//achar(8)//SHOW)
   end subroutine test_bottom_right

end module test_fm_dispnew

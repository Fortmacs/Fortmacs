! fm_fileio.f90 -- file names and file input/output.
! Ported from GNU Emacs 31.1 src/fileio.c (the Unix parts in use).
!
! Copyright (C) 1985-1988, 1993-2026 Free Software Foundation, Inc.
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

!> File name manipulation (`expand-file-name` and friends), file
!> predicates, and reading and writing buffer text.
!>
!> File names are strings of bytes, as they are on Linux; Fortmacs does no
!> file-name encoding (`file-name-coding-system` is effectively utf-8).
!> Where upstream returns nil for "no directory", the functions here
!> return "" (a directory name is never empty).  There are no file name
!> handlers (TRAMP and friends).
!>
!> Text is read as UTF-8 with invalid bytes kept as raw-byte characters
!> and written back the same way, so any file survives a visit and save
!> unchanged.  End-of-line conversion and other coding systems are not
!> ported yet.
module fm_fileio
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use fm_error, only: status_type, set_error, FM_FILE_ERROR, FM_FILE_MISSING, &
                        FM_GENERIC_ERROR, FM_WRONG_TYPE_ARGUMENT
    use fm_buffer, only: buffers, cur, BEG, BEG_BYTE, buf_bytes, barf_if_buffer_read_only
    use fm_character, only: str_as_multibyte, str_as_unibyte
    use fm_marker, only: char_to_byte
    use fm_insdel, only: insert
    use fm_editfns, only: point, goto_char, validate_region
    use fm_sysdep, only: file_stat_type, emacs_open_read, emacs_creat, sys_access, sys_rename, &
                         sys_chmod, sys_stat, sys_mkdir, sys_rmdir, sys_unlink, emacs_get_current_dir_name, passwd_home_by_name, &
                         passwd_home_of_user, emacs_strerror, emacs_read, emacs_full_write, &
                         close_fd, emacs_geteuid, emacs_getegid, ENOENT, EISDIR, F_OK, R_OK, &
                         W_OK, X_OK, S_IFMT, S_IFDIR, S_IFREG, S_IFLNK
    implicit none(type, external)
    private

    public :: file_stat_type
    public :: file_name_directory, file_name_nondirectory, file_name_as_directory, &
              directory_file_name, directory_name_p, file_name_absolute_p, expand_file_name, &
              get_homedir, init_fileio, emacs_wd
    public :: file_exists_p, file_readable_p, file_writable_p, file_directory_p, &
              file_accessible_directory_p, file_attributes, file_modes, set_file_modes, &
              rename_file, copy_file, report_file_errno, file_regular_p, file_symlink_p, &
              user_uid, group_gid, make_directory_internal, delete_directory_internal, &
              delete_file_internal
    public :: insert_file_contents, write_region, verify_visited_file_modtime, &
              set_visited_file_modtime

    !> emacs.c:emacs_wd, the directory Emacs started in ("" if unknown).
    character(len=:), allocatable, protected :: emacs_wd

    !> NONEXISTENT_MODTIME_NSECS: the modtime of a file that doesn't exist.
    integer(int32), parameter :: NONEXISTENT_MODTIME_NSECS = -1

    !> Bytes per read(2) in insert_file_contents (READ_BUF_SIZE).
    integer, parameter :: READ_BUF_SIZE = 65536

contains

    ! ------------------------------------------------------------------
    ! File names
    ! ------------------------------------------------------------------

    !> Record the starting directory (emacs.c:main sets emacs_wd).
    subroutine init_fileio()
        emacs_wd = emacs_get_current_dir_name()
    end subroutine init_fileio

    !> The directory part of `filename`, ending in a slash, or "" if it has
    !> none.  Port of fileio.c:file_name_directory.
    pure function file_name_directory(filename) result(dir)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dir

        dir = filename(1:index(filename, "/", back=.true.))
    end function file_name_directory

    !> `filename` without its directory part.
    !> Port of fileio.c:Ffile_name_nondirectory.
    pure function file_name_nondirectory(filename) result(name)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: name

        name = filename(index(filename, "/", back=.true.) + 1:)
    end function file_name_nondirectory

    !> `file` as a directory name: with a trailing slash ("./" for "").
    !> Port of fileio.c:file_name_as_directory.
    pure function file_name_as_directory(file) result(dir)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: dir

        if (len(file) == 0) then
            dir = "./"
        else if (file(len(file):len(file)) == "/") then
            dir = file
        else
            dir = file//"/"
        end if
    end function file_name_as_directory

    !> The file name of directory `directory`: without trailing slashes,
    !> except that "/" and "//" stay and "///..." becomes "/".
    !> Port of fileio.c:directory_file_name.
    pure function directory_file_name(directory) result(file)
        character(len=*), intent(in) :: directory
        character(len=:), allocatable :: file
        integer :: n

        n = len(directory)
        if (.not. (n == 2 .and. directory(1:1) == "/")) then
            do while (n > 1)
                if (directory(n:n) /= "/") exit
                n = n - 1
            end do
        end if
        file = directory(1:n)
    end function directory_file_name

    !> Whether `name` ends in a slash.  Port of fileio.c:Fdirectory_name_p.
    pure logical function directory_name_p(name)
        character(len=*), intent(in) :: name

        directory_name_p = .false.
        if (len(name) > 0) directory_name_p = name(len(name):len(name)) == "/"
    end function directory_name_p

    !> Whether `filename` is absolute: it starts with "/", "~", "~/" or
    !> "~USER" for an existing USER.  Port of fileio.c:file_name_absolute_p.
    logical function file_name_absolute_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: home

        file_name_absolute_p = .false.
        if (len(filename) == 0) return
        if (filename(1:1) == "/") then
            file_name_absolute_p = .true.
        else if (filename(1:1) == "~") then
            if (len(filename) == 1) then
                file_name_absolute_p = .true.
            else if (filename(2:2) == "/") then
                file_name_absolute_p = .true.
            else
                file_name_absolute_p = user_homedir(filename(2:), home)
            end if
        end if
    end function file_name_absolute_p

    !> Home directory of the user named at the start of `name` (up to a
    !> slash); false if there is no such user or the home directory is
    !> not absolute.  Port of fileio.c:user_homedir.
    logical function user_homedir(name, home)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: home
        integer :: length
        logical :: found

        user_homedir = .false.
        home = ""
        length = index(name, "/") - 1
        if (length < 0) length = len(name)
        if (length == 0) return
        call passwd_home_by_name(name(1:length), home, found)
        if (.not. found) return
        if (len(home) == 0) return
        user_homedir = home(1:1) == "/"
    end function user_homedir

    !> The user's home directory: $HOME, or the password entry of $LOGNAME,
    !> $USER or the real user; a relative $HOME is taken relative to the
    !> starting directory.  Port of fileio.c:get_homedir.
    function get_homedir() result(home)
        character(len=:), allocatable :: home
        character(len=*), parameter :: userenv(2) = [character(len=7) :: "LOGNAME", "USER"]
        character(len=:), allocatable :: user
        integer :: length, stat, i
        logical :: found

        call get_environment_variable("HOME", length=length, status=stat)
        if (stat == 0) then
            allocate (character(len=length) :: home)
            if (length > 0) call get_environment_variable("HOME", home)
        else
            found = .false.
            do i = 1, size(userenv)
                call get_environment_variable(trim(userenv(i)), length=length, status=stat)
                if (stat /= 0) cycle
                if (allocated(user)) deallocate (user)
                allocate (character(len=length) :: user)
                if (length > 0) call get_environment_variable(trim(userenv(i)), user)
                call passwd_home_by_name(user, home, found)
                if (found) exit
            end do
            if (.not. found) call passwd_home_of_user(home, found)
            if (.not. found) home = ""
            if (len(home) == 0) return
        end if
        if (len(home) > 0) then
            if (home(1:1) == "/") return
        end if
        if (.not. allocated(emacs_wd)) call init_fileio()
        ! splice_dir_file
        home = file_name_as_directory(emacs_wd)//home
    end function get_homedir

    !> Whether `s` holds a NUL byte (CHECK_STRING_NULL_BYTES).
    pure logical function has_null_byte(s)
        character(len=*), intent(in) :: s

        has_null_byte = index(s, achar(0)) > 0
    end function has_null_byte

    !> `name` made absolute and canonical: relative names are taken
    !> relative to `default_directory` (or the current buffer's
    !> `default-directory`, or "/"), "~" and "~USER" become home
    !> directories, and "." and "FOO/.." components and repeated slashes
    !> are removed (without looking at the file system).
    !> Port of fileio.c:Fexpand_file_name (Unix; no file name handlers).
    recursive subroutine expand_file_name(name, expanded, status, default_directory)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: expanded
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: default_directory
        character(len=:), allocatable :: defdir, nm, newdir, target, tmp, home
        logical :: have_defdir, have_newdir, lose
        integer :: p, length, ulen

        if (has_null_byte(name)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "filenamep")
            return
        end if

        ! The default directory: the argument, else the buffer's.
        have_defdir = present(default_directory)
        if (have_defdir) then
            defdir = default_directory
        else if (cur /= 0) then
            if (allocated(buffers(cur)%directory)) then
                if (has_null_byte(buffers(cur)%directory)) then
                    call set_error(status, FM_WRONG_TYPE_ARGUMENT, "filenamep")
                    return
                end if
                have_defdir = .true.
                if (len(buffers(cur)%directory) > 0 .and. buffers(cur)%directory(1:1) == "/") then
                    defdir = buffers(cur)%directory
                else
                    ! Relative to the invocation directory (unknown here),
                    ! so to the root, as upstream when that isn't absolute.
                    call expand_file_name(buffers(cur)%directory, defdir, status, "/")
                    if (.not. status%ok()) return
                end if
            end if
        end if
        if (.not. have_defdir) defdir = "/"
        if (has_null_byte(defdir)) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "filenamep")
            return
        end if
        ! Make sure the default directory is itself absolute.
        if (len(defdir) == 0) then
            call expand_file_name(defdir, tmp, status)
            if (.not. status%ok()) return
            defdir = tmp
        else if (defdir(1:1) /= "/" .and. defdir /= name) then
            call expand_file_name(defdir, tmp, status)
            if (.not. status%ok()) return
            defdir = tmp
        end if

        nm = name
        ! An absolute name with nothing to simplify is returned as is.
        if (len(nm) > 0) then
            if (nm(1:1) == "/") then
                lose = .false.
                do p = 1, len(nm)
                    if (nm(p:p) /= "/") cycle
                    if (at(nm, p + 1) == "." .and. &
                        (at(nm, p + 2) == "/" .or. at(nm, p + 2) == "" .or. &
                         (at(nm, p + 2) == "." .and. (at(nm, p + 3) == "/" .or. at(nm, p + 3) == "")))) then
                        lose = .true.
                    else if (at(nm, p + 1) == "/" .and. (p /= 1 .or. at(nm, p + 2) == "/")) then
                        lose = .true.
                    end if
                    if (lose) exit
                end do
                if (.not. lose) then
                    expanded = nm
                    return
                end if
            end if
        end if

        ! The prefix: a home directory for ~ or ~USER, else the default
        ! directory for a relative name.
        have_newdir = .false.
        if (at(nm, 1) == "~") then
            if (at(nm, 2) == "/" .or. at(nm, 2) == "") then
                newdir = get_homedir()
                have_newdir = .true.
                nm = nm(2:)
            else if (user_homedir(nm(2:), home)) then
                newdir = home
                have_newdir = .true.
                ulen = index(nm(2:), "/") - 1
                if (ulen < 0) ulen = len(nm) - 1
                nm = nm(2 + ulen:)
            end if
            ! An unknown user leaves the name unchanged.
        end if
        if (.not. have_newdir .and. at(nm, 1) /= "/") then
            newdir = defdir
            have_newdir = .true.
        end if

        target = ""
        if (have_newdir) then
            ! Ignore a trailing slash of newdir, unless it is "/" or "//".
            length = len(newdir)
            do while (length > 1)
                if (newdir(length:length) /= "/") exit
                if (length == 2 .and. newdir(1:1) == "/") exit
                length = length - 1
            end do
            if (len(nm) == 0) then
                target = newdir(1:length)
            else if (nm(1:1) == "/") then
                target = newdir(1:length)
            else
                target = file_name_as_directory(newdir(1:length))
            end if
        end if
        target = target//nm

        expanded = canonicalize(target)
    end subroutine expand_file_name

    !> The byte of `s` at `i`, or "" past the end (C's NUL terminator).
    pure function at(s, i) result(c)
        character(len=*), intent(in) :: s
        integer, intent(in) :: i
        character(len=:), allocatable :: c

        if (i >= 1 .and. i <= len(s)) then
            c = s(i:i)
        else
            c = ""
        end if
    end function at

    !> Remove "//", "/." and "/FOO/.." from `target`: the last step of
    !> Fexpand_file_name.
    pure function canonicalize(target) result(out)
        character(len=*), intent(in) :: target
        character(len=:), allocatable :: out
        character(len=len(target)) :: buf
        integer :: p, o, n

        n = len(target)
        p = 1
        o = 0
        do while (p <= n)
            if (target(p:p) /= "/") then
                o = o + 1
                buf(o:o) = target(p:p)
                p = p + 1
            else if (at(target, p + 1) == "." .and. (at(target, p + 2) == "/" .or. at(target, p + 2) == "")) then
                ! "/." alone stays "/"; otherwise "/." goes.
                if (o == 0 .and. p + 2 > n) then
                    o = 1
                    buf(1:1) = "/"
                end if
                p = p + 2
            else if (at(target, p + 1) == "." .and. at(target, p + 2) == "." .and. o /= 0 .and. &
                     (at(target, p + 3) == "/" .or. at(target, p + 3) == "")) then
                ! Back up over the previous component.
                do while (o > 0)
                    o = o - 1
                    if (buf(o + 1:o + 1) == "/") exit
                end do
                ! Keep the initial slash only if this is the whole name.
                if (o == 0 .and. buf(1:1) == "/" .and. p + 3 > n) o = 1
                p = p + 3
            else if (at(target, p + 1) == "/" .and. (p /= 1 .or. at(target, p + 2) == "/")) then
                ! Collapse repeated slashes, except a leading "//".
                p = p + 1
            else
                o = o + 1
                buf(o:o) = target(p:p)
                p = p + 1
            end if
        end do
        out = buf(1:o)
    end function canonicalize

    ! ------------------------------------------------------------------
    ! File predicates and attributes
    ! ------------------------------------------------------------------

    !> Expand `filename`, quietly giving "" on error (for predicates).
    function expand_quietly(filename) result(absname)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        type(status_type) :: status

        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) absname = ""
    end function expand_quietly

    !> Whether `filename` exists.  Port of fileio.c:Ffile_exists_p.
    logical function file_exists_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname

        absname = expand_quietly(filename)
        file_exists_p = len(absname) > 0
        if (file_exists_p) file_exists_p = sys_access(absname, F_OK) == 0
    end function file_exists_p

    !> Whether `filename` exists and can be read.
    !> Port of fileio.c:Ffile_readable_p.
    logical function file_readable_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname

        absname = expand_quietly(filename)
        file_readable_p = len(absname) > 0
        if (file_readable_p) file_readable_p = sys_access(absname, R_OK) == 0
    end function file_readable_p

    !> Whether `filename` can be written or created.
    !> Port of fileio.c:Ffile_writable_p.
    logical function file_writable_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        integer(int32) :: err

        file_writable_p = .false.
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        err = sys_access(absname, W_OK)
        if (err == 0) then
            file_writable_p = .true.
            return
        end if
        if (err /= ENOENT) return
        file_writable_p = sys_access(file_name_directory(absname), ior(W_OK, X_OK)) == 0
    end function file_writable_p

    !> Whether `filename` names a directory.  Port of
    !> fileio.c:Ffile_directory_p via file_directory_p's portable path
    !> (O_DIRECTORY differs between the two targets).
    logical function file_directory_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        type(file_stat_type) :: st

        file_directory_p = .false.
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        if (file_accessible_directory_p(absname)) then
            file_directory_p = .true.
            return
        end if
        if (sys_stat(absname, st) /= 0) return
        file_directory_p = iand(st%mode, S_IFMT) == S_IFDIR
    end function file_directory_p

    !> Whether `filename` is a directory whose files can be accessed (a
    !> "/." appended to it exists).
    !> Port of fileio.c:file_accessible_directory_p.
    logical function file_accessible_directory_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname

        file_accessible_directory_p = .false.
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        file_accessible_directory_p = sys_access(file_name_as_directory(absname)//".", F_OK) == 0
    end function file_accessible_directory_p

    !> Status of `filename` (following links); `found` is false if it
    !> can't be read.  The parts of dired.c:Ffile_attributes in use.
    subroutine file_attributes(filename, st, found)
        character(len=*), intent(in) :: filename
        type(file_stat_type), intent(out) :: st
        logical, intent(out) :: found
        character(len=:), allocatable :: absname

        found = .false.
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        found = sys_stat(absname, st) == 0
    end subroutine file_attributes

    !> Whether `filename` is a regular file (following links).
    !> Port of fileio.c:Ffile_regular_p.
    logical function file_regular_p(filename)
        character(len=*), intent(in) :: filename
        type(file_stat_type) :: st
        logical :: found

        call file_attributes(filename, st, found)
        file_regular_p = found
        if (found) file_regular_p = iand(st%mode, S_IFMT) == S_IFREG
    end function file_regular_p

    !> Whether `filename` is a symbolic link.  Like
    !> fileio.c:Ffile_symlink_p, but a truth value instead of the target.
    logical function file_symlink_p(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: absname
        type(file_stat_type) :: st

        file_symlink_p = .false.
        absname = expand_quietly(filename)
        if (len(absname) == 0) return
        if (sys_stat(absname, st, nofollow=.true.) /= 0) return
        file_symlink_p = iand(st%mode, S_IFMT) == S_IFLNK
    end function file_symlink_p

    !> The effective user id.  Port of editfns.c:Fuser_uid.
    integer(int32) function user_uid()
        user_uid = emacs_geteuid()
    end function user_uid

    !> The effective group id.  Port of editfns.c:Fgroup_gid.
    integer(int32) function group_gid()
        group_gid = emacs_getegid()
    end function group_gid

    !> Permission bits of `filename`, or -1 if it doesn't exist.
    !> Port of fileio.c:Ffile_modes.
    integer(int32) function file_modes(filename)
        character(len=*), intent(in) :: filename
        type(file_stat_type) :: st
        logical :: found

        call file_attributes(filename, st, found)
        file_modes = -1
        if (found) file_modes = iand(st%mode, int(o'7777', int32))
    end function file_modes

    !> Set the permission bits of `filename`.
    !> Port of fileio.c:Fset_file_modes.
    subroutine set_file_modes(filename, mode, status)
        character(len=*), intent(in) :: filename
        integer(int32), intent(in) :: mode
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname
        integer(int32) :: err

        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        err = sys_chmod(absname, mode)
        if (err /= 0) call report_file_errno("Doing chmod", absname, err, status)
    end subroutine set_file_modes

    !> Rename `file` to `newname`, replacing it if it exists.  Port of
    !> fileio.c:Frename_file with OK-IF-ALREADY-EXISTS non-nil, for files
    !> on one file system.
    subroutine rename_file(file, newname, status)
        character(len=*), intent(in) :: file, newname
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from, to
        integer(int32) :: err

        call expand_file_name(file, from, status)
        if (.not. status%ok()) return
        call expand_file_name(newname, to, status)
        if (.not. status%ok()) return
        err = sys_rename(from, to)
        if (err /= 0) call report_file_errno("Renaming", from//", "//to, err, status)
    end subroutine rename_file

    !> Copy `file` to `newname`, replacing it, with the same permission
    !> bits.  Port of fileio.c:Fcopy_file with OK-IF-ALREADY-EXISTS
    !> non-nil (times, owner and ACLs are not copied yet).
    subroutine copy_file(file, newname, status)
        character(len=*), intent(in) :: file, newname
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: from, to, data
        integer(int32) :: err, fd, mode
        integer(int64) :: written
        type(status_type) :: st

        call expand_file_name(file, from, status)
        if (.not. status%ok()) return
        call expand_file_name(newname, to, status)
        if (.not. status%ok()) return
        call read_file(from, "Opening input file", data, status)
        if (.not. status%ok()) return
        mode = file_modes(from)
        if (mode < 0) mode = int(o'666', int32)
        err = emacs_creat(to, mode, fd)
        if (err /= 0) then
            call report_file_errno("Opening output file", to, err, status)
            return
        end if
        call emacs_full_write(fd, data, written, st, err)
        call close_fd(fd)
        if (.not. st%ok()) then
            call report_file_errno("Write error", to, err, status)
            return
        end if
        ! creat applied the umask; give the copy the original's bits.
        err = sys_chmod(to, mode)
    end subroutine copy_file

    !> Create directory `directory` (its parent must exist).
    !> Port of fileio.c:Fmake_directory_internal.
    subroutine make_directory_internal(directory, status)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir
        integer(int32) :: err

        call expand_file_name(directory, dir, status)
        if (.not. status%ok()) return
        dir = directory_file_name(dir)
        err = sys_mkdir(dir, int(o'777', int32))
        if (err /= 0) call report_file_errno("Creating directory", dir, err, status)
    end subroutine make_directory_internal

    !> Delete the empty directory `directory`.
    !> Port of fileio.c:Fdelete_directory_internal.
    subroutine delete_directory_internal(directory, status)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir
        integer(int32) :: err

        call expand_file_name(directory, dir, status)
        if (.not. status%ok()) return
        dir = directory_file_name(dir)
        err = sys_rmdir(dir)
        if (err /= 0) call report_file_errno("Removing directory", dir, err, status)
    end subroutine delete_directory_internal

    !> Delete file `filename`.  Port of fileio.c:Fdelete_file_internal.
    subroutine delete_file_internal(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname
        integer(int32) :: err

        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        err = sys_unlink(absname)
        if (err /= 0 .and. err /= ENOENT) call report_file_errno("Removing old name", absname, err, status)
    end subroutine delete_file_internal

    !> A file error for `name` from errno `err`: "STRING: <strerror>, NAME",
    !> file-missing for ENOENT.  Port of fileio.c:report_file_errno.
    subroutine report_file_errno(string, name, err, status)
        character(len=*), intent(in) :: string, name
        integer(int32), intent(in) :: err
        type(status_type), intent(inout) :: status
        integer(int32) :: code

        code = FM_FILE_ERROR
        if (err == ENOENT) code = FM_FILE_MISSING
        call set_error(status, code, string//": "//emacs_strerror(err)//", "//name)
    end subroutine report_file_errno

    ! ------------------------------------------------------------------
    ! Reading and writing
    ! ------------------------------------------------------------------

    !> All the bytes of file `absname`.  Errors opening it are reported
    !> with `what` ("Opening input file").
    subroutine read_file(absname, what, data, status, st, opened)
        character(len=*), intent(in) :: absname, what
        character(len=:), allocatable, intent(out) :: data
        type(status_type), intent(out) :: status
        !> The file's status when it was read.
        type(file_stat_type), intent(out), optional :: st
        !> Whether the file could be opened at all.
        logical, intent(out), optional :: opened
        type(file_stat_type) :: fst
        character(len=:), allocatable :: grown
        character(len=READ_BUF_SIZE) :: chunk
        integer(int32) :: fd, err
        integer(int64) :: n, nread

        data = ""
        if (present(opened)) opened = .false.
        err = emacs_open_read(absname, fd)
        if (err /= 0) then
            call report_file_errno(what, absname, err, status)
            return
        end if
        if (present(opened)) opened = .true.
        err = sys_stat(absname, fst)
        if (present(st)) st = fst
        ! POSIX lets read succeed on a directory; refuse it here.
        if (err == 0 .and. iand(fst%mode, S_IFMT) == S_IFDIR) then
            call close_fd(fd)
            call report_file_errno("Read error", absname, EISDIR, status)
            return
        end if
        deallocate (data)
        allocate (character(len=max(int(fst%size, int64), 1_int64)) :: data)
        n = 0
        do
            call emacs_read(fd, chunk, nread, status, err)
            if (.not. status%ok()) then
                call close_fd(fd)
                status = status_type()
                call report_file_errno("Read error", absname, err, status)
                return
            end if
            if (nread == 0) exit
            if (n + nread > len(data, kind=int64)) then
                allocate (character(len=max(2*len(data, kind=int64), n + nread)) :: grown)
                grown(1:n) = data(1:n)
                call move_alloc(grown, data)
            end if
            data(n + 1:n + nread) = chunk(1:nread)
            n = n + nread
        end do
        call close_fd(fd)
        data = data(1:n)
    end subroutine read_file

    !> Insert the contents of file `filename` after point, leaving point
    !> before them.  With `visit`, the buffer (which must be empty) visits
    !> the file: its file name and modification time are recorded and it
    !> is marked unmodified, even if the file doesn't exist (which is then
    !> reported as file-missing).  `inserted` is the number of characters
    !> inserted.  Port of fileio.c:Finsert_file_contents (no BEG, END or
    !> REPLACE; decoding as described in the module documentation).
    subroutine insert_file_contents(filename, visit, inserted, status)
        character(len=*), intent(in) :: filename
        logical, intent(in) :: visit
        integer(int64), intent(out) :: inserted
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname, data, text
        type(file_stat_type) :: st
        integer(int64) :: opoint, nchars
        logical :: opened

        inserted = 0
        if (buffers(cur)%read_only) then
            call barf_if_buffer_read_only(status)
            if (.not. status%ok()) return
        end if
        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        if (visit .and. buffers(cur)%text%z > BEG) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot do file visiting in a non-empty buffer")
            return
        end if

        call read_file(absname, "Opening input file", data, status, st, opened)
        if (.not. status%ok()) then
            ! notfound: a visit still records the file, as nonexistent.
            if (visit .and. .not. opened) then
                buffers(cur)%modtime_known = .true.
                buffers(cur)%modtime_sec = 0
                buffers(cur)%modtime_nsec = NONEXISTENT_MODTIME_NSECS
                buffers(cur)%modtime_size = -1
                buffers(cur)%filename = absname
                buffers(cur)%text%save_modiff = buffers(cur)%text%modiff
            end if
            return
        end if

        if (buffers(cur)%multibyte) then
            call str_as_multibyte(data, text, nchars)
        else
            text = data
            nchars = len(data, kind=int64)
        end if
        opoint = point()
        if (len(text) > 0) then
            call insert(text, status)
            if (.not. status%ok()) return
            call goto_char(opoint)
        end if
        inserted = nchars

        if (visit) then
            buffers(cur)%modtime_known = .true.
            buffers(cur)%modtime_sec = st%mtime_sec
            buffers(cur)%modtime_nsec = st%mtime_nsec
            buffers(cur)%modtime_size = st%size
            buffers(cur)%filename = absname
            buffers(cur)%text%save_modiff = buffers(cur)%text%modiff
        end if
    end subroutine insert_file_contents

    !> Write the text between `start` and `end` (default: the whole
    !> buffer, ignoring narrowing) to file `filename`, replacing it.  With
    !> `visit`, the buffer then visits that file and is unmodified.
    !> `message`, if present, receives "Wrote FILE" for the echo area.
    !> Port of fileio.c:Fwrite_region (no APPEND, LOCKNAME or MUSTBENEW;
    !> no fsync, as write-region-inhibit-fsync defaults to t).
    subroutine write_region(filename, visit, status, start, end, message)
        character(len=*), intent(in) :: filename
        logical, intent(in) :: visit
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start, end
        character(len=:), allocatable, intent(out), optional :: message
        character(len=:), allocatable :: absname, text
        type(file_stat_type) :: st
        type(status_type) :: wst
        integer(int64) :: s, e, written
        integer(int32) :: fd, err

        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        if (present(start) .and. present(end)) then
            s = start
            e = end
            call validate_region(s, e, status)
            if (.not. status%ok()) return
            text = buf_bytes(buffers(cur), char_to_byte(s), char_to_byte(e))
        else
            text = buf_bytes(buffers(cur), BEG_BYTE, buffers(cur)%text%z_byte)
        end if
        if (buffers(cur)%multibyte) text = str_as_unibyte(text)

        err = emacs_creat(absname, int(o'666', int32), fd)
        if (err /= 0) then
            call report_file_errno("Opening output file", absname, err, status)
            return
        end if
        call emacs_full_write(fd, text, written, wst, err)
        call close_fd(fd)
        if (.not. wst%ok()) then
            call report_file_errno("Write error", absname, err, status)
            return
        end if

        if (visit) then
            buffers(cur)%filename = absname
            buffers(cur)%text%save_modiff = buffers(cur)%text%modiff
            if (sys_stat(absname, st) == 0) then
                buffers(cur)%modtime_known = .true.
                buffers(cur)%modtime_sec = st%mtime_sec
                buffers(cur)%modtime_nsec = st%mtime_nsec
                buffers(cur)%modtime_size = st%size
            else
                buffers(cur)%modtime_known = .false.
            end if
        end if
        if (present(message)) message = "Wrote "//absname
    end subroutine write_region

    !> Whether the current buffer's visited file is unchanged since it was
    !> last read or written (true if the buffer visits no file or the time
    !> is unknown).  Port of fileio.c:Fverify_visited_file_modtime.
    logical function verify_visited_file_modtime()
        type(file_stat_type) :: st
        integer(int64) :: sec
        integer(int32) :: nsec

        verify_visited_file_modtime = .true.
        if (.not. allocated(buffers(cur)%filename)) return
        if (.not. buffers(cur)%modtime_known) return
        if (sys_stat(buffers(cur)%filename, st) == 0) then
            sec = st%mtime_sec
            nsec = st%mtime_nsec
        else
            sec = 0
            nsec = NONEXISTENT_MODTIME_NSECS
        end if
        verify_visited_file_modtime = sec == buffers(cur)%modtime_sec .and. &
                                      nsec == buffers(cur)%modtime_nsec .and. &
                                      (buffers(cur)%modtime_size < 0 .or. &
                                       nsec == NONEXISTENT_MODTIME_NSECS .or. &
                                       st%size == buffers(cur)%modtime_size)
    end function verify_visited_file_modtime

    !> Record the visited file's current modification time as the
    !> buffer's.  Port of fileio.c:Fset_visited_file_modtime (no argument).
    subroutine set_visited_file_modtime()
        type(file_stat_type) :: st

        if (.not. allocated(buffers(cur)%filename)) return
        buffers(cur)%modtime_known = .true.
        if (sys_stat(buffers(cur)%filename, st) == 0) then
            buffers(cur)%modtime_sec = st%mtime_sec
            buffers(cur)%modtime_nsec = st%mtime_nsec
            buffers(cur)%modtime_size = st%size
        else
            buffers(cur)%modtime_sec = 0
            buffers(cur)%modtime_nsec = NONEXISTENT_MODTIME_NSECS
            buffers(cur)%modtime_size = -1
        end if
    end subroutine set_visited_file_modtime

end module fm_fileio

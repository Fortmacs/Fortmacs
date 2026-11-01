! files_dir_fm.f90 -- directory listings, file names and deletion, a
! submodule of files_fm.
! Ported from GNU Emacs 31.1 lisp/files.el.
!
! Copyright (C) 1985-1987, 1992-2026 Free Software Foundation, Inc.
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

!> `insert_directory' (running `insert_directory_program' through
!> `execute_process'), `file_truename',
!> `delete_file', `delete_directory', file size strings and
!> `revert_buffer': a submodule because they use callproc_fm and dired_fm,
!> which use files_fm.
submodule(files_fm) files_dir_fm
    use lisp_fm, only: make_list, list_elements, xstring, value_type
    use lread_fm, only: read_string_literal
    use callproc_fm, only: execute_process, shell_file_name, init_callproc
    use dired_fm, only: directory_files
    use search_fm, only: string_match, match_beginning, match_end, looking_at, string_index_to_offset
    use subr_fm, only: match_string
    use textprop_fm, only: put_text_property
    use cmds_fm, only: forward_line
    use editfns_fm, only: line_end_position, char_after, is_bol, is_eol
    use fileio_fm, only: is_file_name_absolute, delete_file_internal, delete_directory_internal, file_system_info, &
                         emacs_wd
    use character_fm, only: string_char_and_length, is_char_byte8
    use casefiddle_fm, only: downcase_string
    use minibuffer_fm, only: read_file_name_completion_ignore_case
    use sysdep_fm, only: emacs_readlink, emacs_mkostemp, close_fd
    use error_fm, only: FM_FILE_MISSING
    use search_fm, only: case_fold_search
    use editfns_fm, only: buffer_substring, delete_region, pos_bol
    use buffer_fm, only: buf_fetch_char, buf_next_char_len
    use marker_fm, only: char_to_byte
    use fileio_fm, only: report_file_errno
    implicit none(type, external)

    !> Two strings (an old directory name and its truename).
    type :: string_pair_type
        character(len=:), allocatable :: a, b
    end type string_pair_type

contains

    ! ------------------------------------------------------------------
    ! Listing directories
    ! ------------------------------------------------------------------

    module subroutine create_temp_file(prefix, file, status)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: pattern
        integer(int32) :: fd, err

        call expand_file_name(prefix//"XXXXXX", pattern, status, temporary_file_directory())
        if (.not. status%ok()) return
        file = pattern
        err = emacs_mkostemp(file, fd)

        if (err /= 0) then
            call report_file_errno("Creating file with prefix", prefix, err, status)
            return
        end if

        call close_fd(fd)
    end subroutine create_temp_file

    !> The shell `insert_directory' runs a wildcard listing with:
    !> `shell_file_name' (callproc.c).  Not a port.
    function shell_name() result(sh)
        character(len=:), allocatable :: sh

        if (.not. allocated(shell_file_name%value)) call init_callproc()
        sh = shell_file_name%value
    end function shell_name

    recursive module subroutine insert_directory(file, switches, status, wildcard, full_directory)
        character(len=*), intent(in) :: file, switches
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: wildcard, full_directory
        character(len=:), allocatable :: errfile, dir, pattern, command, sw, f, saved_dir
        type(value_type) :: result, parts, args
        type(value_type), allocatable :: elems(:), argv(:)
        type(handle_type) :: errbuf, old
        type(file_stat_type) :: st
        type(status_type) :: dst
        integer(int64) :: beginning, inserted
        integer :: k
        logical :: wild, full, found, had_dir

        wild = .false.
        if (present(wildcard)) wild = wildcard
        full = .false.
        if (present(full_directory)) full = full_directory
        beginning = point()
        call create_temp_file("lserr", errfile, status)
        if (.not. status%ok()) return

        if (wild) then
            ! If the wildcard is just in the file part, then run ls in the
            ! directory part of the file pattern using the last component as
            ! argument.  Otherwise, run ls in the longest subdirectory of the
            ! directory part free of wildcards; use the remaining of the file
            ! pattern as argument.
            if (is_insert_directory_wildcard_in_dir(file, dir, pattern)) then
                continue
            else
                if (is_file_name_absolute(file)) then
                    dir = file_name_directory(file)
                else
                    call expand_file_name(file, f, status)
                    if (.not. status%ok()) return
                    dir = file_name_directory(f)
                end if

                pattern = file_name_nondirectory(file)
            end if

            ! NB since switches is passed to the shell, be careful of
            ! malicious values, eg "-l;reboot".
            command = "\"//insert_directory_program//" -d "
            call split_string_and_unquote(switches, parts, status)
            if (.not. status%ok()) return
            call list_elements(parts, elems)

            do k = 1, size(elems)
                if (k > 1) command = command//" "
                command = command//shell_quote_wildcard_pattern(xstring(elems(k)))
            end do

            command = command//" -- "//shell_quote_wildcard_pattern(pattern)
            had_dir = allocated(buffers(current)%directory)
            if (had_dir) saved_dir = buffers(current)%directory
            buffers(current)%directory = dir
            allocate (argv(2))
            argv(1) = value_type(text="-c")
            argv(2) = value_type(text=command)
            args = make_list(argv)
            call execute_process(shell_name(), result, status, destination=value_type(.true.), args=args, error_file=errfile)

            if (had_dir) then
                buffers(current)%directory = saved_dir
            else
                deallocate (buffers(current)%directory)
            end if
        else
            ! SunOS 4.1.3, SVr4 and others need the "." to list the
            ! directory if FILE is a symbolic link.
            sw = switches
            if (.not. full) sw = switches//" -d"
            f = file

            if (index(f, "~") == 1) then
                call expand_file_name(file, f, status)
                if (.not. status%ok()) return
            end if

            if (len(sw) > 0) then
                ! Split the switches at any spaces so we can pass separate
                ! options as separate args.
                call split_string_and_unquote(sw, parts, status)
                if (.not. status%ok()) return
                call list_elements(parts, elems)
            else
                allocate (elems(0))
            end if

            allocate (argv(size(elems) + 2))

            do k = 1, size(elems)
                argv(k) = elems(k)
            end do

            ! Avoid lossage if FILE starts with `-'.
            argv(size(elems) + 1) = value_type(text="--")
            argv(size(elems) + 2) = value_type(text=f)
            args = make_list(argv)
            call execute_process(insert_directory_program, result, status, destination=value_type(.true.), args=args, &
                                 error_file=errfile)
        end if

        if (.not. status%ok()) return
        ! If `ls' emits an error message, copy it to a buffer that will be
        ! displayed when a Dired invocation results in the `ls' error.
        call file_attributes(errfile, st, found)

        if (found .and. st%size > 0) then
            call get_or_make_buffer("*ls error*", errbuf, status)
            if (.not. status%ok()) return
            old = current_buffer()
            call set_buffer(errbuf, status)
            call erase_buffer(status)
            call insert_file_contents(errfile, .false., inserted, status)
            call set_buffer(old, dst)
            if (.not. status%ok()) return
            dired_ls_error_buffer = errbuf
        end if

        dired_ls_error_file = errfile
        call delete_file(errfile, status)
        if (.not. status%ok()) return
        call insert_directory_clean(beginning, switches, status)
        ! The output was decoded as UTF-8 by execute_process.
    end subroutine insert_directory

    recursive module subroutine insert_directory_clean(beginning, switches, status)
        integer(int64), intent(in) :: beginning
        character(len=*), intent(in) :: switches
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: line
        integer(int64) :: end, linebeg, shortage, start, fend, k, num, nums(2), offset, pos, byte, ext
        integer(int32) :: c, n
        logical :: saved, found, have
        integer :: got

        status = status_type()
        if (string_match("--dired\>", switches, status) == 0) return
        if (.not. status%ok()) return
        ! The following overshoots by one line for an empty directory
        ! listed with "--dired", but without "-a" switch, where the ls
        ! output contains a "//DIRED-OPTIONS//" line, but no "//DIRED//"
        ! line.  We take care of that case later.
        shortage = forward_line(-2_int64)
        saved = case_fold_search%value
        case_fold_search%value = .false.

        if (looking_at("//SUBDIRED//", status)) then
            call delete_region(point(), next_line_start(), status)
            shortage = forward_line(-1_int64)
        end if

        if (looking_at("//DIRED//", status)) then
            end = line_end_position()
            linebeg = point()
            call buffer_substring(linebeg, end, line, status)
            ! Read the numeric positions of file names (after "//DIRED// "),
            ! as byte offsets from BEGINNING in the program's output: walk the text
            ! once, counting each character's bytes as the program wrote them
            ! (a raw byte as one).
            pos = beginning
            byte = 0
            k = len("//DIRED//") + 1
            got = 0

            do
                call next_number(line, k, num, have)
                if (.not. have) exit
                got = got + 1
                nums(got) = num
                if (got < 2) cycle
                got = 0
                start = position_of(nums(1))
                fend = position_of(nums(2))
                call char_after(c, found, fend)

                if (found) then
                    ! End is followed by \n or by output of -F.
                    if (index(achar(10)//" /*@%=|", achar(min(c, 127))) > 0 .and. c < 128) then
                        call put_text_property(start, fend, value_type(name="dired_filename"), value_type(.true.), status)
                        if (.not. status%ok()) exit
                    end if
                end if
            end do

            call goto_char(end)
            call goto_char(pos_bol())
            call delete_region(point(), next_line_start(), status)
        else
            ! Take care of the case where the ls output contains a
            ! "//DIRED-OPTIONS//"-line, but no "//DIRED//"-line and we went
            ! one line too far back (see above).
            shortage = forward_line(1_int64)
        end if

        if (looking_at("//DIRED-OPTIONS//", status)) then
            call delete_region(point(), next_line_start(), status)
        end if

        case_fold_search%value = saved

    contains

        !> The position after this line (forward-line 1).
        integer(int64) function next_line_start() result(p)
            integer(int64) :: opoint, sh

            opoint = point()
            sh = forward_line(1_int64)
            p = point()
            call goto_char(opoint)
        end function next_line_start

        !> The buffer position of output byte offset OFFSET (offsets come in
        !> increasing order).
        integer(int64) function position_of(offset_in) result(p)
            integer(int64), intent(in) :: offset_in

            offset = offset_in

            do while (byte < offset .and. pos < buffers(current)%zv)
                c = buf_fetch_char(buffers(current), char_to_byte(pos))
                n = buf_next_char_len(buffers(current), char_to_byte(pos))
                ext = n
                if (is_char_byte8(c)) ext = 1
                byte = byte + ext
                pos = pos + 1
            end do

            p = pos
        end function position_of

        !> The next decimal number in TEXT from K on (skipping spaces).
        subroutine next_number(text, k, value, ok)
            character(len=*), intent(in) :: text
            integer(int64), intent(inout) :: k
            integer(int64), intent(out) :: value
            logical, intent(out) :: ok

            value = 0
            ok = .false.

            do while (k <= len(text))
                if (text(k:k) /= " ") exit
                k = k + 1
            end do

            do while (k <= len(text))
                if (text(k:k) < "0" .or. text(k:k) > "9") exit
                value = value*10 + (iachar(text(k:k)) - iachar("0"))
                ok = .true.
                k = k + 1
            end do
        end subroutine next_number
    end subroutine insert_directory_clean

    module function is_insert_directory_wildcard_in_dir(dir, directory, wildcard) result(found)
        character(len=*), intent(in) :: dir
        character(len=:), allocatable, intent(out), optional :: directory, wildcard
        logical :: found
        type(status_type) :: st

        found = .false.
        if (string_match("[[?*]", file_name_directory(dir), st, inhibit_modify=.true.) == 0) return
        ! Prefer an existing file to wildcards.
        if (file_exists(dir)) return
        if (string_match("\`\([^[?*]*/\)\([^[?*]*[[?*].*\)", dir, st) == 0) return
        found = .true.
        if (present(directory)) directory = match_string(1, dir)
        if (present(wildcard)) wildcard = match_string(2, dir)
    end function is_insert_directory_wildcard_in_dir

    module subroutine split_string_and_unquote(string, parts, status)
        character(len=*), intent(in) :: string
        type(value_type), intent(out) :: parts
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: acc(:), grown(:)
        character(len=:), allocatable :: rest, value
        integer(int64) :: next
        integer :: i, n

        status = status_type()
        allocate (acc(0))
        rest = string

        do
            i = index(rest, '"')

            if (i == 0) then
                ! No quoting: easy.
                call split_whitespace(rest)
                exit
            end if

            if (i > 1) call split_whitespace(rest(1:i - 1))
            call read_string_literal(rest, int(i + 1, int64), value, next, status)
            if (.not. status%ok()) return
            call append(value)
            rest = rest(next:)
        end do

        parts = make_list(acc)

    contains

        !> (split-string STRING "\\s-+" t)
        subroutine split_whitespace(piece)
            character(len=*), intent(in) :: piece
            integer(int64) :: start, b, e
            type(status_type) :: st

            start = 1

            do
                b = string_match("\s-+", piece, st, start)

                if (b == 0) then
                    if (start <= len_chars(piece)) call append(substr(piece, start, len_chars(piece) + 1))
                    exit
                end if

                e = match_end(0, st)
                if (b > start) call append(substr(piece, start, b))
                start = e
            end do
        end subroutine split_whitespace

        !> Characters START up to END (exclusive) of STRING.
        function substr(piece, start, end) result(sub)
            character(len=*), intent(in) :: piece
            integer(int64), intent(in) :: start, end
            character(len=:), allocatable :: sub

            sub = piece(string_index_to_offset(piece, start) + 1:string_index_to_offset(piece, end))
        end function substr

        !> The number of characters of STRING.
        integer(int64) function len_chars(piece) result(m)
            character(len=*), intent(in) :: piece
            integer(int64) :: p
            integer(int32) :: ch, l

            m = 0
            p = 1

            do while (p <= len(piece))
                call string_char_and_length(piece, p, ch, l)
                p = p + l
                m = m + 1
            end do
        end function len_chars

        subroutine append(text)
            character(len=*), intent(in) :: text

            n = size(acc)
            allocate (grown(n + 1))
            grown(1:n) = acc
            grown(n + 1) = value_type(text=text)
            call move_alloc(grown, acc)
        end subroutine append
    end subroutine split_string_and_unquote

    module function shell_quote_wildcard_pattern(pattern) result(quoted)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: quoted
        character(len=*), parameter :: special = " "//achar(9)//achar(10)//";<>&|()`'""#$"
        integer :: k

        quoted = ""

        do k = 1, len(pattern)
            if (index(special, pattern(k:k)) > 0) quoted = quoted//"\"
            quoted = quoted//pattern(k:k)
        end do
    end function shell_quote_wildcard_pattern

    module function is_string_prefix(prefix, string, ignore_case) result(p)
        character(len=*), intent(in) :: prefix, string
        logical, intent(in), optional :: ignore_case
        logical :: p
        logical :: fold

        fold = .false.
        if (present(ignore_case)) fold = ignore_case
        p = .false.
        if (len(prefix) > len(string)) return

        if (fold) then
            p = downcase_string(prefix) == downcase_string(string(1:len(prefix)))
        else
            p = prefix == string(1:len(prefix))
        end if
    end function is_string_prefix

    ! ------------------------------------------------------------------
    ! File names
    ! ------------------------------------------------------------------

    recursive module subroutine file_truename(filename, truename, status)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: truename
        type(status_type), intent(out) :: status
        type(string_pair_type), allocatable :: prev_dirs(:)
        character(len=:), allocatable :: name
        integer :: counter
        type(status_type) :: st

        name = filename

        if (len(filename) == 0 .or. filename == "~") then
            call expand_file_name(filename, name, status)
            if (.not. status%ok()) return
            if (len(name) == 0) name = "/"
        else if (filename(1:1) == "~") then
            if (string_match("~[^/]*/?", filename, st) > 0) then
                call expand_file_name(match_string(0, filename), name, status)
                if (.not. status%ok()) return
                name = name//filename(string_index_to_offset(filename, match_end(0, st)) + 1:)
            end if
        end if

        counter = 100
        allocate (prev_dirs(0))
        call truename_1(name, counter, prev_dirs, truename, status)
    end subroutine file_truename

    !> The loop of file-truename, with COUNTER links left to chase and the
    !> PREV_DIRS truenames already computed.
    !> Port of files.el:file-truename, in part.
    recursive subroutine truename_1(filename_in, counter, prev_dirs, filename, status)
        character(len=*), intent(in) :: filename_in
        integer, intent(inout) :: counter
        type(string_pair_type), allocatable, intent(inout) :: prev_dirs(:)
        character(len=:), allocatable, intent(out) :: filename
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir, dirfile, new, nodir, target
        type(string_pair_type), allocatable :: grown(:)
        logical :: done, cached
        integer :: k
        integer(int32) :: err

        status = status_type()
        filename = filename_in
        done = .false.

        do while (.not. done)
            counter = counter - 1

            if (counter < 0) then
                call set_error(status, FM_GENERIC_ERROR, "Apparent cycle of symbolic links for "//filename)
                return
            end if

            dir = file_name_directory(filename)

            if (len(dir) == 0) then
                dir = "./"

                if (current /= 0) then
                    if (allocated(buffers(current)%directory)) dir = buffers(current)%directory
                end if
            end if

            ! Get the truename of the directory.
            dirfile = directory_file_name(dir)

            ! If these are equal, we have the (or a) root directory.
            if (dir /= dirfile .or. len(dir) /= len(dirfile)) then
                ! If this is the same dir we last got the truename for, save
                ! time--don't recalculate.
                cached = .false.

                do k = 1, size(prev_dirs)
                    if (prev_dirs(k)%a == dir) then
                        dir = prev_dirs(k)%b
                        cached = .true.
                        exit
                    end if
                end do

                if (.not. cached) then
                    call truename_1(dirfile, counter, prev_dirs, new, status)
                    if (.not. status%ok()) return
                    new = file_name_as_directory(new)
                    allocate (grown(size(prev_dirs) + 1))
                    grown(1:size(prev_dirs)) = prev_dirs
                    grown(size(prev_dirs) + 1)%a = dir
                    grown(size(prev_dirs) + 1)%b = new
                    call move_alloc(grown, prev_dirs)
                    dir = new
                end if
            end if

            nodir = file_name_nondirectory(filename)

            if (nodir == "..") then
                filename = directory_file_name(file_name_directory(directory_file_name(dir)))
                done = .true.
            else if (nodir == ".") then
                filename = directory_file_name(dir)
                done = .true.
            else
                ! Put it back on the file name.
                filename = dir//nodir
                ! Is the file name the name of a link?
                target = ""
                err = -1
                if (is_file_symlink(filename)) err = emacs_readlink(filename, target)

                if (err == 0) then
                    ! Yes => chase that link, then start all over since the
                    ! link may point to a directory name that uses links.
                    ! (files--splice-dirname-file)
                    if (index(target, "/") == 1) then
                        filename = target
                    else
                        filename = dir//target
                    end if
                else
                    ! No, we are done!
                    done = .true.
                end if
            end if
        end do
    end subroutine truename_1

    pure module function wildcard_to_regexp(wildcard) result(regexp)
        character(len=*), intent(in) :: wildcard
        character(len=:), allocatable :: regexp
        character(len=:), allocatable :: result
        character :: ch
        integer :: i, j, n

        n = len(wildcard)
        ! Copy the initial run of non-special characters.
        i = scan(wildcard, "[.*+\^$?")

        if (i == 0) then
            regexp = "\`"//wildcard//"\'"
            return
        end if

        result = wildcard(:i - 1)

        do while (i <= n)
            ch = wildcard(i:i)

            if (ch == "[" .and. i + 1 <= n) then
                if (wildcard(i + 1:i + 1) == "]") then
                    result = result//"\["
                    i = i + 1
                    cycle
                end if
            end if

            select case (ch)
            case ("[")
                ! [...] maps to regexp char class.
                i = i + 1

                if (i > n) then
                    result = result//"["
                else if (wildcard(i:i) == "!") then
                    ! [!...] -> [^...]
                    i = i + 1

                    if (i <= n) then
                        if (wildcard(i:i) == "]") then
                            i = i + 1
                            result = result//"[^]"
                        else
                            result = result//"[^"
                        end if
                    else
                        result = result//"[^"
                    end if
                else if (wildcard(i:i) == "^") then
                    ! Found "[^".  Insert a NUL character (which cannot
                    ! happen in a filename) into the character class, so
                    ! that `^' is not the first character after `[', and
                    ! thus non-special in a regexp.
                    i = i + 1
                    result = result//"["//achar(0)//"^"
                else if (wildcard(i:i) == "]") then
                    ! I don't think `]' can appear in a character class in
                    ! a wildcard, but let's be general here.
                    i = i + 1
                    result = result//"[]"
                else
                    result = result//"["
                end if

                ! Copy everything up to next `]'.
                j = 0
                if (i <= n) j = index(wildcard(i:), "]")

                if (j > 0) then
                    j = i + j - 1
                    result = result//wildcard(i:j - 1)
                    i = j - 1
                else
                    if (i <= n) result = result//wildcard(i:)
                    i = n
                end if
            case (".")
                result = result//"\."
            case ("*")
                result = result//"[^"//achar(0)//"]*"
            case ("+")
                result = result//"\+"
            case ("^")
                result = result//"\^"
            case ("$")
                result = result//"\$"
            case ("\")
                ! Probably cannot happen...
                result = result//"\\"
            case ("?")
                result = result//"[^"//achar(0)//"]"
            case default
                result = result//ch
            end select

            i = i + 1
        end do

        ! Shell wildcards should match the entire filename, not its part.
        ! Make the regexp say so.
        regexp = "\`"//result//"\'"
    end function wildcard_to_regexp

    module subroutine file_name_extension(filename, extension, found, period)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: extension
        logical, intent(out) :: found
        logical, intent(in), optional :: period
        character(len=:), allocatable :: file
        integer(int64) :: b
        type(status_type) :: st
        logical :: with_period

        with_period = .false.
        if (present(period)) with_period = period
        file = file_name_sans_versions(file_name_nondirectory(filename))
        found = .false.
        extension = ""
        b = string_match("\.[^.]*\'", file, st)

        if (b > 1) then
            found = .true.

            if (with_period) then
                extension = file(string_index_to_offset(file, b) + 1:)
            else
                extension = file(string_index_to_offset(file, b) + 2:)
            end if
        else if (with_period) then
            found = .true.
        end if
    end subroutine file_name_extension

    module function file_name_sans_extension(filename) result(sans)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: sans
        character(len=:), allocatable :: file
        integer(int64) :: b
        type(status_type) :: st

        file = file_name_sans_versions(file_name_nondirectory(filename))
        b = string_match("\.[^.]*\'", file, st)

        if (b > 1) then
            ! Don't use expand-file-name here; if the directory is
            ! relative, we don't want to expand it.
            sans = file_name_directory(filename)//file(1:string_index_to_offset(file, b))
        else
            sans = filename
        end if
    end function file_name_sans_extension

    module function is_auto_save_file_name(filename) result(p)
        character(len=*), intent(in) :: filename
        logical :: p
        type(status_type) :: st

        p = string_match("\`#.*#\'", filename, st) > 0
    end function is_auto_save_file_name

    module function file_size_human_readable(file_size, flavor, space, unit) result(text)
        real(real64), intent(in) :: file_size
        character(len=*), intent(in), optional :: flavor, space, unit
        character(len=:), allocatable :: text
        character(len=1), parameter :: prefixes(11) = [character(len=1) :: "", "k", "M", "G", "T", "P", "E", "Z", "Y", "R", "Q"]
        character(len=:), allocatable :: fl, prefix, prefixed_unit, num
        real(real64) :: amount, power, frac
        character(len=40) :: buffer
        integer :: k

        fl = ""
        if (present(flavor)) fl = flavor
        power = 1000.0_real64
        if (fl == "" .or. fl == "iec") power = 1024.0_real64
        amount = file_size
        k = 1

        do while (amount >= power .and. k < size(prefixes))
            amount = amount/power
            k = k + 1
        end do

        prefix = trim(prefixes(k))

        if (fl == "iec") then
            if (prefix == "k") then
                prefixed_unit = "K"
            else
                prefixed_unit = prefix
            end if

            if (len(prefix) > 0) prefixed_unit = prefixed_unit//"i"

            if (present(unit)) then
                prefixed_unit = prefixed_unit//unit
            else
                prefixed_unit = prefixed_unit//"B"
            end if
        else
            prefixed_unit = prefix
            if (present(unit)) prefixed_unit = prefixed_unit//unit
        end if

        ! Mimic what GNU "ls -lh" does: if the formatted size will have just
        ! one digit before the decimal and its fractional part is not too
        ! small, then emit one digit after the decimal.
        frac = modulo(amount, 1.0_real64)

        if (amount < 10 .and. frac >= 0.05_real64 .and. frac < 0.95_real64) then
            write (buffer, "(f0.1)") amount
        else
            write (buffer, "(f0.0)") amount
        end if

        num = trim(adjustl(buffer))

        if (len(num) > 0) then
            if (num(len(num):len(num)) == ".") num = num(1:len(num) - 1)
        end if

        if (index(num, ".") == 1) num = "0"//num
        if (len(num) == 0) num = "0"
        text = num
        if (len(prefixed_unit) > 0 .and. present(space)) text = text//space
        text = text//prefixed_unit
    end function file_size_human_readable

    module function byte_count_to_string(size) result(text)
        real(real64), intent(in) :: size
        character(len=:), allocatable :: text

        text = file_size_human_readable(size, "iec", " ")
    end function byte_count_to_string

    module subroutine get_free_disk_space(dir, text, found, status)
        character(len=*), intent(in) :: dir
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: total, free, avail

        text = ""
        call file_system_info(dir, total, free, avail, found, status)
        if (found) text = byte_count_to_string(real(avail, real64))
    end subroutine get_free_disk_space

    ! ------------------------------------------------------------------
    ! Deleting
    ! ------------------------------------------------------------------

    module subroutine delete_file(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname

        status = status_type()

        if (is_file_directory(filename) .and. .not. is_file_symlink(filename)) then
            call set_error(status, FM_FILE_ERROR, "Removing old name: is a directory: "//filename)
            return
        end if

        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        call delete_file_internal(absname, status)
    end subroutine delete_file

    recursive module subroutine delete_directory(directory, status, recursive)
        character(len=*), intent(in) :: directory
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: recursive
        character(len=:), allocatable :: dir, name
        type(value_type) :: files
        type(value_type), allocatable :: elems(:)
        type(status_type) :: st
        logical :: rec, proceed
        integer :: k

        rec = .false.
        if (present(recursive)) rec = recursive
        call expand_file_name(directory, dir, status)
        if (.not. status%ok()) return
        dir = directory_file_name(dir)
        ! Call ourselves recursively if needed.
        proceed = .true.

        if (rec .and. .not. is_file_symlink(dir)) then
            call directory_files(dir, files, st, full=.true.)

            if (.not. st%ok()) then
                if (.not. condition_matches(st%code, FM_FILE_MISSING)) then
                    status = st
                    return
                end if

                proceed = .false.
            else
                call list_elements(files, elems)

                do k = 1, size(elems)
                    name = xstring(elems(k))
                    ! directory-files-no-dot-files-regexp
                    if (file_name_nondirectory(name) == "." .or. file_name_nondirectory(name) == "..") cycle

                    ! This test is equivalent to but more efficient than
                    ! (and (file-directory-p fn) (not (file-symlink-p fn))).
                    if (is_file_directory(name) .and. .not. is_file_symlink(name)) then
                        call delete_directory(name, status, rec)
                        if (.not. status%ok()) return
                    else
                        call delete_file(name, st)

                        if (.not. st%ok()) then
                            if (.not. condition_matches(st%code, FM_FILE_MISSING)) then
                                status = st
                                return
                            end if
                        end if
                    end if
                end do
            end if
        end if

        if (.not. proceed) return
        call delete_directory_internal(dir, st)

        if (.not. st%ok()) then
            if (rec .and. condition_matches(st%code, FM_FILE_MISSING)) return
            status = st
        end if
    end subroutine delete_directory

    ! ------------------------------------------------------------------
    ! Reverting
    ! ------------------------------------------------------------------

    recursive module subroutine revert_buffer(status, ignore_auto, noconfirm, preserve_modes)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: ignore_auto, noconfirm, preserve_modes
        procedure(revert_function_procedure), pointer :: fn
        logical :: ia, nc, saved_in_progress, saved_preserve, restore_read_only, read_only_state

        ia = .false.
        if (present(ignore_auto)) ia = ignore_auto
        nc = .false.
        if (present(noconfirm)) nc = noconfirm
        saved_in_progress = revert_buffer_in_progress
        saved_preserve = revert_buffer_preserve_modes
        revert_buffer_in_progress = .true.
        revert_buffer_preserve_modes = .false.
        if (present(preserve_modes)) revert_buffer_preserve_modes = preserve_modes
        ! revert-buffer-restore-functions: revert-buffer-restore-read-only.
        restore_read_only = buffers(current)%read_only_mode_state_set
        read_only_state = buffers(current)%read_only_mode_state
        fn => revert_buffer_function%current()

        if (associated(fn)) then
            call fn(ia, nc, status)
        else
            call revert_buffer_default(ia, nc, status)
        end if

        if (restore_read_only) then
            buffers(current)%read_only = read_only_state
            buffers(current)%read_only_mode_state = read_only_state
            buffers(current)%read_only_mode_state_set = .true.
        end if

        revert_buffer_in_progress = saved_in_progress
        revert_buffer_preserve_modes = saved_preserve
    end subroutine revert_buffer

    !> Default function for `revert_buffer'.
    !> The arguments IGNORE_AUTO and NOCONFIRM are as described for `revert_buffer'.
    !> Runs the hooks `before_revert_hook' and `after_revert_hook' at the
    !> start and end.
    !> An error condition occurs if the buffer is not associated with
    !> a file.
    !>
    !> Calls `revert_buffer_insert_file_contents__default_function' to
    !> reread the contents of the visited file, with two arguments: the
    !> first is the file name, the second is true if reading an auto-save
    !> file.
    !>
    !> This function handles only buffers that are visiting files.
    !> Non-file buffers need a custom `revert_buffer_function'; see
    !> `revert_buffer'.  Port of files.el:revert-buffer--default (there
    !> are no auto-save files or indirect buffers, so it always reverts
    !> from the visited file).
    recursive subroutine revert_buffer_default(ignore_auto, noconfirm, status)
        use fns_fm, only: read_yes_or_no
        use buffer_fm, only: is_buffer_modified, inhibit_read_only
        use data_fm, only: set
        use lisp_fm, only: value_type
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file_name
        type(hook_type) :: run_hook
        type(status_type) :: st
        logical :: proceed, modified, auto_save_p
        integer :: k

        status = status_type()
        ! No buffer is auto-saved (buffer_auto_save_file_name is never
        ! set), so the auto-save file is never offered, IGNORE_AUTO or not.
        auto_save_p = .false. .and. .not. ignore_auto

        if (.not. allocated(buffers(current)%filename)) then
            call set_error(status, FM_GENERIC_ERROR, "Buffer does not seem to be associated with any file")
            return
        end if

        file_name = buffers(current)%filename
        call is_buffer_modified(current_buffer(), modified, st)
        proceed = noconfirm

        if (.not. proceed .and. .not. modified .and. allocated(revert_without_query)) then
            do k = 1, size(revert_without_query)
                if (string_match(trim(revert_without_query(k)), file_name, st) > 0) then
                    proceed = .true.
                    exit
                end if
            end do
        end if

        if (.not. proceed) then
            if (modified) then
                call read_yes_or_no("Discard edits and reread from "//file_name//"? ", proceed, status)
            else
                call read_yes_or_no("Revert buffer from file "//file_name//"? ", proceed, status)
            end if

            if (.not. status%ok() .or. .not. proceed) return
        end if

        call before_revert_hook%execute(st)
        ! If file was backed up but has changed since,
        ! we should make another backup.
        if (.not. auto_save_p .and. .not. is_visited_file_modtime_current()) buffers(current)%backed_up = .false.
        ! Effectively copy the after_revert_hook status,
        ! since after_find_file will clobber it.
        run_hook = after_revert_hook

        block
            type(value_type) :: saved_inhibit
            saved_inhibit = inhibit_read_only()
            call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
            call revert_buffer_insert_file_contents_default(file_name, auto_save_p, status)
            call set(value_type(name="inhibit_read_only"), saved_inhibit, st)
        end block

        if (.not. status%ok()) return
        call after_find_file(.false., .false., revert_buffer_preserve_modes)
        ! Run after_revert_hook as it was before we reverted.
        call run_hook%execute(st)
    end subroutine revert_buffer_default

    !> Default function for `revert_buffer_insert_file_contents_function'.
    !> The function `revert_buffer__default' calls this.
    !> FILE_NAME is the name of the file.  AUTO_SAVE_P is true if this is
    !> an auto-save file.  Port of
    !> files.el:revert-buffer-insert-file-contents--default-function.
    recursive subroutine revert_buffer_insert_file_contents_default(file_name, auto_save_p, status)
        use fileio_fm, only: NONEXISTENT_MODTIME_NSECS
        use coding_fm, only: coding_system_for_read, buffer_file_coding_system
        use mule_fm, only: buffer_file_coding_system_explicit
        character(len=*), intent(in) :: file_name
        logical, intent(in) :: auto_save_p
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved_coding, explicit_read, explicit_write
        logical :: had_coding, file_number
        integer(int64) :: inserted

        status = status_type()
        ! buffer_file_number: the file existed when last visited or saved.
        file_number = buffers(current)%modtime_known .and. buffers(current)%modtime_nsec /= NONEXISTENT_MODTIME_NSECS

        if (.not. file_exists(file_name)) then
            if (file_number) then
                call set_error(status, FM_GENERIC_ERROR, "File "//file_name//" no longer exists!")
            else
                call set_error(status, FM_GENERIC_ERROR, "Cannot revert nonexistent file "//file_name)
            end if

            return
        end if

        if (.not. is_file_readable(file_name)) then
            if (file_number) then
                call set_error(status, FM_GENERIC_ERROR, "File "//file_name//" no longer readable!")
            else
                call set_error(status, FM_GENERIC_ERROR, "Cannot revert unreadable file "//file_name)
            end if

            return
        end if

        call widen()
        had_coding = allocated(coding_system_for_read%value)
        if (had_coding) saved_coding = coding_system_for_read%value

        if (.not. had_coding) then
            call buffer_file_coding_system_explicit(explicit_read, explicit_write)
            if (len(explicit_read) > 0) coding_system_for_read%value = explicit_read
        end if

        ! This force after_insert_file_set_coding (called from
        ! insert_file_contents) to set buffer_file_coding_system to a
        ! proper value.
        call buffer_file_coding_system%clear_local()
        ! Note that this preserves point in an intelligent way.
        call insert_file_contents(file_name,.not. auto_save_p, inserted, status, replace=.true.)

        if (had_coding) then
            coding_system_for_read%value = saved_coding
        else if (allocated(coding_system_for_read%value)) then
            deallocate (coding_system_for_read%value)
        end if
    end subroutine revert_buffer_insert_file_contents_default

    module procedure file_backup_file_names
    use dired_fm, only: file_name_all_completions
    use fileio_fm, only: is_file_directory, is_file_newer_than_file, expand_file_name, file_name_directory, &
                         file_name_nondirectory
    character(len=:), allocatable :: name, expanded, dir, candidate, tmp
    type(value_type) :: all
    type(value_type), allocatable :: elts(:)
    character(len=:), allocatable :: s1
    type :: name_box_type
        character(len=:), allocatable :: s
    end type name_box_type
    type(name_box_type), allocatable :: kept(:)
    type(status_type) :: st
    integer :: k, n, i, j

    names = value_type()
    ! `make-backup-file-name' will get us the right directory for
    ! ordinary or numeric backups.
    call expand_file_name(filename, expanded, st)
    if (.not. st%ok()) return
    name = file_name_sans_versions(make_backup_file_name(expanded))
    dir = file_name_directory(name)
    if (.not. is_file_directory(dir)) return
    call file_name_all_completions(file_name_nondirectory(name), dir, all, st)
    if (.not. st%ok()) return
    call list_elements(all, elts)
    allocate (kept(size(elts)))
    n = 0

    do k = 1, size(elts)
        candidate = dir//xstring(elts(k))
        if (.not. is_backup_file_name(candidate)) cycle
        s1 = file_name_sans_versions(candidate)
        if (s1 /= name .or. len(s1) /= len(name)) cycle
        n = n + 1
        kept(n)%s = candidate
    end do

    ! (sort ... #'file-newer-than-file-p), stably.
    do i = 2, n
        tmp = kept(i)%s
        j = i - 1

        do while (j >= 1)
            if (.not. is_file_newer_than_file(tmp, kept(j)%s)) exit
            kept(j + 1)%s = kept(j)%s
            j = j - 1
        end do

        kept(j + 1)%s = tmp
    end do

    deallocate (elts)
    allocate (elts(n))

    do k = 1, n
        elts(k) = value_type(text=kept(k)%s)
    end do

    names = make_list(elts)
    end procedure file_backup_file_names

    module procedure file_newest_backup
    type(value_type) :: names
    type(value_type), allocatable :: elts(:)

    call file_backup_file_names(filename, names)
    call list_elements(names, elts)
    found = size(elts) > 0
    if (found) backup = xstring(elts(1))
    end procedure file_newest_backup
end submodule files_dir_fm

! files_mode_mod.f90 -- choosing the major mode, a submodule of files_mod.
! Ported from GNU Emacs 31.1 lisp/files.el.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> `normal_mode', `set_auto_mode' and `auto_modes': a submodule
!> because the modes use files_mod's dependencies.
submodule(files_mod) files_mode_mod
    use character_mod, only: downcase_ascii
    use execute_interactively_mod, only: prefix_argument_type
    use data_mod, only: clear_all_local_variables
    use basic_subprograms_mod, only: delaying_mode_hooks, change_major_mode_after_body_hook, after_change_major_mode_hook
    use execute_interactively_mod, only: find_command, current_prefix_argument, command_type
    use search_mod, only: string_match, case_fold_search, match_beginning, match_end, looking_at
    use editing_functions_mod, only: point_minimum, point_maximum, buffer_substring, point, goto_character
    implicit none(type, external)

    !> An entry of auto-mode-alist.
    type :: auto_mode_type
        character(len=:), allocatable :: regular_expression, mode
        !> An entry with REGULAR_EXPRESSION, MODE and stripping: remove the match from the name and
        !> keep looking (MODE may be empty).
        logical :: strip = .false.
    end type auto_mode_type

    !> auto-mode-alist, first entries first; set up on first use.
    type(auto_mode_type), allocatable :: auto_modes(:)
    integer :: auto_mode_count = 0

    !> `interpreter_modes' (the records with REGULAR_EXPRESSION and MODE) for the modes that exist so
    !> far, in files.el's order.
    character(len=*), parameter :: interpreter_regular_expressions(21) = [character(len=24) :: "python[0-9.]*", "rhino", "gjs", &
                                                                       "nodejs", "node", "[acjkwz]sh", "r?bash2?", "dash", "mksh", &
                                                                  "\(dt\|pd\|w\)ksh", "es", "i?tcsh", "oash", "rc", "rpm", "sh5?", &
                                                                                               "tail", "more", "less", "pg", "make"]
    character(len=*), parameter :: interpreter_modes(21) = [character(len=19) :: "python_mode", "js_mode", "js_mode", &
                                                                 "js_mode", "js_mode", "sh_mode", "sh_mode", "sh_mode", "sh_mode", &
                                                                 "sh_mode", "sh_mode", "sh_mode", "sh_mode", "sh_mode", "sh_mode", &
                                                                    "sh_mode", "text_mode", "text_mode", "text_mode", "text_mode", &
                                                                                                              "makefile_gmake_mode"]

    !> How much of the buffer the -*- line is looked for in.
    integer(int64), parameter :: SPECIFICATION_LIMIT = 4096

contains

    !> Append an entry at the end of auto-mode-alist.  Not a port.
    subroutine append_auto_mode(regular_expression, mode, strip)
        character(len=*), intent(in) :: regular_expression, mode
        logical, intent(in), optional :: strip
        type(auto_mode_type), allocatable :: grown(:)

        if (.not. allocated(auto_modes)) allocate (auto_modes(16))

        if (auto_mode_count == size(auto_modes)) then
            allocate (grown(2*auto_mode_count))
            grown(1:auto_mode_count) = auto_modes
            call move_alloc(grown, auto_modes)
        end if

        auto_mode_count = auto_mode_count + 1
        auto_modes(auto_mode_count)%regular_expression = regular_expression
        auto_modes(auto_mode_count)%mode = mode
        auto_modes(auto_mode_count)%strip = .false.
        if (present(strip)) auto_modes(auto_mode_count)%strip = strip
    end subroutine append_auto_mode

    !> The default auto-mode-alist: files.el's entries for the modes
    !> that exist so far (in its order).
    !> Port of the auto-mode-alist defvar of files.el (in part).
    subroutine initialize_auto_modes()
        if (allocated(auto_modes)) return
        ! (The autoloaded add-to-list entries of python.el and css-mode.el come
        ! first.)
        call append_auto_mode("\(?:\.\(?:p\(?:th\|y[iw]?\)\)\|/\(?:SCons\(?:\(?:crip\|truc\)t\)\)\)\'", "python_mode")
        call append_auto_mode("\.scss\'", "scss_mode")
        call append_auto_mode("\.te?xt\'", "text_mode")
        call append_auto_mode("Imakefile\'", "makefile_imake_mode")
        ! Put this before .mk
        call append_auto_mode("Makeppfile\(?:\.mk\)?\'", "makefile_makepp_mode")
        call append_auto_mode("\.makepp\'", "makefile_makepp_mode")
        ! Might be any make, give GNU the host advantage
        call append_auto_mode("\.mk\'", "makefile_gmake_mode")
        call append_auto_mode("\.make\'", "makefile_gmake_mode")
        call append_auto_mode("[Mm]akefile\'", "makefile_gmake_mode")
        call append_auto_mode("\.am\'", "makefile_automake_mode")
        call append_auto_mode("\.css\'", "css_mode")
        call append_auto_mode("\.[ckz]?sh\'\|\.shar\'\|/\.z?profile\'", "sh_mode")
        call append_auto_mode("\.bash\'", "sh_mode")
        ! Bash builtin 'fc' creates a temp file named "bash-fc.XXXXXX"
        ! to edit shell commands from its history list.
        call append_auto_mode("/bash-fc\.[0-9A-Za-z]\{6\}\'", "sh_mode")
        ! A login sh first attempts to read /etc/profile.
        call append_auto_mode("\`/etc/profile\'", "sh_mode")
        call append_auto_mode("/PKGBUILD\'", "sh_mode")
        call append_auto_mode("\(/\|\`\)\.\(bash_\(profile\|history\|log\(in\|out\)\)\|z?log\(in\|out\)\)\'", &
                              "sh_mode")
        call append_auto_mode("\(/\|\`\)\.\(shrc\|zshrc\|m?kshrc\|bashrc\|t?cshrc\|esrc\)\'", "sh_mode")
        call append_auto_mode("\(/\|\`\)\.\([kz]shenv\|xinitrc\|startxrc\|xsession\)\'", "sh_mode")
        call append_auto_mode("\.m?spec\'", "sh_mode")
        call append_auto_mode("\.article\'", "text_mode")
        call append_auto_mode("\.letter\'", "text_mode")
        ! The Fortran standard does not say anything about file extensions.
        ! .f90 was widely used for F90, now we seem to be trapped into
        ! using a different extension for each language revision.
        ! Anyway, the following extensions are supported by gfortran.
        call append_auto_mode("\.f9[05]\'", "f90_mode")
        call append_auto_mode("\.f0[38]\'", "f90_mode")
        call append_auto_mode("\`/tmp/Re", "text_mode")
        call append_auto_mode("/Message[0-9]*\'", "text_mode")
        call append_auto_mode("\`/tmp/fol/", "text_mode")
        call append_auto_mode("\.sgml?\'", "sgml_mode")
        call append_auto_mode("\.dtd\'", "sgml_mode")
        call append_auto_mode("\.js[mx]?\'", "javascript_mode")
        ! https://en.wikipedia.org/wiki/.har
        call append_auto_mode("\.har\'", "javascript_mode")
        call append_auto_mode("\.json\'", "js_json_mode")
        call append_auto_mode("/crontab\.X*[0-9]+\'", "shell_script_mode")
        call append_auto_mode("\.docbook\'", "sgml_mode")
        call append_auto_mode("/config\.\(?:bat\|log\)\'", "fundamental_mode")
        ! Windows candidates may be opened case sensitively on Unix
        call append_auto_mode("\.\(?:[iI][nN][iI]\|[lL][sS][tT]\|[rR][eE][gG]\|[sS][yY][sS]\)\'", "configuration_mode")
        call append_auto_mode("\.la\'", "configuration_unix_mode")
        call append_auto_mode("\.ppd\'", "configuration_ppd_mode")
        call append_auto_mode("java.+\.conf\'", "configuration_javaprop_mode")
        call append_auto_mode("\.properties\(?:\.[a-zA-Z0-9._-]+\)?\'", "configuration_javaprop_mode")
        call append_auto_mode("\.toml\'", "configuration_toml_mode")
        call append_auto_mode("\.desktop\'", "configuration_desktop_mode")
        ! Dot is excluded from npmrc, because global configs may lack it,
        ! e.g. in /etc/npmrc files.
        call append_auto_mode("npmrc\'", "configuration_npmrc_mode")
        call append_auto_mode("/\.redshift\.conf\'", "configuration_windows_mode")
        call append_auto_mode("\`/etc/\(?:DIR_COLORS\|ethers\|.?fstab\|.*hosts\|lesskey\|login\.?de\(?:fs\|vperm\)\|" &
                              //"magic\|mtab\|pam\.d/.*\|permissions\(?:\.d/.+\)?\|protocols\|rpc\|services\)\'", &
                              "configuration_space_mode")
        call append_auto_mode("\`/etc/\(?:acpid?/.+\|aliases\(?:\.d/.+\)?\|default/.+\|group-?\|hosts\..+\|inittab\|" &
                              //"ksysguarddrc\|opera6rc\|passwd-?\|shadow-?\|sysconfig/.+\)\'", "configuration_mode")
        ! either user's dot-files or under /etc or some such
        call append_auto_mode("/\.?\(?:gitconfig\|gnokiirc\|hgrc\|kde.*rc\|mime\.types\|wgetrc\)\'", "configuration_mode")
        call append_auto_mode("/\.mailmap\'", "configuration_unix_mode")
        ! alas not all ~/.*rc files are like this
        call append_auto_mode("/\.\(?:asound\|enigma\|fetchmail\|gltron\|gtk\|hxplayer\|mairix\|mbsync\|msmtp\|net\|" &
                              //"neverball\|nvidia-settings-\|offlineimap\|qt/.+\|realplayer\|reportbug\|rtorrent\.\|" &
                              //"screen\|scummvm\|sversion\|sylpheed/.+\|xmp\)rc\'", "configuration_mode")
        call append_auto_mode("/\.\(?:gdbtkinit\|grip\|mpdconf\|notmuch-config\|orbital/.+txt\|rhosts\|" &
                              //"tuxracer/options\)\'", "configuration_mode")
        call append_auto_mode("/\.?X\(?:default\|resource\|re\)s\>", "configuration_xdefaults_mode")
        call append_auto_mode("/X11.+app-defaults/\|\.ad\'", "configuration_xdefaults_mode")
        call append_auto_mode("/X11.+locale/.+/Compose\'", "configuration_colon_mode")
        ! this contains everything twice, with space and with colon :-(
        call append_auto_mode("/X11.+locale/compose\.dir\'", "configuration_javaprop_mode")
        ! Get rid of any trailing .n.m and try again.
        ! This is for files saved by cvs-merge that look like .#<file>.<rev>
        ! or .#<file>.<rev>-<rev> or VC's <file>.~<rev>~.
        call append_auto_mode("\.~?[0-9]+\.[0-9][-.0-9]*~?\'", "", strip=.true.)
        call append_auto_mode("\.\(?:orig\|in\|[bB][aA][kK]\)\'", "", strip=.true.)
        ! This should come after "in" stripping (e.g. config.h.in).
        ! *.cf, *.cfg, *.conf, *.config[.local|.de_DE.UTF8|...], */config
        call append_auto_mode("[/.]c\(?:on\)?f\(?:i?g\)?\(?:\.[a-zA-Z0-9._-]+\)?\'", "configuration_mode_maybe")
    end subroutine initialize_auto_modes

    module procedure prepend_auto_mode
    type(auto_mode_type), allocatable :: grown(:)
    integer :: i

    call initialize_auto_modes()

    do i = 1, auto_mode_count
        if (auto_modes(i)%regular_expression == regular_expression .and. auto_modes(i)%mode == mode) return
    end do

    call append_auto_mode(regular_expression, mode)
    ! Move it to the front.
    allocate (grown(auto_mode_count))
    grown(1) = auto_modes(auto_mode_count)
    grown(2:auto_mode_count) = auto_modes(1:auto_mode_count - 1)
    auto_modes(1:auto_mode_count) = grown
    end procedure prepend_auto_mode

    module procedure file_name_sans_versions
    integer :: n, i

    base = name
    n = len(base)
    if (n == 0) return
    if (base(n:n) /= "~") return
    ! NAME.~N~ (numbered backups)
    i = index(base(1:n - 1), ".~", back=.true.)

    if (i > 0) then
        if (i + 2 <= n - 1) then
            if (verify(base(i + 2:n - 1), "0123456789") == 0) then
                base = base(1:i - 1)
                return
            end if
        end if
    end if

    ! NAME~ and NAME.~~
    base = base(1:n - 1)
    end procedure file_name_sans_versions

    !> S without leading and trailing spaces and tabs.  Not a port.
    pure function strip(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: a, b

        a = verify(s, " "//char(9))
        b = verify(s, " "//char(9), back=.true.)

        if (a == 0) then
            t = ""
        else
            t = s(a:b)
        end if
    end function strip

    module procedure auto_mode_specification
    character(len=:), allocatable :: line, rest, specification, lower, value
    integer :: a, newline, i, j, k
    logical :: skip

    mode = ""
    ! (skip-chars-forward " \t\n")
    a = verify(text, " "//char(9)//char(10))
    if (a == 0) return
    newline = index(text(a:), char(10))

    if (newline == 0) then
        line = text(a:)
    else
        line = text(a:a + newline - 2)
    end if

    ! If the file begins with "#!" or '\" (file-auto-mode-skip), look in
    ! the first two lines.
    skip = .false.
    if (len(line) >= 2) skip = line(1:2) == "#!"
    if (len(line) >= 3) skip = skip .or. line(1:3) == "'\"""
    i = index(line, "-*-")

    if (i == 0 .and. skip .and. newline > 0) then
        line = text(a + newline:)
        newline = index(line, char(10))
        if (newline > 0) line = line(1:newline - 1)
        i = index(line, "-*-")
    end if

    if (i == 0) return
    rest = line(i + 3:)
    j = index(rest, "-*-")
    if (j == 0) return
    specification = strip(rest(1:j - 1))

    if (index(specification, ":") == 0) then
        ! Simple -*-MODE-*- case.
        if (len(specification) > 0) mode = underscores(downcase_ascii(specification))//"_mode"
        return
    end if

    ! The first `mode:' entry: at the start or after [ \t;].
    lower = downcase_ascii(specification)
    k = 0

    if (len(lower) >= 5) then
        if (lower(1:5) == "mode:") k = 1
    end if

    if (k == 0) then
        do i = 2, len(lower) - 4
            if (lower(i:i + 4) == "mode:" .and. index(" "//char(9)//";", lower(i - 1:i - 1)) > 0) then
                k = i
                exit
            end if
        end do
    end if

    if (k == 0) return
    value = specification(k + 5:)
    j = index(value, ";")
    if (j > 0) value = value(1:j - 1)
    value = strip(value)
    if (len(value) > 0) mode = underscores(downcase_ascii(value))//"_mode"
    end procedure auto_mode_specification

    !> S with hyphens turned into underscores: a mode named in a file, as
    !> Emacs names it, as Fortmacs names it.  Not a port.
    function underscores(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: i

        t = s

        do i = 1, len(t)
            if (t(i:i) == "-") t(i:i) = "_"
        end do
    end function underscores

    !> Apply MODE.  Port of files.el:set-auto-mode-0.  Returns whether it
    !> was applied (or kept).
    recursive logical function set_auto_mode_0(mode, keep_mode_if_same, status) result(done)
        character(len=*), intent(in) :: mode
        logical, intent(in) :: keep_mode_if_same
        type(status_type), intent(out) :: status
        type(command_type), pointer :: fn
        type(prefix_argument_type) :: saved

        status = status_type()
        done = .false.

        if (keep_mode_if_same .and. allocated(buffers(current)%major_mode)) then
            if (buffers(current)%major_mode == mode) then
                done = .true.
                return
            end if
        end if

        fn => find_command(mode)

        if (.not. associated(fn)) then
            call message(format_message("Ignoring unknown mode `"//mode//"'"))
            return
        end if

        ! The mode function is called with no prefix argument
        ! (eval.c:funcall_nil).
        saved = current_prefix_argument
        current_prefix_argument = prefix_argument_type()
        call fn%execute(status)
        current_prefix_argument = saved
        done = status%ok()
    end function set_auto_mode_0

    !> The mode for the interpreter the first line names (as
    !> `auto_mode_interpreter_regular_expression' finds it) in `interpreter_modes',
    !> or "".  Part of files.el:set-auto-mode.
    !> Port of part of files.el:set-auto-mode.
    recursive subroutine interpreter_mode(mode, status)
        character(len=:), allocatable, intent(out) :: mode
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: interp, name
        integer(int64) :: here, index
        integer :: i
        logical :: found

        mode = ""
        here = point()
        if (.not. allocated(auto_mode_interpreter_regular_expression)) call initialize_auto_mode_interpreter_regular_expression()
        call goto_character(point_minimum())
        found = looking_at(auto_mode_interpreter_regular_expression, status)
        if (found) call buffer_substring(match_beginning(2, status), match_end(2, status), interp, status)
        call goto_character(here)
        if (.not. status%ok() .or. .not. found) return
        ! Map interpreter name to a mode, signaling we're done at the
        ! same time.
        name = file_name_nondirectory(interp)

        do i = 1, size(interpreter_regular_expressions)
            index = string_match("\`"//trim(interpreter_regular_expressions(i))//"\'", name, status, inhibit_modify=.true.)
            if (.not. status%ok()) return

            if (index > 0) then
                mode = trim(interpreter_modes(i))
                return
            end if
        end do
    end subroutine interpreter_mode

    !> The mode of the first auto-mode-alist entry matching NAME, or "".
    !> A (REGULAR_EXPRESSION MODE t) entry removes the match from the name and the
    !> search goes on; its MODE is run if a later entry matches too.
    !> Port of files.el:set-auto-mode--find-matching-alist-entry.
    recursive subroutine find_matching_entry(name, mode, status)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: mode
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rest
        integer :: i
        integer(int64) :: mb
        logical :: done

        status = status_type()
        mode = ""
        rest = name

        do
            i = matching_entry(rest, mb)
            if (i == 0) exit

            if (len(mode) > 0) then
                ! We had already found a mode but in a (REGULAR_EXPRESSION MODE t)
                ! entry, so we still have to run MODE.  Let's do it now.
                done = set_auto_mode_0(mode, .true., status)
                if (.not. status%ok()) return
            end if

            mode = auto_modes(i)%mode
            if (.not. auto_modes(i)%strip) exit
            ! It's a (REGULAR_EXPRESSION MODE t): Keep looking but remember the MODE.
            rest = rest(1:mb - 1)
        end do
    end subroutine find_matching_entry

    !> The index of the first auto-mode-alist entry matching NAME (0 for
    !> none), first case-sensitively, then (auto-mode-case-fold)
    !> case-insensitively, and the 1-based start MB of the match.
    !> Not a port.
    integer function matching_entry(name, mb) result(found)
        character(len=*), intent(in) :: name
        integer(int64), intent(out) :: mb
        logical :: saved
        integer :: pass, i
        type(status_type) :: st

        found = 0
        mb = 0
        saved = case_fold_search%value

        do pass = 1, 2
            case_fold_search = pass == 2

            do i = 1, auto_mode_count
                mb = string_match(auto_modes(i)%regular_expression, name, st, inhibit_modify=.true.)

                if (mb > 0) then
                    found = i
                    case_fold_search = saved
                    return
                end if
            end do
        end do

        mb = 0
        case_fold_search = saved
    end function matching_entry

    module procedure set_auto_mode
    character(len=:), allocatable :: text, mode, name
    logical :: keep

    status = status_type()
    if (current == 0) return
    keep = .false.
    if (present(keep_mode_if_same)) keep = keep_mode_if_same
    ! Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
    call buffer_substring(point_minimum(), min(point_maximum(), point_minimum() + SPECIFICATION_LIMIT), text, status)
    if (.not. status%ok()) return
    mode = auto_mode_specification(text)

    if (len(mode) > 0) then
        if (set_auto_mode_0(mode, keep, status)) return
        if (.not. status%ok()) return
    end if

    ! If we didn't, look for an interpreter specified in the first line.
    ! As a special case, allow for things like "#!/bin/env perl", which
    ! finds the interpreter anywhere in $PATH.
    call interpreter_mode(mode, status)
    if (.not. status%ok()) return

    if (len(mode) > 0) then
        ! If we found an interpreter mode to use, invoke it now.
        if (set_auto_mode_0(mode, keep, status)) return
        if (.not. status%ok()) return
    end if

    ! Next compare the filename against the entries in auto-mode-alist.
    call initialize_auto_modes()
    if (.not. allocated(buffers(current)%filename)) return
    name = file_name_sans_versions(buffers(current)%filename)
    call find_matching_entry(name, mode, status)
    if (.not. status%ok()) return

    if (len(mode) > 0) then
        if (set_auto_mode_0(mode, keep, status)) return
    end if
    end procedure set_auto_mode

    module procedure normal_mode
    type(status_type) :: st

    status = status_type()
    if (current == 0) return
    ! FIND_FILE only matters for file-local variables, not yet supported.
    if (present(find_file)) continue
    call clear_all_local_variables()

    if (.not. delaying_mode_hooks()) then
        call change_major_mode_after_body_hook%execute(status)
        if (.not. status%ok()) return
        call after_change_major_mode_hook%execute(status)
        if (.not. status%ok()) return
    end if

    ! (with-demoted-errors "File mode specification error: %S" ...)
    call set_auto_mode(st)
    if (.not. st%ok()) call message("File mode specification error: "//format_error(st))
    end procedure normal_mode
end submodule files_mode_mod

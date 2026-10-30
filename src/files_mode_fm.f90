! files_mode_fm.f90 -- choosing the major mode, a submodule of files_fm.
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

!> `normal_mode', `set_auto_mode' and `auto_mode_alist': a submodule
!> because the modes use files_fm's dependencies.
submodule(files_fm) files_mode_fm
    use data_fm, only: kill_all_local_variables
    use eval_fm, only: funcall_noargs
    use subr_fm, only: delaying_mode_hooks, change_major_mode_after_body_hook, after_change_major_mode_hook
    use callint_fm, only: fboundp
    use search_fm, only: string_match, case_fold_search, match_beginning, match_end, looking_at
    use editfns_fm, only: point_min, point_max, buffer_substring, point, goto_char
    implicit none(type, external)

    !> An entry of auto-mode-alist.
    type :: auto_mode_type
        character(len=:), allocatable :: regexp, mode
        !> A (REGEXP MODE t) entry: remove the match from the name and
        !> keep looking (MODE may be empty, nil).
        logical :: strip = .false.
    end type auto_mode_type

    !> auto-mode-alist, first entries first; set up on first use.
    type(auto_mode_type), allocatable :: auto_modes(:)
    integer :: n_auto_modes = 0

    !> interpreter-mode-alist: (REGEXP . MODE) for the modes that exist so
    !> far, in files.el's order.
    character(len=*), parameter :: interpreter_regexps(21) = [character(len=24) :: "python[0-9.]*", "rhino", "gjs", &
                                                                       "nodejs", "node", "[acjkwz]sh", "r?bash2?", "dash", "mksh", &
                                                                  "\(dt\|pd\|w\)ksh", "es", "i?tcsh", "oash", "rc", "rpm", "sh5?", &
                                                                                               "tail", "more", "less", "pg", "make"]
    character(len=*), parameter :: interpreter_modes(21) = [character(len=19) :: "python_mode", "js_mode", "js_mode", &
                                                                 "js_mode", "js_mode", "sh_mode", "sh_mode", "sh_mode", "sh_mode", &
                                                                 "sh_mode", "sh_mode", "sh_mode", "sh_mode", "sh_mode", "sh_mode", &
                                                                    "sh_mode", "text_mode", "text_mode", "text_mode", "text_mode", &
                                                                                                              "makefile_gmake_mode"]

    !> How much of the buffer the -*- line is looked for in.
    integer(int64), parameter :: SPEC_LIMIT = 4096

contains

    !> Append an entry at the end of auto-mode-alist.
    subroutine append_auto_mode(regexp, mode, strip)
        character(len=*), intent(in) :: regexp, mode
        logical, intent(in), optional :: strip
        type(auto_mode_type), allocatable :: grown(:)

        if (.not. allocated(auto_modes)) allocate (auto_modes(16))

        if (n_auto_modes == size(auto_modes)) then
            allocate (grown(2*n_auto_modes))
            grown(1:n_auto_modes) = auto_modes
            call move_alloc(grown, auto_modes)
        end if

        n_auto_modes = n_auto_modes + 1
        auto_modes(n_auto_modes)%regexp = regexp
        auto_modes(n_auto_modes)%mode = mode
        auto_modes(n_auto_modes)%strip = .false.
        if (present(strip)) auto_modes(n_auto_modes)%strip = strip
    end subroutine append_auto_mode

    !> The default auto-mode-alist: files.el's entries for the modes
    !> that exist so far (in its order).
    subroutine init_auto_modes()
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
        call append_auto_mode("\.\(?:[iI][nN][iI]\|[lL][sS][tT]\|[rR][eE][gG]\|[sS][yY][sS]\)\'", "conf_mode")
        call append_auto_mode("\.la\'", "conf_unix_mode")
        call append_auto_mode("\.ppd\'", "conf_ppd_mode")
        call append_auto_mode("java.+\.conf\'", "conf_javaprop_mode")
        call append_auto_mode("\.properties\(?:\.[a-zA-Z0-9._-]+\)?\'", "conf_javaprop_mode")
        call append_auto_mode("\.toml\'", "conf_toml_mode")
        call append_auto_mode("\.desktop\'", "conf_desktop_mode")
        ! Dot is excluded from npmrc, because global configs may lack it,
        ! e.g. in /etc/npmrc files.
        call append_auto_mode("npmrc\'", "conf_npmrc_mode")
        call append_auto_mode("/\.redshift\.conf\'", "conf_windows_mode")
        call append_auto_mode("\`/etc/\(?:DIR_COLORS\|ethers\|.?fstab\|.*hosts\|lesskey\|login\.?de\(?:fs\|vperm\)\|" &
                              //"magic\|mtab\|pam\.d/.*\|permissions\(?:\.d/.+\)?\|protocols\|rpc\|services\)\'", &
                              "conf_space_mode")
        call append_auto_mode("\`/etc/\(?:acpid?/.+\|aliases\(?:\.d/.+\)?\|default/.+\|group-?\|hosts\..+\|inittab\|" &
                              //"ksysguarddrc\|opera6rc\|passwd-?\|shadow-?\|sysconfig/.+\)\'", "conf_mode")
        ! either user's dot-files or under /etc or some such
        call append_auto_mode("/\.?\(?:gitconfig\|gnokiirc\|hgrc\|kde.*rc\|mime\.types\|wgetrc\)\'", "conf_mode")
        call append_auto_mode("/\.mailmap\'", "conf_unix_mode")
        ! alas not all ~/.*rc files are like this
        call append_auto_mode("/\.\(?:asound\|enigma\|fetchmail\|gltron\|gtk\|hxplayer\|mairix\|mbsync\|msmtp\|net\|" &
                              //"neverball\|nvidia-settings-\|offlineimap\|qt/.+\|realplayer\|reportbug\|rtorrent\.\|" &
                              //"screen\|scummvm\|sversion\|sylpheed/.+\|xmp\)rc\'", "conf_mode")
        call append_auto_mode("/\.\(?:gdbtkinit\|grip\|mpdconf\|notmuch-config\|orbital/.+txt\|rhosts\|" &
                              //"tuxracer/options\)\'", "conf_mode")
        call append_auto_mode("/\.?X\(?:default\|resource\|re\)s\>", "conf_xdefaults_mode")
        call append_auto_mode("/X11.+app-defaults/\|\.ad\'", "conf_xdefaults_mode")
        call append_auto_mode("/X11.+locale/.+/Compose\'", "conf_colon_mode")
        ! this contains everything twice, with space and with colon :-(
        call append_auto_mode("/X11.+locale/compose\.dir\'", "conf_javaprop_mode")
        ! Get rid of any trailing .n.m and try again.
        ! This is for files saved by cvs-merge that look like .#<file>.<rev>
        ! or .#<file>.<rev>-<rev> or VC's <file>.~<rev>~.
        call append_auto_mode("\.~?[0-9]+\.[0-9][-.0-9]*~?\'", "", strip=.true.)
        call append_auto_mode("\.\(?:orig\|in\|[bB][aA][kK]\)\'", "", strip=.true.)
        ! This should come after "in" stripping (e.g. config.h.in).
        ! *.cf, *.cfg, *.conf, *.config[.local|.de_DE.UTF8|...], */config
        call append_auto_mode("[/.]c\(?:on\)?f\(?:i?g\)?\(?:\.[a-zA-Z0-9._-]+\)?\'", "conf_mode_maybe")
    end subroutine init_auto_modes

    module procedure add_auto_mode
    type(auto_mode_type), allocatable :: grown(:)
    integer :: i

    call init_auto_modes()

    do i = 1, n_auto_modes
        if (auto_modes(i)%regexp == regexp .and. auto_modes(i)%mode == mode) return
    end do

    call append_auto_mode(regexp, mode)
    ! Move it to the front.
    allocate (grown(n_auto_modes))
    grown(1) = auto_modes(n_auto_modes)
    grown(2:n_auto_modes) = auto_modes(1:n_auto_modes - 1)
    auto_modes(1:n_auto_modes) = grown
    end procedure add_auto_mode

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

    pure function downcase(s) result(d)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: d
        integer :: i

        d = s

        do i = 1, len(s)
            if (s(i:i) >= "A" .and. s(i:i) <= "Z") d(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function downcase

    !> S without leading and trailing spaces and tabs.
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

    module procedure auto_mode_spec
    character(len=:), allocatable :: line, rest, spec, lower, value
    integer :: a, nl, i, j, k
    logical :: skip

    mode = ""
    ! (skip-chars-forward " \t\n")
    a = verify(text, " "//char(9)//char(10))
    if (a == 0) return
    nl = index(text(a:), char(10))

    if (nl == 0) then
        line = text(a:)
    else
        line = text(a:a + nl - 2)
    end if

    ! If the file begins with "#!" or '\" (file-auto-mode-skip), look in
    ! the first two lines.
    skip = .false.
    if (len(line) >= 2) skip = line(1:2) == "#!"
    if (len(line) >= 3) skip = skip .or. line(1:3) == "'\"""
    i = index(line, "-*-")

    if (i == 0 .and. skip .and. nl > 0) then
        line = text(a + nl:)
        nl = index(line, char(10))
        if (nl > 0) line = line(1:nl - 1)
        i = index(line, "-*-")
    end if

    if (i == 0) return
    rest = line(i + 3:)
    j = index(rest, "-*-")
    if (j == 0) return
    spec = strip(rest(1:j - 1))

    if (index(spec, ":") == 0) then
        ! Simple -*-MODE-*- case.
        if (len(spec) > 0) mode = underscores(downcase(spec))//"_mode"
        return
    end if

    ! The first `mode:' entry: at the start or after [ \t;].
    lower = downcase(spec)
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
    value = spec(k + 5:)
    j = index(value, ";")
    if (j > 0) value = value(1:j - 1)
    value = strip(value)
    if (len(value) > 0) mode = underscores(downcase(value))//"_mode"
    end procedure auto_mode_spec

    !> S with hyphens turned into underscores: a mode named in a file, as
    !> Emacs names it, as Fortmacs names it.
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

        status = status_type()
        done = .false.

        if (keep_mode_if_same .and. allocated(buffers(cur)%major_mode)) then
            if (buffers(cur)%major_mode == mode) then
                done = .true.
                return
            end if
        end if

        if (.not. fboundp(sym(mode))) then
            call message(format_message("Ignoring unknown mode `"//mode//"'"))
            return
        end if

        call funcall_noargs(sym(mode), status)
        done = status%ok()
    end function set_auto_mode_0

    !> The mode for the interpreter the first line names (as
    !> `auto_mode_interpreter_regexp' finds it) in interpreter-mode-alist,
    !> or "".  Part of files.el:set-auto-mode.
    recursive subroutine interpreter_mode(mode, status)
        character(len=:), allocatable, intent(out) :: mode
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: interp, name
        integer(int64) :: here, idx
        integer :: i
        logical :: found

        mode = ""
        here = point()
        if (.not. allocated(auto_mode_interpreter_regexp)) call init_auto_mode_interpreter_regexp()
        call goto_char(point_min())
        found = looking_at(auto_mode_interpreter_regexp, status)
        if (found) call buffer_substring(match_beginning(2, status), match_end(2, status), interp, status)
        call goto_char(here)
        if (.not. status%ok() .or. .not. found) return
        ! Map interpreter name to a mode, signaling we're done at the
        ! same time.
        name = file_name_nondirectory(interp)

        do i = 1, size(interpreter_regexps)
            idx = string_match("\`"//trim(interpreter_regexps(i))//"\'", name, status, inhibit_modify=.true.)
            if (.not. status%ok()) return

            if (idx > 0) then
                mode = trim(interpreter_modes(i))
                return
            end if
        end do
    end subroutine interpreter_mode

    !> The mode of the first auto-mode-alist entry matching NAME, or "".
    !> A (REGEXP MODE t) entry removes the match from the name and the
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
                ! We had already found a mode but in a (REGEXP MODE t)
                ! entry, so we still have to run MODE.  Let's do it now.
                done = set_auto_mode_0(mode, .true., status)
                if (.not. status%ok()) return
            end if

            mode = auto_modes(i)%mode
            if (.not. auto_modes(i)%strip) exit
            ! It's a (REGEXP MODE t): Keep looking but remember the MODE.
            rest = rest(1:mb - 1)
        end do
    end subroutine find_matching_entry

    !> The index of the first auto-mode-alist entry matching NAME (0 for
    !> none), first case-sensitively, then (auto-mode-case-fold)
    !> case-insensitively, and the 1-based start MB of the match.
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

            do i = 1, n_auto_modes
                mb = string_match(auto_modes(i)%regexp, name, st, inhibit_modify=.true.)

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
    if (cur == 0) return
    keep = .false.
    if (present(keep_mode_if_same)) keep = keep_mode_if_same
    ! Look for -*-MODENAME-*- or -*- ... mode: MODENAME; ... -*-
    call buffer_substring(point_min(), min(point_max(), point_min() + SPEC_LIMIT), text, status)
    if (.not. status%ok()) return
    mode = auto_mode_spec(text)

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
    call init_auto_modes()
    if (.not. allocated(buffers(cur)%filename)) return
    name = file_name_sans_versions(buffers(cur)%filename)
    call find_matching_entry(name, mode, status)
    if (.not. status%ok()) return

    if (len(mode) > 0) then
        if (set_auto_mode_0(mode, keep, status)) return
    end if
    end procedure set_auto_mode

    module procedure normal_mode
    type(status_type) :: st

    status = status_type()
    if (cur == 0) return
    ! FIND_FILE only matters for file-local variables, not yet supported.
    if (present(find_file)) continue
    call kill_all_local_variables()

    if (.not. delaying_mode_hooks()) then
        call change_major_mode_after_body_hook%run(status)
        if (.not. status%ok()) return
        call after_change_major_mode_hook%run(status)
        if (.not. status%ok()) return
    end if

    ! (with-demoted-errors "File mode specification error: %S" ...)
    call set_auto_mode(st)
    if (.not. st%ok()) call message("File mode specification error: "//format_error(st))
    end procedure normal_mode
end submodule files_mode_fm

! mail_abbreviations_mod.f90 -- abbrev-expansion of mail aliases.
! Ported from GNU Emacs 31.1 lisp/mail/mailabbrev.el.
!
! Copyright (C) 1985-1987, 1992-1993, 1996-1997, 2000-2026 Free Software
! Foundation, Inc.
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

!> Mail aliases as abbrevs: the aliases of `mail_personal_alias_file'
!> (a `.mailrc' file: "alias NAME DEFINITION", "group", "source FILE")
!> become the abbrevs of the table `mail_abbreviations', which expand as you
!> type in the address fields of a Mail mode buffer (the fields
!> `mail_abbreviation_mode_regular_expression' matches).  Turn it on with
!> `mail_abbreviations_mode', or put `set_up_mail_abbreviations' on `mail_mode_hook'.
!> Aliases can refer to other aliases, and are resolved when expanded.
!>
!> `mail_abbreviation_expand_wrapper' takes the place of upstream's wrapper
!> around the buffer-local `abbreviation_expand_function': `set_up_mail_abbreviations'
!> marks the buffer, and the wrapper calls the previous function in the
!> other buffers.
module mail_abbreviations_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_mod, only: commands
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, prefix_argument_type, this_command, &
                                         PREFIX_NONE
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use casefiddle_mod, only: downcase_string
    use handle_mod, only: handle_type
    use value_mod, only: value_type, vtag, V_INTEGER, make_list
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, is_buffer_live, &
                          buffer_serial_live, generate_new_buffer_name, get_file_buffer, fill_column, auto_fill_function
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_line, is_end_of_buffer, &
                                     buffer_substring, buffer_string, delete_region, narrow_to_region, widen, preceding_character, &
                                     line_beginning_position, insert_buffer_substring
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_character, end_of_line, beginning_of_line, delete_character
    use syntax_mod, only: skip_characters_forward, copy_syntax_table, syntax_table, set_syntax_table, modify_syntax_entry, &
                          character_syntax_class, character_syntax, destroy_syntax_table, is_syntax_table_live, SSYMBOL
    use search_mod, only: regular_expression_search_forward, regular_expression_search_backward, search_forward, search_backward, &
                          looking_at, match_beginning, match_end, replace_match, string_match, case_fold_search, SEARCH_NOERROR, &
                          SEARCH_MOVE
    use basic_subprograms_mod, only: match_string
    use indent_mod, only: current_column
    use keyboard_mod, only: last_command_event
    use window_mod, only: is_window_minibuffer
    use file_input_output_mod, only: expand_file_name, file_exists, file_attributes, insert_file_contents, substitute_in_file_name
    use system_dependent_mod, only: file_status_type
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use display_engine_mod, only: message
    use simple_mod, only: rfc822_goto_end_of_headers, next_line, end_of_buffer
    use abbreviation_mod, only: abbreviation_table_type, abbreviation_reference_type, abbreviation_mode, abbreviations_changed, &
                                abbreviation_expand_function, abbreviation_expand_procedure, abbreviation_default_expand, &
                                expand_abbreviation, local_abbreviation_table, abbreviation_table_set_local
    use modes_mod, only: minor_modes
    use keymap_mod, only: keymap_set
    use minibuffer_reading_mod, only: completion_ignore_case
    use minibuffer_mod, only: completing_read, read_file_name, completion_in_region, REQUIRE_MATCH_YES
    use replace_mod, only: format_prompt
    use minibuffer_reading_tables_mod, only: completion_table_type, TABLE_LIST
    use send_mail_mod, only: mail_mode_hook, mail_mode_map, mail_personal_alias_file, initialize_sendmail
    implicit none(type, external)
    private

    public :: mail_abbreviations_mode, set_up_mail_abbreviations, enable_mail_abbreviations, disable_mail_abbreviations, &
              build_mail_abbreviations, &
              define_mail_abbreviation, resolve_all_mail_aliases, mail_abbreviation_expand_hook, &
              is_mail_abbreviation_in_expansion_header, &
              mail_abbreviation_expand_wrapper, merge_mail_abbreviations, rebuild_mail_abbreviations, &
              insert_mail_abbreviation_alias, &
              complete_mail_abbreviation_alias, mail_abbreviation_next_line, mail_abbreviation_end_of_buffer, initialize_mailabbrev

    !> True means only mail abbrevs should expand automatically.
    !> Other abbrevs expand only when you explicitly use `expand_abbreviation'.
    !> Port of mailabbrev.el mail-abbrevs-only.
    logical, public :: mail_abbreviations_only = .false.

    !> Word-abbrev table of mail address aliases.
    !> Until `mail_abbreviations_built' is true, the aliases have not yet been
    !> initialized and should be read from the .mailrc file.  (This is
    !> distinct from there being no aliases, which is represented by this
    !> being a table with no entries.)  Port of mailabbrev.el mail-abbrevs.
    type(abbreviation_table_type), target, public :: mail_abbreviations

    !> Whether `mail_abbreviations' holds the aliases (upstream's mail-abbrevs
    !> being an abbrev table, not unset).  Not a port.
    logical, public :: mail_abbreviations_built = .false.

    !> String inserted between addresses in multi-address mail aliases.
    !> This has to contain a comma, so ", " is a reasonable value.  You might
    !> also want something like ", " followed by a newline and spaces to get
    !> each address on its own line.  Port of mailabbrev.el
    !> mail-alias-separator-string.
    character(len=:), allocatable, public :: mail_alias_separator_string

    !> Regexp matching mail headers in which mail abbrevs should be expanded.
    !> This string will be handed to `looking_at' with point at the beginning
    !> of the current line; if it matches, abbrev mode will be turned on, otherwise
    !> it will be turned off.  (You don't need to worry about continuation lines.)
    !> This should be set to match those mail fields in which you want abbreviations
    !> turned on.  Port of mailabbrev.el mail-abbrev-mode-regexp.
    character(len=:), allocatable, public :: mail_abbreviation_mode_regular_expression

    ! The modification time of your mail alias file when it was last
    ! examined (mail-abbrev-modtime; HAS_MODTIME false for nil).
    logical :: has_modtime = .false.
    integer(int64) :: modtime_second = 0
    integer(int32) :: modtime_nsec = 0

    ! define-mail-abbrev sets this flag, which causes
    ! resolve_all_mail_aliases to be called before expanding abbrevs if
    ! it's necessary (mail-abbrev-aliases-need-to-be-resolved).
    logical :: aliases_need_to_be_resolved = .true.

    ! The function the wrapper replaced as `abbreviation_expand_function' (null:
    ! `abbreviation_default_expand').
    procedure(abbreviation_expand_procedure), pointer :: next_expand_function => null()
    logical :: wrapper_installed = .false.

    ! The buffers where `set_up_mail_abbreviations' ran, with their
    ! mail-abbrev-syntax-table (the null handle until made), by serial
    ! number.
    type :: local_type
        integer(int64) :: serial = 0
        logical :: set_up = .false.
        type(handle_type) :: syntax_table
    end type local_type

    type(local_type), allocatable :: locals(:)

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `locals' (0 if none).
    ! Not a port.
    integer function local_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(locals)) return
        k = findloc(locals%serial, buffers(current)%serial, dim=1)
    end function local_index

    ! Find or make the current buffer's entry in `locals'.  Not a port.
    subroutine make_local_entry(k)
        integer, intent(out) :: k
        type(local_type) :: entry

        k = local_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        if (k > size(locals)) locals = [locals, entry]
        locals(k) = local_type(serial=buffers(current)%serial)
    end subroutine make_local_entry

    ! The characters of STRING from index FROM (inclusive) to TO
    ! (exclusive), 1-based; TO 0 means to the end.  Not a port.
    function chars(string, from, to) result(text)
        use search_mod, only: string_index_to_offset
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: from, to
        character(len=:), allocatable :: text
        integer(int64) :: a, b

        a = string_index_to_offset(string, from)
        b = len(string, kind=int64)
        if (to > 0) b = string_index_to_offset(string, to)
        text = string(a + 1:b)
    end function chars

    ! The index of the abbrev NAME in `mail_abbreviations' (0 if none).
    ! Not a port.
    integer function alias_index(name) result(k)
        character(len=*), intent(in) :: name

        do k = 1, mail_abbreviations%n
            if (mail_abbreviations%abbreviations(k)%name == name) return
        end do

        k = 0
    end function alias_index

    ! The modification time of FILE (FOUND false if it has none).
    ! Not a port.
    subroutine file_modtime(file, second, nsec, found)
        character(len=*), intent(in) :: file
        integer(int64), intent(out) :: second
        integer(int32), intent(out) :: nsec
        logical, intent(out) :: found
        type(file_status_type) :: st

        call file_attributes(file, st, found)
        second = st%mtime_second
        nsec = st%mtime_nsec
    end subroutine file_modtime

    ! ------------------------------------------------------------------
    ! The mode and the alias file
    ! ------------------------------------------------------------------

    !> Toggle abbrev expansion of mail aliases (Mail Abbrevs mode).
    !>
    !> Mail Abbrevs mode is a global minor mode.  When enabled,
    !> abbrev-like expansion is performed when editing certain mail
    !> headers (those specified by `mail_abbreviation_mode_regular_expression'), based on
    !> the entries in your `mail_personal_alias_file'.  Port of
    !> mailabbrev.el:mail-abbrevs-mode.
    recursive subroutine mail_abbreviations_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%mail_abbreviations_mode%command(status)
    end subroutine mail_abbreviations_mode

    !> The body of Mail Abbrevs mode.  Port of
    !> mailabbrev.el:mail-abbrevs-mode (its body).
    recursive subroutine mail_abbreviations_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()

        if (enable) then
            call enable_mail_abbreviations()
        else
            call disable_mail_abbreviations(status)
        end if
    end subroutine mail_abbreviations_mode_body

    ! Rebuild the aliases if the alias file changed since it was last
    ! examined.  Port of mailabbrev.el:mail-abbrevs-sync-aliases.
    recursive subroutine synchronize_mail_abbreviations_aliases(status)
        type(status_type), intent(out) :: status
        integer(int64) :: second
        integer(int32) :: nsec
        logical :: found

        status = status_type()
        if (.not. allocated(mail_personal_alias_file)) return
        if (.not. file_exists(mail_personal_alias_file)) return
        call file_modtime(mail_personal_alias_file, second, nsec, found)

        if (.not. (has_modtime .and. second == modtime_second .and. nsec == modtime_nsec)) then
            has_modtime = .true.
            modtime_second = second
            modtime_nsec = nsec
            call build_mail_abbreviations(status)
        end if
    end subroutine synchronize_mail_abbreviations_aliases

    !> Initialize use of the `mailabbrev' package.  Port of
    !> mailabbrev.el:mail-abbrevs-setup.
    recursive subroutine set_up_mail_abbreviations(status)
        type(status_type), intent(out) :: status
        logical :: found
        integer :: k

        status = status_type()

        if (.not. mail_abbreviations_built .and. allocated(mail_personal_alias_file)) then
            if (file_exists(mail_personal_alias_file)) then
                call file_modtime(mail_personal_alias_file, modtime_second, modtime_nsec, found)
                has_modtime = found
                call build_mail_abbreviations(status)
                if (.not. status%ok()) return
            end if
        end if

        call synchronize_mail_abbreviations_aliases(status)
        if (.not. status%ok()) return
        ! (add-function :around (local 'abbrev-expand-function) ...)
        call install_wrapper()
        call make_local_entry(k)
        if (k > 0) locals(k)%set_up = .true.
        call minor_modes%abbreviation_mode%enable(status)
    end subroutine set_up_mail_abbreviations

    ! Make `mail_abbreviation_expand_wrapper' the `abbreviation_expand_function',
    ! remembering the function it replaces.  Not a port.
    subroutine install_wrapper()
        if (wrapper_installed) return
        next_expand_function => abbreviation_expand_function
        abbreviation_expand_function => mail_abbreviation_expand_wrapper
        wrapper_installed = .true.
    end subroutine install_wrapper

    !> Turn on use of the `mailabbrev' package in Mail mode.  Port of
    !> mailabbrev.el:mail-abbrevs-enable.
    subroutine enable_mail_abbreviations()
        call mail_mode_hook%prepend(set_up_mail_abbreviations)
    end subroutine enable_mail_abbreviations

    !> Turn off use of the `mailabbrev' package.  Port of
    !> mailabbrev.el:mail-abbrevs-disable.
    recursive subroutine disable_mail_abbreviations(status)
        type(status_type), intent(out) :: status
        logical :: default

        status = status_type()
        call mail_mode_hook%remove(set_up_mail_abbreviations)
        ! (abbrev-mode (if (default-value 'abbrev-mode) 1 -1)): the value
        ! once the buffer's own is cleared is the default.
        call abbreviation_mode%clear_local()
        default = abbreviation_mode%value

        if (default) then
            call minor_modes%abbreviation_mode%enable(status)
        else
            call minor_modes%abbreviation_mode%disable(status)
        end if
    end subroutine disable_mail_abbreviations

    !> Read mail aliases from personal mail alias file and set `mail_abbreviations'.
    !> By default this is the file specified by `mail_personal_alias_file'.
    !> RECURSIVE is true for a file named by a "source" line.  Port of
    !> mailabbrev.el:build-mail-abbrevs.
    recursive subroutine build_mail_abbreviations(status, file_in, recursive)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file_in
        logical, intent(in), optional :: recursive
        character(len=:), allocatable :: file, name, definition, sourced
        type(handle_type) :: old, temporary, visiting
        integer(int64) :: found, p, moved, start, inserted
        logical :: is_recursive, sourcing, saved_changed
        type(status_type) :: st

        is_recursive = .false.
        if (present(recursive)) is_recursive = recursive

        if (present(file_in)) then
            call expand_file_name(file_in, file, status)
        else
            call expand_file_name(mail_personal_alias_file, file, status)
        end if

        if (.not. status%ok()) return

        if (.not. mail_abbreviations_built) then
            saved_changed = abbreviations_changed
            call mail_abbreviations%clear()
            call mail_abbreviations%define_table("mail_abbreviations")
            abbreviations_changed = saved_changed
            mail_abbreviations_built = .true.
        end if

        call message("Parsing "//file//"...")
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)
        if (.not. status%ok()) return
        call set_buffer(temporary, status)

        body: block
            visiting = get_file_buffer(file)

            if (is_buffer_live(visiting)) then
                call insert_buffer_substring(visiting, status)
            else if (file_exists(file)) then
                call insert_file_contents(file, .false., inserted, status)
            end if

            if (.not. status%ok()) exit body
            ! Don't lose if no final newline.
            call goto_character(point_maximum())
            if (preceding_character() /= 10) call insert(LF, status)
            call goto_character(point_minimum())

            ! Delete comments from the file
            do
                call search_forward("# ", found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) exit body
                if (found == 0) exit
                p = point() - 2
                call end_of_line()
                call delete_region(p, point(), status)
                if (.not. status%ok()) exit body
            end do

            call goto_character(point_minimum())

            ! handle "\\\n" continuation lines
            do while (.not. is_end_of_buffer())
                call end_of_line()

                if (preceding_character() == iachar("\")) then
                    call delete_character(-1_int64, status)
                    if (status%ok()) call delete_character(1_int64, status)
                    if (status%ok()) call insert(" ", status)
                    if (.not. status%ok()) exit body
                else
                    call forward_character(status, 1_int64)
                    if (.not. status%ok()) exit body
                end if
            end do

            call goto_character(point_minimum())

            do
                call regular_expression_search_forward("^\(a\(lias\)?\|g\(roup\)?\|source\)[ "//TAB//"]+", found, status, &
                                                       noerror=SEARCH_NOERROR)
                if (.not. status%ok()) exit body
                if (found == 0) exit
                call beginning_of_line()
                sourcing = looking_at("source[ "//TAB//"]+\([^ "//TAB//LF//"]+\)", status)
                if (.not. status%ok()) exit body

                if (sourcing) then
                    sourced = match_string(1)
                    call end_of_line()
                    call build_mail_abbreviations(status, substitute_in_file_name(sourced), .true.)
                    call set_buffer(temporary, status)
                    if (.not. status%ok()) exit body
                else
                    call regular_expression_search_forward("[ "//TAB//"]+\([^ "//TAB//LF//"]+\)", found, status)
                    if (.not. status%ok()) exit body
                    name = match_string(1)
                    moved = skip_characters_forward(" "//TAB, status)
                    start = point()
                    call end_of_line()
                    call buffer_substring(start, point(), definition, status)
                    if (.not. status%ok()) exit body
                    call define_mail_abbreviation(name, definition, status, .true.)
                    if (.not. status%ok()) exit body
                end if
            end do

            ! Resolve forward references in .mailrc file.
            ! This would happen automatically before the first abbrev was
            ! expanded, but why not do it now.
            if (.not. is_recursive) call resolve_all_mail_aliases(status)
        end block body

        call close_buffer(temporary, st)
        if (is_buffer_live(old)) call set_buffer(old, st)
        if (status%ok()) call message("Parsing "//file//"... done")
    end subroutine build_mail_abbreviations

    !> Define NAME as a mail alias abbrev that translates to DEFINITION.
    !> If DEFINITION contains multiple addresses, separate them with commas.
    !>
    !> Optional argument FROM_MAILRC_FILE means that DEFINITION comes
    !> from a mailrc file.  In that case, addresses are separated with
    !> spaces and addresses with embedded spaces are surrounded by
    !> double-quotes.  Port of mailabbrev.el:define-mail-abbrev.
    recursive subroutine define_mail_abbreviation(name, definition_in, status, from_mailrc_file)
        character(len=*), intent(in) :: name, definition_in
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: from_mailrc_file
        character(len=:), allocatable :: definition, this_entry, joined
        character(len=:), allocatable :: result(:)
        integer(int64) :: l, start, end, m, me
        logical :: from_mailrc, saved_changed
        type(status_type) :: st

        status = status_type()
        from_mailrc = .false.
        if (present(from_mailrc_file)) from_mailrc = from_mailrc_file

        ! Read the defaults first, if we have not done so.
        if (.not. mail_abbreviations_built) then
            call build_mail_abbreviations(status)
            if (.not. status%ok()) return
        end if

        definition = definition_in
        ! strip garbage from front and end
        if (string_match("\`[ "//TAB//LF//",]+", definition, st) > 0) definition = chars(definition, match_end(0, st), 0_int64)
        m = string_match("[ "//TAB//LF//",]+\'", definition, st)
        if (m > 0) definition = chars(definition, 1_int64, m)
        l = string_characters_of(definition)
        start = 0
        if (l > 0) start = 1
        allocate (character(len=0) :: result(0))

        do while (start > 0)
            if (from_mailrc) then
                ! If we're reading from the mailrc file, addresses are
                ! delimited by spaces, and addresses with embedded spaces are
                ! surrounded by non-escaped double-quotes.
                if (character_is(definition, start, '"')) then
                    start = start + 1
                    end = 0

                    if (string_match("[^\]\(\([\][\]\)*\)""[ "//TAB//",]*", definition, st, start=start) > 0) then
                        end = match_end(1, st)
                    end if
                else
                    end = string_match("[ "//TAB//",]+", definition, st, start=start)
                end if

                ! Extract the address and advance the loop past it.
                this_entry = chars(definition, start, end)
                me = match_end(0, st)

                if (end > 0 .and. me /= l + 1) then
                    start = me
                else
                    start = 0
                end if

                call quote_full_name(this_entry)
                call push(this_entry)
            else if (string_match("[ "//TAB//",]*\(""\(?:[^""]\|[^\]\(?:[\][\]\)*""\)*""[ "//TAB//"]*"// &
                                  "<[-.!#$%&'*+/0-9=?A-Za-z^_`{|}~@]+>\)[ "//TAB//",]*", definition, st, start=start) &
                     == start) then
                ! If an entry has a valid [ "foo bar" <foo@example.com> ]
                ! form, use it literally .  This also allows commas in the
                ! quoted string, e.g.  [ "foo bar, jr" <foo@example.com> ]
                call push(match_string(1, definition))
                me = match_end(0, st)

                if (me /= l + 1) then
                    start = me
                else
                    start = 0
                end if
            else
                ! Otherwise, read the next address by looking for a comma.
                end = string_match("[ "//TAB//LF//",]*,[ "//TAB//LF//"]*", definition, st, start=start)
                this_entry = chars(definition, start, end)
                me = match_end(0, st)

                ! Advance the loop past this address.
                if (end > 0 .and. me /= l + 1) then
                    start = me
                else
                    start = 0
                end if

                call quote_full_name(this_entry)
                call push(this_entry)
            end if
        end do

        joined = ""

        do m = 1, size(result)
            if (m > 1) joined = joined//mail_alias_separator_string
            joined = joined//trim(result(m))
        end do

        aliases_need_to_be_resolved = .true.
        ! use an abbrev table instead of an alist for mail-abbrevs.
        saved_changed = abbreviations_changed
        call mail_abbreviations%define(downcase_string(name), joined, hook=mail_abbreviation_expand_hook, count=0_int64, &
                                       system=.true.)
        abbreviations_changed = saved_changed

    contains

        ! If the full name contains a problem character, quote it.
        subroutine quote_full_name(text)
            character(len=:), allocatable, intent(inout) :: text
            character(len=:), allocatable :: full_name, replaced
            type(status_type) :: qst

            if (string_match("\(.+?\)[ "//TAB//"]*\(<.*>\)", text, qst) == 0) return
            full_name = match_string(1, text)
            if (string_match("[^- !#$%&'*+/0-9=?A-Za-z^_`{|}~]", full_name, qst) == 0) return
            ! (replace-regexp-in-string ...)
            if (string_match("\(.+?\)[ "//TAB//"]*\(<.*>\)", text, qst) == 0) return
            call replace_match("""\1"" \2", qst, string=text, result=replaced)
            if (qst%ok()) text = replaced
        end subroutine quote_full_name

        ! Add TEXT at the end of RESULT (upstream pushes, then reverses).
        subroutine push(text)
            character(len=*), intent(in) :: text
            character(len=:), allocatable :: grown(:)
            integer :: j, n

            n = max(len(result), len(text))
            allocate (character(len=n) :: grown(size(result) + 1))

            do j = 1, size(result)
                grown(j) = result(j)
            end do

            grown(size(grown)) = text
            call move_alloc(grown, result)
        end subroutine push

        logical function character_is(text, k, c)
            character(len=*), intent(in) :: text, c
            integer(int64), intent(in) :: k
            character(len=:), allocatable :: one

            one = chars(text, k, k + 1)
            character_is = one == c
        end function character_is

        integer(int64) function string_characters_of(text)
            use utility_functions_mod, only: count_characters
            character(len=*), intent(in) :: text

            string_characters_of = count_characters(text)
        end function string_characters_of
    end subroutine define_mail_abbreviation

    !> Resolve all forward references in the mail aliases table.  Port of
    !> mailabbrev.el:mail-resolve-all-aliases.
    recursive subroutine resolve_all_mail_aliases(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: value
        integer :: k
        integer, allocatable :: so_far(:)

        status = status_type()
        if (.not. aliases_need_to_be_resolved) return

        if (mail_abbreviations_built) then
            allocate (so_far(0))

            do k = 1, mail_abbreviations%n
                call resolve_all_mail_aliases_1(k, so_far, value, status)
                if (.not. status%ok()) return
            end do
        end if

        aliases_need_to_be_resolved = .false.
    end subroutine resolve_all_mail_aliases

    ! Resolve the alias K, whose definition refers to the aliases SO_FAR
    ! (the latest first); VALUE is its resolved definition (unallocated
    ! for an alias without one).  Port of
    ! mailabbrev.el:mail-resolve-all-aliases-1.
    recursive subroutine resolve_all_mail_aliases_1(k, so_far, value, status)
        integer, intent(in) :: k
        integer, intent(in) :: so_far(:)
        character(len=:), allocatable, intent(out) :: value
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: definition, part, resolved, names
        integer(int64) :: start, end
        integer :: j, i
        logical :: first
        type(status_type) :: st

        status = status_type()

        if (any(so_far == k)) then
            names = trim(mail_abbreviations%abbreviations(k)%name)

            do i = 1, size(so_far)
                names = names//" <- "//trim(mail_abbreviations%abbreviations(so_far(i))%name)
            end do

            call set_error(status, GENERIC_ERROR_CODE, "Mail alias loop detected: "//names)
            return
        end if

        if (allocated(mail_abbreviations%abbreviations(k)%expansion)) then
            definition = mail_abbreviations%abbreviations(k)%expansion
            resolved = ""
            first = .true.
            start = 1

            do while (start > 0)
                end = string_match("[ "//TAB//LF//"]*,[, "//TAB//LF//"]*", definition, st, start=start)
                part = chars(definition, start, end)

                if (end > 0) then
                    start = match_end(0, st)
                else
                    start = 0
                end if

                if (.not. first) resolved = resolved//mail_alias_separator_string
                first = .false.
                j = alias_index(downcase_string(part))

                if (j > 0) then
                    call resolve_all_mail_aliases_1(j, [k, so_far], value, status)
                    if (.not. status%ok()) return

                    if (allocated(value)) then
                        resolved = resolved//value
                        cycle
                    end if
                end if

                resolved = resolved//part
            end do

            mail_abbreviations%abbreviations(k)%expansion = resolved
        end if

        if (allocated(value)) deallocate (value)
        if (allocated(mail_abbreviations%abbreviations(k)%expansion)) value = mail_abbreviations%abbreviations(k)%expansion
    end subroutine resolve_all_mail_aliases_1

    !> The hook of the mail abbrevs (`define_mail_abbreviation').
    !> After expanding a mail-abbrev, if Auto Fill mode is on and we're past the
    !> `fill_column', break the line at the previous comma, and indent the next line
    !> with a space.  Port of mailabbrev.el:mail-abbrev-expand-hook.
    recursive subroutine mail_abbreviation_expand_hook(value, status)
        logical, intent(out) :: value
        type(status_type), intent(out) :: status
        integer(int64) :: p, found
        type(handle_type) :: here
        logical :: at_comma, spaces
        type(status_type) :: st

        value = .true.
        status = status_type()
        if (.not. associated(auto_fill_function%value)) return
        call copy_marker_at(point(), here, .false., st)
        p = 0

        fill: do while (current_column() >= fill_column%value)
            do
                call search_backward(",", found, status, line_beginning_position(), SEARCH_MOVE)
                if (.not. status%ok()) exit fill
                at_comma = found /= 0
                if (.not. at_comma) exit
                if (current_column() < fill_column%value - 1) exit
                p = point()
            end do

            if (is_beginning_of_line() .and. p > 0) call goto_character(p)

            if (.not. is_beginning_of_line()) then
                p = 0
                call forward_character(status, 1_int64)
                if (status%ok()) call insert(LF, status)
                if (.not. status%ok()) exit fill
                spaces = looking_at("["//TAB//" ]+", status)
                if (.not. status%ok()) exit fill

                if (spaces) then
                    found = match_end(0, st)
                    call delete_region(point(), found, status)
                    if (.not. status%ok()) exit fill
                end if

                call insert(" ", status)
                if (.not. status%ok()) exit fill
                call end_of_line()
            else
                exit fill
            end if
        end do fill

        call marker_position(here, found, st)
        call goto_character(found)
        call destroy_marker(here, st)
    end subroutine mail_abbreviation_expand_hook

    ! ------------------------------------------------------------------
    ! Syntax tables and abbrev-expansion
    ! ------------------------------------------------------------------

    ! Make the buffer's mail-abbrev-syntax-table, unless made: a copy of
    ! the syntax table in which the characters of identifier syntax, and
    ! @ % ! . _ -, are word constituents.  Port of
    ! mailabbrev.el:mail-abbrev-make-syntax-table (for the ASCII
    ! characters; the other characters of identifier syntax are Deferred).
    subroutine make_mail_abbreviation_syntax_table(table)
        type(handle_type), intent(out) :: table
        type(handle_type) :: saved
        type(status_type) :: st
        integer :: k, c

        call make_local_entry(k)

        if (k > 0) then
            if (is_syntax_table_live(locals(k)%syntax_table)) then
                table = locals(k)%syntax_table
                return
            end if
        end if

        saved = syntax_table()
        call copy_syntax_table(table, st, saved)
        call set_syntax_table(table, st)

        do c = 0, 127
            if (character_syntax_class(c) == SSYMBOL) call modify_syntax_entry(c, c, "w", st, table)
        end do

        call set_syntax_table(saved, st)
        call modify_syntax_entry(iachar("@"), iachar("@"), "w", st, table)
        call modify_syntax_entry(iachar("%"), iachar("%"), "w", st, table)
        call modify_syntax_entry(iachar("!"), iachar("!"), "w", st, table)
        call modify_syntax_entry(iachar("."), iachar("."), "w", st, table)
        call modify_syntax_entry(iachar("_"), iachar("_"), "w", st, table)
        call modify_syntax_entry(iachar("-"), iachar("-"), "w", st, table)
        if (k > 0) locals(k)%syntax_table = table
    end subroutine make_mail_abbreviation_syntax_table

    !> Whether point is in a mail-address header field.  Port of
    !> mailabbrev.el:mail-abbrev-in-expansion-header-p.
    recursive logical function is_mail_abbreviation_in_expansion_header(status) result(in_header)
        type(status_type), intent(out) :: status
        integer(int64) :: here, found, end_of_headers
        integer(int64) :: beginning_of_accessible_portion, end_of_accessible_portion
        logical :: saved_fold

        in_header = .false.
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        here = point()
        ! we are on an appropriate header line...
        if (.not. is_end_of_buffer()) call forward_character(status, 1_int64)
        call regular_expression_search_backward("^[^ "//TAB//"]", found, status, noerror=SEARCH_MOVE)
        ! are we at the front of an appropriate header line?
        if (status%ok()) in_header = looking_at(mail_abbreviation_mode_regular_expression, status)
        call goto_character(here)

        if (in_header .and. status%ok()) then
            ! ...and are we in the headers?
            beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
            end_of_accessible_portion = buffers(current)%end_of_accessible_portion
            call widen()
            call rfc822_goto_end_of_headers(status)
            end_of_headers = point()
            call narrow_to_region(beginning_of_accessible_portion, end_of_accessible_portion, status)
            call goto_character(here)
            in_header = here < end_of_headers
        end if

        case_fold_search%value = saved_fold
    end function is_mail_abbreviation_in_expansion_header

    !> Expand an abbrev with the mail aliases in the mail headers (the
    !> wrapper upstream puts around `abbreviation_expand_function' in buffers
    !> where `set_up_mail_abbreviations' ran).  Port of
    !> mailabbrev.el:mail-abbrev-expand-wrapper.
    recursive subroutine mail_abbreviation_expand_wrapper(abbreviation, status)
        type(abbreviation_reference_type), intent(out) :: abbreviation
        type(status_type), intent(out) :: status
        type(abbreviation_table_type), pointer :: saved_table
        type(handle_type) :: table, saved_syntax
        type(status_type) :: st
        logical :: in_header, skip
        integer(int64) :: event
        integer :: k

        status = status_type()
        k = local_index()
        skip = k == 0
        if (.not. skip) skip = .not. locals(k)%set_up

        ! Not a Mail mode buffer set up for mail abbrevs, or no mail-abbrevs
        ! at all: do the normal thing.
        if (skip .or. .not. mail_abbreviations_built) then
            call expand(abbreviation, status)
            return
        end if

        in_header = is_mail_abbreviation_in_expansion_header(status)
        if (.not. status%ok()) return
        ! Necessary for `message-read-from-minibuffer' to work.
        if (.not. in_header) in_header = is_window_minibuffer(status=st)

        if (in_header) then
            ! We are in a To: (or Cc:, or whatever) header or a minibuffer,
            ! and should use word-abbrevs to expand mail aliases.
            saved_table => local_abbreviation_table()
            call abbreviation_table_set_local(mail_abbreviations)
            ! Before anything else, resolve aliases if they need it.
            if (aliases_need_to_be_resolved) call resolve_all_mail_aliases(status)

            if (status%ok()) then
                ! Now proceed with the abbrev section.
                !   -  We already installed mail-abbrevs as the abbrev table.
                !   -  Then install the mail-abbrev-syntax-table, which
                !      temporarily marks all of the
                !      non-alphanumeric-atom-characters (the "_"
                !      syntax ones) as being normal word-syntax.  We do this
                !      because the code for expand-abbrev only works on words,
                !      and we want these characters to be considered words for
                !      the purpose of abbrev expansion.
                !   -  Then we call the expand function, to do
                !      the abbrev expansion with the above syntax table.
                call make_mail_abbreviation_syntax_table(table)
                ! If the character just typed was non-alpha-identifier-syntax,
                ! then don't expand the abbrev now (that is, don't expand
                ! when the user types -.)  Check the character's syntax in
                ! the usual syntax table.
                skip = .false.

                if (vtag(last_command_event) == V_INTEGER .and. associated(this_command)) then
                    ! Some commands such as M-> may want to expand first.
                    if (associated(this_command, commands%self_insert_command)) then
                        event = last_command_event%a(1)%i
                        skip = any(event == [iachar("@"), iachar("."), iachar("%"), iachar("!"), iachar("_"), iachar("-")])

                        if (.not. skip .and. event >= 0 .and. event < 4194304) then
                            ! Don't expand on @.
                            skip = character_syntax(int(event)) == "_"
                        end if
                    end if
                end if

                if (.not. skip) then
                    ! Use this table so that abbrevs can have hyphens in them.
                    saved_syntax = syntax_table()
                    call set_syntax_table(table, st)
                    call expand(abbreviation, status)
                    call set_syntax_table(saved_syntax, st)
                end if
            end if

            call abbreviation_table_set_local(saved_table)
        else if (.not. mail_abbreviations_only .or. associated(this_command, commands%expand_abbreviation)) then
            ! We're not in a mail header where mail aliases should
            ! be expanded, then use the normal mail-mode abbrev table
            ! (if any) and the normal mail-mode syntax table.
            call expand(abbreviation, status)
        end if

    contains

        ! The function this one is around.
        recursive subroutine expand(abbreviation, status)
            type(abbreviation_reference_type), intent(out) :: abbreviation
            type(status_type), intent(out) :: status

            if (associated(next_expand_function)) then
                call next_expand_function(abbreviation, status)
            else
                call abbreviation_default_expand(abbreviation, status)
            end if
        end subroutine expand
    end subroutine mail_abbreviation_expand_wrapper

    ! ------------------------------------------------------------------
    ! Utilities
    ! ------------------------------------------------------------------

    !> Merge mail aliases from the given file with existing ones.  Port of
    !> mailabbrev.el:merge-mail-abbrevs.
    recursive subroutine merge_mail_abbreviations(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status

        call build_mail_abbreviations(status, file)
    end subroutine merge_mail_abbreviations

    !> Rebuild all the mail aliases from the given file (the visited file
    !> if FILE is absent).  Port of mailabbrev.el:rebuild-mail-abbrevs.
    recursive subroutine rebuild_mail_abbreviations(status, file)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file
        character(len=:), allocatable :: name

        if (present(file)) then
            name = file
        else if (allocated(buffers(current)%filename)) then
            name = buffers(current)%filename
        end if

        mail_abbreviations_built = .false.

        if (allocated(name)) then
            call build_mail_abbreviations(status, name)
        else
            call build_mail_abbreviations(status)
        end if
    end subroutine rebuild_mail_abbreviations

    !> Prompt for and insert a mail alias.  Port of
    !> mailabbrev.el:mail-abbrev-insert-alias.
    recursive subroutine insert_mail_abbreviation_alias(status, alias)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: alias
        logical :: value
        integer :: k

        status = status_type()

        if (.not. mail_abbreviations_built) then
            call set_up_mail_abbreviations(status)
            if (.not. status%ok()) return
        end if

        if (present(alias)) then
            k = alias_index(alias)

            if (k > 0) then
                if (allocated(mail_abbreviations%abbreviations(k)%expansion)) then
                    call insert(mail_abbreviations%abbreviations(k)%expansion, status)
                    if (.not. status%ok()) return
                end if
            end if
        end if

        call mail_abbreviation_expand_hook(value, status)
    end subroutine insert_mail_abbreviation_alias

    !> Perform completion on alias preceding point.  Port of
    !> mailabbrev.el:mail-abbrev-complete-alias.
    recursive subroutine complete_mail_abbreviation_alias(status)
        use simple_mod, only: backward_word
        type(status_type), intent(out) :: status
        type(handle_type) :: table, saved_syntax
        type(completion_table_type) :: collection
        integer(int64) :: end, beginning
        logical :: done
        type(status_type) :: st

        status = status_type()
        call make_mail_abbreviation_syntax_table(table)
        end = point()
        saved_syntax = syntax_table()
        call set_syntax_table(table, st)
        call backward_word(1_int64)
        beginning = point()
        call goto_character(end)
        call set_syntax_table(saved_syntax, st)
        call alias_table(collection)
        done = completion_in_region(beginning, end, collection)
    end subroutine complete_mail_abbreviation_alias

    ! The names of `mail_abbreviations' as a completion table.  Not a port.
    subroutine alias_table(collection)
        type(completion_table_type), intent(out) :: collection
        type(value_type), allocatable :: names(:)
        type(value_type) :: item
        integer :: k

        allocate (names(0))

        do k = 1, mail_abbreviations%n
            if (.not. allocated(mail_abbreviations%abbreviations(k)%expansion)) cycle
            item = value_type(text=mail_abbreviations%abbreviations(k)%name)
            names = [names, item]
        end do

        collection%kind = TABLE_LIST
        collection%list = make_list(names)
    end subroutine alias_table

    !> Expand a mail abbrev before point, then move vertically down ARGUMENT lines.
    !> This only expands an abbrev (if one is present) if called with
    !> point at the end of a line, or on whitespace before the end of a line.
    !>
    !> In terms of line motion, this behaves like `next_line', which see.
    !> Port of mailabbrev.el:mail-abbrev-next-line.
    recursive subroutine mail_abbreviation_next_line(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical :: at_end

        at_end = looking_at("[ "//TAB//"]*"//LF, status)
        if (.not. status%ok()) return

        if (at_end) then
            call expand_abbreviation(status)
            if (.not. status%ok()) return
        end if

        this_command => commands%next_line
        call next_line(argument, status)
    end subroutine mail_abbreviation_next_line

    !> Expand a mail abbrev before point, then move to the end of the buffer.
    !> This only expands an abbrev (if one is present) if called with
    !> point at the end of a line, or on whitespace before the end of a line.
    !>
    !> In other respects, this behaves like `end_of_buffer', which see.
    !> Port of mailabbrev.el:mail-abbrev-end-of-buffer.
    recursive subroutine mail_abbreviation_end_of_buffer(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical :: at_end

        at_end = looking_at("[ "//TAB//"]*"//LF, status)
        if (.not. status%ok()) return

        if (at_end) then
            call expand_abbreviation(status)
            if (.not. status%ok()) return
        end if

        this_command => commands%end_of_buffer
        call end_of_buffer(argument)
    end subroutine mail_abbreviation_end_of_buffer

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine execute_define_mail_abbreviation(status)
        use minibuffer_reading_mod, only: read_string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name, definition

        call read_string("Define mail alias: ", name, status)
        if (.not. status%ok()) return
        call read_string("Define "//name//" as mail alias for: ", definition, status)
        if (status%ok()) call define_mail_abbreviation(name, definition, status)
    end subroutine execute_define_mail_abbreviation

    ! The file name argument of merge-mail-abbrevs and
    ! rebuild-mail-abbrevs, read with PROMPT.  Not a port.
    recursive subroutine read_alias_file(prompt, file, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory, def, default

        call expand_file_name("~/", directory, status)
        if (.not. status%ok()) return
        def = mail_personal_alias_file
        call expand_file_name(def, default, status, directory)
        if (.not. status%ok()) return
        call read_file_name(format_prompt(prompt, def), file, status, directory=directory, default_filename=default, &
                            mustmatch=REQUIRE_MATCH_YES)
    end subroutine read_alias_file

    recursive subroutine execute_merge_mail_abbreviations(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_alias_file("Read additional aliases from file", file, status)
        if (status%ok()) call merge_mail_abbreviations(file, status)
    end subroutine execute_merge_mail_abbreviations

    recursive subroutine execute_rebuild_mail_abbreviations(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_alias_file("Read mail aliases from file", file, status)
        if (status%ok()) call rebuild_mail_abbreviations(status, file)
    end subroutine execute_rebuild_mail_abbreviations

    recursive subroutine execute_insert_mail_abbreviation_alias(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: collection
        character(len=:), allocatable :: alias

        if (.not. mail_abbreviations_built) then
            call set_up_mail_abbreviations(status)
            if (.not. status%ok()) return
        end if

        call alias_table(collection)
        call completing_read("Expand alias: ", collection, alias, status, require_match=REQUIRE_MATCH_YES)
        if (status%ok()) call insert_mail_abbreviation_alias(status, alias)
    end subroutine execute_insert_mail_abbreviation_alias

    recursive subroutine execute_mail_abbreviation_next_line(status)
        type(status_type), intent(out) :: status

        call mail_abbreviation_next_line(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_mail_abbreviation_next_line

    recursive subroutine execute_mail_abbreviation_end_of_buffer(status)
        type(status_type), intent(out) :: status

        call mail_abbreviation_end_of_buffer(current_prefix_argument, status)
    end subroutine execute_mail_abbreviation_end_of_buffer

    !> Define Mail Abbrevs mode and register the commands; bind C-c C-a
    !> and M-TAB in `mail_mode_map' (upstream's eval-after-load of
    !> sendmail).  Not a port.
    subroutine initialize_mailabbrev()
        type(status_type) :: st

        if (.not. allocated(mail_alias_separator_string)) mail_alias_separator_string = ", "

        if (.not. allocated(mail_abbreviation_mode_regular_expression)) then
            mail_abbreviation_mode_regular_expression = "^\(Resent-\)?\(To\|From\|Cc\|Bcc\|Reply-To\|X-Debbugs-Cc\):"
        end if

        call minor_modes%mail_abbreviations_mode%define("mail_abbreviations_mode", global=.true., body=mail_abbreviations_mode_body)
        commands%mail_abbreviations_mode%execute => mail_abbreviations_mode
        commands%define_mail_abbreviation%execute => execute_define_mail_abbreviation
        commands%merge_mail_abbreviations%execute => execute_merge_mail_abbreviations
        commands%rebuild_mail_abbreviations%execute => execute_rebuild_mail_abbreviations
        commands%insert_mail_abbreviation_alias%execute => execute_insert_mail_abbreviation_alias
        commands%complete_mail_abbreviation_alias%execute => complete_mail_abbreviation_alias
        commands%mail_abbreviation_next_line%execute => execute_mail_abbreviation_next_line
        commands%mail_abbreviation_end_of_buffer%execute => execute_mail_abbreviation_end_of_buffer
        call initialize_sendmail()
        call keymap_set(mail_mode_map, "C-c C-a", "insert_mail_abbreviation_alias", st)
        ! like completion-at-point
        call keymap_set(mail_mode_map, "M-TAB", "complete_mail_abbreviation_alias", st)
    end subroutine initialize_mailabbrev
end module mail_abbreviations_mod

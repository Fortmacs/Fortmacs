! sgml_mode_fm.f90 -- SGML- and HTML-editing modes.
! Ported from GNU Emacs 31.1 lisp/textmodes/sgml-mode.el.
!
! Copyright (C) 1992, 1995-1996, 1998, 2001-2026 Free Software
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

!> SGML mode (`sgml_mode') and HTML mode (`html_mode'): the syntax tables,
!> the syntax properties of comments, declarations and quotes, Font Lock,
!> indentation (`sgml_indent_line' through `sgml_lexical_context' and
!> `sgml_get_context'), tag motion (`sgml_skip_tag_backward',
!> `sgml_skip_tag_forward'), `sgml_delete_tag', `sgml_close_tag',
!> `sgml_quote', `sgml_pretty_print', `sgml_slash', the character names
!> (`sgml_name_char', `sgml_namify_char', `sgml_name_8bit_mode'),
!> `sgml_validate', `sgml_guess_indent', `sgml_parse_dtd' and
!> `html_current_heading_name'.
!>
!> Deferred: what uses skeletons or the tag tables built on them
!> (`sgml_tag', `sgml_attributes', `sgml_auto_attributes', `sgml_value',
!> `sgml_tag_table', `sgml_tag_help', `html_tag_table' with its completion
!> at point, the `html_*' skeleton commands and `sgml_quick_keys'),
!> `sgml_tags_invisible' (cursor sensors), `sgml_electric_tag_pair_mode'
!> (text clones), `sgml_show_context', facemenu, tildify, Imenu,
!> outline and add-log variables, `sgml_comment_indent_new_line',
!> the buffer-local `adaptive_fill_regexp', `fill_nobreak_predicate' and
!> `sentence_end_base', the classes and IDs of HTML buffers (libxml),
!> yank-media handlers, `html_autoview_mode' and `browse_url_of_buffer'.
module sgml_mode_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_list, vtag, V_NAME, V_INTEGER, xsymbol, symbols
    use data_fm, only: integer_setting, logical_setting, string_setting, forward_setting, add_local_variables_killer, &
                       value_eq
    use commands_fm, only: commands
    use buffer_fm, only: buffers, cur, buffer_serial_live
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes, minor_modes
    use keymap_fm, only: keymap_set
    use keyboard_fm, only: last_command_event, this_command, sit_for
    use syntax_fm, only: make_syntax_table, modify_syntax_entry, is_syntax_table_live, syntax_ppss, parse_state_type, &
                         parse_partial_expression, forward_comment, skip_chars_forward, skip_chars_backward, skip_syntax_forward, &
                         skip_syntax_backward, syntax_table, set_syntax_table, syntax_entry_at, ST_STRING_STYLE, &
                         syntax_descriptor_type, string_to_syntax, syntax_descriptor_value
    use editfns_fm, only: point, point_min, point_max, goto_char, is_bol, is_eol, is_bob, is_eob, char_after, char_before, &
                          line_beginning_position, line_end_position, narrow_to_region, widen, buffer_substring, &
                          delete_region, following_char, preceding_char
    use cmds_fm, only: forward_line, forward_char, self_insert_command, delete_char
    use insdel_fm, only: insert
    use indent_fm, only: current_column, current_indentation, indent_line_to, indent_line_function, &
                         indent_according_to_mode
    use search_fm, only: looking_at, re_search_forward, re_search_backward, search_backward, search_forward, &
                         string_match, match_beginning, match_end, replace_match, case_fold_search, regexp_quote, &
                         SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: match_string, match_string_no_properties, looking_back
    use simple_fm, only: back_to_indentation, read_quoted_char, read_shell_command, reindent_then_newline_and_indent
    use textprop_fm, only: put_text_property, get_text_property
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use xdisp_fm, only: message
    use faces_fm, only: named_face_type, display_when, face_attrs
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                            font_lock_keywords_type, font_lock_keywords_level
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight, font_lock_prepend_text_property
    use expression_motion_fm, only: forward_expression, backward_expression, kill_expression, up_group
    use newcomment_fm, only: comment_start, comment_end, comment_start_skip, comment_end_skip, comment_column, &
                             comment_indent_function, comment_indent_type, COMMENT_INDENT_COLUMN
    use paragraphs_fm, only: paragraph_start, paragraph_separate
    use syntax_propertize_fm, only: syntax_propertize_function, syntax_propertize_rules_type, syntax_highlight_type, &
                                    syntax_property_type, syntax_ppss_table
    use regexp_opt_fm, only: regexp_opt, REGEXP_OPT_GROUP
    use undo_fm, only: undo_boundary
    use frame_fm, only: selected_window
    use window_fm, only: is_pos_visible_in_window
    use files_fm, only: save_some_buffers
    use compile_fm, only: compilation_start, compilation_ask_about_save
    use text_mode_fm, only: text_mode_syntax_table
    implicit none(type, external)
    private

    public :: syms_of_sgml_mode, sgml_mode, html_mode, sgml_indent_line, sgml_calculate_indent, sgml_lexical_context, &
              sgml_beginning_of_tag, sgml_skip_tag_backward, sgml_skip_tag_forward, sgml_delete_tag, sgml_close_tag, &
              sgml_quote, sgml_pretty_print, sgml_xml_guess, sgml_parse_tag_backward, sgml_get_context, sgml_tag_type, &
              sgml_namify_char, sgml_name_char, sgml_parse_dtd, html_current_heading_name, sgml_syntax_propertize, &
              sgml_font_lock_syntactic_face

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    !> The types of a tag (`sgml_tag_type'%type).
    integer, parameter, public :: SGML_TAG_OPEN = 1, SGML_TAG_CLOSE = 2, SGML_TAG_EMPTY = 3, SGML_TAG_COMMENT = 4, &
                                  SGML_TAG_CDATA = 5, SGML_TAG_PI = 6, SGML_TAG_DECL = 7, SGML_TAG_JSP = 8

    !> The lexical contexts of `sgml_lexical_context'.
    integer, parameter, public :: SGML_CONTEXT_TEXT = 0, SGML_CONTEXT_STRING = 1, SGML_CONTEXT_COMMENT = 2, &
                                  SGML_CONTEXT_TAG = 3, SGML_CONTEXT_CDATA = 4, SGML_CONTEXT_PI = 5

    !> Information about a tag: its type (SGML_TAG_*), start, end and name.
    !> Port of sgml-mode.el's sgml-tag structure.
    type, public :: sgml_tag_type
        integer :: type = 0
        integer(int64) :: start = 0, end = 0
        character(len=:), allocatable :: name
    end type sgml_tag_type

    !> Specifies the basic indentation level for `sgml_indent_line'.
    !> Port of sgml-mode.el sgml-basic-offset.
    type(integer_setting), target, public :: sgml_basic_offset = integer_setting(name="sgml_basic_offset", value=2)

    !> Specifies a delta for attribute indentation in `sgml_indent_line'.
    !>
    !> When 0, attribute indentation looks like this:
    !>
    !>     <element
    !>       attribute="value">
    !>     </element>
    !>
    !> When 2, attribute indentation looks like this:
    !>
    !>     <element
    !>         attribute="value">
    !>     </element>
    !>
    !> Port of sgml-mode.el sgml-attribute-offset.
    type(integer_setting), target, public :: sgml_attribute_offset = &
                                             integer_setting(name="sgml_attribute_offset", value=0)

    !> When true, tag insertion functions will be XML-compliant.
    !> It is set to be buffer-local when the file has a DOCTYPE or an XML
    !> declaration.  Port of sgml-mode.el sgml-xml-mode.
    type(logical_setting), target, public :: sgml_xml_mode = logical_setting(name="sgml_xml_mode", value=.false.)

    !> List of characters that have a special meaning for SGML mode.
    !> This list is used when first loading the `sgml_mode' library.
    !> The supported characters are ", ', and -.
    !>
    !> Including - makes double dashes into comment delimiters, but they are
    !> really only supposed to delimit comments within DTD definitions.  So
    !> we normally turn it off.  Port of sgml-mode.el sgml-specials.
    integer, allocatable, public :: sgml_specials(:)

    !> The command to validate an SGML document.
    !> The file name of current buffer file name will be appended to this,
    !> separated by a space.  Port of sgml-mode.el sgml-validate-command.
    type(string_setting), target, public :: sgml_validate_command = string_setting(name="sgml_validate_command")

    !> If positive, is the maximum distance to search for matching `/'
    !> (0 or less: no limit).  Port of sgml-mode.el sgml-slash-distance.
    type(integer_setting), target, public :: sgml_slash_distance = &
                                             integer_setting(name="sgml_slash_distance", value=1000)

    !> Array of symbolic character names without `&' and `;' ("" for
    !> none), by character code.  Port of sgml-mode.el sgml-char-names.
    character(len=6), public :: sgml_char_names(0:255) = ""

    !> List of tags whose !ELEMENT definition says EMPTY.  Port of
    !> sgml-mode.el sgml-empty-tags (its default; HTML mode's are its own).
    character(len=16), allocatable, public :: sgml_empty_tags(:)

    !> List of tags whose !ELEMENT definition says the end-tag is optional.
    !> Port of sgml-mode.el sgml-unclosed-tags (its default; HTML mode's
    !> are its own).
    character(len=16), allocatable, public :: sgml_unclosed_tags(:)

    !> List of tags where the contents shouldn't be reindented.  Port of
    !> sgml-mode.el sgml-whitespace-sensitive-tags (its default; HTML
    !> mode's are its own).
    character(len=16), allocatable, public :: sgml_whitespace_sensitive_tags(:)

    !> Tag names and face or list of faces to fontify with when invisible.
    !> When `font_lock_maximum_decoration' is 1 this is always used for
    !> fontifying.  When more these are fontified together with
    !> `sgml_font_lock_keywords'.  Port of sgml-mode.el
    !> sgml-tag-face-alist (by HTML mode's value, `html_tag_face_table').
    type, public :: sgml_tag_face_type
        character(len=16) :: tag = ""
        character(len=32) :: faces(2) = ""
    end type sgml_tag_face_type

    !> Value of `sgml_tag_face_table' for HTML mode.  Port of sgml-mode.el
    !> html-tag-face-alist.
    type(sgml_tag_face_type), allocatable, public :: html_tag_face_table(:)

    !> `sgml_mode' face used to highlight the namespace part of
    !> identifiers.  Port of sgml-mode.el:sgml-namespace.
    type(named_face_type), target, public :: sgml_namespace

    !> Hook run by command `sgml_mode'.
    !> `text_mode_hook' is run first.  Port of sgml-mode.el sgml-mode-hook.
    type(hook_type), target, public :: sgml_mode_hook = hook_type(name="sgml_mode_hook")
    !> Hook run by command `html_mode'.
    !> `text_mode_hook' and `sgml_mode_hook' are run first.  Port of
    !> sgml-mode.el html-mode-hook.
    type(hook_type), target, public :: html_mode_hook = hook_type(name="html_mode_hook")

    !> Keymap for SGML mode.  See also `sgml_specials'.  Port of
    !> sgml-mode.el sgml-mode-map.
    type(handle_type), public :: sgml_mode_map
    !> Keymap for commands for use in HTML mode.  Port of sgml-mode.el
    !> html-mode-map.
    type(handle_type), public :: html_mode_map
    !> Syntax table used in SGML mode.  See also `sgml_specials'.  Port of
    !> sgml-mode.el sgml-mode-syntax-table.
    type(handle_type), public :: sgml_mode_syntax_table
    !> Syntax table used to parse SGML tags.  Port of sgml-mode.el
    !> sgml-tag-syntax-table.
    type(handle_type), public :: sgml_tag_syntax_table

    ! One buffer's own variables: sgml-saved-validate-command, whether it
    ! is in HTML mode (its sgml-tag-face-alist, sgml-empty-tags,
    ! sgml-unclosed-tags and sgml-whitespace-sensitive-tags are HTML's) and
    ! sgml--syntax-propertize-ppss.
    type :: sgml_local_type
        integer(int64) :: serial = 0
        character(len=:), allocatable :: saved_validate_command
        logical :: html = .false.
        integer(int64) :: ppss_pos = 0
        type(parse_state_type) :: ppss
    end type sgml_local_type

    type(sgml_local_type), allocatable :: locals(:)

    ! The rules of `sgml_syntax_propertize' and the END of the current one.
    type(syntax_propertize_rules_type) :: propertize_rules
    integer(int64) :: propertize_end = 0

    ! The Unicode paired brackets (BidiBrackets.txt), punctuation in
    ! `sgml_tag_syntax_table'.
    integer, parameter :: PAIRED_BRACKETS(128) = [40, 41, 91, 93, 123, 125, 3898, 3899, 3900, 3901, 5787, 5788, &
                        8261, 8262, 8317, 8318, 8333, 8334, 8968, 8969, 8970, 8971, 9001, 9002, 10088, 10089, 10090, 10091, 10092, &
                          10093, 10094, 10095, 10096, 10097, 10098, 10099, 10100, 10101, 10181, 10182, 10214, 10215, 10216, 10217, &
                          10218, 10219, 10220, 10221, 10222, 10223, 10627, 10628, 10629, 10630, 10631, 10632, 10633, 10634, 10635, &
                          10636, 10637, 10638, 10639, 10640, 10641, 10642, 10643, 10644, 10645, 10646, 10647, 10648, 10712, 10713, &
                          10714, 10715, 10748, 10749, 11810, 11811, 11812, 11813, 11814, 11815, 11816, 11817, 11861, 11862, 11863, &
                          11864, 11865, 11866, 11867, 11868, 12296, 12297, 12298, 12299, 12300, 12301, 12302, 12303, 12304, 12305, &
                          12308, 12309, 12310, 12311, 12312, 12313, 12314, 12315, 65113, 65114, 65115, 65116, 65117, 65118, 65288, &
                                                  65289, 65339, 65341, 65371, 65373, 65375, 65376, 65378, 65379]

    ! The values of UNTIL of `sgml_get_context'.
    integer, parameter :: UNTIL_DEFAULT = 0, UNTIL_EMPTY = 1, UNTIL_NOW = 2

    ! sgml-namespace-re, sgml-name-re and sgml-tag-name-re.
    character(len=*), parameter :: SGML_NAMESPACE_RE = "[_[:alpha:]][-_.[:alnum:]]*", &
                                   SGML_NAME_RE = "[_:[:alpha:]][-_.:[:alnum:]]*", &
                                   SGML_TAG_NAME_RE = "<\([!/?]?"//SGML_NAME_RE//"\)"

    ! The regexps and data of sgml-mode.el.
    character(len=*), parameter :: SGML_CHAR_NAMES_DATA(256) = [character(len=6) :: &
                                                   "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", &
                                                    "", "", "", "", "", "", "", "", "", "", "", "", "nbsp", "excl", "quot", "num", &
                                    "dollar", "percnt", "amp", "apos", "lpar", "rpar", "ast", "plus", "comma", "hyphen", "period", &
                                                 "sol", "", "", "", "", "", "", "", "", "", "", "colon", "semi", "lt", "eq", "gt", &
                                                "quest", "commat", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", &
                                            "", "", "", "", "", "", "", "", "", "", "lsqb", "", "rsqb", "uarr", "lowbar", "lsquo", &
                                                   "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", &
                                            "", "", "", "", "", "", "lcub", "verbar", "rcub", "tilde", "", "", "", "", "", "", "", &
                                                   "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", &
                                      "", "", "", "", "", "", "nbsp", "iexcl", "cent", "pound", "curren", "yen", "brvbar", "sect", &
                                    "uml", "copy", "ordf", "laquo", "not", "shy", "reg", "macr", "ring", "plusmn", "sup2", "sup3", &
                               "acute", "micro", "para", "middot", "cedil", "sup1", "ordm", "raquo", "frac14", "frac12", "frac34", &
                                    "iquest", "Agrave", "Aacute", "Acirc", "Atilde", "Auml", "Aring", "AElig", "Ccedil", "Egrave", &
                                        "Eacute", "Ecirc", "Euml", "Igrave", "Iacute", "Icirc", "Iuml", "ETH", "Ntilde", "Ograve", &
                                 "Oacute", "Ocirc", "Otilde", "Ouml", "", "Oslash", "Ugrave", "Uacute", "Ucirc", "Uuml", "Yacute", &
                                      "THORN", "szlig", "agrave", "aacute", "acirc", "atilde", "auml", "aring", "aelig", "ccedil", &
                                        "egrave", "eacute", "ecirc", "euml", "igrave", "iacute", "icirc", "iuml", "eth", "ntilde", &
                                   "ograve", "oacute", "ocirc", "otilde", "ouml", "divide", "oslash", "ugrave", "uacute", "ucirc", &
                                                                "uuml", "yacute", "thorn", "yuml"]
    character(len=*), parameter :: SGML_START_TAG_REGEX = &
                                   "<[_:[:alpha:]][-_.:[:alnum:]]*\(?:[^_.:""'/><[:alnu" &
                                   //"m:]-]\(?:[^""'/><]\|""[^""]*""\|'[^']*'\)*\)?"
    character(len=*), parameter :: SGML_ATTRS_RE = &
                                   "\(?:[^_.:""'/><[:alnum:]-]\(?:[^""'/><]\|""[^""]*""\|'[" &
                                   //"^']*'\)*\)?"
    character(len=*), parameter :: KW1_RE = &
                                   "<\([!?][_:[:alpha:]][-_.:[:alnum:]]*\)"
    character(len=*), parameter :: KW2_RE = &
                                   "</?\([_[:alpha:]][-_.[:alnum:]]*\)\(?::\([_:[:alph" &
                                   //"a:]][-_.:[:alnum:]]*\)\)?"
    character(len=*), parameter :: KW3_RE = &
                                   "\(?:^\|[ "//TAB//"]\)\([_[:alpha:]][-_.[:alnum:]]*\)\(?::\" &
                                   //"([_:[:alpha:]][-_.:[:alnum:]]*\)\)?=[""']"
    character(len=*), parameter :: KW4_RE = &
                                   "[&%][_:[:alpha:]][-_.:[:alnum:]]*;?"
    character(len=*), parameter :: XML_GUESS_RE = &
                                   "<!DOCTYPE\s-+\(\w+\)\s-+\(\w+\)\s-+""\([^""]+\)""\s-+" &
                                   //"""\([^""]+\)"""
    character(len=*), parameter :: QUOTE_RE = &
                                   "\(&\(amp\|quot\|lt\|gt\|#\([0-9]+\|[xX][[:xdigit:]" &
                                   //"]+\)\)\)\([][<>&;"//LF//TAB//" ""%!'(),/=?]\|$\)"
    character(len=*), parameter :: SGML_PARAGRAPH_START_RE = &
                                   "[ "//TAB//"]*$\|[ "//TAB//"]*</?\([_:[:alpha:]][-_.:[:alnum:]]*\(?" &
                                   //":[^_.:""'/><[:alnum:]-]\(?:[^""'/><]\|""[^""]*""\|'[^']" &
                                   //"*'\)*\)?\)?>"
    character(len=*), parameter :: HTML_SUBPROGRAM_RE = &
                                   "<[ "//TAB//achar(13)//LF//"]*\(?:[hH][0-6]\|title\|TITLE\|Title\)[^>]*>" &
                                   //"[ "//TAB//achar(13)//LF//"]*\([^<"//achar(13)//LF//"]*[^ <"//TAB//achar(13)//LF//"]+\)"
    character(len=*), parameter :: DTD_RE = &
                                  "<!ELEMENT[ "//TAB//LF//"]+\([^ "//TAB//LF//"]+\)[ "//TAB//LF//"]+[-O][ "//TAB//LF//"]+\([-O]\)" &
                                   //"[ "//TAB//LF//"]+\([^ "//TAB//LF//"]+\)"

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `locals' (made if MAKE),
    ! 0 if none.
    ! Not a port.
    recursive integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(sgml_local_type), allocatable :: grown(:)
        integer :: n, i

        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (locals(k)%serial == buffers(cur)%serial) return
        end do

        k = 0
        if (.not. make) return

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        if (k > size(locals)) then
            n = size(locals)
            allocate (grown(n + 1))

            do i = 1, n
                grown(i)%serial = locals(i)%serial

                if (allocated(locals(i)%saved_validate_command)) then
                    call move_alloc(locals(i)%saved_validate_command, grown(i)%saved_validate_command)
                end if

                grown(i)%html = locals(i)%html
                grown(i)%ppss_pos = locals(i)%ppss_pos
                grown(i)%ppss = locals(i)%ppss
            end do

            call move_alloc(grown, locals)
            k = n + 1
        end if

        locals(k)%serial = buffers(cur)%serial
        if (allocated(locals(k)%saved_validate_command)) deallocate (locals(k)%saved_validate_command)
        locals(k)%html = .false.
        locals(k)%ppss_pos = 0
        locals(k)%ppss = parse_state_type()
    end function local_index

    ! kill-all-local-variables: forget the current buffer's sgml-mode.el
    ! variables.
    ! Not a port.
    subroutine kill_sgml_locals()
        integer :: k

        k = local_index(.false.)
        if (k > 0) locals(k)%serial = 0
    end subroutine kill_sgml_locals

    ! Whether the current buffer is in HTML mode (has its variables).
    ! Not a port.
    recursive logical function html_buffer()
        integer :: k

        html_buffer = .false.
        k = local_index(.false.)
        if (k > 0) html_buffer = locals(k)%html
    end function html_buffer

    ! The character after POS, -1 if none.
    ! Not a port.
    recursive integer function after_char_at(pos)
        integer(int64), intent(in) :: pos
        integer(int32) :: c
        logical :: found

        after_char_at = -1
        if (pos < point_min() .or. pos >= point_max()) return
        call char_after(c, found, pos)
        if (found) after_char_at = c
    end function after_char_at

    ! The character before POS, -1 if none.
    ! Not a port.
    recursive integer function before_char_at(pos)
        integer(int64), intent(in) :: pos
        integer(int32) :: c
        logical :: found

        before_char_at = -1
        if (pos <= point_min() .or. pos > point_max()) return
        call char_before(c, found, pos)
        if (found) before_char_at = c
    end function before_char_at

    ! The start of subexpression N of the last match, 0 if it didn't match.
    ! Not a port.
    integer(int64) function mb(n)
        integer, intent(in) :: n
        type(status_type) :: st

        mb = match_beginning(n, st)
    end function mb

    ! The end of subexpression N of the last match, 0 if it didn't match.
    ! Not a port.
    integer(int64) function me(n)
        integer, intent(in) :: n
        type(status_type) :: st

        me = match_end(n, st)
    end function me

    ! looking-at REGEXP.
    ! Not a port.
    recursive logical function at(regexp)
        character(len=*), intent(in) :: regexp
        type(status_type) :: st

        at = looking_at(regexp, st)
    end function at

    ! looking-at-p REGEXP.
    ! Not a port.
    recursive logical function is_at(regexp)
        character(len=*), intent(in) :: regexp
        type(status_type) :: st

        is_at = looking_at(regexp, st, inhibit_modify=.true.)
    end function is_at

    ! The text from START to END.
    ! Not a port.
    function text_between(start, end) result(text)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable :: text
        type(status_type) :: st

        call buffer_substring(start, end, text, st)
        if (.not. allocated(text)) text = ""
    end function text_between

    ! S in lower case (ASCII).
    ! Not a port.
    pure function lower(s) result(out)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: out
        integer :: i

        out = s

        do i = 1, len(s)
            if (s(i:i) >= "A" .and. s(i:i) <= "Z") out(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function lower

    ! Whether NAME is in LIST, ignoring case (assoc-string with
    ! IGNORE-CASE).
    ! Not a port.
    pure logical function member_ignore_case(name, list)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: list(:)
        integer :: i

        member_ignore_case = .false.

        do i = 1, size(list)
            if (lower(trim(list(i))) == lower(name)) member_ignore_case = .true.
        end do
    end function member_ignore_case

    ! The current buffer's sgml-empty-tags.
    ! Not a port.
    recursive function empty_tags() result(tags)
        character(len=16), allocatable :: tags(:)

        if (html_buffer()) then
            ! From HTML-4.01's loose.dtd, parsed with `sgml-parse-dtd', plus
            ! manual additions of "source" and "wbr".
            tags = [character(len=16) :: "area", "base", "basefont", "br", "col", "frame", "hr", "img", "input", &
                    "isindex", "link", "meta", "source", "param", "wbr"]
        else
            tags = sgml_empty_tags
        end if
    end function empty_tags

    ! The current buffer's sgml-unclosed-tags.
    ! Not a port.
    recursive function unclosed_tags() result(tags)
        character(len=16), allocatable :: tags(:)

        if (html_buffer()) then
            ! From HTML-4.01's loose.dtd, parsed with `sgml-parse-dtd'.
            tags = [character(len=16) :: "body", "colgroup", "dd", "dt", "head", "html", "li", "option", "p", &
                    "tbody", "td", "tfoot", "th", "thead", "tr"]
        else
            tags = sgml_unclosed_tags
        end if
    end function unclosed_tags

    ! The current buffer's sgml-whitespace-sensitive-tags.
    ! Not a port.
    recursive function whitespace_sensitive_tags() result(tags)
        character(len=16), allocatable :: tags(:)

        if (html_buffer()) then
            tags = [character(len=16) :: "pre", "textarea"]
        else
            tags = sgml_whitespace_sensitive_tags
        end if
    end function whitespace_sensitive_tags

    ! with-syntax-table sgml-tag-syntax-table: switch to it, saving the
    ! current table in SAVED.
    ! Not a port.
    recursive subroutine use_tag_table(saved)
        type(handle_type), intent(out) :: saved
        type(status_type) :: st

        saved = syntax_table()
        call set_syntax_table(sgml_tag_syntax_table, st)
    end subroutine use_tag_table

    ! Restore the syntax table SAVED.
    ! Not a port.
    recursive subroutine restore_table(saved)
        type(handle_type), intent(in) :: saved
        type(status_type) :: st

        call set_syntax_table(saved, st)
    end subroutine restore_table

    ! save-excursion: remember point in the marker MARK.
    ! Not a port.
    subroutine excursion_begin(mark)
        type(handle_type), intent(out) :: mark
        type(status_type) :: st

        call copy_marker_at(point(), mark, .false., st)
    end subroutine excursion_begin

    ! Return to the position of MARK, and free it.
    ! Not a port.
    subroutine excursion_end(mark)
        type(handle_type), intent(in) :: mark
        type(status_type) :: st
        integer(int64) :: pos

        call marker_position(mark, pos, st)
        call goto_char(pos)
        call free_marker(mark, st)
    end subroutine excursion_end

    ! sgml-forward-sexp: (forward-sexp N) with forward-sexp-function nil.
    ! Port of sgml-mode.el:sgml-forward-sexp.
    recursive subroutine sgml_forward_expression(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status

        call forward_expression(n, status)
    end subroutine sgml_forward_expression

    ! (string-to-syntax S) as a text property value.
    ! Not a port.
    recursive function syntax_value(s) result(v)
        character(len=*), intent(in) :: s
        type(value_type) :: v
        type(syntax_descriptor_type) :: d
        type(status_type) :: st

        call string_to_syntax(s, d, st)
        v = syntax_descriptor_value(d)
    end function syntax_value

    ! ------------------------------------------------------------------
    ! Syntax properties and Font Lock
    ! ------------------------------------------------------------------

    !> `font_lock_syntactic_face_function' for `sgml_mode'.  Port of
    !> sgml-mode.el:sgml-font-lock-syntactic-face.
    subroutine sgml_font_lock_syntactic_face(state, face)
        type(parse_state_type), intent(in) :: state
        character(len=:), allocatable, intent(out) :: face
        logical :: in_paren

        face = ""
        ! Don't use string face outside of tags.
        in_paren = .false.
        if (allocated(state%open_parens)) in_paren = size(state%open_parens) > 0

        if (in_paren .and. state%instring >= 0) then
            face = "font_lock_string_face"
        else if (state%incomment /= 0) then
            face = "font_lock_comment_face"
        end if
    end subroutine sgml_font_lock_syntactic_face

    ! Return PPSS at POS, fixing the syntax of any lone `>' along the way.
    ! Port of sgml-mode.el:sgml--syntax-propertize-ppss.
    recursive function syntax_propertize_ppss(pos) result(state)
        integer(int64), intent(in) :: pos
        type(parse_state_type) :: state, old
        type(status_type) :: st
        integer :: k

        k = local_index(.true.)
        old = locals(k)%ppss
        call parse_partial_expression(locals(k)%ppss_pos, pos, state, st, targetdepth=-1_int64, oldstate=old)

        do while (state%depth == -1)
            call put_text_property(point() - 1, point(), value_type(name="syntax_table"), syntax_value("."), st)
            ! Hack attack: rather than recompute the ppss from
            ! (car sgml--syntax-propertize-ppss), we manually "fix it".
            state%depth = 0
            old = state
            call parse_partial_expression(point(), pos, state, st, targetdepth=-1_int64, oldstate=old)
        end do

        locals(k)%ppss = state
        locals(k)%ppss_pos = pos
    end function syntax_propertize_ppss

    !> Syntactic keywords for `sgml_mode'.  Port of
    !> sgml-mode.el:sgml-syntax-propertize.
    recursive subroutine sgml_syntax_propertize(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state
        integer :: k

        status = status_type()
        k = local_index(.true.)
        call goto_char(start)
        call syntax_ppss(state, start)
        locals(k)%ppss_pos = start
        locals(k)%ppss = state
        call goto_char(start)
        if (state%depth < 0) return
        call syntax_propertize_inside(end)
        propertize_end = end
        call propertize_rules%apply(point(), end, status)
        ! Catch any '>' after the last quote.
        state = syntax_propertize_ppss(end)
    end subroutine sgml_syntax_propertize

    ! Inside a declaration or processing instruction, put the closing
    ! fence before END.  Port of sgml-mode.el:sgml-syntax-propertize-inside.
    recursive subroutine syntax_propertize_inside(end)
        integer(int64), intent(in) :: end
        type(parse_state_type) :: state
        type(status_type) :: st
        character(len=:), allocatable :: endre
        integer(int64) :: opoint, pos

        call syntax_ppss(state)
        if (state%instring /= ST_STRING_STYLE) return
        opoint = point()
        call goto_char(state%comstr_start)

        if (is_at("<!\[CDATA\[")) then
            endre = "]]>"
        else if (is_at("<\?")) then
            endre = ">"
            if (sgml_xml_mode%value) endre = "\?>"
        else
            endre = ">"
        end if

        call goto_char(opoint)
        call re_search_forward(endre, pos, st, bound=end, noerror=SEARCH_MOVE)
        if (pos > 0) call put_text_property(point() - 1, point(), value_type(name="syntax_table"), syntax_value("|<"), st)
    end subroutine syntax_propertize_inside

    ! The (prog1 "|>" (sgml-syntax-propertize-inside end)) of a declaration.
    ! Port of sgml-mode.el:sgml-syntax-propertize-rules (in part).
    recursive subroutine declaration_after(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call syntax_propertize_inside(propertize_end)
    end subroutine declaration_after

    ! The value of the quote rule: quotes outside of tags should not
    ! introduce strings which end up hiding tags.
    ! Port of sgml-mode.el:sgml-syntax-propertize-rules (in part).
    recursive subroutine quote_value(property, status)
        type(syntax_property_type), intent(out) :: property
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state
        type(status_type) :: st
        integer(int64) :: opoint, moved

        status = status_type()
        property%apply = .false.

        if (after_char_at(point()) == after_char_at(mb(0))) then
            ! Fast-track case.
            call forward_char(st, 1_int64)
            return
        end if

        ! Point has moved to the end of the text we matched after the
        ! quote, but this risks overlooking a match to one of the other
        ! regexp in the rules.  We could just (goto-char (match-end 1)) to
        ! solve this, but that would be too easy, so instead we only move
        ! back enough to avoid skipping comment ender, which happens to be
        ! the only one that we could have overlooked.
        if (after_char_at(point()) == iachar(">")) moved = skip_chars_backward("-", st)
        ! Be careful to call `syntax-ppss' on a position before the one
        ! we're going to change, so as not to need to flush the data we just
        ! computed.
        opoint = point()
        state = syntax_propertize_ppss(mb(0))
        call goto_char(opoint)

        if (state%depth == 0) then
            property%apply = .true.
            call string_to_syntax(".", property%descriptor, st)
        end if
    end subroutine quote_value

    ! A highlight of subexpression N in FACE.
    ! Not a port.
    function h(n, face, lax) result(hl)
        integer, intent(in) :: n
        character(len=*), intent(in) :: face
        logical, intent(in), optional :: lax
        type(font_lock_highlight_type) :: hl

        hl = font_lock_highlight(n, face=face, laxmatch=lax)
    end function h

    ! The face of a name with a namespace part (sgml-namespace) or not.
    ! Port of sgml-mode.el:sgml-font-lock-keywords-1 (in part).
    subroutine function_name_face(face)
        character(len=:), allocatable, intent(out) :: face

        face = "font_lock_function_name_face"
        if (me(2) > 0) face = "sgml_namespace"
    end subroutine function_name_face

    ! Port of sgml-mode.el:sgml-font-lock-keywords-1 (in part).
    subroutine variable_name_face(face)
        character(len=:), allocatable, intent(out) :: face

        face = "font_lock_variable_name_face"
        if (me(2) > 0) face = "sgml_namespace"
    end subroutine variable_name_face

    ! sgml-font-lock-keywords-1.
    ! Port of sgml-mode.el:sgml-font-lock-keywords-1.
    subroutine keywords_1(kw)
        type(font_lock_keyword_type), allocatable, intent(out) :: kw(:)
        type(font_lock_highlight_type) :: h1(1), h2(2)

        allocate (kw(4))
        h1(1) = h(1, "font_lock_keyword_face")
        kw(1) = font_lock_keyword(KW1_RE, highlights=h1)
        h2(1) = font_lock_highlight(1, face_function=function_name_face)
        h2(2) = h(2, "font_lock_function_name_face", lax=.true.)
        kw(2) = font_lock_keyword(KW2_RE, highlights=h2)
        ! FIXME: this doesn't cover the variables using a default value.
        h2(1) = font_lock_highlight(1, face_function=variable_name_face)
        h2(2) = h(2, "font_lock_variable_name_face", lax=.true.)
        kw(3) = font_lock_keyword(KW3_RE, highlights=h2)
        h1(1) = h(0, "font_lock_variable_name_face")
        kw(4) = font_lock_keyword(KW4_RE, highlights=h1)
    end subroutine keywords_1

    ! The faces of the tag `match_string(1)' in `sgml_tag_face_table'
    ! (its contents get them, prepended).
    ! Port of sgml-mode.el:sgml-font-lock-keywords-2 (in part).
    subroutine tag_faces(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(value_type) :: faces(2)
        character(len=:), allocatable :: tag
        integer :: i, j, n

        status = status_type()
        if (.not. html_buffer()) return
        tag = lower(match_string_no_properties(1))

        do i = 1, size(html_tag_face_table)
            if (trim(html_tag_face_table(i)%tag) /= tag) cycle
            n = 0

            do j = 1, 2
                if (len_trim(html_tag_face_table(i)%faces(j)) == 0) cycle
                n = n + 1
                faces(n) = value_type(name=trim(html_tag_face_table(i)%faces(j)))
            end do

            if (n == 1) then
                call font_lock_prepend_text_property(start, end, value_type(name="face"), faces(1), status)
            else
                call font_lock_prepend_text_property(start, end, value_type(name="face"), make_list(faces(1:n)), status)
            end if

            return
        end do
    end subroutine tag_faces

    ! No face (the face of the tag contents comes from `tag_faces').
    ! Not a port.
    subroutine no_face(face)
        character(len=:), allocatable, intent(out) :: face

        face = ""
    end subroutine no_face

    ! sgml-font-lock-keywords-2, built when Font Lock sets up.
    ! Port of sgml-mode.el:sgml-font-lock-keywords-2.
    subroutine keywords_2(kw)
        type(font_lock_keyword_type), allocatable, intent(out) :: kw(:)
        type(font_lock_keyword_type), allocatable :: k1(:)
        type(font_lock_highlight_type) :: h1(1)
        character(len=16), allocatable :: tags(:)
        integer :: i

        call keywords_1(k1)

        if (.not. html_buffer()) then
            allocate (tags(0))
        else
            allocate (character(len=16) :: tags(size(html_tag_face_table)))

            do i = 1, size(html_tag_face_table)
                tags(i) = html_tag_face_table(i)%tag
            end do
        end if

        h1(1) = font_lock_highlight(3, face_function=no_face, properties_function=tag_faces)
        allocate (kw(5))
        kw(1:4) = k1
        kw(5) = font_lock_keyword("<"//regexp_opt(tags, REGEXP_OPT_GROUP)//"\([ "//TAB//"][^>]*\)?>\([^<]+\)</\1>", highlights=h1)
    end subroutine keywords_2

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Guess whether the current buffer is XML.  Return true if so.
    !> Port of sgml-mode.el:sgml-xml-guess.
    recursive logical function sgml_xml_guess() result(xml)
        type(status_type) :: st
        character(len=:), allocatable :: file, ext
        integer(int64) :: opoint, pos, k

        opoint = point()
        call goto_char(point_min())
        file = ""
        if (allocated(buffers(cur)%filename)) file = buffers(cur)%filename
        ext = extension(file)
        xml = ext == "xml"
        ! Maybe the buffer-size check isn't needed, I don't know.
        if (.not. xml) xml = point_max() == point_min() .and. ext == "xhtml"
        if (.not. xml) xml = at("\s-*<\?xml")

        if (.not. xml) then
            call re_search_forward(XML_GUESS_RE, pos, st, noerror=SEARCH_NOERROR)

            if (pos > 0) then
                k = string_match("X\(HT\)?ML", match_string_no_properties(3), st)
                xml = k > 0
            end if
        end if

        call goto_char(opoint)

    contains

        ! file-name-extension.
        function extension(name) result(e)
            character(len=*), intent(in) :: name
            character(len=:), allocatable :: e
            integer :: slash, dot

            e = ""
            slash = index(name, "/", back=.true.)
            dot = index(name(slash + 1:), ".", back=.true.)
            if (dot > 1) e = name(slash + dot + 1:)
        end function extension
    end function sgml_xml_guess

    ! The `comment_indent_function' of SGML mode.  Port of
    ! sgml-mode.el:sgml-comment-indent.
    recursive subroutine sgml_comment_indent(indent, status)
        type(comment_indent_type), intent(out) :: indent
        type(status_type), intent(out) :: status

        status = status_type()
        indent%kind = COMMENT_INDENT_COLUMN

        if (at("--")) then
            indent%column = comment_column%value
        else
            indent%column = 0
        end if
    end subroutine sgml_comment_indent

    !> Insert ARG slash characters.  Port of sgml-mode.el:sgml-slash
    !> (`sgml_quick_keys' is deferred, so this is `sgml_slash_matching').
    recursive subroutine sgml_slash(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        call sgml_slash_matching(arg, status)
    end subroutine sgml_slash

    !> Insert `/' and display any previous matching `/'.
    !> Two `/'s are treated as matching if the first `/' ends a
    !> net-enabling start tag, and the second `/' is the corresponding null
    !> end tag.  Port of sgml-mode.el:sgml-slash-matching.
    recursive subroutine sgml_slash_matching(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: oldpos, blinkpos, level, pos, tagend, begv, zv, opoint

        call insert(repeat("/", int(max(arg, 0_int64))), status)
        if (.not. status%ok() .or. arg <= 0) return
        oldpos = point()
        blinkpos = 0
        level = 0
        opoint = point()
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv

        if (sgml_slash_distance%value > 0) then
            call narrow_to_region(max(point_min(), point() - sgml_slash_distance%value), oldpos, st)
        end if

        call re_search_backward(SGML_START_TAG_REGEX, pos, st, bound=point_min(), noerror=SEARCH_NOERROR)

        if (.not. (pos > 0 .and. me(0) == oldpos - 1)) then
            call goto_char(oldpos - 1)

            do while (blinkpos == 0)
                call search_backward("/", pos, st, bound=point_min(), noerror=SEARCH_NOERROR)
                if (pos == 0) exit
                tagend = 0
                pos = point()
                call re_search_backward(SGML_START_TAG_REGEX, tagend, st, bound=point_min(), noerror=SEARCH_NOERROR)
                if (tagend > 0) tagend = me(0)
                call goto_char(pos)

                if (tagend == point()) then
                    if (level == 0) then
                        blinkpos = point()
                    else
                        level = level - 1
                    end if
                else
                    level = level + 1
                end if
            end do
        end if

        call widen()
        call narrow_to_region(begv, zv, st)

        if (blinkpos > 0) then
            call goto_char(blinkpos)

            if (is_pos_visible_in_window(point(), selected_window())) then
                call sit_for(1000_int32)
            else
                call message("Matches "//text_between(line_beginning_position(), blinkpos + 1))
            end if
        end if

        call goto_char(opoint)
    end subroutine sgml_slash_matching

    !> Insert a symbolic character name according to `sgml_char_names'
    !> (CHAR, read when absent).  Port of sgml-mode.el:sgml-name-char.
    recursive subroutine sgml_name_char(status, char)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: char
        integer(int64) :: c

        call insert("&", status)
        if (.not. status%ok()) return

        if (present(char)) then
            c = char
        else
            call read_quoted_char(c, status)
            if (.not. status%ok()) return
        end if

        call delete_char(-1_int64, status)
        call insert(char_string(int(c)), status)
        call undo_boundary()
        call sgml_namify_char(status)
    end subroutine sgml_name_char

    ! The UTF-8 text of the character C.
    ! Not a port.
    function char_string(c) result(s)
        integer, intent(in) :: c
        character(len=:), allocatable :: s

        if (c < 128) then
            s = achar(c)
        else if (c < 2048) then
            s = achar(192 + c/64)//achar(128 + mod(c, 64))
        else if (c < 65536) then
            s = achar(224 + c/4096)//achar(128 + mod(c/64, 64))//achar(128 + mod(c, 64))
        else
            s = achar(240 + c/262144)//achar(128 + mod(c/4096, 64))//achar(128 + mod(c/64, 64))//achar(128 + mod(c, 64))
        end if
    end function char_string

    !> Change the char before point into its `&name;' equivalent.
    !> Uses `sgml_char_names'.  Port of sgml-mode.el:sgml-namify-char
    !> (characters beyond Latin-1 get their number).
    recursive subroutine sgml_namify_char(status)
        type(status_type), intent(out) :: status
        character(len=16) :: buf
        integer :: c

        status = status_type()
        c = before_char_at(point())

        if (c < 0) then
            call set_error(status, FM_GENERIC_ERROR, "No char before point")
            return
        end if

        call delete_char(-1_int64, status)

        if (c < 256) then
            if (len_trim(sgml_char_names(c)) > 0) then
                call insert("&"//trim(sgml_char_names(c))//";", status)
                return
            end if
        end if

        write (buf, "(i0)") c
        call insert("&#"//trim(buf)//";", status)
    end subroutine sgml_namify_char

    ! The command `sgml_namify_char'.
    recursive subroutine cmd_sgml_namify_char(status)
        type(status_type), intent(out) :: status

        call sgml_namify_char(status)
    end subroutine cmd_sgml_namify_char

    ! The command `sgml_name_char'.
    recursive subroutine cmd_sgml_name_char(status)
        use buffer_fm, only: barf_if_buffer_read_only
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call sgml_name_char(status)
    end subroutine cmd_sgml_name_char

    ! The event typed, as a character (-1 if not one).
    ! Not a port.
    recursive integer(int64) function event_char()
        use lisp_fm, only: xfixnum

        event_char = -1
        if (vtag(last_command_event) == V_INTEGER) event_char = xfixnum(last_command_event)
    end function event_char

    !> Insert a symbolic character name according to `sgml_char_names'.
    !> Port of sgml-mode.el:sgml-name-self.
    recursive subroutine sgml_name_self(status)
        type(status_type), intent(out) :: status

        call sgml_name_char(status, event_char())
    end subroutine sgml_name_self

    !> Insert a symbolic character name according to `sgml_char_names'
    !> when `sgml_name_8bit_mode' is on.  Port of
    !> sgml-mode.el:sgml-maybe-name-self.
    recursive subroutine sgml_maybe_name_self(status)
        type(status_type), intent(out) :: status

        if (minor_modes%sgml_name_8bit_mode%state%value) then
            call sgml_name_char(status, event_char())
        else
            call self_insert_command(1_int64, event_char(), status)
        end if
    end subroutine sgml_maybe_name_self

    !> Name self unless in position to end a tag or a prefix ARG is given.
    !> Port of sgml-mode.el:sgml-maybe-end-tag.
    recursive subroutine sgml_maybe_end_tag(status)
        use callint_fm, only: current_prefix_arg, prefix_numeric_value, PREFIX_NONE
        type(status_type), intent(out) :: status
        integer(int64) :: start
        integer :: ctype

        if (current_prefix_arg%kind /= PREFIX_NONE) then
            call self_insert_command(prefix_numeric_value(current_prefix_arg), event_char(), status)
            return
        end if

        call sgml_lexical_context(ctype, start)

        if (ctype == SGML_CONTEXT_TAG) then
            call self_insert_command(1_int64, event_char(), status)
        else
            call sgml_name_self(status)
        end if
    end subroutine sgml_maybe_end_tag

    !> Skip to beginning of tag or matching opening tag if present.
    !> With prefix argument ARG, repeat this ARG times.
    !> Return true if we skipped over matched tags.  Port of
    !> sgml-mode.el:sgml-skip-tag-backward.
    recursive logical function sgml_skip_tag_backward(arg) result(skipped)
        integer(int64), intent(in) :: arg
        type(status_type) :: st
        character(len=:), allocatable :: re
        integer(int64) :: n, pos
        logical :: saved_fold, dummy

        skipped = .true.
        n = arg

        do while (n >= 1)
            call search_backward("<", pos, st, noerror=SEARCH_NOERROR)

            if (at("</\([^ "//LF//TAB//">]+\)")) then
                ! end tag, skip any nested pairs
                saved_fold = case_fold_search%value
                case_fold_search%value = .true.
                ! Ignore empty tags like <foo/>.
                re = "</?"//regexp_quote(match_string(1))//"\([^>]*[^/>]\)?>"

                do
                    call re_search_backward(re, pos, st, noerror=SEARCH_NOERROR)
                    if (pos == 0) exit
                    if (after_char_at(point() + 1) /= iachar("/")) exit
                    call forward_char(st, 1_int64)
                    dummy = sgml_skip_tag_backward(1_int64)
                end do

                case_fold_search%value = saved_fold
            else
                skipped = .false.
            end if

            n = n - 1
        end do
    end function sgml_skip_tag_backward

    ! The command `sgml_skip_tag_backward'.
    recursive subroutine cmd_sgml_skip_tag_backward(status)
        use callint_fm, only: current_prefix_arg, prefix_numeric_value
        type(status_type), intent(out) :: status
        logical :: skipped

        status = status_type()
        skipped = sgml_skip_tag_backward(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_sgml_skip_tag_backward

    !> Skip to end of tag or matching closing tag if present.
    !> With prefix argument ARG, repeat this ARG times.
    !> Return true if after a closing tag.  Port of
    !> sgml-mode.el:sgml-skip-tag-forward.
    recursive logical function sgml_skip_tag_forward(arg) result(after_close)
        integer(int64), intent(in) :: arg
        type(handle_type) :: saved
        type(status_type) :: st
        character(len=:), allocatable :: re
        integer(int64) :: n, pos, moved, here
        logical :: saved_fold, closing

        after_close = .true.
        call use_tag_table(saved)
        n = arg

        do while (n >= 1)
            moved = skip_chars_forward("^<>", st)
            if (following_char() == iachar(">")) call up_group(-1_int64, st)

            if (at("<\([^/ "//LF//TAB//">]+\)\([^>]*[^/>]\)?>")) then
                ! start tag, skip any nested same pairs _and_ closing tag
                saved_fold = case_fold_search%value
                case_fold_search%value = .true.
                ! Ignore empty tags like <foo/>.
                re = "</?"//regexp_quote(match_string(1))//"\([^>]*[^/>]\)?>"
                call sgml_forward_expression(1_int64, st)
                here = point()
                closing = .false.

                ! FIXME: This re-search-forward will mistakenly match
                ! tag-like text inside attributes.
                do
                    call re_search_forward(re, pos, st, noerror=SEARCH_NOERROR)
                    if (pos == 0) exit
                    closing = after_char_at(mb(0) + 1) == iachar("/")
                    if (closing) exit
                    call goto_char(mb(0))
                    if (.not. sgml_skip_tag_forward(1_int64)) exit
                    closing = .false.
                end do

                case_fold_search%value = saved_fold

                if (.not. closing) then
                    call goto_char(here)
                    after_close = .false.
                end if
            else
                call sgml_forward_expression(1_int64, st)
            end if

            n = n - 1
        end do

        call restore_table(saved)
    end function sgml_skip_tag_forward

    ! The command `sgml_skip_tag_forward'.
    recursive subroutine cmd_sgml_skip_tag_forward(status)
        use callint_fm, only: current_prefix_arg, prefix_numeric_value
        type(status_type), intent(out) :: status
        logical :: skipped

        status = status_type()
        skipped = sgml_skip_tag_forward(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_sgml_skip_tag_forward

    ! Return t if the text before point matches STR.  Port of
    ! sgml-mode.el:sgml-looking-back-at.
    recursive logical function looking_back_at(str)
        character(len=*), intent(in) :: str
        integer(int64) :: start

        looking_back_at = .false.
        start = point() - len(str)
        if (start < point_min()) return
        looking_back_at = text_between(start, point()) == str
    end function looking_back_at

    !> Delete tag on or after cursor, and matching closing or opening tag.
    !> With prefix argument ARG, repeat this ARG times.  Port of
    !> sgml-mode.el:sgml-delete-tag.
    recursive subroutine sgml_delete_tag(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(handle_type) :: mark
        character(len=:), allocatable :: name
        integer(int64) :: n, close_pos, open_pos, here, shortage, tag_pos
        logical :: dummy

        status = status_type()
        n = arg

        do while (n >= 1)
            call excursion_begin(mark)
            close_pos = 0

            if (at("[ "//TAB//LF//"]*<")) then
                ! just before tag
                if (after_char_at(me(0)) == iachar("/")) then
                    ! closing tag
                    close_pos = point()
                    call goto_char(me(0))
                end if
            else
                ! on tag?
                here = point()
                call sgml_beginning_of_tag(name, .false.)

                if (len(name) > 0) then
                    if (name(1:1) == "/") close_pos = point()
                end if

                call goto_char(here)

                if (close_pos == 0) then
                    ! not on closing tag
                    dummy = sgml_skip_tag_backward(1_int64)

                    if (following_char() /= iachar("<")) then
                        call not_on_tag()
                        return
                    end if

                    tag_pos = point()
                    call sgml_forward_expression(1_int64, status)
                    if (.not. status%ok()) return

                    if (point() <= here) then
                        call not_on_tag()
                        return
                    end if

                    call goto_char(tag_pos)
                end if
            end if

            if (close_pos > 0) then
                dummy = sgml_skip_tag_backward(1_int64)
                open_pos = point()
                call goto_char(close_pos)
                call kill_expression(1_int64, status)
            else
                open_pos = point()

                if (sgml_skip_tag_forward(1_int64)) then
                    if (.not. looking_back_at("/>")) call kill_expression(-1_int64, status)
                end if
            end if

            if (.not. status%ok()) return
            ! Delete any resulting empty line.  If we didn't kill-sexp, this
            ! *should* do nothing, because we're right after the tag.
            shortage = forward_line(0_int64)
            if (at("\(?:[ "//TAB//"]*$\)"//LF//"?")) call delete_region(mb(0), me(0), status)
            call goto_char(open_pos)
            call kill_expression(1_int64, status)
            if (.not. status%ok()) return
            shortage = forward_line(0_int64)
            if (at("\(?:[ "//TAB//"]*$\)"//LF//"?")) call delete_region(mb(0), me(0), status)
            call excursion_end(mark)
            n = n - 1
        end do

    contains

        ! (error "Not on or before tag"), leaving point where it was.
        subroutine not_on_tag()
            call excursion_end(mark)
            call set_error(status, FM_GENERIC_ERROR, "Not on or before tag")
        end subroutine not_on_tag
    end subroutine sgml_delete_tag

    ! The command `sgml_delete_tag'.
    recursive subroutine cmd_sgml_delete_tag(status)
        use callint_fm, only: current_prefix_arg, prefix_numeric_value
        type(status_type), intent(out) :: status

        call sgml_delete_tag(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_sgml_delete_tag

    !> Validate an SGML document.
    !> Runs COMMAND, a shell command, in a separate process asynchronously
    !> with output going to the buffer `*compilation*'.  You can then use
    !> the command \[next_error] to find the next error message and move to
    !> the line in the SGML document that caused it.  Port of
    !> sgml-mode.el:sgml-validate.
    recursive subroutine sgml_validate(command, status)
        character(len=*), intent(in) :: command
        type(status_type), intent(out) :: status
        integer :: k

        k = local_index(.true.)
        locals(k)%saved_validate_command = command
        call save_some_buffers(.not. compilation_ask_about_save%value, .false., status)
        if (.not. status%ok()) return
        call compilation_start(command, status)
    end subroutine sgml_validate

    ! The command `sgml_validate': read the command.
    recursive subroutine cmd_sgml_validate(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: initial, command, name
        integer :: k, slash

        k = local_index(.true.)

        if (allocated(locals(k)%saved_validate_command)) then
            initial = locals(k)%saved_validate_command
        else
            initial = ""
            if (allocated(sgml_validate_command%value)) initial = sgml_validate_command%value

            if (allocated(buffers(cur)%filename)) then
                name = buffers(cur)%filename
                slash = index(name, "/", back=.true.)
                initial = initial//" "//shell_quote(name(slash + 1:))
            end if
        end if

        call read_shell_command("Validate command: ", command, status, initial=initial)
        if (.not. status%ok()) return
        call sgml_validate(command, status)
    end subroutine cmd_sgml_validate

    ! S quoted for the shell (shell-quote-argument).
    ! Not a port.
    function shell_quote(s) result(q)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: q
        integer :: i

        q = ""

        do i = 1, len(s)
            if (index("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./@%+=:,", s(i:i)) == 0) then
                q = q//"\"//s(i:i)
            else
                q = q//s(i:i)
            end if
        end do

        if (len(s) == 0) q = "''"
    end function shell_quote

    ! ------------------------------------------------------------------
    ! Parsing
    ! ------------------------------------------------------------------

    ! Return t if point is at the first non-whitespace character on the
    ! line.  Port of sgml-mode.el:sgml-at-indentation-p.
    recursive logical function is_at_indentation()
        type(status_type) :: st
        integer(int64) :: opoint, moved

        opoint = point()
        moved = skip_chars_backward(" "//TAB, st)
        is_at_indentation = is_bol()
        call goto_char(opoint)
    end function is_at_indentation

    !> Return the lexical context at point as (TYPE . START): CTYPE, an
    !> SGML_CONTEXT_* value, and START, the location of the start of the
    !> lexical element.
    !>
    !> Optional argument LIMIT is the position to start parsing from.  If
    !> absent, start from a preceding tag at indentation.  Port of
    !> sgml-mode.el:sgml-lexical-context.
    recursive subroutine sgml_lexical_context(ctype, start, limit)
        integer, intent(out) :: ctype
        integer(int64), intent(out) :: start
        integer(int64), intent(in), optional :: limit
        type(handle_type) :: saved
        type(parse_state_type) :: state
        type(sgml_tag_type) :: tag
        type(status_type) :: st
        character(len=:), allocatable :: skip, end_skip
        integer(int64) :: opoint, pos, text_start, found, moved
        logical :: ok
        ! The kind of STATE: none (nil), a parse, an unclosed cdata,
        ! comment or pi.
        integer, parameter :: NONE = 0, PARSED = 1, CDATA = 2, COMMENT = 3, PI = 4
        integer :: kind

        opoint = point()
        pos = point()
        text_start = point()
        kind = NONE
        skip = ""
        if (allocated(comment_start_skip%value)) skip = comment_start_skip%value
        end_skip = ""
        if (allocated(comment_end_skip%value)) end_skip = comment_end_skip%value

        if (present(limit)) then
            call goto_char(limit)
        else
            ! Skip tags backwards until we find one at indentation
            do
                call sgml_parse_tag_backward(tag, ok)
                if (.not. ok) exit
                if (is_at_indentation()) exit
            end do
        end if

        call use_tag_table(saved)

        do while (point() < pos)
            ! When entering this loop we're inside text.
            text_start = point()
            moved = skip_chars_forward("^<", st, pos)

            if (point() == pos) then
                ! We got to the end without seeing a tag.
                kind = NONE
            else if (at("<!\[[A-Z]+\[")) then
                ! We've found a CDATA section or similar.
                start = point()
                kind = NONE
                call search_forward("]]>", found, st, bound=pos, noerror=SEARCH_MOVE)
                if (found == 0) kind = CDATA
            else if (at(skip)) then
                ! parse-partial-sexp doesn't handle <!-- comments -->, or
                ! only if ?- is in sgml-specials, so match explicitly
                start = point()
                kind = NONE
                call re_search_forward(end_skip, found, st, bound=pos, noerror=SEARCH_MOVE)
                if (found == 0) kind = COMMENT
            else if (sgml_xml_mode%value .and. at("<\?")) then
                ! Processing Instructions.
                start = point()
                kind = NONE
                call search_forward("?>", found, st, bound=pos, noerror=SEARCH_MOVE)
                if (found == 0) kind = PI
            else
                ! We've reached a tag.  Parse it.
                ! FIXME: Handle net-enabling start-tags
                call parse_partial_expression(point(), pos, state, st, targetdepth=0_int64)
                kind = PARSED
            end if
        end do

        call restore_table(saved)
        call goto_char(opoint)

        select case (kind)
        case (CDATA)
            ctype = SGML_CONTEXT_CDATA
        case (PI)
            ctype = SGML_CONTEXT_PI
        case (COMMENT)
            ctype = SGML_CONTEXT_COMMENT
        case (PARSED)
            if (state%instring >= 0) then
                ctype = SGML_CONTEXT_STRING
                start = state%comstr_start
            else if (state%incomment /= 0) then
                ctype = SGML_CONTEXT_COMMENT
                start = state%comstr_start
            else if (state%depth > 0) then
                ctype = SGML_CONTEXT_TAG
                start = state%innermost_start
            else
                ctype = SGML_CONTEXT_TEXT
                start = text_start
            end if
        case default
            ctype = SGML_CONTEXT_TEXT
            start = text_start
        end select
    end subroutine sgml_lexical_context

    !> Skip to beginning of tag and return its name ("" if this can't be
    !> done).  Port of sgml-mode.el:sgml-beginning-of-tag.
    recursive subroutine sgml_beginning_of_tag(name, only_immediate)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(in) :: only_immediate
        integer(int64) :: start
        integer :: ctype

        name = ""
        call sgml_lexical_context(ctype, start)

        if (ctype == SGML_CONTEXT_TAG) then
            call goto_char(start)
            if (at(SGML_TAG_NAME_RE)) name = match_string_no_properties(1)
        else if (.not. only_immediate .and. ctype /= SGML_CONTEXT_TEXT) then
            call goto_char(start)
            call sgml_beginning_of_tag(name, .true.)
        end if
    end subroutine sgml_beginning_of_tag

    !> Quote SGML text in region START ... END.
    !> Only &, <, >, ' and " characters are quoted, the rest is left
    !> untouched.  This is sufficient to use quoted text as SGML argument.
    !>
    !> With UNQUOTEP, unquote the region.  All numeric entities, "amp",
    !> "lt", "gt" and "quot" named entities are unquoted.  Port of
    !> sgml-mode.el:sgml-quote.
    recursive subroutine sgml_quote(start, end, status, unquotep)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: unquotep
        character(len=:), allocatable :: num, replacement
        integer(int64) :: begv, zv, pos, code
        integer :: c, subexp, ios
        logical :: unquote
        type(handle_type) :: begv_marker, zv_marker

        unquote = .false.
        if (present(unquotep)) unquote = unquotep
        ! save-restriction.
        call copy_marker_at(buffers(cur)%begv, begv_marker, .false., status)
        call copy_marker_at(buffers(cur)%zv, zv_marker, .true., status)
        call narrow_to_region(start, end, status)
        call goto_char(point_min())

        if (unquote) then
            ! FIXME: We should unquote other named character references as
            ! well.
            do
                call re_search_forward(QUOTE_RE, pos, status, noerror=SEARCH_NOERROR)
                if (pos == 0) exit
                c = after_char_at(mb(2))

                select case (c)
                case (iachar("a"))
                    code = iachar("&")
                case (iachar("q"))
                    code = iachar("""")
                case (iachar("l"))
                    code = iachar("<")
                case (iachar("g"))
                    code = iachar(">")
                case default
                    num = match_string(3)

                    if (num(1:1) == "x" .or. num(1:1) == "X") then
                        read (num(2:), "(z16)", iostat=ios) code
                    else
                        read (num, *, iostat=ios) code
                    end if

                    if (ios /= 0) code = 0
                end select

                replacement = char_string(int(code))
                subexp = 1
                if (before_char_at(me(0)) == iachar(";")) subexp = 0
                call replace_match(replacement, status, fixedcase=.true., literal=.true., subexp=subexp)
            end do
        else
            do
                call re_search_forward("[&<>""']", pos, status, noerror=SEARCH_NOERROR)
                if (pos == 0) exit

                select case (before_char_at(point()))
                case (iachar("&"))
                    replacement = "&amp;"
                case (iachar("<"))
                    replacement = "&lt;"
                case (iachar(">"))
                    replacement = "&gt;"
                case (iachar(""""))
                    replacement = "&#34;"
                case default
                    replacement = "&#39;"
                end select

                call replace_match(replacement, status, fixedcase=.true., literal=.true.)
            end do
        end if

        status = status_type()
        call widen()
        call marker_position(begv_marker, begv, status)
        call marker_position(zv_marker, zv, status)
        call narrow_to_region(begv, zv, status)
        call free_marker(begv_marker, status)
        call free_marker(zv_marker, status)
    end subroutine sgml_quote

    ! The command `sgml_quote'.
    recursive subroutine cmd_sgml_quote(status)
        use callint_fm, only: current_prefix_arg, PREFIX_NONE
        use editfns_fm, only: region_beginning, region_end
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        call sgml_quote(b, e, status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_sgml_quote

    !> Simple-minded pretty printer for SGML.
    !> Re-indents the code and inserts newlines between BEG and END.
    !> You might want to turn on `auto_fill_mode' to get better results.
    !> Port of sgml-mode.el:sgml-pretty-print.
    recursive subroutine sgml_pretty_print(beg_in, end_in, status)
        integer(int64), intent(in) :: beg_in, end_in
        type(status_type), intent(out) :: status
        type(handle_type) :: mark, end_marker, saved
        type(status_type) :: st
        integer(int64) :: end, pos, moved

        status = status_type()
        call excursion_begin(mark)
        call goto_char(min(beg_in, end_in))
        end = max(beg_in, end_in)
        ! Don't use narrowing because it screws up auto-indent.
        call copy_marker_at(end, end_marker, .true., st)
        call use_tag_table(saved)

        do
            call marker_position(end_marker, end, st)
            call re_search_forward("<", pos, st, bound=end, noerror=SEARCH_NOERROR)
            if (pos == 0) exit
            call goto_char(mb(0))
            moved = skip_chars_backward(" "//TAB, st)

            if (.not. is_bol()) then
                call goto_char(mb(0))
                call reindent_then_newline_and_indent(status)
            else
                call goto_char(mb(0))
            end if

            call sgml_forward_expression(1_int64, st)
            if (.not. st%ok()) exit
        end do

        call restore_table(saved)
        call free_marker(end_marker, st)
        call excursion_end(mark)
    end subroutine sgml_pretty_print

    ! The command `sgml_pretty_print'.
    recursive subroutine cmd_sgml_pretty_print(status)
        use editfns_fm, only: region_beginning, region_end
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        call sgml_pretty_print(b, e, status)
    end subroutine cmd_sgml_pretty_print

    ! Skip past a tag-name, and return the name.  Port of
    ! sgml-mode.el:sgml-parse-tag-name.
    recursive function parse_tag_name() result(name)
        character(len=:), allocatable :: name
        integer(int64) :: start, moved

        start = point()
        moved = skip_syntax_forward("w_")
        name = text_between(start, point())
    end function parse_tag_name

    ! Return true if text between START and END is a tag.
    ! Checks among other things that the tag does not contain spurious
    ! unquoted < or > chars inside, which would indicate that it really
    ! isn't a tag after all.  Port of sgml-mode.el:sgml-tag-text-p.
    recursive logical function is_tag_text(start, end)
        integer(int64), intent(in) :: start, end
        type(handle_type) :: saved
        type(parse_state_type) :: pps
        type(status_type) :: st
        integer(int64) :: opoint

        opoint = point()
        call use_tag_table(saved)
        call parse_partial_expression(start, end, pps, st, targetdepth=2_int64)
        is_tag_text = pps%depth == 0
        call restore_table(saved)
        call goto_char(opoint)
    end function is_tag_text

    ! Search backward for a '<' or '>' character.
    ! The character must have open or close syntax.
    ! Returns t if found, nil otherwise.  Port of
    ! sgml-mode.el:sgml--find-<>-backward.
    recursive logical function find_angle_backward(limit) result(found)
        use syntax_fm, only: syntax_class_at
        integer(int64), intent(in) :: limit
        type(status_type) :: st
        type(value_type) :: face
        integer(int64) :: pos
        integer :: class
        logical :: comment

        found = .false.

        do
            if (limit > 0) then
                call re_search_backward("[<>]", pos, st, bound=limit, noerror=SEARCH_MOVE)
            else
                call re_search_backward("[<>]", pos, st, noerror=SEARCH_MOVE)
            end if

            if (pos == 0) return
            ! If this character has "open" or "close" syntax, then we've
            ! found the one we want.
            class = syntax_class_at(after_char_at(point()), point())

            if (class == 4 .or. class == 5) then
                ! We want to ignore tags in comments.  We could also check
                ! `syntax-ppss', but that can become expensive in a busy
                ! loop, so we re-use the face instead.
                call get_text_property(point(), value_type(name="face"), face, st)
                comment = .false.

                if (vtag(face) == V_NAME) then
                    comment = symbols(xsymbol(face))%name == "font_lock_comment_delimiter_face" &
                              .or. symbols(xsymbol(face))%name == "font_lock_comment_face"
                end if

                if (.not. comment) then
                    found = .true.
                    return
                end if
            end if
        end do
    end function find_angle_backward

    !> Parse an SGML tag backward, and return information about the tag
    !> (`ok' false when there is no tag: the error "No tag found").
    !> Assume that parsing starts from within a textual context.  Leave
    !> point at the beginning of the tag.  Port of
    !> sgml-mode.el:sgml-parse-tag-backward.
    recursive subroutine sgml_parse_tag_backward(tag, ok, limit)
        type(sgml_tag_type), intent(out) :: tag
        logical, intent(out) :: ok
        integer(int64), intent(in), optional :: limit
        type(handle_type) :: saved
        type(status_type) :: st
        integer(int64) :: lim, tag_start, tag_end, pos, found
        integer :: c

        lim = 0
        if (present(limit)) lim = limit
        ok = find_angle_backward(lim)
        if (.not. ok) return
        tag_start = 0

        if (after_char_at(point()) == iachar("<")) then
            ! Oops!! Looks like we were not in a textual context after all!.
            ! Let's try to recover.
            ! Remember the tag-start so we don't need to look for it later.
            ! This is not just an optimization but also makes sure we don't
            ! get stuck in infloops in cases where "looking back for <"
            ! would not go back far enough.
            tag_start = point()
            pos = point()
            call use_tag_table(saved)
            ! FIXME: This does not correctly skip over PI and CDATA tags.
            call sgml_forward_expression(1_int64, st)
            call restore_table(saved)

            if (.not. st%ok()) then
                ! This < seems to be just a spurious one, let's ignore it.
                call goto_char(pos)
                call sgml_parse_tag_backward(tag, ok, limit)
                return
            end if

            ! Check it is really a tag, without any extra < or > inside.
            if (.not. is_tag_text(pos, point())) then
                call goto_char(pos)
                call sgml_parse_tag_backward(tag, ok, limit)
                return
            end if

            call forward_char(st, -1_int64)
        end if

        tag_end = point() + 1

        if (looking_back_at("--")) then
            ! comment
            tag%type = SGML_TAG_COMMENT

            if (tag_start == 0) then
                call search_backward("<!--", found, st, noerror=SEARCH_NOERROR)
                tag_start = found
            end if
        else if (looking_back_at("]]")) then
            ! cdata
            tag%type = SGML_TAG_CDATA

            if (tag_start == 0) then
                call re_search_backward("<!\[[A-Z]+\[", found, st, noerror=SEARCH_NOERROR)
                tag_start = found
            end if
        else if (looking_back_at("?")) then
            ! XML processing-instruction
            tag%type = SGML_TAG_PI

            if (tag_start == 0) then
                call search_backward("<?", found, st, noerror=SEARCH_NOERROR)
                tag_start = found
            end if
        else
            if (tag_start == 0) then
                call use_tag_table(saved)
                call goto_char(tag_end)
                call sgml_forward_expression(-1_int64, st)
                call restore_table(saved)

                if (.not. st%ok()) then
                    ! This > isn't really the end of a tag. Skip it.
                    call goto_char(tag_end - 1)
                    call sgml_parse_tag_backward(tag, ok, limit)
                    return
                end if

                tag_start = point()
            end if

            call goto_char(tag_start + 1)
            c = after_char_at(point())

            select case (c)
            case (iachar("!"))
                ! declaration
                tag%type = SGML_TAG_DECL
            case (iachar("?"))
                ! processing-instruction
                tag%type = SGML_TAG_PI
            case (iachar("%"))
                ! JSP tags
                tag%type = SGML_TAG_JSP
            case (iachar("/"))
                ! close-tag
                call forward_char(st, 1_int64)
                tag%type = SGML_TAG_CLOSE
                tag%name = parse_tag_name()
            case default
                ! open or empty tag
                tag%type = SGML_TAG_OPEN
                tag%name = parse_tag_name()
                if (before_char_at(tag_end - 1) == iachar("/") .or. is_empty_tag(tag%name)) tag%type = SGML_TAG_EMPTY
            end select
        end if

        call goto_char(tag_start)
        tag%start = tag_start
        tag%end = tag_end
        if (.not. allocated(tag%name)) tag%name = ""
        ok = .true.
    end subroutine sgml_parse_tag_backward

    ! Push TAG on top of the list CONTEXT (its first element).
    ! Not a port.
    subroutine push_tag(context, tag)
        type(sgml_tag_type), allocatable, intent(inout) :: context(:)
        type(sgml_tag_type), intent(in) :: tag
        type(sgml_tag_type), allocatable :: grown(:)
        integer :: i, n

        n = size(context)
        allocate (grown(n + 1))
        grown(1) = tag

        do i = 1, n
            grown(i + 1) = context(i)
        end do

        call move_alloc(grown, context)
    end subroutine push_tag

    ! Drop the first element of CONTEXT.
    ! Not a port.
    subroutine pop_tag(context)
        type(sgml_tag_type), allocatable, intent(inout) :: context(:)
        type(sgml_tag_type), allocatable :: rest(:)
        integer :: i

        allocate (rest(size(context) - 1))

        do i = 1, size(rest)
            rest(i) = context(i + 1)
        end do

        call move_alloc(rest, context)
    end subroutine pop_tag

    !> Determine the context of the current position.
    !> By default, parse until we find a start-tag as the first thing on a
    !> line.  If UNTIL is UNTIL_EMPTY, return even if the context is empty
    !> (i.e. we just skipped over some element and got to a beginning of
    !> line).
    !>
    !> The context is a list of tag-info structures.  The last one is the
    !> tag immediately enclosing the current position.
    !>
    !> Point is assumed to be outside of any tag.  If we discover that it's
    !> not the case, the first tag returned is the one inside which we are.
    !> Port of sgml-mode.el:sgml-get-context.
    recursive subroutine sgml_get_context(context, until_in)
        type(sgml_tag_type), allocatable, intent(out) :: context(:)
        integer, intent(in), optional :: until_in
        type(sgml_tag_type) :: tag_info
        type(status_type) :: st
        character(len=64), allocatable :: stack(:), ignore(:)
        integer(int64) :: here, moved
        integer :: until, i
        logical :: more, ok

        until = UNTIL_DEFAULT
        if (present(until_in)) until = until_in
        here = point()
        allocate (context(0), stack(0), ignore(0))
        ! CONTEXT keeps track of the tag-stack
        ! STACK keeps track of the end tags we've seen (and thus the
        !   start-tags we'll have to ignore) when skipping over matching
        !   open..close pairs.
        ! IGNORE is a list of tags that can be ignored because they have
        !   been closed implicitly.
        ! Make sure we're not at indentation.
        moved = skip_chars_backward(" "//TAB//LF, st)

        do
            if (until == UNTIL_NOW) exit
            more = size(stack) > 0

            if (.not. more) then
                if (until /= UNTIL_DEFAULT) then
                    more = until /= UNTIL_EMPTY
                else
                    more = size(context) == 0
                end if
            end if

            if (.not. more) more = .not. is_at_indentation()

            if (.not. more .and. size(context) > 0) then
                if (point() /= context(1)%start) more = is_unclosed_tag(context(1)%name)
            end if

            if (.not. more) exit
            call sgml_parse_tag_backward(tag_info, ok)
            if (.not. ok) exit

            ! This tag may enclose things we thought were tags.  If so,
            ! discard them.
            do while (size(context) > 0)
                if (.not. tag_info%end > context(1)%end) exit
                call pop_tag(context)
            end do

            if (tag_info%end > here) then
                ! Oops!!  Looks like we were not outside of any tag, after
                ! all.
                call push_tag(context, tag_info)
                until = UNTIL_NOW
            else if (tag_info%type == SGML_TAG_OPEN) then
                ! start-tag
                if (size(stack) == 0) then
                    if (member_ignore_case(tag_info%name, ignore)) then
                        ! There was an implicit end-tag.
                        continue
                    else
                        call push_tag(context, tag_info)
                        ! We're changing context so the tags implicitly
                        ! closed inside the previous context aren't
                        ! implicitly closed here any more.
                        deallocate (ignore)
                        allocate (ignore(0))
                    end if
                else if (lower(tag_info%name) == lower(trim(stack(1)))) then
                    stack = stack(2:)
                else
                    ! The open and close tags don't match.
                    if (.not. sgml_xml_mode%value) then
                        if (.not. is_unclosed_tag(tag_info%name)) then
                            call message("Unclosed tag <"//tag_info%name//">")
                            ! We could just assume that the tag is simply
                            ! not closed but it's a bad assumption when tags
                            ! *are* closed but not properly nested.
                            i = 1

                            do while (i + 1 <= size(stack))
                                if (lower(tag_info%name) == lower(trim(stack(i + 1)))) exit
                                i = i + 1
                            end do

                            if (i + 1 <= size(stack)) stack = [stack(1:i), stack(i + 2:)]
                        end if
                    else
                        call message("Unmatched tags <"//tag_info%name//"> and </"//trim(stack(1))//">")
                        stack = stack(2:)
                    end if
                end if

                ! This is a top-level open of an implicitly closed tag, so
                ! any occurrence of such an open tag at the same level can be
                ! ignored because it's been implicitly closed.
                if (size(stack) == 0 .and. is_unclosed_tag(tag_info%name)) then
                    ignore = [character(len=64) :: tag_info%name, ignore]
                end if
            else if (tag_info%type == SGML_TAG_CLOSE) then
                ! end-tag
                if (is_empty_tag(tag_info%name)) then
                    call message("Spurious </"//tag_info%name//">: empty tag")
                else
                    stack = [character(len=64) :: tag_info%name, stack]
                end if
            end if
        end do
    end subroutine sgml_get_context

    !> Close current element.
    !> Depending on context, inserts a matching close-tag, or closes the
    !> current start-tag or the current comment or the current cdata, ...
    !> Port of sgml-mode.el:sgml-close-tag.
    recursive subroutine sgml_close_tag(status)
        type(status_type), intent(out) :: status
        type(sgml_tag_type), allocatable :: context(:)
        integer(int64) :: start, opoint
        integer :: ctype

        status = status_type()
        call sgml_lexical_context(ctype, start)

        select case (ctype)
        case (SGML_CONTEXT_COMMENT)
            call insert(" -->", status)
        case (SGML_CONTEXT_CDATA)
            call insert("]]>", status)
        case (SGML_CONTEXT_PI)
            call insert(" ?>", status)
        case (SGML_CONTEXT_TAG)
            call insert(" />", status)
        case (SGML_CONTEXT_TEXT)
            opoint = point()
            call sgml_get_context(context)
            call goto_char(opoint)

            if (size(context) > 0) then
                call insert("</"//context(size(context))%name//">", status)
                call indent_according_to_mode(status)
            end if
        case default
            call set_error(status, FM_GENERIC_ERROR, "Nothing to close")
        end select
    end subroutine sgml_close_tag

    ! Return true if TAG_NAME is an implicitly empty tag.  Port of
    ! sgml-mode.el:sgml-empty-tag-p.
    recursive logical function is_empty_tag(tag_name)
        character(len=*), intent(in) :: tag_name

        is_empty_tag = .false.
        if (sgml_xml_mode%value) return
        is_empty_tag = member_ignore_case(tag_name, empty_tags())
    end function is_empty_tag

    ! Return true if TAG_NAME is a tag for which an end-tag is optional.
    ! Port of sgml-mode.el:sgml-unclosed-tag-p.
    recursive logical function is_unclosed_tag(tag_name)
        character(len=*), intent(in) :: tag_name

        is_unclosed_tag = .false.
        if (sgml_xml_mode%value) return
        is_unclosed_tag = member_ignore_case(tag_name, unclosed_tags())
    end function is_unclosed_tag

    ! ------------------------------------------------------------------
    ! Indentation
    ! ------------------------------------------------------------------

    !> Calculate the column to which this line should be indented
    !> (`found' false if there is none).  The lexical context is CTYPE_IN and
    !> START_IN if given.  Port of sgml-mode.el:sgml-calculate-indent.
    recursive subroutine sgml_calculate_indent(column, found, ctype_in, start_in)
        integer, intent(out) :: column
        logical, intent(out) :: found
        integer, intent(in), optional :: ctype_in
        integer(int64), intent(in), optional :: start_in
        type(sgml_tag_type), allocatable :: context(:), reversed(:)
        type(status_type) :: st
        character(len=16), allocatable :: sensitive(:)
        character(len=:), allocatable :: unclosed
        integer(int64) :: start, here, there, opoint, moved, shortage, found_pos
        integer :: ctype, i, n
        logical :: mark, sensitive_found, skip, saved_fold

        column = 0
        found = .true.

        if (present(ctype_in)) then
            ctype = ctype_in
            start = start_in
        else
            call sgml_lexical_context(ctype, start)
        end if

        ! Indent comment-start markers inside <!-- just like comment-end
        ! markers.
        if (ctype == SGML_CONTEXT_TAG .and. at("--")) then
            opoint = point()
            call goto_char(start)
            mark = at("<!--")
            call goto_char(opoint)

            if (mark) then
                ctype = SGML_CONTEXT_COMMENT
                start = start + 2
            end if
        end if

        ! (pcase-exhaustive: a whitespace-sensitive tag in the context, pi
        ! and cdata give nil.)
        sensitive = whitespace_sensitive_tags()
        sensitive_found = .false.

        if (size(sensitive) > 0) then
            opoint = point()
            call sgml_get_context(context)
            call goto_char(opoint)
            saved_fold = case_fold_search%value
            case_fold_search%value = .true.

            do i = 1, size(context)
                if (member_ignore_case(context(i)%name, sensitive)) sensitive_found = .true.
            end do

            case_fold_search%value = saved_fold
        end if

        if (sensitive_found .or. ctype == SGML_CONTEXT_PI .or. ctype == SGML_CONTEXT_CDATA) then
            ! We don't know how to indent it.  Let's be honest about it.
            found = .false.
            return
        end if

        select case (ctype)
        case (SGML_CONTEXT_STRING)
            ! Go back to previous non-empty line.
            do
                if (.not. point() > start) exit
                shortage = forward_line(-1_int64)
                if (shortage /= 0) exit
                if (.not. at("[ "//TAB//"]*$")) exit
            end do

            if (point() > start) then
                ! Previous line is inside the string.
                column = int(current_indentation())
            else
                call goto_char(start)
                column = int(current_column()) + 1
            end if
        case (SGML_CONTEXT_COMMENT)
            mark = at("--")

            ! Go back to previous non-empty line.
            do
                if (.not. point() > start) exit
                shortage = forward_line(-1_int64)
                if (shortage /= 0) exit
                skip = at("[ "//TAB//"]*$")
                if (.not. skip .and. mark) skip = .not. at("[ "//TAB//"]*--")
                if (.not. skip) exit
            end do

            if (point() > start) then
                ! Previous line is inside the comment.
                moved = skip_chars_forward(" "//TAB, st)
            else
                call goto_char(start)
                ! Skip `<!' to get to the `--' with which we want to align.
                call search_forward("--", found_pos, st, noerror=SEARCH_NOERROR)
                call goto_char(mb(0))
            end if

            if (.not. mark .and. at("--")) then
                call forward_char(st, 2_int64)
                moved = skip_chars_forward(" "//TAB, st)
            end if

            column = int(current_column())
        case (SGML_CONTEXT_TAG)
            call goto_char(start + sgml_attribute_offset%value)
            ! Skip tag name.
            moved = skip_chars_forward("^ "//TAB//LF, st)
            moved = skip_chars_forward(" "//TAB, st)

            if (.not. is_eol()) then
                column = int(current_column())
            else
                ! This is the first attribute: indent.
                call goto_char(start + sgml_attribute_offset%value)
                column = int(current_column() + sgml_basic_offset%value)
            end if
        case default
            ! text
            do while (at("</"))
                call sgml_forward_expression(1_int64, st)
                if (.not. st%ok()) exit
                moved = skip_chars_forward(" "//TAB, st)
            end do

            here = point()
            unclosed = ""

            if (at(SGML_TAG_NAME_RE)) then
                if (member_ignore_case(match_string(1), unclosed_tags())) unclosed = match_string(1)
            end if

            ! If possible, align on the previous non-empty text line.
            ! Otherwise, do a more serious parsing to find the tag(s)
            ! relative to which we should be indenting.
            skip = .false.

            if (len(unclosed) == 0) then
                moved = skip_chars_backward(" "//TAB, st)

                if (skip_chars_backward(" "//TAB//LF, st) < 0) then
                    call back_to_indentation()
                    skip = point() > start
                end if
            end if

            if (skip) then
                allocate (context(0))
            else
                call goto_char(here)

                if (len(unclosed) > 0) then
                    call sgml_get_context(reversed)
                else
                    call sgml_get_context(reversed, UNTIL_EMPTY)
                end if

                n = size(reversed)
                allocate (context(n))

                do i = 1, n
                    context(i) = reversed(n + 1 - i)
                end do
            end if

            there = point()

            ! Ignore previous unclosed start-tag in context.
            do while (size(context) > 0 .and. len(unclosed) > 0)
                if (lower(context(1)%name) /= lower(unclosed)) exit
                call pop_tag(context)
            end do

            ! Indent to reflect nesting.
            if (size(context) > 0) then
                if (context(1)%end > here) then
                    ! If we were not in a text context after all, let's try
                    ! again.
                    call goto_char(here)

                    if (context(1)%type == SGML_TAG_COMMENT) then
                        call sgml_calculate_indent(column, found, SGML_CONTEXT_COMMENT, context(1)%start)
                    else if (context(1)%type == SGML_TAG_CDATA) then
                        call sgml_calculate_indent(column, found, SGML_CONTEXT_CDATA, context(1)%start)
                    else
                        call sgml_calculate_indent(column, found, SGML_CONTEXT_TAG, context(1)%start)
                    end if

                    return
                end if

                ! Align on the first element after the nearest open-tag, if
                ! any.
                call goto_char(context(1)%end)
                moved = skip_chars_forward(" "//TAB//LF, st)

                if (point() < here .and. is_at_indentation()) then
                    column = int(current_column())
                    return
                end if
            end if

            ! Otherwise, just follow the rules.
            call goto_char(there)
            column = int(current_column() + sgml_basic_offset%value*size(context))
        end select
    end subroutine sgml_calculate_indent

    !> Indent the current line as SGML.  NOINDENT is true when its
    !> indentation is not known.  Port of sgml-mode.el:sgml-indent-line.
    recursive subroutine sgml_indent_line(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent
        type(handle_type) :: mark
        integer(int64) :: savep
        integer :: column
        logical :: found, keep

        status = status_type()
        savep = point()
        call back_to_indentation()
        keep = point() < savep
        call sgml_calculate_indent(column, found)
        call goto_char(savep)
        noindent = .not. found
        if (.not. found) return

        if (keep) then
            call excursion_begin(mark)
            call indent_line_to(int(column, int64), status)
            call excursion_end(mark)
        else
            call indent_line_to(int(column, int64), status)
        end if
    end subroutine sgml_indent_line

    ! The command `sgml_indent_line'.
    recursive subroutine cmd_sgml_indent_line(status)
        type(status_type), intent(out) :: status
        logical :: noindent

        call sgml_indent_line(status, noindent)
    end subroutine cmd_sgml_indent_line

    !> Guess an appropriate value for `sgml_basic_offset'.
    !> Base the guessed indentation level on the first indented tag in the
    !> buffer.  Add this to `sgml_mode_hook' for convenience.  Port of
    !> sgml-mode.el:sgml-guess-indent.
    recursive subroutine sgml_guess_indent(status)
        type(status_type), intent(out) :: status
        character(len=16) :: buf
        integer(int64) :: opoint, pos

        status = status_type()
        opoint = point()
        call goto_char(point_min())
        call re_search_forward("^\([ "//TAB//"]+\)<", pos, status, bound=500_int64, noerror=SEARCH_NOERROR)

        if (pos > 0) then
            call sgml_basic_offset%set_local(current_column() - 1)
            write (buf, "(i0)") sgml_basic_offset%value
            call message("Guessed sgml-basic-offset = "//trim(buf))
        end if

        call goto_char(opoint)
    end subroutine sgml_guess_indent

    !> Simplistic parse of the current buffer as a DTD.
    !> Currently just returns (EMPTY-TAGS UNCLOSED-TAGS), sorted.  Port of
    !> sgml-mode.el:sgml-parse-dtd.
    recursive subroutine sgml_parse_dtd(empty, unclosed)
        character(len=64), allocatable, intent(out) :: empty(:), unclosed(:)
        type(status_type) :: st
        integer(int64) :: pos

        call goto_char(point_min())
        allocate (empty(0), unclosed(0))

        do
            call re_search_forward(DTD_RE, pos, st, noerror=SEARCH_NOERROR)
            if (pos == 0) exit

            if (match_string(3) == "EMPTY") then
                empty = [character(len=64) :: lower(match_string_no_properties(1)), empty]
            else if (match_string(2) == "O") then
                unclosed = [character(len=64) :: lower(match_string_no_properties(1)), unclosed]
            end if
        end do

        call sort_strings(empty)
        call sort_strings(unclosed)
    end subroutine sgml_parse_dtd

    ! Sort S (string<).
    ! Not a port.
    subroutine sort_strings(s)
        character(len=64), intent(inout) :: s(:)
        character(len=64) :: t
        integer :: i, j

        do i = 2, size(s)
            t = s(i)
            j = i - 1

            do while (j >= 1)
                if (.not. llt(t, s(j))) exit
                s(j + 1) = s(j)
                j = j - 1
            end do

            s(j + 1) = t
        end do
    end subroutine sort_strings

    !> Return the name of the last HTML title or heading (`found' false for
    !> none).  Port of sgml-mode.el:html-current-defun-name.
    recursive subroutine html_current_heading_name(name, found)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: found
        type(status_type) :: st
        integer(int64) :: opoint, pos

        name = ""
        opoint = point()
        call re_search_backward(HTML_SUBPROGRAM_RE, pos, st, noerror=SEARCH_NOERROR)
        found = pos > 0
        if (found) name = match_string_no_properties(1)
        call goto_char(opoint)
    end subroutine html_current_heading_name

    ! ------------------------------------------------------------------
    ! Modes
    ! ------------------------------------------------------------------

    ! The commands of the keymaps and the prefix argument wrappers.
    recursive subroutine cmd_sgml_slash(status)
        use callint_fm, only: current_prefix_arg, prefix_numeric_value
        type(status_type), intent(out) :: status

        call sgml_slash(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_sgml_slash

    recursive subroutine cmd_sgml_slash_matching(status)
        use callint_fm, only: current_prefix_arg, prefix_numeric_value
        type(status_type), intent(out) :: status

        call sgml_slash_matching(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_sgml_slash_matching

    recursive subroutine cmd_sgml_close_tag(status)
        type(status_type), intent(out) :: status

        call sgml_close_tag(status)
    end subroutine cmd_sgml_close_tag

    recursive subroutine cmd_sgml_guess_indent(status)
        type(status_type), intent(out) :: status

        call sgml_guess_indent(status)
    end subroutine cmd_sgml_guess_indent

    recursive subroutine cmd_sgml_name_self(status)
        type(status_type), intent(out) :: status

        call sgml_name_self(status)
    end subroutine cmd_sgml_name_self

    recursive subroutine cmd_sgml_maybe_name_self(status)
        type(status_type), intent(out) :: status

        call sgml_maybe_name_self(status)
    end subroutine cmd_sgml_maybe_name_self

    recursive subroutine cmd_sgml_maybe_end_tag(status)
        type(status_type), intent(out) :: status

        call sgml_maybe_end_tag(status)
    end subroutine cmd_sgml_maybe_end_tag

    ! `sgml_name_8bit_mode': when on, insert non-ASCII characters as named
    ! entities.
    ! Not a port.
    recursive subroutine sgml_name_8bit_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%sgml_name_8bit_mode%command(status)
    end subroutine sgml_name_8bit_mode

    ! sgml-make-syntax-table: the syntax table of SPECIALS.  Port of
    ! sgml-mode.el:sgml-make-syntax-table.
    subroutine make_sgml_syntax_table(table)
        type(handle_type), intent(out) :: table
        type(status_type) :: st

        call make_syntax_table(table, st, text_mode_syntax_table)
        call modify_syntax_entry(iachar("<"), iachar("<"), "(>", st, table)
        call modify_syntax_entry(iachar(">"), iachar(">"), ")<", st, table)
        call modify_syntax_entry(iachar(":"), iachar(":"), "_", st, table)
        call modify_syntax_entry(iachar("_"), iachar("_"), "_", st, table)
        call modify_syntax_entry(iachar("."), iachar("."), "_", st, table)
        if (any(sgml_specials == iachar("-"))) call modify_syntax_entry(iachar("-"), iachar("-"), "_ 1234", st, table)
        if (any(sgml_specials == iachar(""""))) call modify_syntax_entry(iachar(""""), iachar(""""), """""", st, table)
        if (any(sgml_specials == iachar("'"))) call modify_syntax_entry(iachar("'"), iachar("'"), """'", st, table)
    end subroutine make_sgml_syntax_table

    ! sgml-mode-syntax-table and sgml-tag-syntax-table.
    ! Not a port.
    subroutine init_syntax_tables()
        type(status_type) :: st
        character(len=*), parameter :: PUNCT = "$%&*+/"
        integer :: i

        call make_sgml_syntax_table(sgml_mode_syntax_table)
        call make_sgml_syntax_table(sgml_tag_syntax_table)

        do i = 1, size(PAIRED_BRACKETS)
            call modify_syntax_entry(PAIRED_BRACKETS(i), PAIRED_BRACKETS(i), ".", st, sgml_tag_syntax_table)
        end do

        do i = 1, len(PUNCT)
            call modify_syntax_entry(iachar(PUNCT(i:i)), iachar(PUNCT(i:i)), ".", st, sgml_tag_syntax_table)
        end do

        ! Avoid that skipping a tag backwards skips any "'" prefixing it.
        if (.not. any(sgml_specials == iachar("'"))) then
            call modify_syntax_entry(iachar("'"), iachar("'"), "w", st, sgml_tag_syntax_table)
        end if
    end subroutine init_syntax_tables

    ! sgml-syntax-propertize-rules.
    ! Port of sgml-mode.el:sgml-syntax-propertize-rules.
    subroutine init_rules()
        type(syntax_highlight_type) :: hl

        allocate (propertize_rules%rules(4))
        ! Use the `b' style of comments to avoid interference with the -- ...
        ! -- comments recognized when `sgml-specials' includes ?-.
        ! FIXME: beware of <!--> blabla <!--> !!
        propertize_rules%rules(1)%regexp = "\(<\)!--"
        hl = syntax_highlight_type(group=1, syntax="< b")
        propertize_rules%rules(1)%highlights = [hl]
        propertize_rules%rules(2)%regexp = "--[ "//TAB//LF//"]*\(>\)"
        hl = syntax_highlight_type(group=1, syntax="> b")
        propertize_rules%rules(2)%highlights = [hl]
        propertize_rules%rules(3)%regexp = "\(<\)[?!]"
        hl = syntax_highlight_type(group=1, syntax="|>")
        hl%after => declaration_after
        propertize_rules%rules(3)%highlights = [hl]
        ! Quotes outside of tags should not introduce strings which end up
        ! hiding tags.
        propertize_rules%rules(4)%regexp = "\([""']\)[^""'<>]*"
        hl = syntax_highlight_type(group=1)
        hl%value => quote_value
        propertize_rules%rules(4)%highlights = [hl]
    end subroutine init_rules

    ! Not a port.
    ! The keywords of the levels: sgml-font-lock-keywords (level 0), -1 and
    ! -2.
    subroutine levels_of(levels)
        type(font_lock_keywords_type), intent(out) :: levels(3)

        levels(1)%function => keywords_1
        levels(2)%function => keywords_1
        levels(3)%function => keywords_2
    end subroutine levels_of

! Not a port.
    ! `sgml_indent_line' as an `indent_line_function'.
    recursive subroutine indent_line_proc(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent

        call sgml_indent_line(status, noindent)
    end subroutine indent_line_proc

    !> Major mode for editing SGML documents.
    !> Makes > match <.
    !>
    !> Use \[sgml_validate] to validate your document with an SGML parser.
    !>
    !> Do \[describe_variable] sgml_ SPC to see available variables.  Do
    !> \[describe_key] on the following bindings to discover what they do.
    !> Port of sgml-mode.el:sgml-mode.
    recursive subroutine sgml_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keywords_type) :: levels(3)
        logical :: saved

        if (.not. is_syntax_table_live(sgml_mode_syntax_table)) call syms_of_sgml_mode()
        call major_modes%sgml_mode%begin(saved, status)
        if (.not. status%ok()) return
        call setup()
        call levels_of(levels)
        call font_lock_defaults%set_local(levels=levels, case_fold=.true., &
                                          syntactic_face_function=sgml_font_lock_syntactic_face)
        call major_modes%sgml_mode%finish(saved, status)
    end subroutine sgml_mode

! Port of sgml-mode.el:sgml-mode (in part: its body).
    ! The body of `sgml_mode'.
    recursive subroutine setup()
        integer :: k

        k = local_index(.true.)
        ! A start or end tag by itself on a line separates a paragraph.
        ! This is desirable because SGML discards a newline that appears
        ! immediately after a start tag or immediately before an end tag.
        call paragraph_start%set_local(SGML_PARAGRAPH_START_RE)
        call paragraph_separate%set_local(SGML_PARAGRAPH_START_RE//"$")
        call indent_line_function%set_local(indent_line_proc)
        call comment_start%set_local("<!-- ")
        call comment_end%set_local(" -->")
        call comment_indent_function%set_local(sgml_comment_indent)
        call syntax_propertize_function%set_local(sgml_syntax_propertize)
        call syntax_ppss_table%set_local(sgml_tag_syntax_table)
        if (sgml_xml_guess()) call sgml_xml_mode%set_local(.true.)
        ! This will allow existing comments within declarations to be
        ! recognized.
        call comment_start_skip%set_local("<!--[ "//TAB//"]*")
        call comment_end_skip%set_local("[ "//TAB//"]*--[ "//TAB//LF//"]*>")
    end subroutine setup

    !> Major mode based on SGML mode for editing HTML documents.  Port of
    !> sgml-mode.el:html-mode.
    recursive subroutine html_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        integer :: k

        if (.not. is_syntax_table_live(sgml_mode_syntax_table)) call syms_of_sgml_mode()
        call major_modes%html_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! sgml-tag-face-alist, sgml-whitespace-sensitive-tags,
        ! sgml-empty-tags and sgml-unclosed-tags.
        k = local_index(.true.)
        locals(k)%html = .true.
        call major_modes%html_mode%finish(saved, status)
    end subroutine html_mode

    ! Not a port.
    !> Define SGML mode and HTML mode: their variables, tables, keymaps and
    !> commands.
    recursive subroutine syms_of_sgml_mode()
        type(status_type) :: st
        type(named_face_type), pointer :: f
        integer :: c

        if (is_syntax_table_live(sgml_mode_syntax_table)) return
        call forward_setting(sgml_basic_offset)
        call forward_setting(sgml_attribute_offset)
        call forward_setting(sgml_xml_mode)
        call forward_setting(sgml_validate_command)
        call forward_setting(sgml_slash_distance)
        if (.not. allocated(sgml_specials)) sgml_specials = [iachar(""""), iachar("'")]
        if (all(sgml_char_names == "")) sgml_char_names = SGML_CHAR_NAMES_DATA
        if (.not. allocated(sgml_empty_tags)) allocate (sgml_empty_tags(0))
        if (.not. allocated(sgml_unclosed_tags)) allocate (sgml_unclosed_tags(0))
        if (.not. allocated(sgml_whitespace_sensitive_tags)) allocate (sgml_whitespace_sensitive_tags(0))
        call init_validate_command()
        call init_html_tag_face_table()
        f => sgml_namespace
        call f%define("sgml_namespace", "`sgml_mode' face used to highlight the namespace part of identifiers.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_builtin_face"))
        call init_syntax_tables()
        call init_rules()
        call add_local_variables_killer(kill_sgml_locals)
        call major_modes%sgml_mode%define("sgml_mode", "SGML", sgml_mode, sgml_mode_hook, parent=major_modes%text_mode, &
                                          syntax_table=sgml_mode_syntax_table)
        sgml_mode_map = major_modes%sgml_mode%map
        call major_modes%html_mode%define("html_mode", "HTML", html_mode, html_mode_hook, parent=major_modes%sgml_mode)
        html_mode_map = major_modes%html_mode%map
        call minor_modes%sgml_name_8bit_mode%define("sgml_name_8bit_mode")
        call keymap_set(sgml_mode_map, "/", "sgml_slash", st)
        call keymap_set(sgml_mode_map, "C-c C-n", "sgml_name_char", st)
        call keymap_set(sgml_mode_map, "C-c C-b", "sgml_skip_tag_backward", st)
        call keymap_set(sgml_mode_map, "C-c <left>", "sgml_skip_tag_backward", st)
        call keymap_set(sgml_mode_map, "C-c C-f", "sgml_skip_tag_forward", st)
        call keymap_set(sgml_mode_map, "C-c <right>", "sgml_skip_tag_forward", st)
        call keymap_set(sgml_mode_map, "C-c C-d", "sgml_delete_tag", st)
        call keymap_set(sgml_mode_map, "C-c ^ ?", "sgml_delete_tag", st)
        call keymap_set(sgml_mode_map, "C-c ]", "sgml_close_tag", st)
        call keymap_set(sgml_mode_map, "C-c /", "sgml_close_tag", st)
        ! Redundant keybindings, for consistency with TeX mode.
        call keymap_set(sgml_mode_map, "C-c C-e", "sgml_close_tag", st)
        call keymap_set(sgml_mode_map, "C-c 8", "sgml_name_8bit_mode", st)
        call keymap_set(sgml_mode_map, "C-c C-v", "sgml_validate", st)

        do c = 128, 255
            call keymap_set(sgml_mode_map, char_string(c), "sgml_maybe_name_self", st)
        end do

        commands%sgml_mode%execute => sgml_mode
        commands%html_mode%execute => html_mode
        commands%sgml_slash%execute => cmd_sgml_slash
        commands%sgml_slash_matching%execute => cmd_sgml_slash_matching
        commands%sgml_name_char%execute => cmd_sgml_name_char
        commands%sgml_namify_char%execute => cmd_sgml_namify_char
        commands%sgml_name_self%execute => cmd_sgml_name_self
        commands%sgml_maybe_name_self%execute => cmd_sgml_maybe_name_self
        commands%sgml_maybe_end_tag%execute => cmd_sgml_maybe_end_tag
        commands%sgml_skip_tag_backward%execute => cmd_sgml_skip_tag_backward
        commands%sgml_skip_tag_forward%execute => cmd_sgml_skip_tag_forward
        commands%sgml_delete_tag%execute => cmd_sgml_delete_tag
        commands%sgml_validate%execute => cmd_sgml_validate
        commands%sgml_quote%execute => cmd_sgml_quote
        commands%sgml_pretty_print%execute => cmd_sgml_pretty_print
        commands%sgml_close_tag%execute => cmd_sgml_close_tag
        commands%sgml_indent_line%execute => cmd_sgml_indent_line
        commands%sgml_guess_indent%execute => cmd_sgml_guess_indent
        commands%sgml_name_8bit_mode%execute => sgml_name_8bit_mode
    end subroutine syms_of_sgml_mode

    ! Port of sgml-mode.el:sgml-validate-command (its default value).
    ! The default `sgml_validate_command': prefer tidy because (o)nsgmls is
    ! often built without --enable-http which makes it next to useless.
    subroutine init_validate_command()
        use files_fm, only: executable_find
        character(len=:), allocatable :: found_path

        if (allocated(sgml_validate_command%value)) return
        call executable_find("tidy", found_path)

        if (len(found_path) > 0) then
            ! tidy is available from https://tidy.sourceforge.net/
            sgml_validate_command%value = "tidy --gnu-emacs yes -utf8 -e -q"
            return
        end if

        call executable_find("nsgmls", found_path)

        if (len(found_path) > 0) then
            ! nsgmls is a free SGML parser in the SP suite available from
            ! ftp.jclark.com, replaced old `sgmls'.
            sgml_validate_command%value = "nsgmls -s"
            return
        end if

        call executable_find("onsgmls", found_path)
        ! onsgmls is the community version of `nsgmls' hosted on
        ! https://openjade.sourceforge.net/
        if (len(found_path) > 0) sgml_validate_command%value = "onsgmls -s"
    end subroutine init_validate_command

! Port of sgml-mode.el:html-tag-face-alist (its value).
    ! html-tag-face-alist.
    subroutine init_html_tag_face_table()
        if (allocated(html_tag_face_table)) return
        html_tag_face_table = [sgml_tag_face_type(tag="b", faces=[character(len=32) :: "bold", ""]), &
                               sgml_tag_face_type(tag="big", faces=[character(len=32) :: "bold", ""]), &
                               sgml_tag_face_type(tag="blink", faces=[character(len=32) :: "highlight", ""]), &
                               sgml_tag_face_type(tag="cite", faces=[character(len=32) :: "italic", ""]), &
                               sgml_tag_face_type(tag="em", faces=[character(len=32) :: "italic", ""]), &
                               sgml_tag_face_type(tag="h1", faces=[character(len=32) :: "bold", "underline"]), &
                               sgml_tag_face_type(tag="h2", faces=[character(len=32) :: "bold_italic", "underline"]), &
                               sgml_tag_face_type(tag="h3", faces=[character(len=32) :: "italic", "underline"]), &
                               sgml_tag_face_type(tag="h4", faces=[character(len=32) :: "underline", ""]), &
                               sgml_tag_face_type(tag="h5", faces=[character(len=32) :: "underline", ""]), &
                               sgml_tag_face_type(tag="h6", faces=[character(len=32) :: "underline", ""]), &
                               sgml_tag_face_type(tag="i", faces=[character(len=32) :: "italic", ""]), &
                               sgml_tag_face_type(tag="rev", faces=[character(len=32) :: "mode_line", ""]), &
                               sgml_tag_face_type(tag="s", faces=[character(len=32) :: "underline", ""]), &
                               sgml_tag_face_type(tag="small", faces=[character(len=32) :: "default", ""]), &
                               sgml_tag_face_type(tag="strong", faces=[character(len=32) :: "bold", ""]), &
                               sgml_tag_face_type(tag="title", faces=[character(len=32) :: "bold", "underline"]), &
                               sgml_tag_face_type(tag="tt", faces=[character(len=32) :: "default", ""]), &
                               sgml_tag_face_type(tag="u", faces=[character(len=32) :: "underline", ""]), &
                               sgml_tag_face_type(tag="var", faces=[character(len=32) :: "italic", ""])]
    end subroutine init_html_tag_face_table
end module sgml_mode_fm

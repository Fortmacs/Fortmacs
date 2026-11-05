! sgml_mode_mod.f90 -- SGML- and HTML-editing modes.
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
!> (`sgml_name_character', `sgml_namify_character', `sgml_name_8bit_mode'),
!> `sgml_validate', `sgml_guess_indent', `sgml_parse_dtd' and
!> `html_current_heading_name'.
!>
!> The tag tables (`sgml_tags', `html_tags') drive `sgml_tag' (a skeleton),
!> `sgml_attributes', `sgml_auto_attributes' and `sgml_value'; `sgml_tag_help'
!> describes tags; the `html_*' commands are skeletons.
!>
!> Deferred: the completion at point of `html_tags' (html-mode--complete-at-point),
!> `sgml_tags_invisible' (cursor sensors), `sgml_electric_tag_pair_mode'
!> (text clones), `sgml_show_context', facemenu, tildify,
!> outline variables, `sgml_comment_indent_new_line',
!> the buffer-local `adaptive_fill_regular_expression', `fill_nobreak_functions' and
!> `sentence_end_base', the classes and IDs of HTML buffers (libxml),
!> yank-media handlers, `html_autoview_mode' and `browse_url_of_buffer'.
module sgml_mode_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use character_mod, only: downcase_ascii
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, USER_ERROR_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_list, vtag, V_NAME, V_INTEGER, xsymbol, symbols
    use data_mod, only: integer_setting_type, logical_setting_type, string_setting_type, forward_setting, append_locals_clearer, &
                        value_eq
    use commands_mod, only: commands
    use buffer_mod, only: buffers, current, buffer_serial_live
    use hooks_mod, only: hook_type
    use modes_mod, only: major_modes, minor_modes
    use add_log_mod, only: add_log_current_subprogram_function
    use keymap_mod, only: keymap_set
    use keyboard_mod, only: last_command_event, this_command, sit_for
    use syntax_mod, only: make_syntax_table, modify_syntax_entry, is_syntax_table_live, syntax_parser_state, parse_state_type, &
                          parse_partial_expression, forward_comment, skip_characters_forward, skip_characters_backward, &
                          skip_syntax_forward, skip_syntax_backward, syntax_table, set_syntax_table, syntax_entry_at, &
                          ST_STRING_STYLE, syntax_descriptor_type, string_to_syntax, syntax_descriptor_value
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, is_beginning_of_line, is_end_of_line, &
                                     is_beginning_of_buffer, is_end_of_buffer, character_after, character_before, &
                                     line_beginning_position, line_end_position, narrow_to_region, widen, buffer_substring, &
                                     delete_region, following_character, preceding_character, character_to_string
    use editing_commands_mod, only: forward_line, forward_character, self_insert_command, delete_character
    use insert_delete_mod, only: insert
    use indent_mod, only: current_column, current_indentation, indent_line_to, indent_line_function, &
                          indent_according_to_mode
    use search_mod, only: looking_at, regular_expression_search_forward, regular_expression_search_backward, search_backward, &
                          search_forward, string_match, match_beginning, match_end, replace_match, case_fold_search, &
                          regular_expression_quote, SEARCH_NOERROR, SEARCH_MOVE
    use basic_subprograms_mod, only: match_string, match_string_no_properties, looking_back
    use simple_mod, only: back_to_indentation, read_quoted_character, read_shell_command, reindent_then_newline_and_indent
    use text_properties_mod, only: put_text_property, get_text_property
    use marker_mod, only: copy_marker_at, marker_position, destroy_marker
    use display_engine_mod, only: message
    use faces_mod, only: named_face_type, display_when, face_attributes
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                             font_lock_keywords_type, font_lock_keywords_level
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight, font_lock_prepend_text_property
    use expression_motion_mod, only: forward_expression, backward_expression, cut_expression, up_group
    use newcomment_mod, only: comment_start, comment_end, comment_start_skip, comment_end_skip, comment_column, &
                              comment_indent_function, comment_indent_type, COMMENT_INDENT_COLUMN
    use paragraphs_mod, only: paragraph_start, paragraph_separate
    use syntax_propertize_mod, only: syntax_propertize_function, syntax_propertize_rules_type, syntax_highlight_type, &
                                     syntax_property_type, syntax_parser_state_table
    use regular_expression_optimization_mod, only: regular_expression_optimization, REGULAR_EXPRESSION_OPTIMIZATION_GROUP
    use undo_mod, only: undo_boundary
    use frame_mod, only: selected_window
    use window_mod, only: is_position_visible_in_window
    use files_mod, only: save_some_buffers
    use compile_mod, only: compilation_start, compilation_ask_about_save
    use text_mode_mod, only: text_mode_syntax_table
    use skeleton_mod, only: skeleton_element_type, skeleton_state, skeleton_text, skeleton_literal, skeleton_delete, &
                            skeleton_prompt, skeleton_interactor, skeleton_for_each, skeleton_action, skeleton_value, &
                            skeleton_command, skeleton_read, skeleton_transform, skeleton_transformation_procedure, &
                            skeleton_transformation_function_set_local, skeleton_further_elements_set_local, &
                            skeleton_end_newline, skeleton_end_hook, SKELETON_NEWLINE, SKELETON_POINT, SKELETON_STRING, &
                            SKELETON_INDENT, SKELETON_END, SKELETON_RESUME, SKELETON_V2, SKELETON_IF_NOT_MOVED, &
                            SKELETON_NO_INTERACTOR
    use execute_interactively_mod, only: prefix_argument_type, current_prefix_argument, prefix_numeric_value, PREFIX_NONE
    use minibuffer_reading_mod, only: read_string, completion_table_type, completion_ignore_case
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use replace_mod, only: format_prompt
    use basic_subprograms_mod, only: read_y_or_n
    use simple_mod, only: newline_and_indent
    use buffer_mod, only: barf_if_buffer_read_only
    use casefiddle_mod, only: capitalize_string
    use editing_functions_mod, only: user_full_name
    use startup_mod, only: user_mail_address
    use files_mod, only: file_relative_name
    use minibuffer_mod, only: read_file_name
    use print_mod, only: fortran_string
    use data_mod, only: history_setting_type
    use error_mod, only: WRONG_TYPE_ARGUMENT_CODE
    use index_menu_mod, only: imenu_patterns, imenu_pattern_type, imenu_create_index_function, imenu_index_type
    use string_type_mod, only: string_type, append_string
    use data_mod, only: choice_setting_type
    implicit none(type, external)
    private

    public :: initialize_sgml_mode, sgml_mode, html_mode, sgml_indent_line, sgml_calculate_indent, sgml_lexical_context, &
              sgml_beginning_of_tag, sgml_skip_tag_backward, sgml_skip_tag_forward, sgml_delete_tag, sgml_close_tag, &
              sgml_quote, sgml_pretty_print, sgml_xml_guess, sgml_parse_tag_backward, sgml_get_context, sgml_tag_type, &
              sgml_namify_character, sgml_name_character, sgml_parse_dtd, html_current_heading_name, sgml_syntax_propertize, &
              sgml_font_lock_syntactic_face, sgml_tag, sgml_attributes, sgml_auto_attributes, sgml_tag_help, sgml_value, &
              html_href_anchor, html_href_anchor_file, html_name_anchor, html_id_anchor, html_headline_1, html_headline_2, &
              html_headline_3, html_headline_4, html_headline_5, html_headline_6, html_horizontal_rule, html_image, html_line, &
              html_ordered_list, html_unordered_list, html_list_item, html_paragraph, html_div, html_span, html_checkboxes, &
              html_radio_buttons, html_navigational_links, html_html5_template

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    !> The types of a tag (`sgml_tag_type'%type).
    integer, parameter, public :: SGML_TAG_OPEN = 1, SGML_TAG_CLOSE = 2, SGML_TAG_EMPTY = 3, SGML_TAG_COMMENT = 4, &
                                  SGML_TAG_CDATA = 5, SGML_TAG_PI = 6, SGML_TAG_DECLARATION = 7, SGML_TAG_JSP = 8

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
    type(integer_setting_type), target, public :: sgml_basic_offset = integer_setting_type(name="sgml_basic_offset", value=2)

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
    type(integer_setting_type), target, public :: sgml_attribute_offset = &
                                                  integer_setting_type(name="sgml_attribute_offset", value=0)

    !> When true, tag insertion functions will be XML-compliant.
    !> It is set to be buffer-local when the file has a DOCTYPE or an XML
    !> declaration.  Port of sgml-mode.el sgml-xml-mode.
    type(logical_setting_type), target, public :: sgml_xml_mode = logical_setting_type(name="sgml_xml_mode", value=.false.)

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
    type(string_setting_type), target, public :: sgml_validate_command = string_setting_type(name="sgml_validate_command")

    !> If positive, is the maximum distance to search for matching `/'
    !> (0 or less: no limit).  Port of sgml-mode.el sgml-slash-distance.
    type(integer_setting_type), target, public :: sgml_slash_distance = &
                                                  integer_setting_type(name="sgml_slash_distance", value=1000)

    !> Array of symbolic character names without `&' and `;' ("" for
    !> none), by character code.  Port of sgml-mode.el sgml-char-names.
    character(len=6), public :: sgml_character_names(0:255) = ""

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
    !> sgml-tag-face-alist (by HTML mode's value, `html_tag_faces').
    type, public :: sgml_tag_face_type
        character(len=16) :: tag = ""
        character(len=32) :: faces(2) = ""
    end type sgml_tag_face_type

    !> Value of `sgml_tag_faces' for HTML mode.  Port of sgml-mode.el
    !> html-tag-face-alist.
    type(sgml_tag_face_type), allocatable, public :: html_tag_faces(:)

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
        integer(int64) :: parser_state_position = 0
        type(parse_state_type) :: parser_state
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
    character(len=*), parameter :: SGML_NAMESPACE_REGULAR_EXPRESSION = "[_[:alpha:]][-_.[:alnum:]]*", &
                                   SGML_NAME_REGULAR_EXPRESSION = "[_:[:alpha:]][-_.:[:alnum:]]*", &
                                   SGML_TAG_NAME_REGULAR_EXPRESSION = "<\([!/?]?"//SGML_NAME_REGULAR_EXPRESSION//"\)"

    ! The regexps and data of sgml-mode.el.
    character(len=*), parameter :: SGML_CHARACTER_NAMES_DATA(256) = [character(len=6) :: &
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
    character(len=*), parameter :: SGML_START_TAG_REGULAR_EXPRESSION = &
                                   "<[_:[:alpha:]][-_.:[:alnum:]]*\(?:[^_.:""'/><[:alnu" &
                                   //"m:]-]\(?:[^""'/><]\|""[^""]*""\|'[^']*'\)*\)?"
    character(len=*), parameter :: SGML_ATTRIBUTES_REGULAR_EXPRESSION = &
                                   "\(?:[^_.:""'/><[:alnum:]-]\(?:[^""'/><]\|""[^""]*""\|'[" &
                                   //"^']*'\)*\)?"
    character(len=*), parameter :: KW1_REGULAR_EXPRESSION = &
                                   "<\([!?][_:[:alpha:]][-_.:[:alnum:]]*\)"
    character(len=*), parameter :: KW2_REGULAR_EXPRESSION = &
                                   "</?\([_[:alpha:]][-_.[:alnum:]]*\)\(?::\([_:[:alph" &
                                   //"a:]][-_.:[:alnum:]]*\)\)?"
    character(len=*), parameter :: KW3_REGULAR_EXPRESSION = &
                                   "\(?:^\|[ "//TAB//"]\)\([_[:alpha:]][-_.[:alnum:]]*\)\(?::\" &
                                   //"([_:[:alpha:]][-_.:[:alnum:]]*\)\)?=[""']"
    character(len=*), parameter :: KW4_REGULAR_EXPRESSION = &
                                   "[&%][_:[:alpha:]][-_.:[:alnum:]]*;?"
    character(len=*), parameter :: XML_GUESS_REGULAR_EXPRESSION = &
                                   "<!DOCTYPE\s-+\(\w+\)\s-+\(\w+\)\s-+""\([^""]+\)""\s-+" &
                                   //"""\([^""]+\)"""
    character(len=*), parameter :: QUOTE_REGULAR_EXPRESSION = &
                                   "\(&\(amp\|quot\|lt\|gt\|#\([0-9]+\|[xX][[:xdigit:]" &
                                   //"]+\)\)\)\([][<>&;"//LF//TAB//" ""%!'(),/=?]\|$\)"
    character(len=*), parameter :: SGML_PARAGRAPH_START_REGULAR_EXPRESSION = &
                                   "[ "//TAB//"]*$\|[ "//TAB//"]*</?\([_:[:alpha:]][-_.:[:alnum:]]*\(?" &
                                   //":[^_.:""'/><[:alnum:]-]\(?:[^""'/><]\|""[^""]*""\|'[^']" &
                                   //"*'\)*\)?\)?>"
    !> A regular expression matching a head line to be added to the menu.
    !> The first `match_string' should be a number from 1-9.
    !> The second `match_string' matches extra tags and is ignored.
    !> The third `match_string' will be the used in the menu.  Port of
    !> sgml-mode.el html-imenu-regexp.
    character(len=:), allocatable, public :: html_imenu_regular_expression

    character(len=*), parameter :: HTML_SUBPROGRAM_REGULAR_EXPRESSION = &
                                   "<[ "//TAB//achar(13)//LF//"]*\(?:[hH][0-6]\|title\|TITLE\|Title\)[^>]*>" &
                                   //"[ "//TAB//achar(13)//LF//"]*\([^<"//achar(13)//LF//"]*[^ <"//TAB//achar(13)//LF//"]+\)"
    character(len=*), parameter :: DTD_REGULAR_EXPRESSION = &
                                  "<!ELEMENT[ "//TAB//LF//"]+\([^ "//TAB//LF//"]+\)[ "//TAB//LF//"]+[-O][ "//TAB//LF//"]+\([-O]\)" &
                                   //"[ "//TAB//LF//"]+\([^ "//TAB//LF//"]+\)"

    !> The first element of a tag's rule in `sgml_tags' (what follows the
    !> tag): none (SGML_END_TAG_NONE), the default end tag written out (SGML_END_TAG_DEFAULT,
    !> upstream an empty first element), no end
    !> tag (SGML_END_TAG_NO), \n, the end tag separated by newlines
    !> (SGML_END_TAG_NEWLINES), or a skeleton (SGML_END_TAG_SKELETON).
    integer, parameter, public :: SGML_END_TAG_NONE = 0, SGML_END_TAG_DEFAULT = 1, SGML_END_TAG_NO = 2, &
                                  SGML_END_TAG_NEWLINES = 3, SGML_END_TAG_SKELETON = 4

    !> Values of `sgml_quick_keys' besides disabled (.false.) and enabled
    !> (.true.): enabled and indent when closing tags
    !> (SGML_QUICK_KEYS_INDENT), closing the tag at `/'
    !> (SGML_QUICK_KEYS_CLOSE).
    integer, parameter, public :: SGML_QUICK_KEYS_INDENT = 2, SGML_QUICK_KEYS_CLOSE = 3

    !> Values of `html_quick_keys' besides disabled (.false.) and enabled
    !> (.true.): HTML_QUICK_KEYS_INDENT and HTML_QUICK_KEYS_CLOSE, taken
    !> from SGML_QUICK_KEYS_INDENT and SGML_QUICK_KEYS_CLOSE (in HTML mode
    !> they are enabled); HTML_QUICK_KEYS_DEFAULT takes the value of
    !> SGML mode's option when SGML mode first runs.
    integer, parameter, public :: HTML_QUICK_KEYS_INDENT = 2, HTML_QUICK_KEYS_CLOSE = 3, &
                                  HTML_QUICK_KEYS_DEFAULT = -1

    !> An entry of a tag's attribute list in `sgml_tags': an attribute NAME
    !> and its ATTRIBUTERULE (a fixed VALUE, OPTIONAL with possible VALUES:
    !> no value when no input, or possible VALUES), or a plain name (BARE,
    !> upstream a string in the list, as an always present attribute).
    type, public :: sgml_attribute_rule_type
        character(len=:), allocatable :: name
        logical :: bare = .false.
        character(len=:), allocatable :: value
        logical :: optional = .false.
        type(string_type), allocatable :: values(:)
    end type sgml_attribute_rule_type

    !> A tag of `sgml_tags': its NAME, what follows it (END_TAG, see
    !> SGML_END_TAG_NONE ...), for a skeleton its SKELETON (without
    !> interactor) and SKELETON_SLOT (SGML_END_TAG_DEFAULT, _NO or _NEWLINES in
    !> place of the interactor), and its ATTRIBUTES.
    type, public :: sgml_tag_rule_type
        character(len=:), allocatable :: name
        integer :: end_tag = SGML_END_TAG_NONE
        integer :: skeleton_slot = SGML_END_TAG_DEFAULT
        type(skeleton_element_type), allocatable :: skeleton(:)
        type(sgml_attribute_rule_type), allocatable :: attributes(:)
    end type sgml_tag_rule_type

    !> A table of tags (`sgml_tags'), built with `%append' (a tag),
    !> `%append_attribute' and `%append_name' (an entry of the last tag's
    !> attributes).
    type, public :: sgml_tags_type
        type(sgml_tag_rule_type), allocatable :: entries(:)
    contains
        !> Append the tag NAME (END_TAG, or a SKELETON with SKELETON_SLOT).
        procedure :: append => tags_append
        !> Append the attribute NAME (a fixed VALUE, OPTIONAL, VALUES).
        procedure :: append_attribute => tags_append_attribute
        !> Append the plain name NAME.
        procedure :: append_name => tags_append_name
    end type sgml_tags_type

    !> A tag name and short description (`sgml_tag_help_variable').
    type, public :: sgml_tag_help_type
        character(len=16) :: tag = ""
        character(len=100) :: description = ""
    end type sgml_tag_help_type

    !> List of tag names for completing read and insertion rules.
    !> This list is made up of tags (`sgml_tag_rule_type'), each with what
    !> follows it: optionally no endtag (SGML_END_TAG_NO) or the endtag
    !> separated by newlines (SGML_END_TAG_NEWLINES) or a skeleton with none, no end tag
    !> or \n in place of the interactor, followed by an ATTRIBUTERULE (for an
    !> always present attribute) or an attribute list.
    !>
    !> An attribute (`sgml_attribute_rule_type') has an ATTRIBUTERULE: optionally
    !> no value when no input (`optional') followed by an optional list of
    !> possible values.  Port of sgml-mode.el sgml-tag-alist (`html_tags' in
    !> HTML mode).
    type(sgml_tags_type), target, public :: sgml_tags

    !> Value of `sgml_tags' for HTML mode.  Port of sgml-mode.el html-tag-alist.
    type(sgml_tags_type), target, public :: html_tags

    !> List of tag name and short description.  Port of sgml-mode.el
    !> sgml-tag-help (renamed: `sgml_tag_help' is the command;
    !> `html_tag_help' in HTML mode).
    type(sgml_tag_help_type), allocatable, public :: sgml_tag_help_variable(:)

    !> Value of variable `sgml_tag_help_variable' for HTML mode.  Port of
    !> sgml-mode.el html-tag-help.
    type(sgml_tag_help_type), allocatable, public :: html_tag_help(:)

    !> Use <, >, &, /, SPC and `sgml_specials' keys "electrically" when
    !> true.  By setting the option to SGML_QUICK_KEYS_INDENT, Fortmacs will
    !> eagerly reindent the current line when you manually close a tag.
    !> Port of sgml-mode.el sgml-quick-keys (its key bindings are made when
    !> SGML mode first runs, as loading sgml-mode.el makes them).
    type(choice_setting_type), public :: sgml_quick_keys = &
                                         choice_setting_type(name="sgml_quick_keys")

    !> Use C-c X combinations for quick insertion of frequent tags when
    !> true.  This defaults to `sgml_quick_keys', which see.  Port
    !> of sgml-mode.el html-quick-keys.
    type(choice_setting_type), public :: html_quick_keys = &
                                         choice_setting_type(name="html_quick_keys", choice=HTML_QUICK_KEYS_DEFAULT)

    !> Default value for `skeleton_transformation_function' in SGML mode.
    !> Port of sgml-mode.el sgml-transformation-function (null for identity;
    !> its :set function, which updates the SGML buffers, is Deferred with
    !> Customize).
    procedure(skeleton_transformation_procedure), pointer, public :: sgml_transformation_function => null()

    !> The last tag `sgml_tag' read.  Port of sgml-mode.el sgml-tag-last.
    character(len=:), allocatable, public :: sgml_tag_last

    !> The history of `sgml_tag' and `sgml_tag_help'.  Port of sgml-mode.el
    !> sgml-tag-history.
    type(history_setting_type), target, public :: sgml_tag_history = history_setting_type(name="sgml_tag_history")

    ! The prompt and candidates of `read_from_names'.
    character(len=:), allocatable :: reading_prompt
    type(string_type), allocatable :: reading_names(:)

    ! The v2 of `sgml_tag': its tag's end tag kind, rule, and whether a
    ! skeleton rule has \n in place of its interactor.
    integer :: tag_first = SGML_END_TAG_NONE, tag_rule = 0
    logical :: tag_slot_newline = .false.
    character(len=:), allocatable :: tag_string

    ! completion-ignore-case while a skeleton is inserted.
    logical :: further_saved_ignore_case = .false.

    ! The v1 and v2 of the checkboxes and radio buttons skeletons.
    character(len=:), allocatable :: input_name, input_newline
    logical :: input_checked = .false.

    ! Whether the quick keys have been bound.
    logical :: quick_keys_set = .false.

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

    ! Find or make the current buffer's entry in `locals', reusing the
    ! entry of a closed buffer; K is its index (0 without a current
    ! buffer).  Not a port.
    recursive subroutine make_local_entry(k)
        integer, intent(out) :: k
        type(sgml_local_type) :: entry

        k = local_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        if (k > size(locals)) locals = [locals, entry]
        locals(k) = sgml_local_type(serial=buffers(current)%serial)
    end subroutine make_local_entry

    ! kill-all-local-variables: forget the current buffer's sgml-mode.el
    ! variables.
    ! Not a port.
    subroutine clear_sgml_locals()
        integer :: k

        k = local_index()
        if (k > 0) locals(k)%serial = 0
    end subroutine clear_sgml_locals

    ! Whether the current buffer is in HTML mode (has its variables).
    ! Not a port.
    recursive logical function html_buffer()
        integer :: k

        html_buffer = .false.
        k = local_index()
        if (k > 0) html_buffer = locals(k)%html
    end function html_buffer

    ! The character after POSITION, -1 if none.
    ! Not a port.
    recursive integer function after_character_at(position)
        integer(int64), intent(in) :: position
        integer(int32) :: c
        logical :: found

        after_character_at = -1
        if (position < point_minimum() .or. position >= point_maximum()) return
        call character_after(c, found, position)
        if (found) after_character_at = c
    end function after_character_at

    ! The character before POSITION, -1 if none.
    ! Not a port.
    recursive integer function before_character_at(position)
        integer(int64), intent(in) :: position
        integer(int32) :: c
        logical :: found

        before_character_at = -1
        if (position <= point_minimum() .or. position > point_maximum()) return
        call character_before(c, found, position)
        if (found) before_character_at = c
    end function before_character_at

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

    ! looking-at REGULAR_EXPRESSION.
    ! Not a port.
    recursive logical function at(regular_expression)
        character(len=*), intent(in) :: regular_expression
        type(status_type) :: st

        at = looking_at(regular_expression, st)
    end function at

    ! looking-at-p REGULAR_EXPRESSION.
    ! Not a port.
    recursive logical function is_at(regular_expression)
        character(len=*), intent(in) :: regular_expression
        type(status_type) :: st

        is_at = looking_at(regular_expression, st, inhibit_modify=.true.)
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

    ! Whether NAME is in LIST, ignoring case (assoc-string with
    ! IGNORE-CASE).
    ! Not a port.
    pure logical function member_ignore_case(name, list)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: list(:)
        integer :: i

        member_ignore_case = .false.

        do i = 1, size(list)
            if (downcase_ascii(trim(list(i))) == downcase_ascii(name)) member_ignore_case = .true.
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
        integer(int64) :: position

        call marker_position(mark, position, st)
        call goto_character(position)
        call destroy_marker(mark, st)
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
        logical :: in_parenthesis

        face = ""
        ! Don't use string face outside of tags.
        in_parenthesis = .false.
        if (allocated(state%open_parentheses)) in_parenthesis = size(state%open_parentheses) > 0

        if (in_parenthesis .and. state%instring >= 0) then
            face = "font_lock_string_face"
        else if (state%incomment /= 0) then
            face = "font_lock_comment_face"
        end if
    end subroutine sgml_font_lock_syntactic_face

    ! Return PARSER_STATE at POSITION, fixing the syntax of any lone `>' along the way.
    ! Port of sgml-mode.el:sgml--syntax-propertize-ppss.
    recursive function syntax_propertize_parser_state(position) result(state)
        integer(int64), intent(in) :: position
        type(parse_state_type) :: state, old
        type(status_type) :: st
        integer :: k

        call make_local_entry(k)
        old = locals(k)%parser_state
        call parse_partial_expression(locals(k)%parser_state_position, position, state, st, targetdepth=-1_int64, oldstate=old)

        do while (state%depth == -1)
            call put_text_property(point() - 1, point(), value_type(name="syntax_table"), syntax_value("."), st)
            ! Hack attack: rather than recompute the ppss from
            ! (car sgml--syntax-propertize-ppss), we manually "fix it".
            state%depth = 0
            old = state
            call parse_partial_expression(point(), position, state, st, targetdepth=-1_int64, oldstate=old)
        end do

        locals(k)%parser_state = state
        locals(k)%parser_state_position = position
    end function syntax_propertize_parser_state

    !> Syntactic keywords for `sgml_mode'.  Port of
    !> sgml-mode.el:sgml-syntax-propertize.
    recursive subroutine sgml_syntax_propertize(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(parse_state_type) :: state
        integer :: k

        status = status_type()
        call make_local_entry(k)
        call goto_character(start)
        call syntax_parser_state(state, start)
        locals(k)%parser_state_position = start
        locals(k)%parser_state = state
        call goto_character(start)
        if (state%depth < 0) return
        call syntax_propertize_inside(end)
        propertize_end = end
        call propertize_rules%apply(point(), end, status)
        ! Catch any '>' after the last quote.
        state = syntax_propertize_parser_state(end)
    end subroutine sgml_syntax_propertize

    ! Inside a declaration or processing instruction, put the closing
    ! fence before END.  Port of sgml-mode.el:sgml-syntax-propertize-inside.
    recursive subroutine syntax_propertize_inside(end)
        integer(int64), intent(in) :: end
        type(parse_state_type) :: state
        type(status_type) :: st
        character(len=:), allocatable :: endre
        integer(int64) :: original_point, position

        call syntax_parser_state(state)
        if (state%instring /= ST_STRING_STYLE) return
        original_point = point()
        call goto_character(state%comstr_start)

        if (is_at("<!\[CDATA\[")) then
            endre = "]]>"
        else if (is_at("<\?")) then
            endre = ">"
            if (sgml_xml_mode%value) endre = "\?>"
        else
            endre = ">"
        end if

        call goto_character(original_point)
        call regular_expression_search_forward(endre, position, st, bound=end, noerror=SEARCH_MOVE)
        if (position > 0) call put_text_property(point() - 1, point(), value_type(name="syntax_table"), syntax_value("|<"), st)
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
        integer(int64) :: original_point, moved

        status = status_type()
        property%apply = .false.

        if (after_character_at(point()) == after_character_at(mb(0))) then
            ! Fast-track case.
            call forward_character(st, 1_int64)
            return
        end if

        ! Point has moved to the end of the text we matched after the
        ! quote, but this risks overlooking a match to one of the other
        ! regexp in the rules.  We could just (goto-char (match-end 1)) to
        ! solve this, but that would be too easy, so instead we only move
        ! back enough to avoid skipping comment ender, which happens to be
        ! the only one that we could have overlooked.
        if (after_character_at(point()) == iachar(">")) moved = skip_characters_backward("-", st)
        ! Be careful to call `syntax-ppss' on a position before the one
        ! we're going to change, so as not to need to flush the data we just
        ! computed.
        original_point = point()
        state = syntax_propertize_parser_state(mb(0))
        call goto_character(original_point)

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
        kw(1) = font_lock_keyword(KW1_REGULAR_EXPRESSION, highlights=h1)
        h2(1) = font_lock_highlight(1, face_function=function_name_face)
        h2(2) = h(2, "font_lock_function_name_face", lax=.true.)
        kw(2) = font_lock_keyword(KW2_REGULAR_EXPRESSION, highlights=h2)
        ! FIXME: this doesn't cover the variables using a default value.
        h2(1) = font_lock_highlight(1, face_function=variable_name_face)
        h2(2) = h(2, "font_lock_variable_name_face", lax=.true.)
        kw(3) = font_lock_keyword(KW3_REGULAR_EXPRESSION, highlights=h2)
        h1(1) = h(0, "font_lock_variable_name_face")
        kw(4) = font_lock_keyword(KW4_REGULAR_EXPRESSION, highlights=h1)
    end subroutine keywords_1

    ! The faces of the tag `match_string(1)' in `sgml_tag_faces'
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
        tag = downcase_ascii(match_string_no_properties(1))

        do i = 1, size(html_tag_faces)
            if (trim(html_tag_faces(i)%tag) /= tag) cycle
            n = 0

            do j = 1, 2
                if (len_trim(html_tag_faces(i)%faces(j)) == 0) cycle
                n = n + 1
                faces(n) = value_type(name=trim(html_tag_faces(i)%faces(j)))
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
            allocate (character(len=16) :: tags(size(html_tag_faces)))

            do i = 1, size(html_tag_faces)
                tags(i) = html_tag_faces(i)%tag
            end do
        end if

        h1(1) = font_lock_highlight(3, face_function=no_face, properties_function=tag_faces)
        allocate (kw(5))
        kw(1:4) = k1
        kw(5) = font_lock_keyword("<"//regular_expression_optimization(tags, &
                                     REGULAR_EXPRESSION_OPTIMIZATION_GROUP)//"\([ "//TAB//"][^>]*\)?>\([^<]+\)</\1>", highlights=h1)
    end subroutine keywords_2

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Guess whether the current buffer is XML.  Return true if so.
    !> Port of sgml-mode.el:sgml-xml-guess.
    recursive logical function sgml_xml_guess() result(xml)
        type(status_type) :: st
        character(len=:), allocatable :: file, ext
        integer(int64) :: original_point, position, k

        original_point = point()
        call goto_character(point_minimum())
        file = ""
        if (allocated(buffers(current)%filename)) file = buffers(current)%filename
        ext = extension(file)
        xml = ext == "xml"
        ! Maybe the buffer-size check isn't needed, I don't know.
        if (.not. xml) xml = point_maximum() == point_minimum() .and. ext == "xhtml"
        if (.not. xml) xml = at("\s-*<\?xml")

        if (.not. xml) then
            call regular_expression_search_forward(XML_GUESS_REGULAR_EXPRESSION, position, st, noerror=SEARCH_NOERROR)

            if (position > 0) then
                k = string_match("X\(HT\)?ML", match_string_no_properties(3), st)
                xml = k > 0
            end if
        end if

        call goto_character(original_point)

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

    !> Insert ARGUMENT slash characters.
    !> Behaves electrically if `sgml_quick_keys' is true.  Port of
    !> sgml-mode.el:sgml-slash.
    recursive subroutine sgml_slash(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        integer(int64) :: k

        status = status_type()

        if (.not. (before_character_at(point()) == iachar("<") .and. argument == 1)) then
            call sgml_slash_matching(argument, status)
        else if (sgml_quick_keys%choice == SGML_QUICK_KEYS_INDENT) then
            call insert("/", status)
            call indent_according_to_mode(status)
        else if (sgml_quick_keys%choice == SGML_QUICK_KEYS_CLOSE) then
            call delete_character(-1_int64, status)
            if (status%ok()) call sgml_close_tag(status)
        else
            do k = 1, argument
                call insert("/", status)
            end do
        end if
    end subroutine sgml_slash

    !> Insert `/' and display any previous matching `/'.
    !> Two `/'s are treated as matching if the first `/' ends a
    !> net-enabling start tag, and the second `/' is the corresponding null
    !> end tag.  Port of sgml-mode.el:sgml-slash-matching.
    recursive subroutine sgml_slash_matching(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: oldpos, blinkpos, level, position, tagend, beginning_of_accessible_portion, end_of_accessible_portion, &
                          original_point

        call insert(repeat("/", int(max(argument, 0_int64))), status)
        if (.not. status%ok() .or. argument <= 0) return
        oldpos = point()
        blinkpos = 0
        level = 0
        original_point = point()
        beginning_of_accessible_portion = buffers(current)%beginning_of_accessible_portion
        end_of_accessible_portion = buffers(current)%end_of_accessible_portion

        if (sgml_slash_distance%value > 0) then
            call narrow_to_region(max(point_minimum(), point() - sgml_slash_distance%value), oldpos, st)
        end if

        call regular_expression_search_backward(SGML_START_TAG_REGULAR_EXPRESSION, position, st, bound=point_minimum(), &
                                                noerror=SEARCH_NOERROR)

        if (.not. (position > 0 .and. me(0) == oldpos - 1)) then
            call goto_character(oldpos - 1)

            do while (blinkpos == 0)
                call search_backward("/", position, st, bound=point_minimum(), noerror=SEARCH_NOERROR)
                if (position == 0) exit
                tagend = 0
                position = point()
                call regular_expression_search_backward(SGML_START_TAG_REGULAR_EXPRESSION, tagend, st, bound=point_minimum(), &
                                                        noerror=SEARCH_NOERROR)
                if (tagend > 0) tagend = me(0)
                call goto_character(position)

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
        call narrow_to_region(beginning_of_accessible_portion, end_of_accessible_portion, st)

        if (blinkpos > 0) then
            call goto_character(blinkpos)

            if (is_position_visible_in_window(point(), selected_window())) then
                call sit_for(1000_int32)
            else
                call message("Matches "//text_between(line_beginning_position(), blinkpos + 1))
            end if
        end if

        call goto_character(original_point)
    end subroutine sgml_slash_matching

    !> Insert a symbolic character name according to `sgml_character_names'
    !> (CHAR, read when absent).  Port of sgml-mode.el:sgml-name-char.
    recursive subroutine sgml_name_character(status, char)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: char
        integer(int64) :: c
        character(len=:), allocatable :: text

        call insert("&", status)
        if (.not. status%ok()) return

        if (present(char)) then
            c = char
        else
            call read_quoted_character(c, status)
            if (.not. status%ok()) return
        end if

        call delete_character(-1_int64, status)
        call character_to_string(int(c, int32), text, status)
        if (.not. status%ok()) return
        call insert(text, status)
        call undo_boundary()
        call sgml_namify_character(status)
    end subroutine sgml_name_character

    !> Change the char before point into its `&name;' equivalent.
    !> Uses `sgml_character_names'.  Port of sgml-mode.el:sgml-namify-char
    !> (characters beyond Latin-1 get their number).
    recursive subroutine sgml_namify_character(status)
        type(status_type), intent(out) :: status
        character(len=16) :: buffer
        integer :: c

        status = status_type()
        c = before_character_at(point())

        if (c < 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No char before point")
            return
        end if

        call delete_character(-1_int64, status)

        if (c < 256) then
            if (len_trim(sgml_character_names(c)) > 0) then
                call insert("&"//trim(sgml_character_names(c))//";", status)
                return
            end if
        end if

        write (buffer, "(i0)") c
        call insert("&#"//trim(buffer)//";", status)
    end subroutine sgml_namify_character

    ! The command `sgml_namify_character'.
    recursive subroutine execute_sgml_namify_character(status)
        type(status_type), intent(out) :: status

        call sgml_namify_character(status)
    end subroutine execute_sgml_namify_character

    ! The command `sgml_name_character'.
    recursive subroutine execute_sgml_name_character(status)
        use buffer_mod, only: barf_if_buffer_read_only
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call sgml_name_character(status)
    end subroutine execute_sgml_name_character

    ! The event typed, as a character (-1 if not one).
    ! Not a port.
    recursive integer(int64) function event_character()
        use value_mod, only: xfixnum

        event_character = -1
        if (vtag(last_command_event) == V_INTEGER) event_character = xfixnum(last_command_event)
    end function event_character

    !> Insert a symbolic character name according to `sgml_character_names'.
    !> Port of sgml-mode.el:sgml-name-self.
    recursive subroutine sgml_name_self(status)
        type(status_type), intent(out) :: status

        call sgml_name_character(status, event_character())
    end subroutine sgml_name_self

    !> Insert a symbolic character name according to `sgml_character_names'
    !> when `sgml_name_8bit_mode' is on.  Port of
    !> sgml-mode.el:sgml-maybe-name-self.
    recursive subroutine sgml_maybe_name_self(status)
        type(status_type), intent(out) :: status

        if (minor_modes%sgml_name_8bit_mode%state%value) then
            call sgml_name_character(status, event_character())
        else
            call self_insert_command(1_int64, event_character(), status)
        end if
    end subroutine sgml_maybe_name_self

    !> Name self unless in position to end a tag or a prefix ARGUMENT is given.
    !> Port of sgml-mode.el:sgml-maybe-end-tag.
    recursive subroutine sgml_maybe_end_tag(status)
        use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, PREFIX_NONE
        type(status_type), intent(out) :: status
        integer(int64) :: start
        integer :: ctype

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            call self_insert_command(prefix_numeric_value(current_prefix_argument), event_character(), status)
            return
        end if

        call sgml_lexical_context(ctype, start)

        if (ctype == SGML_CONTEXT_TAG) then
            call self_insert_command(1_int64, event_character(), status)
        else
            call sgml_name_self(status)
        end if
    end subroutine sgml_maybe_end_tag

    !> Skip to beginning of tag or matching opening tag if present.
    !> With prefix argument ARGUMENT, repeat this ARGUMENT times.
    !> Return true if we skipped over matched tags.  Port of
    !> sgml-mode.el:sgml-skip-tag-backward.
    recursive logical function sgml_skip_tag_backward(argument) result(skipped)
        integer(int64), intent(in) :: argument
        type(status_type) :: st
        character(len=:), allocatable :: regular_expression
        integer(int64) :: n, position
        logical :: saved_fold, dummy

        skipped = .true.
        n = argument

        do while (n >= 1)
            call search_backward("<", position, st, noerror=SEARCH_NOERROR)

            if (at("</\([^ "//LF//TAB//">]+\)")) then
                ! end tag, skip any nested pairs
                saved_fold = case_fold_search%value
                case_fold_search%value = .true.
                ! Ignore empty tags like <foo/>.
                regular_expression = "</?"//regular_expression_quote(match_string(1))//"\([^>]*[^/>]\)?>"

                do
                    call regular_expression_search_backward(regular_expression, position, st, noerror=SEARCH_NOERROR)
                    if (position == 0) exit
                    if (after_character_at(point() + 1) /= iachar("/")) exit
                    call forward_character(st, 1_int64)
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
    recursive subroutine execute_sgml_skip_tag_backward(status)
        use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
        type(status_type), intent(out) :: status
        logical :: skipped

        status = status_type()
        skipped = sgml_skip_tag_backward(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_sgml_skip_tag_backward

    !> Skip to end of tag or matching closing tag if present.
    !> With prefix argument ARGUMENT, repeat this ARGUMENT times.
    !> Return true if after a closing tag.  Port of
    !> sgml-mode.el:sgml-skip-tag-forward.
    recursive logical function sgml_skip_tag_forward(argument) result(after_close)
        integer(int64), intent(in) :: argument
        type(handle_type) :: saved
        type(status_type) :: st
        character(len=:), allocatable :: regular_expression
        integer(int64) :: n, position, moved, here
        logical :: saved_fold, closing

        after_close = .true.
        call use_tag_table(saved)
        n = argument

        do while (n >= 1)
            moved = skip_characters_forward("^<>", st)
            if (following_character() == iachar(">")) call up_group(-1_int64, st)

            if (at("<\([^/ "//LF//TAB//">]+\)\([^>]*[^/>]\)?>")) then
                ! start tag, skip any nested same pairs _and_ closing tag
                saved_fold = case_fold_search%value
                case_fold_search%value = .true.
                ! Ignore empty tags like <foo/>.
                regular_expression = "</?"//regular_expression_quote(match_string(1))//"\([^>]*[^/>]\)?>"
                call sgml_forward_expression(1_int64, st)
                here = point()
                closing = .false.

                ! FIXME: This re-search-forward will mistakenly match
                ! tag-like text inside attributes.
                do
                    call regular_expression_search_forward(regular_expression, position, st, noerror=SEARCH_NOERROR)
                    if (position == 0) exit
                    closing = after_character_at(mb(0) + 1) == iachar("/")
                    if (closing) exit
                    call goto_character(mb(0))
                    if (.not. sgml_skip_tag_forward(1_int64)) exit
                    closing = .false.
                end do

                case_fold_search%value = saved_fold

                if (.not. closing) then
                    call goto_character(here)
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
    recursive subroutine execute_sgml_skip_tag_forward(status)
        use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
        type(status_type), intent(out) :: status
        logical :: skipped

        status = status_type()
        skipped = sgml_skip_tag_forward(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_sgml_skip_tag_forward

    ! Return t if the text before point matches STRING.  Port of
    ! sgml-mode.el:sgml-looking-back-at.
    recursive logical function looking_back_at(string)
        character(len=*), intent(in) :: string
        integer(int64) :: start

        looking_back_at = .false.
        start = point() - len(string)
        if (start < point_minimum()) return
        looking_back_at = text_between(start, point()) == string
    end function looking_back_at

    !> Delete tag on or after cursor, and matching closing or opening tag.
    !> With prefix argument ARGUMENT, repeat this ARGUMENT times.  Port of
    !> sgml-mode.el:sgml-delete-tag.
    recursive subroutine sgml_delete_tag(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(handle_type) :: mark
        character(len=:), allocatable :: name
        integer(int64) :: n, close_position, open_position, here, shortage, tag_position
        logical :: dummy

        status = status_type()
        n = argument

        do while (n >= 1)
            call excursion_begin(mark)
            close_position = 0

            if (at("[ "//TAB//LF//"]*<")) then
                ! just before tag
                if (after_character_at(me(0)) == iachar("/")) then
                    ! closing tag
                    close_position = point()
                    call goto_character(me(0))
                end if
            else
                ! on tag?
                here = point()
                call sgml_beginning_of_tag(name, .false.)

                if (len(name) > 0) then
                    if (name(1:1) == "/") close_position = point()
                end if

                call goto_character(here)

                if (close_position == 0) then
                    ! not on closing tag
                    dummy = sgml_skip_tag_backward(1_int64)

                    if (following_character() /= iachar("<")) then
                        call not_on_tag()
                        return
                    end if

                    tag_position = point()
                    call sgml_forward_expression(1_int64, status)
                    if (.not. status%ok()) return

                    if (point() <= here) then
                        call not_on_tag()
                        return
                    end if

                    call goto_character(tag_position)
                end if
            end if

            if (close_position > 0) then
                dummy = sgml_skip_tag_backward(1_int64)
                open_position = point()
                call goto_character(close_position)
                call cut_expression(1_int64, status)
            else
                open_position = point()

                if (sgml_skip_tag_forward(1_int64)) then
                    if (.not. looking_back_at("/>")) call cut_expression(-1_int64, status)
                end if
            end if

            if (.not. status%ok()) return
            ! Delete any resulting empty line.  If we didn't kill-sexp, this
            ! *should* do nothing, because we're right after the tag.
            shortage = forward_line(0_int64)
            if (at("\(?:[ "//TAB//"]*$\)"//LF//"?")) call delete_region(mb(0), me(0), status)
            call goto_character(open_position)
            call cut_expression(1_int64, status)
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
            call set_error(status, GENERIC_ERROR_CODE, "Not on or before tag")
        end subroutine not_on_tag
    end subroutine sgml_delete_tag

    ! The command `sgml_delete_tag'.
    recursive subroutine execute_sgml_delete_tag(status)
        use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
        type(status_type), intent(out) :: status

        call sgml_delete_tag(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_sgml_delete_tag

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

        call make_local_entry(k)
        locals(k)%saved_validate_command = command
        call save_some_buffers(.not. compilation_ask_about_save%value, .false., status)
        if (.not. status%ok()) return
        call compilation_start(command, status)
    end subroutine sgml_validate

    ! The command `sgml_validate': read the command.
    recursive subroutine execute_sgml_validate(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: initial_input, command, name
        integer :: k, slash

        call make_local_entry(k)

        if (allocated(locals(k)%saved_validate_command)) then
            initial_input = locals(k)%saved_validate_command
        else
            initial_input = ""
            if (allocated(sgml_validate_command%value)) initial_input = sgml_validate_command%value

            if (allocated(buffers(current)%filename)) then
                name = buffers(current)%filename
                slash = index(name, "/", back=.true.)
                initial_input = initial_input//" "//shell_quote(name(slash + 1:))
            end if
        end if

        call read_shell_command("Validate command: ", command, status, initial_input=initial_input)
        if (.not. status%ok()) return
        call sgml_validate(command, status)
    end subroutine execute_sgml_validate

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
        integer(int64) :: original_point, moved

        original_point = point()
        moved = skip_characters_backward(" "//TAB, st)
        is_at_indentation = is_beginning_of_line()
        call goto_character(original_point)
    end function is_at_indentation

    !> Return the lexical context at point as two values: CTYPE, an
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
        integer(int64) :: original_point, position, text_start, found, moved
        logical :: ok
        ! The kind of STATE: none (nil), a parse, an unclosed cdata,
        ! comment or pi.
        integer, parameter :: NONE = 0, PARSED = 1, CDATA = 2, COMMENT = 3, PI = 4
        integer :: kind

        original_point = point()
        position = point()
        text_start = point()
        kind = NONE
        skip = ""
        if (allocated(comment_start_skip%value)) skip = comment_start_skip%value
        end_skip = ""
        if (allocated(comment_end_skip%value)) end_skip = comment_end_skip%value

        if (present(limit)) then
            call goto_character(limit)
        else
            ! Skip tags backwards until we find one at indentation
            do
                call sgml_parse_tag_backward(tag, ok)
                if (.not. ok) exit
                if (is_at_indentation()) exit
            end do
        end if

        call use_tag_table(saved)

        do while (point() < position)
            ! When entering this loop we're inside text.
            text_start = point()
            moved = skip_characters_forward("^<", st, position)

            if (point() == position) then
                ! We got to the end without seeing a tag.
                kind = NONE
            else if (at("<!\[[A-Z]+\[")) then
                ! We've found a CDATA section or similar.
                start = point()
                kind = NONE
                call search_forward("]]>", found, st, bound=position, noerror=SEARCH_MOVE)
                if (found == 0) kind = CDATA
            else if (at(skip)) then
                ! parse-partial-sexp doesn't handle <!-- comments -->, or
                ! only if ?- is in sgml-specials, so match explicitly
                start = point()
                kind = NONE
                call regular_expression_search_forward(end_skip, found, st, bound=position, noerror=SEARCH_MOVE)
                if (found == 0) kind = COMMENT
            else if (sgml_xml_mode%value .and. at("<\?")) then
                ! Processing Instructions.
                start = point()
                kind = NONE
                call search_forward("?>", found, st, bound=position, noerror=SEARCH_MOVE)
                if (found == 0) kind = PI
            else
                ! We've reached a tag.  Parse it.
                ! FIXME: Handle net-enabling start-tags
                call parse_partial_expression(point(), position, state, st, targetdepth=0_int64)
                kind = PARSED
            end if
        end do

        call restore_table(saved)
        call goto_character(original_point)

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
            call goto_character(start)
            if (at(SGML_TAG_NAME_REGULAR_EXPRESSION)) name = match_string_no_properties(1)
        else if (.not. only_immediate .and. ctype /= SGML_CONTEXT_TEXT) then
            call goto_character(start)
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
        character(len=:), allocatable :: number, replacement
        integer(int64) :: beginning_of_accessible_portion, end_of_accessible_portion, position, code
        integer :: c, subexp, ios
        logical :: unquote
        type(handle_type) :: beginning_of_accessible_portion_marker, end_of_accessible_portion_marker

        unquote = .false.
        if (present(unquotep)) unquote = unquotep
        ! save-restriction.
        call copy_marker_at(buffers(current)%beginning_of_accessible_portion, beginning_of_accessible_portion_marker, .false., &
                            status)
        call copy_marker_at(buffers(current)%end_of_accessible_portion, end_of_accessible_portion_marker, .true., status)
        call narrow_to_region(start, end, status)
        call goto_character(point_minimum())

        if (unquote) then
            ! FIXME: We should unquote other named character references as
            ! well.
            do
                call regular_expression_search_forward(QUOTE_REGULAR_EXPRESSION, position, status, noerror=SEARCH_NOERROR)
                if (position == 0) exit
                c = after_character_at(mb(2))

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
                    number = match_string(3)

                    if (number(1:1) == "x" .or. number(1:1) == "X") then
                        read (number(2:), "(z16)", iostat=ios) code
                    else
                        read (number, *, iostat=ios) code
                    end if

                    if (ios /= 0) code = 0
                end select

                ! A code out of range is an invalid character.
                call character_to_string(int(merge(code, -1_int64, code >= 0 .and. code <= huge(0_int32)), int32), &
                                         replacement, status)
                if (.not. status%ok()) exit
                subexp = 1
                if (before_character_at(me(0)) == iachar(";")) subexp = 0
                call replace_match(replacement, status, fixedcase=.true., literal=.true., subexp=subexp)
            end do
        else
            do
                call regular_expression_search_forward("[&<>""']", position, status, noerror=SEARCH_NOERROR)
                if (position == 0) exit

                select case (before_character_at(point()))
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
        call marker_position(beginning_of_accessible_portion_marker, beginning_of_accessible_portion, status)
        call marker_position(end_of_accessible_portion_marker, end_of_accessible_portion, status)
        call narrow_to_region(beginning_of_accessible_portion, end_of_accessible_portion, status)
        call destroy_marker(beginning_of_accessible_portion_marker, status)
        call destroy_marker(end_of_accessible_portion_marker, status)
    end subroutine sgml_quote

    ! The command `sgml_quote'.
    recursive subroutine execute_sgml_quote(status)
        use execute_interactively_mod, only: current_prefix_argument, PREFIX_NONE
        use editing_functions_mod, only: region_beginning, region_end
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        call sgml_quote(b, e, status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_sgml_quote

    !> Simple-minded pretty printer for SGML.
    !> Re-indents the code and inserts newlines between BEGINNING and END.
    !> You might want to turn on `auto_fill_mode' to get better results.
    !> Port of sgml-mode.el:sgml-pretty-print.
    recursive subroutine sgml_pretty_print(beginning_in, end_in, status)
        integer(int64), intent(in) :: beginning_in, end_in
        type(status_type), intent(out) :: status
        type(handle_type) :: mark, end_marker, saved
        type(status_type) :: st
        integer(int64) :: end, position, moved

        status = status_type()
        call excursion_begin(mark)
        call goto_character(min(beginning_in, end_in))
        end = max(beginning_in, end_in)
        ! Don't use narrowing because it screws up auto-indent.
        call copy_marker_at(end, end_marker, .true., st)
        call use_tag_table(saved)

        do
            call marker_position(end_marker, end, st)
            call regular_expression_search_forward("<", position, st, bound=end, noerror=SEARCH_NOERROR)
            if (position == 0) exit
            call goto_character(mb(0))
            moved = skip_characters_backward(" "//TAB, st)

            if (.not. is_beginning_of_line()) then
                call goto_character(mb(0))
                call reindent_then_newline_and_indent(status)
            else
                call goto_character(mb(0))
            end if

            call sgml_forward_expression(1_int64, st)
            if (.not. st%ok()) exit
        end do

        call restore_table(saved)
        call destroy_marker(end_marker, st)
        call excursion_end(mark)
    end subroutine sgml_pretty_print

    ! The command `sgml_pretty_print'.
    recursive subroutine execute_sgml_pretty_print(status)
        use editing_functions_mod, only: region_beginning, region_end
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        call sgml_pretty_print(b, e, status)
    end subroutine execute_sgml_pretty_print

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
        integer(int64) :: original_point

        original_point = point()
        call use_tag_table(saved)
        call parse_partial_expression(start, end, pps, st, targetdepth=2_int64)
        is_tag_text = pps%depth == 0
        call restore_table(saved)
        call goto_character(original_point)
    end function is_tag_text

    ! Search backward for a '<' or '>' character.
    ! The character must have open or close syntax.
    ! Returns t if found, nil otherwise.  Port of
    ! sgml-mode.el:sgml--find-<>-backward.
    recursive logical function find_angle_backward(limit) result(found)
        use syntax_mod, only: syntax_class_at
        integer(int64), intent(in) :: limit
        type(status_type) :: st
        type(value_type) :: face
        integer(int64) :: position
        integer :: class
        logical :: comment

        found = .false.

        do
            if (limit > 0) then
                call regular_expression_search_backward("[<>]", position, st, bound=limit, noerror=SEARCH_MOVE)
            else
                call regular_expression_search_backward("[<>]", position, st, noerror=SEARCH_MOVE)
            end if

            if (position == 0) return
            ! If this character has "open" or "close" syntax, then we've
            ! found the one we want.
            class = syntax_class_at(after_character_at(point()), point())

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
        integer(int64) :: limit_position, tag_start, tag_end, position, found
        integer :: c

        limit_position = 0
        if (present(limit)) limit_position = limit
        ok = find_angle_backward(limit_position)
        if (.not. ok) return
        tag_start = 0

        if (after_character_at(point()) == iachar("<")) then
            ! Oops!! Looks like we were not in a textual context after all!.
            ! Let's try to recover.
            ! Remember the tag-start so we don't need to look for it later.
            ! This is not just an optimization but also makes sure we don't
            ! get stuck in infloops in cases where "looking back for <"
            ! would not go back far enough.
            tag_start = point()
            position = point()
            call use_tag_table(saved)
            ! FIXME: This does not correctly skip over PI and CDATA tags.
            call sgml_forward_expression(1_int64, st)
            call restore_table(saved)

            if (.not. st%ok()) then
                ! This < seems to be just a spurious one, let's ignore it.
                call goto_character(position)
                call sgml_parse_tag_backward(tag, ok, limit)
                return
            end if

            ! Check it is really a tag, without any extra < or > inside.
            if (.not. is_tag_text(position, point())) then
                call goto_character(position)
                call sgml_parse_tag_backward(tag, ok, limit)
                return
            end if

            call forward_character(st, -1_int64)
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
                call regular_expression_search_backward("<!\[[A-Z]+\[", found, st, noerror=SEARCH_NOERROR)
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
                call goto_character(tag_end)
                call sgml_forward_expression(-1_int64, st)
                call restore_table(saved)

                if (.not. st%ok()) then
                    ! This > isn't really the end of a tag. Skip it.
                    call goto_character(tag_end - 1)
                    call sgml_parse_tag_backward(tag, ok, limit)
                    return
                end if

                tag_start = point()
            end if

            call goto_character(tag_start + 1)
            c = after_character_at(point())

            select case (c)
            case (iachar("!"))
                ! declaration
                tag%type = SGML_TAG_DECLARATION
            case (iachar("?"))
                ! processing-instruction
                tag%type = SGML_TAG_PI
            case (iachar("%"))
                ! JSP tags
                tag%type = SGML_TAG_JSP
            case (iachar("/"))
                ! close-tag
                call forward_character(st, 1_int64)
                tag%type = SGML_TAG_CLOSE
                tag%name = parse_tag_name()
            case default
                ! open or empty tag
                tag%type = SGML_TAG_OPEN
                tag%name = parse_tag_name()
                if (before_character_at(tag_end - 1) == iachar("/") .or. is_empty_tag(tag%name)) tag%type = SGML_TAG_EMPTY
            end select
        end if

        call goto_character(tag_start)
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

        context = [tag, context]
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
        moved = skip_characters_backward(" "//TAB//LF, st)

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
                else if (downcase_ascii(tag_info%name) == downcase_ascii(trim(stack(1)))) then
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
                                if (downcase_ascii(tag_info%name) == downcase_ascii(trim(stack(i + 1)))) exit
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
        integer(int64) :: start, original_point
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
            original_point = point()
            call sgml_get_context(context)
            call goto_character(original_point)

            if (size(context) > 0) then
                call insert("</"//context(size(context))%name//">", status)
                call indent_according_to_mode(status)
            end if
        case default
            call set_error(status, GENERIC_ERROR_CODE, "Nothing to close")
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
        integer(int64) :: start, here, there, original_point, moved, shortage, found_position
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
            original_point = point()
            call goto_character(start)
            mark = at("<!--")
            call goto_character(original_point)

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
            original_point = point()
            call sgml_get_context(context)
            call goto_character(original_point)
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
                call goto_character(start)
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
                moved = skip_characters_forward(" "//TAB, st)
            else
                call goto_character(start)
                ! Skip `<!' to get to the `--' with which we want to align.
                call search_forward("--", found_position, st, noerror=SEARCH_NOERROR)
                call goto_character(mb(0))
            end if

            if (.not. mark .and. at("--")) then
                call forward_character(st, 2_int64)
                moved = skip_characters_forward(" "//TAB, st)
            end if

            column = int(current_column())
        case (SGML_CONTEXT_TAG)
            call goto_character(start + sgml_attribute_offset%value)
            ! Skip tag name.
            moved = skip_characters_forward("^ "//TAB//LF, st)
            moved = skip_characters_forward(" "//TAB, st)

            if (.not. is_end_of_line()) then
                column = int(current_column())
            else
                ! This is the first attribute: indent.
                call goto_character(start + sgml_attribute_offset%value)
                column = int(current_column() + sgml_basic_offset%value)
            end if
        case default
            ! text
            do while (at("</"))
                call sgml_forward_expression(1_int64, st)
                if (.not. st%ok()) exit
                moved = skip_characters_forward(" "//TAB, st)
            end do

            here = point()
            unclosed = ""

            if (at(SGML_TAG_NAME_REGULAR_EXPRESSION)) then
                if (member_ignore_case(match_string(1), unclosed_tags())) unclosed = match_string(1)
            end if

            ! If possible, align on the previous non-empty text line.
            ! Otherwise, do a more serious parsing to find the tag(s)
            ! relative to which we should be indenting.
            skip = .false.

            if (len(unclosed) == 0) then
                moved = skip_characters_backward(" "//TAB, st)

                if (skip_characters_backward(" "//TAB//LF, st) < 0) then
                    call back_to_indentation()
                    skip = point() > start
                end if
            end if

            if (allocated(context)) deallocate (context)

            if (skip) then
                allocate (context(0))
            else
                call goto_character(here)

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
                if (downcase_ascii(context(1)%name) /= downcase_ascii(unclosed)) exit
                call pop_tag(context)
            end do

            ! Indent to reflect nesting.
            if (size(context) > 0) then
                if (context(1)%end > here) then
                    ! If we were not in a text context after all, let's try
                    ! again.
                    call goto_character(here)

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
                call goto_character(context(1)%end)
                moved = skip_characters_forward(" "//TAB//LF, st)

                if (point() < here .and. is_at_indentation()) then
                    column = int(current_column())
                    return
                end if
            end if

            ! Otherwise, just follow the rules.
            call goto_character(there)
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
        call goto_character(savep)
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
    recursive subroutine execute_sgml_indent_line(status)
        type(status_type), intent(out) :: status
        logical :: noindent

        call sgml_indent_line(status, noindent)
    end subroutine execute_sgml_indent_line

    !> Guess an appropriate value for `sgml_basic_offset'.
    !> Base the guessed indentation level on the first indented tag in the
    !> buffer.  Add this to `sgml_mode_hook' for convenience.  Port of
    !> sgml-mode.el:sgml-guess-indent.
    recursive subroutine sgml_guess_indent(status)
        type(status_type), intent(out) :: status
        character(len=16) :: buffer
        integer(int64) :: original_point, position

        status = status_type()
        original_point = point()
        call goto_character(point_minimum())
        call regular_expression_search_forward("^\([ "//TAB//"]+\)<", position, status, bound=500_int64, noerror=SEARCH_NOERROR)

        if (position > 0) then
            call sgml_basic_offset%set_local(current_column() - 1)
            write (buffer, "(i0)") sgml_basic_offset%value
            call message("Guessed sgml-basic-offset = "//trim(buffer))
        end if

        call goto_character(original_point)
    end subroutine sgml_guess_indent

    !> Simplistic parse of the current buffer as a DTD.
    !> Currently just returns (EMPTY-TAGS UNCLOSED-TAGS), sorted.  Port of
    !> sgml-mode.el:sgml-parse-dtd.
    recursive subroutine sgml_parse_dtd(empty, unclosed)
        character(len=64), allocatable, intent(out) :: empty(:), unclosed(:)
        type(status_type) :: st
        integer(int64) :: position

        call goto_character(point_minimum())
        allocate (empty(0), unclosed(0))

        do
            call regular_expression_search_forward(DTD_REGULAR_EXPRESSION, position, st, noerror=SEARCH_NOERROR)
            if (position == 0) exit

            if (match_string(3) == "EMPTY") then
                empty = [character(len=64) :: downcase_ascii(match_string_no_properties(1)), empty]
            else if (match_string(2) == "O") then
                unclosed = [character(len=64) :: downcase_ascii(match_string_no_properties(1)), unclosed]
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

    !> Return the name of the last HTML title or heading (unallocated for
    !> none).  Port of sgml-mode.el:html-current-defun-name.
    recursive subroutine html_current_heading_name(name)
        character(len=:), allocatable, intent(out) :: name
        type(status_type) :: st
        integer(int64) :: original_point, position

        original_point = point()
        call regular_expression_search_backward(HTML_SUBPROGRAM_REGULAR_EXPRESSION, position, st, noerror=SEARCH_NOERROR)
        if (st%ok() .and. position > 0) name = match_string_no_properties(1)
        call goto_character(original_point)
    end subroutine html_current_heading_name

    ! ------------------------------------------------------------------
    ! Modes
    ! ------------------------------------------------------------------

    ! The commands of the keymaps and the prefix argument wrappers.
    recursive subroutine execute_sgml_slash(status)
        use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
        type(status_type), intent(out) :: status

        call sgml_slash(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_sgml_slash

    recursive subroutine execute_sgml_slash_matching(status)
        use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
        type(status_type), intent(out) :: status

        call sgml_slash_matching(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_sgml_slash_matching

    recursive subroutine execute_sgml_close_tag(status)
        type(status_type), intent(out) :: status

        call sgml_close_tag(status)
    end subroutine execute_sgml_close_tag

    recursive subroutine execute_sgml_guess_indent(status)
        type(status_type), intent(out) :: status

        call sgml_guess_indent(status)
    end subroutine execute_sgml_guess_indent

    recursive subroutine execute_sgml_name_self(status)
        type(status_type), intent(out) :: status

        call sgml_name_self(status)
    end subroutine execute_sgml_name_self

    recursive subroutine execute_sgml_maybe_name_self(status)
        type(status_type), intent(out) :: status

        call sgml_maybe_name_self(status)
    end subroutine execute_sgml_maybe_name_self

    recursive subroutine execute_sgml_maybe_end_tag(status)
        type(status_type), intent(out) :: status

        call sgml_maybe_end_tag(status)
    end subroutine execute_sgml_maybe_end_tag

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
    subroutine initialize_syntax_tables()
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
    end subroutine initialize_syntax_tables

    ! sgml-syntax-propertize-rules.
    ! Port of sgml-mode.el:sgml-syntax-propertize-rules.
    subroutine initialize_rules()
        type(syntax_highlight_type) :: hl

        allocate (propertize_rules%rules(4))
        ! Use the `b' style of comments to avoid interference with the -- ...
        ! -- comments recognized when `sgml-specials' includes ?-.
        ! FIXME: beware of <!--> blabla <!--> !!
        propertize_rules%rules(1)%regular_expression = "\(<\)!--"
        hl = syntax_highlight_type(group=1, syntax="< b")
        propertize_rules%rules(1)%highlights = [hl]
        propertize_rules%rules(2)%regular_expression = "--[ "//TAB//LF//"]*\(>\)"
        hl = syntax_highlight_type(group=1, syntax="> b")
        propertize_rules%rules(2)%highlights = [hl]
        propertize_rules%rules(3)%regular_expression = "\(<\)[?!]"
        hl = syntax_highlight_type(group=1, syntax="|>")
        hl%after => declaration_after
        propertize_rules%rules(3)%highlights = [hl]
        ! Quotes outside of tags should not introduce strings which end up
        ! hiding tags.
        propertize_rules%rules(4)%regular_expression = "\([""']\)[^""'<>]*"
        hl = syntax_highlight_type(group=1)
        hl%value => quote_value
        propertize_rules%rules(4)%highlights = [hl]
    end subroutine initialize_rules

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
    recursive subroutine indent_line_adapter(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent

        call sgml_indent_line(status, noindent)
    end subroutine indent_line_adapter

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

        if (.not. is_syntax_table_live(sgml_mode_syntax_table)) call initialize_sgml_mode()
        call major_modes%sgml_mode%begin(saved, status)
        if (.not. quick_keys_set) call set_quick_keys()
        quick_keys_set = .true.
        if (.not. status%ok()) return
        call setup()
        call levels_of(levels)
        call font_lock_defaults%set_local(levels=levels, case_fold=.true., &
                                          syntactic_face_function=sgml_font_lock_syntactic_face)
        call major_modes%sgml_mode%end(saved, status)
    end subroutine sgml_mode

! Port of sgml-mode.el:sgml-mode (in part: its body).
    ! The body of `sgml_mode'.
    recursive subroutine setup()
        type(imenu_pattern_type), allocatable :: patterns(:)
        character(len=:), allocatable :: q
        integer :: k

        call make_local_entry(k)
        ! A start or end tag by itself on a line separates a paragraph.
        ! This is desirable because SGML discards a newline that appears
        ! immediately after a start tag or immediately before an end tag.
        call paragraph_start%set_local(SGML_PARAGRAPH_START_REGULAR_EXPRESSION)
        call paragraph_separate%set_local(SGML_PARAGRAPH_START_REGULAR_EXPRESSION//"$")
        call indent_line_function%set_local(indent_line_adapter)
        call comment_start%set_local("<!-- ")
        call comment_end%set_local(" -->")
        call comment_indent_function%set_local(sgml_comment_indent)
        call syntax_propertize_function%set_local(sgml_syntax_propertize)
        call syntax_parser_state_table%set_local(sgml_tag_syntax_table)
        if (sgml_xml_guess()) call sgml_xml_mode%set_local(.true.)
        ! This will allow existing comments within declarations to be
        ! recognized.
        call comment_start_skip%set_local("<!--[ "//TAB//"]*")
        call comment_end_skip%set_local("[ "//TAB//"]*--[ "//TAB//LF//"]*>")
        ! This definition has an HTML leaning but probably fits well for other modes.
        q = "?"
        if (sgml_xml_mode%value) q = ""
        allocate (patterns(3))
        patterns(1) = imenu_pattern_type(menu_title="", regular_expression="<!\(element\|entity\)[ "//TAB//LF//"]+%?[ "//TAB//LF// &
                                         "]*\("//SGML_NAME_REGULAR_EXPRESSION//"\)", subexpression=2)
        patterns(2) = imenu_pattern_type(menu_title="Id", regular_expression="<[^>]+[ "//TAB//LF//"]+[Ii][Dd]=\(['""]"//q// &
                                         "\)\("//SGML_NAME_REGULAR_EXPRESSION//"\)\1", subexpression=2)
        patterns(3) = imenu_pattern_type(menu_title="Name", regular_expression="<[^>]+[ "//TAB//LF//"]+[Nn][Aa][Mm][Ee]=\(['""]"// &
                                         q//"\)\("//SGML_NAME_REGULAR_EXPRESSION//"\)\1", subexpression=2)
        call imenu_patterns%set_local(patterns)
        call skeleton_further_elements_set_local(sgml_further_elements)
        call skeleton_end_newline%set_local(.false.)
        call skeleton_end_hook%prepend(sgml_skeleton_end, local=.true.)
        call skeleton_end_hook%clear()
        call skeleton_end_hook%prepend(sgml_skeleton_end, local=.true.)

        if (.not. sgml_xml_mode%value) then
            if (associated(sgml_transformation_function)) then
                call skeleton_transformation_function_set_local(sgml_transformation_function)
            else
                call skeleton_transformation_function_set_local()
            end if
        end if
    end subroutine setup

    !> Major mode based on SGML mode for editing HTML documents.  Port of
    !> sgml-mode.el:html-mode.
    recursive subroutine html_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        integer :: k

        if (.not. is_syntax_table_live(sgml_mode_syntax_table)) call initialize_sgml_mode()
        call major_modes%html_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! sgml-tag-face-alist, sgml-whitespace-sensitive-tags,
        ! sgml-empty-tags and sgml-unclosed-tags.
        call make_local_entry(k)
        locals(k)%html = .true.
        call add_log_current_subprogram_function%set_local(html_current_heading_name)
        call imenu_create_index_function%set_local(html_imenu_index)
        call major_modes%html_mode%end(saved, status)
    end subroutine html_mode

    !> Return a table of contents for an HTML buffer for use with Imenu.
    !> Port of sgml-mode.el:html-imenu-index.
    recursive subroutine html_imenu_index(index, status)
        type(imenu_index_type), intent(out) :: index
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: digit
        integer(int64) :: here, position_value
        integer :: k, level

        status = status_type()
        allocate (index%entries(0))
        here = point()
        call goto_character(point_minimum())

        if (.not. allocated(html_imenu_regular_expression)) then
            html_imenu_regular_expression = "\s-*<h\([1-9]\)[^"//LF//"<>]*>\(<[^"//LF//"<>]*>\)*\s-*\([^"//LF//"<>]*\)"
        end if

        do
            call regular_expression_search_forward(html_imenu_regular_expression, position_value, status, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. position_value == 0) exit
            digit = match_string(1)
            level = ichar(digit(1:1)) - ichar("0")
            call index%append(repeat(" ", max(0, 2*(level - 1)))//match_string(3), k, position=line_beginning_position())
        end do

        call goto_character(here)
    end subroutine html_imenu_index

    ! Not a port.
    !> Define SGML mode and HTML mode: their variables, tables, keymaps and
    !> commands.
    recursive subroutine initialize_sgml_mode()
        type(status_type) :: st
        type(named_face_type), pointer :: f
        character(len=:), allocatable :: key
        integer :: c

        if (is_syntax_table_live(sgml_mode_syntax_table)) return
        call forward_setting(sgml_basic_offset)
        call forward_setting(sgml_attribute_offset)
        call forward_setting(sgml_xml_mode)
        call forward_setting(sgml_validate_command)
        call forward_setting(sgml_slash_distance)
        if (.not. allocated(sgml_specials)) sgml_specials = [iachar(""""), iachar("'")]
        if (all(sgml_character_names == "")) sgml_character_names = SGML_CHARACTER_NAMES_DATA
        if (.not. allocated(sgml_empty_tags)) allocate (sgml_empty_tags(0))
        if (.not. allocated(sgml_unclosed_tags)) allocate (sgml_unclosed_tags(0))
        if (.not. allocated(sgml_whitespace_sensitive_tags)) allocate (sgml_whitespace_sensitive_tags(0))
        call initialize_validate_command()
        call initialize_html_tag_faces()
        f => sgml_namespace
        call f%define("sgml_namespace", "`sgml_mode' face used to highlight the namespace part of identifiers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_builtin_face"))
        call initialize_syntax_tables()
        call initialize_rules()
        call append_locals_clearer(clear_sgml_locals)
        call major_modes%sgml_mode%define("sgml_mode", "SGML", sgml_mode, sgml_mode_hook, parent=major_modes%text_mode, &
                                          syntax_table=sgml_mode_syntax_table)
        sgml_mode_map = major_modes%sgml_mode%map
        call major_modes%html_mode%define("html_mode", "HTML", html_mode, html_mode_hook, parent=major_modes%sgml_mode)
        html_mode_map = major_modes%html_mode%map
        call minor_modes%sgml_name_8bit_mode%define("sgml_name_8bit_mode")
        call keymap_set(sgml_mode_map, "/", "sgml_slash", st)
        call keymap_set(sgml_mode_map, "C-c C-n", "sgml_name_character", st)
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
        call keymap_set(sgml_mode_map, "C-c C-t", "sgml_tag", st)
        call keymap_set(sgml_mode_map, "C-c C-a", "sgml_attributes", st)
        call keymap_set(sgml_mode_map, "C-c ?", "sgml_tag_help", st)
        call keymap_set(sgml_mode_map, "C-c C-o", "sgml_tag", st)
        call keymap_set(html_mode_map, "C-c 6", "html_headline_6", st)
        call keymap_set(html_mode_map, "C-c 5", "html_headline_5", st)
        call keymap_set(html_mode_map, "C-c 4", "html_headline_4", st)
        call keymap_set(html_mode_map, "C-c 3", "html_headline_3", st)
        call keymap_set(html_mode_map, "C-c 2", "html_headline_2", st)
        call keymap_set(html_mode_map, "C-c 1", "html_headline_1", st)
        call keymap_set(html_mode_map, "C-c C-m", "html_paragraph", st)
        call keymap_set(html_mode_map, "C-c C-j", "html_line", st)
        call keymap_set(html_mode_map, "C-c C-c -", "html_horizontal_rule", st)
        call keymap_set(html_mode_map, "C-c C-c o", "html_ordered_list", st)
        call keymap_set(html_mode_map, "C-c C-c u", "html_unordered_list", st)
        call keymap_set(html_mode_map, "C-c C-c r", "html_radio_buttons", st)
        call keymap_set(html_mode_map, "C-c C-c c", "html_checkboxes", st)
        call keymap_set(html_mode_map, "C-c C-c l", "html_list_item", st)
        call keymap_set(html_mode_map, "C-c C-c h", "html_href_anchor", st)
        call keymap_set(html_mode_map, "C-c C-c f", "html_href_anchor_file", st)
        call keymap_set(html_mode_map, "C-c C-c n", "html_name_anchor", st)
        call keymap_set(html_mode_map, "C-c C-c #", "html_id_anchor", st)
        call keymap_set(html_mode_map, "C-c C-c i", "html_image", st)

        do c = 128, 255
            call character_to_string(int(c, int32), key, st)
            call keymap_set(sgml_mode_map, key, "sgml_maybe_name_self", st)
        end do

        commands%sgml_mode%execute => sgml_mode
        commands%html_mode%execute => html_mode
        commands%sgml_slash%execute => execute_sgml_slash
        commands%sgml_slash_matching%execute => execute_sgml_slash_matching
        commands%sgml_name_character%execute => execute_sgml_name_character
        commands%sgml_namify_character%execute => execute_sgml_namify_character
        commands%sgml_name_self%execute => execute_sgml_name_self
        commands%sgml_maybe_name_self%execute => execute_sgml_maybe_name_self
        commands%sgml_maybe_end_tag%execute => execute_sgml_maybe_end_tag
        commands%sgml_skip_tag_backward%execute => execute_sgml_skip_tag_backward
        commands%sgml_skip_tag_forward%execute => execute_sgml_skip_tag_forward
        commands%sgml_delete_tag%execute => execute_sgml_delete_tag
        commands%sgml_validate%execute => execute_sgml_validate
        commands%sgml_quote%execute => execute_sgml_quote
        commands%sgml_pretty_print%execute => execute_sgml_pretty_print
        commands%sgml_close_tag%execute => execute_sgml_close_tag
        commands%sgml_indent_line%execute => execute_sgml_indent_line
        commands%sgml_guess_indent%execute => execute_sgml_guess_indent
        commands%sgml_name_8bit_mode%execute => sgml_name_8bit_mode
        commands%sgml_tag%execute => execute_sgml_tag
        commands%sgml_attributes%execute => execute_sgml_attributes
        commands%sgml_auto_attributes%execute => execute_sgml_auto_attributes
        commands%sgml_tag_help%execute => execute_sgml_tag_help
        call forward_setting(sgml_tag_history)
        call initialize_tag_tables()
        call initialize_tag_help()
        commands%html_href_anchor%execute => execute_html_href_anchor
        commands%html_href_anchor_file%execute => execute_html_href_anchor_file
        commands%html_name_anchor%execute => execute_html_name_anchor
        commands%html_id_anchor%execute => execute_html_id_anchor
        commands%html_headline_1%execute => execute_html_headline_1
        commands%html_headline_2%execute => execute_html_headline_2
        commands%html_headline_3%execute => execute_html_headline_3
        commands%html_headline_4%execute => execute_html_headline_4
        commands%html_headline_5%execute => execute_html_headline_5
        commands%html_headline_6%execute => execute_html_headline_6
        commands%html_horizontal_rule%execute => execute_html_horizontal_rule
        commands%html_image%execute => execute_html_image
        commands%html_line%execute => execute_html_line
        commands%html_ordered_list%execute => execute_html_ordered_list
        commands%html_unordered_list%execute => execute_html_unordered_list
        commands%html_list_item%execute => execute_html_list_item
        commands%html_paragraph%execute => execute_html_paragraph
        commands%html_div%execute => execute_html_div
        commands%html_span%execute => execute_html_span
        commands%html_checkboxes%execute => execute_html_checkboxes
        commands%html_radio_buttons%execute => execute_html_radio_buttons
        commands%html_navigational_links%execute => execute_html_navigational_links
        commands%html_html5_template%execute => execute_html_html5_template
    end subroutine initialize_sgml_mode

    ! Port of sgml-mode.el:sgml-validate-command (its default value).
    ! The default `sgml_validate_command': prefer tidy because (o)nsgmls is
    ! often built without --enable-http which makes it next to useless.
    subroutine initialize_validate_command()
        use files_mod, only: executable_find
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
    end subroutine initialize_validate_command

! Port of sgml-mode.el:html-tag-face-alist (its value).
    ! html-tag-face-alist.
    subroutine initialize_html_tag_faces()
        if (allocated(html_tag_faces)) return
        html_tag_faces = [sgml_tag_face_type(tag="b", faces=[character(len=32) :: "bold", ""]), &
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
    end subroutine initialize_html_tag_faces

    ! ------------------------------------------------------------------
    ! Tag tables and skeletons
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine tags_append(self, name, end_tag, skeleton, skeleton_slot)
        class(sgml_tags_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: end_tag, skeleton_slot
        type(skeleton_element_type), intent(in), optional :: skeleton(:)
        type(sgml_tag_rule_type) :: entry

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        entry%name = name
        if (present(end_tag)) entry%end_tag = end_tag

        if (present(skeleton)) then
            entry%end_tag = SGML_END_TAG_SKELETON
            entry%skeleton = skeleton
            if (present(skeleton_slot)) entry%skeleton_slot = skeleton_slot
        end if

        allocate (entry%attributes(0))
        self%entries = [self%entries, entry]
    end subroutine tags_append

    ! Append RULE to the last tag of SELF.  Not a port.
    subroutine append_rule(self, rule)
        class(sgml_tags_type), intent(inout) :: self
        type(sgml_attribute_rule_type), intent(in) :: rule
        integer :: k

        k = size(self%entries)
        self%entries(k)%attributes = [self%entries(k)%attributes, rule]
    end subroutine append_rule

    ! Not a port.
    subroutine tags_append_attribute(self, name, value, optional, values)
        class(sgml_tags_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: value
        logical, intent(in), optional :: optional
        character(len=*), intent(in), optional :: values(:)
        type(sgml_attribute_rule_type) :: rule
        integer :: i

        rule%name = name
        if (present(value)) rule%value = value
        if (present(optional)) rule%optional = optional
        allocate (rule%values(0))

        if (present(values)) then
            deallocate (rule%values)
            allocate (rule%values(size(values)))

            do i = 1, size(values)
                rule%values(i)%text = trim(values(i))
            end do
        end if

        call append_rule(self, rule)
    end subroutine tags_append_attribute

    ! Not a port.
    subroutine tags_append_name(self, name)
        class(sgml_tags_type), intent(inout) :: self
        character(len=*), intent(in) :: name
        type(sgml_attribute_rule_type) :: rule

        rule%name = name
        rule%bare = .true.
        allocate (rule%values(0))
        call append_rule(self, rule)
    end subroutine tags_append_name

    ! The tag table of the current buffer (`sgml_tags', or `html_tags' in
    ! HTML mode).  Not a port.
    function current_tags() result(tags)
        type(sgml_tags_type), pointer :: tags
        integer :: k

        tags => sgml_tags
        k = local_index()

        if (k > 0) then
            if (locals(k)%html) tags => html_tags
        end if
    end function current_tags

    ! The index of the tag NAME in TAGS (0 if none).  (assoc NAME TAGS)
    ! Not a port.
    integer function find_tag(tags, name) result(k)
        type(sgml_tags_type), intent(in) :: tags
        character(len=*), intent(in) :: name

        if (allocated(tags%entries)) then
            do k = 1, size(tags%entries)
                if (tags%entries(k)%name == name) return
            end do
        end if

        k = 0
    end function find_tag

    ! A completion table of NAMES.  Not a port.
    function table_of(names) result(table)
        type(string_type), intent(in) :: names(:)
        type(completion_table_type) :: table
        type(value_type), allocatable :: items(:)
        integer :: i

        allocate (items(size(names)))

        do i = 1, size(names)
            items(i) = value_type(text=names(i)%text)
        end do

        table%list = make_list(items)
    end function table_of

    ! The names of the tags of the current table.  Not a port.
    function tag_names() result(names)
        type(string_type), allocatable :: names(:)
        type(sgml_tags_type), pointer :: tags
        integer :: i

        tags => current_tags()
        allocate (names(0))
        if (.not. allocated(tags%entries)) return
        deallocate (names)
        allocate (names(size(tags%entries)))

        do i = 1, size(names)
            names(i)%text = tags%entries(i)%name
        end do
    end function tag_names

    ! completing-read with PROMPT over `reading_names' (the reader of
    ! `skeleton_read' in sgml_attributes and sgml_value).  Not a port.
    recursive subroutine read_from_names(string, status)
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status

        call completing_read(reading_prompt, table_of(reading_names), string, status)
    end subroutine read_from_names

    ! (read-string "Value: ").  Not a port.
    recursive subroutine read_value_string(string, status)
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status

        call read_string("Value: ", string, status)
    end subroutine read_value_string

    !> Interactively insert value taken from attribute-rule RULE.
    !> See `sgml_tags' for info about attribute rules.  Port of
    !> sgml-mode.el:sgml-value.
    recursive subroutine sgml_value(rule, status)
        type(sgml_attribute_rule_type), intent(in) :: rule
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer
        integer :: n, i

        status = status_type()

        if (allocated(rule%value)) then
            call insert('="'//rule%value//'"', status)
        else if (rule%optional .and. .not. sgml_xml_mode%value) then
            if (size(rule%values) > 0) then
                call insert('="', status)
                reading_prompt = "Value: "
                reading_names = rule%values
                call skeleton_read(answer, status, reader=read_from_names)
                if (.not. status%ok()) return

                if (len(answer) > 0) then
                    call insert(answer//'"', status)
                else
                    call delete_character(-2_int64, status)
                end if
            end if
        else
            call insert('="', status)
            n = size(rule%values)
            if (rule%optional) n = n + 1

            if (n >= 2) then
                reading_prompt = "Value: "

                if (rule%optional) then
                    if (allocated(reading_names)) deallocate (reading_names)
                    allocate (reading_names(n))
                    reading_names(1)%text = "t"

                    do i = 1, size(rule%values)
                        reading_names(i + 1)%text = rule%values(i)%text
                    end do
                else
                    reading_names = rule%values
                end if

                call skeleton_read(answer, status, reader=read_from_names)
                if (.not. status%ok()) return
                call insert(answer, status)
            else if (n == 0) then
                call skeleton_read(answer, status, reader=read_value_string)
                if (.not. status%ok()) return
                call insert(answer, status)
            end if

            call insert('"', status)
        end if
    end subroutine sgml_value

    !> When at top level of a tag, interactively insert attributes.
    !>
    !> Completion and configuration of TAG are done according to `sgml_tags'.
    !> If QUIET, do not print a message when there are no attributes for TAG.
    !> FIRST is the end tag kind of TAG's rule (upstream's return value:
    !> SGML_END_TAG_NONE when there is none, else the kinds of
    !> `sgml_tag_rule_type') and RULE the index of the rule in the current
    !> table (0 if none).  Port of sgml-mode.el:sgml-attributes.
    recursive subroutine sgml_attributes(tag, quiet, first, rule, status)
        character(len=*), intent(in) :: tag
        logical, intent(in) :: quiet
        integer, intent(out) :: first, rule
        type(status_type), intent(out) :: status
        type(sgml_tags_type), pointer :: tags
        type(sgml_attribute_rule_type), allocatable :: alist(:), extra(:)
        type(sgml_attribute_rule_type) :: found, always
        character(len=:), allocatable :: attribute
        logical :: saved
        integer :: i, j, k

        status = status_type()
        first = SGML_END_TAG_NONE
        rule = 0

        if (len(tag) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Wrong context for adding attribute")
            return
        end if

        saved = completion_ignore_case%value
        completion_ignore_case = .true.
        tags => current_tags()
        k = find_tag(tags, downcase_ascii(tag))
        allocate (alist(0))

        if (k > 0) then
            rule = k
            alist = tags%entries(k)%attributes
            first = tags%entries(k)%end_tag

            if (first == SGML_END_TAG_NONE .and. size(alist) > 0) then
                if (alist(1)%bare) then
                    completion_ignore_case = saved
                    call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected array, got "//fortran_string(alist(1)%name))
                    return
                end if
            end if
        end if

        if (size(alist) == 0 .and. .not. quiet) call message("No attributes configured.")

        if (size(alist) > 0) then
            ! Add class and id attributes if a) the element has any
            ! other attributes configured, and b) they're not already
            ! present.
            allocate (extra(0))

            if (.not. has_attribute(alist, "class")) then
                found = sgml_attribute_rule_type(name="class")
                allocate (found%values(0))
                extra = [found]
            end if

            if (.not. has_attribute(alist, "id")) then
                found = sgml_attribute_rule_type(name="id")
                allocate (found%values(0))
                extra = [found, extra]
            end if

            alist = [extra, alist]

            if (alist(1)%bare) then
                ! An always present attribute.
                if (preceding_character() == iachar(" ")) then
                    call insert(transform(alist(1)%name), status)
                else
                    call insert(" "//transform(alist(1)%name), status)
                end if

                always = sgml_attribute_rule_type(name=alist(1)%name)
                allocate (always%values(0))

                if (size(alist) > 1) then
                    if (alist(2)%bare) then
                        always%value = alist(2)%name
                    else
                        deallocate (always%values)
                        allocate (always%values(count(.not. alist(2:)%bare)))
                        i = 0

                        do j = 2, size(alist)
                            if (alist(j)%bare) cycle
                            i = i + 1
                            always%values(i)%text = alist(j)%name
                        end do
                    end if
                end if

                call sgml_value(always, status)
            else
                i = size(alist)

                do while (i > 0)
                    call insert(" ", status)
                    reading_prompt = "Attribute: "
                    if (allocated(reading_names)) deallocate (reading_names)
                    allocate (reading_names(size(alist)))

                    do j = 1, size(alist)
                        reading_names(j)%text = alist(j)%name
                    end do

                    call skeleton_read(attribute, status, reader=read_from_names)
                    deallocate (reading_names)
                    if (.not. status%ok()) exit
                    call insert(transform(attribute), status)

                    if (len(attribute) == 0) then
                        i = 0
                    else
                        found = sgml_attribute_rule_type(name=attribute)
                        allocate (found%values(0))

                        do j = 1, size(alist)
                            if (.not. alist(j)%bare .and. alist(j)%name == downcase_ascii(attribute)) then
                                found = alist(j)
                                exit
                            end if
                        end do

                        call sgml_value(found, status)
                        if (.not. status%ok()) exit
                        i = i - 1
                    end if
                end do

                if (status%ok() .and. preceding_character() == iachar(" ")) call delete_character(-1_int64, status)
            end if
        end if

        completion_ignore_case = saved
    end subroutine sgml_attributes

    ! Whether ALIST has an entry named NAME (assoc-string).  Not a port.
    logical function has_attribute(alist, name)
        type(sgml_attribute_rule_type), intent(in) :: alist(:)
        character(len=*), intent(in) :: name
        integer :: i

        has_attribute = .false.

        do i = 1, size(alist)
            if (alist(i)%name == name) has_attribute = .true.
        end do
    end function has_attribute

    ! STRING through the skeleton transformation.  Not a port.
    function transform(string) result(out)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: out

        out = skeleton_transform(string)
    end function transform

    ! Not a port.
    recursive subroutine execute_sgml_attributes(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tag
        integer :: first, rule
        integer(int64) :: original_point

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        original_point = point()
        call sgml_beginning_of_tag(tag, .true.)
        call goto_character(original_point)
        call sgml_attributes(tag, .false., first, rule, status)
    end subroutine execute_sgml_attributes

    !> Self insert the character typed; at top level of tag, prompt for attributes.
    !> With prefix argument ARGUMENT, only self insert.  Port of
    !> sgml-mode.el:sgml-auto-attributes.
    recursive subroutine sgml_auto_attributes(argument, status)
        type(prefix_argument_type), intent(in) :: argument
        type(status_type), intent(out) :: status
        type(sgml_tags_type), pointer :: tags
        character(len=:), allocatable :: tag
        integer(int64) :: original_point
        integer :: first, rule
        logical :: plain

        status = status_type()
        original_point = point()
        tags => current_tags()
        plain = argument%kind /= PREFIX_NONE

        ! no message when nothing configured
        if (.not. plain) then
            plain = .true.
            if (allocated(tags%entries)) plain = size(tags%entries) == 0
        end if

        if (.not. plain) then
            call sgml_beginning_of_tag(tag, .true.)
            call goto_character(original_point)
            plain = len(tag) == 0
            if (.not. plain) plain = tag(1:1) == "/"
        end if

        if (plain) then
            call self_insert_command(prefix_numeric_value(argument), event_character(), status)
            return
        end if

        call sgml_attributes(tag, .false., first, rule, status)
        if (.not. status%ok()) return
        last_command_event = value_type(32_int64)
        if (point() <= original_point) call self_insert_command(1_int64, 32_int64, status)
    end subroutine sgml_auto_attributes

    ! Not a port.
    recursive subroutine execute_sgml_auto_attributes(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call sgml_auto_attributes(current_prefix_argument, status)
    end subroutine execute_sgml_auto_attributes

    !> Display description of tag TAG.  If TAG is absent or empty, use the tag at point.
    !> Port of sgml-mode.el:sgml-tag-help.
    recursive subroutine sgml_tag_help(status, tag_in)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: tag_in
        character(len=:), allocatable :: tag, description
        integer(int64) :: original_point

        status = status_type()
        tag = ""
        if (present(tag_in)) tag = tag_in

        if (len(tag) == 0) then
            original_point = point()
            if (after_character_at(point()) == iachar("<")) call forward_character(status)
            call sgml_beginning_of_tag(tag, .false.)
            call goto_character(original_point)
        end if

        if (len(tag) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No tag selected")
            return
        end if

        tag = downcase_ascii(tag)
        description = tag_description(tag)
        if (len(description) == 0 .and. tag(1:1) == "/") description = tag_description(tag(2:))
        if (len(description) == 0) description = "No description available"
        call message(description)
    end subroutine sgml_tag_help

    ! The description of TAG in the current `sgml_tag_help_variable' ("" if none).
    ! Not a port.
    function tag_description(tag) result(description)
        character(len=*), intent(in) :: tag
        character(len=:), allocatable :: description
        integer :: i, k

        description = ""
        k = local_index()

        if (k > 0) then
            if (locals(k)%html) then
                do i = 1, size(html_tag_help)
                    if (trim(html_tag_help(i)%tag) == tag) then
                        description = trim(html_tag_help(i)%description)
                        return
                    end if
                end do

                return
            end if
        end if

        do i = 1, size(sgml_tag_help_variable)
            if (trim(sgml_tag_help_variable(i)%tag) == tag) then
                description = trim(sgml_tag_help_variable(i)%description)
                return
            end if
        end do
    end function tag_description

    ! Not a port.
    recursive subroutine execute_sgml_tag_help(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: def, tag
        integer(int64) :: original_point

        original_point = point()
        if (after_character_at(point()) == iachar("<")) call forward_character(status)
        call sgml_beginning_of_tag(def, .false.)
        call goto_character(original_point)

        if (len(def) > 0) then
            call completing_read(format_prompt("Tag", def), table_of(tag_names()), tag, status, history="sgml_tag_history", &
                                 default=def)
        else
            call completing_read(format_prompt("Tag"), table_of(tag_names()), tag, status, history="sgml_tag_history")
        end if

        if (.not. status%ok()) return
        call sgml_tag_help(status, tag)
    end subroutine execute_sgml_tag_help

    ! The interactor of `sgml_tag': the tag read with completion, through
    ! the skeleton transformation.  Not a port.
    recursive subroutine tag_interactor(string, status)
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer

        if (allocated(sgml_tag_last)) then
            if (len(sgml_tag_last) > 0) then
                call completing_read(format_prompt("Tag", sgml_tag_last), table_of(tag_names()), answer, status, &
                                     history="sgml_tag_history", default=sgml_tag_last)
            else
                call completing_read("Tag: ", table_of(tag_names()), answer, status, history="sgml_tag_history")
            end if
        else
            call completing_read("Tag: ", table_of(tag_names()), answer, status, history="sgml_tag_history")
        end if

        if (.not. status%ok()) return
        sgml_tag_last = answer
        string = transform(answer)
    end subroutine tag_interactor

    ! '(undo-boundary).  Not a port.
    recursive subroutine undo_boundary_action(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call undo_boundary()
    end subroutine undo_boundary_action

    ! The backquoted sub-skeleton of `sgml_tag', made with the tag read
    ! (string).  Not a port.
    recursive subroutine tag_rest_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        tag_string = ""
        if (allocated(skeleton_state%string)) tag_string = skeleton_state%string
        skeleton_state%value = [skeleton_for_each([character(len=1) :: ""]), skeleton_action(tag_attributes_action), &
                                skeleton_text(">"), skeleton_value(tag_end_value), SKELETON_END]
    end subroutine tag_rest_value

    ! '(setq v2 (sgml-attributes string t)).  Not a port.
    recursive subroutine tag_attributes_action(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: tag

        call sgml_attributes(tag_string, .true., tag_first, tag_rule, status)
        if (allocated(skeleton_state%v2)) deallocate (skeleton_state%v2)
        allocate (skeleton_state%v2(0))
        if (.not. status%ok()) return
        tag_slot_newline = .false.

        if (tag_first == SGML_END_TAG_NEWLINES) then
            skeleton_state%v2 = [SKELETON_NEWLINE]
        else if (tag_first == SGML_END_TAG_SKELETON) then
            associate (tags => current_tags())
                tag_slot_newline = tags%entries(tag_rule)%skeleton_slot == SGML_END_TAG_NEWLINES
            end associate
        end if
    end subroutine tag_attributes_action

    ! The end of the tag of `sgml_tag' (the cond of its skeleton).
    ! Not a port.
    recursive subroutine tag_end_value(status)
        type(status_type), intent(out) :: status
        type(skeleton_element_type), allocatable :: body(:), slot(:)
        character(len=:), allocatable :: string
        logical :: v2_t

        status = status_type()
        string = tag_string
        v2_t = tag_first == SGML_END_TAG_NO

        if (string == "![") then
            call goto_character(point() - 1)
            skeleton_state%value = [skeleton_for_each([character(len=1) :: ""]), skeleton_text(" [ "), SKELETON_POINT, &
                                    skeleton_text(" ]]"), SKELETON_END]
        else if (v2_t .and. sgml_xml_mode%value .and. any(empty_tags() == string)) then
            skeleton_state%value = [skeleton_for_each([character(len=1) :: ""]), skeleton_delete(1), skeleton_text(" />"), &
                                    SKELETON_END]
        else if ((v2_t .and. .not. sgml_xml_mode%value) .or. scan(string(1:min(1, len(string))), "/!?") == 1) then
            return
        else if (tag_first /= SGML_END_TAG_SKELETON) then
            ! Make sure we don't fall into an infinite loop.
            if (v2_t) then
                deallocate (skeleton_state%v2)
                allocate (skeleton_state%v2(0))
            end if

            skeleton_state%value = [skeleton_for_each([character(len=1) :: ""]), SKELETON_V2, SKELETON_POINT, SKELETON_V2, &
                                   skeleton_text("</"), skeleton_literal(string), skeleton_text(">"), SKELETON_INDENT, SKELETON_END]
        else
            associate (tags => current_tags())
                body = tags%entries(tag_rule)%skeleton

                if (tags%entries(tag_rule)%skeleton_slot == SGML_END_TAG_NO) then
                    skeleton_state%value = [skeleton_for_each([character(len=1) :: ""]), body, SKELETON_END]
                else
                    allocate (slot(0))
                    if (tags%entries(tag_rule)%skeleton_slot == SGML_END_TAG_NEWLINES) slot = [SKELETON_NEWLINE]
                    skeleton_state%value = [skeleton_for_each([character(len=1) :: ""]), slot, body, SKELETON_RESUME, slot, &
                                            SKELETON_POINT, skeleton_text("</"), skeleton_literal(string), skeleton_text(">"), &
                                            SKELETON_INDENT, SKELETON_END]
                end if
            end associate
        end if
    end subroutine tag_end_value

    !> Prompt for a tag and insert it, optionally with attributes.
    !> Completion and configuration are done according to `sgml_tags'.
    !> If you like tags and attributes in uppercase, customize
    !> `sgml_transformation_function' to upcase.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:sgml-tag.
    recursive subroutine sgml_tag(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call skeleton_command([skeleton_interactor(tag_interactor), skeleton_text("<"), SKELETON_STRING, &
                               SKELETON_IF_NOT_MOVED, skeleton_for_each([character(len=1) :: ""]), skeleton_delete(1), &
                               skeleton_action(undo_boundary_action), skeleton_literal("&lt;"), SKELETON_END, &
                               SKELETON_IF_NOT_MOVED, skeleton_value(tag_rest_value)], status, string, argument)
    end subroutine sgml_tag

    recursive subroutine execute_sgml_tag(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call sgml_tag(status, argument=current_prefix_argument)
    end subroutine execute_sgml_tag

    ! sgml-mode's `skeleton_end_hook'.  Not a port.
    recursive subroutine sgml_skeleton_end(status)
        type(status_type), intent(out) :: status
        logical :: v2_newline

        status = status_type()
        if (is_end_of_line()) return
        v2_newline = tag_slot_newline

        if (allocated(skeleton_state%v2)) then
            if (size(skeleton_state%v2) > 0) v2_newline = v2_newline .or. skeleton_state%v2(1)%kind == SKELETON_NEWLINE%kind
        end if

        if (v2_newline) call newline_and_indent(status)
        tag_slot_newline = .false.
    end subroutine sgml_skeleton_end

    ! sgml-mode's `skeleton_further_elements': completion-ignore-case is
    ! true while a skeleton is inserted.  Not a port.
    subroutine sgml_further_elements(bind)
        logical, intent(in) :: bind

        if (bind) then
            further_saved_ignore_case = completion_ignore_case%value
            completion_ignore_case = .true.
        else
            completion_ignore_case = further_saved_ignore_case
        end if
    end subroutine sgml_further_elements

    ! Bind or unbind the electric keys of `sgml_quick_keys' in
    ! `sgml_mode_map' and those of `html_quick_keys' in `html_mode_map'
    ! (their :set functions, run when SGML mode first runs, as loading
    ! sgml-mode.el does).  Port of sgml-mode.el sgml-quick-keys and
    ! html-quick-keys (their :set).
    subroutine set_quick_keys()
        type(status_type) :: st
        character(len=8), parameter :: html_keys(14) = [character(len=8) :: "C-c p", "C-c -", "C-c d", "C-c o", "C-c u", &
                                                                    "C-c r", "C-c c", "C-c l", "C-c h", "C-c f", "C-c n", "C-c #", &
                                                                                                                   "C-c i", "C-c s"]
        character(len=32), parameter :: html_commands(14) = [character(len=32) :: "html_paragraph", "html_horizontal_rule", &
                                                                           "html_div", "html_ordered_list", "html_unordered_list", &
                                                                        "html_radio_buttons", "html_checkboxes", "html_list_item", &
                                                                  "html_href_anchor", "html_href_anchor_file", "html_name_anchor", &
                                                                                        "html_id_anchor", "html_image", "html_span"]
        integer :: i

        if (.not. sgml_quick_keys%is_off()) then
            call keymap_set(sgml_mode_map, "&", "sgml_name_character", st)
            call keymap_set(sgml_mode_map, "<", "sgml_tag", st)
            call keymap_set(sgml_mode_map, "SPC", "sgml_auto_attributes", st)
            call keymap_set(sgml_mode_map, ">", "sgml_maybe_end_tag", st)
            if (any(sgml_specials == iachar('"'))) call keymap_set(sgml_mode_map, '"', "sgml_name_self", st)
            if (any(sgml_specials == iachar("'"))) call keymap_set(sgml_mode_map, "'", "sgml_name_self", st)
        end if

        ! Each option has its own named constants.
        follow_sgml_quick_keys: if (html_quick_keys%choice == HTML_QUICK_KEYS_DEFAULT) then
            select case (sgml_quick_keys%choice)
            case (SGML_QUICK_KEYS_INDENT)
                html_quick_keys = HTML_QUICK_KEYS_INDENT
            case (SGML_QUICK_KEYS_CLOSE)
                html_quick_keys = HTML_QUICK_KEYS_CLOSE
            case default
                html_quick_keys = sgml_quick_keys%value
            end select
        end if follow_sgml_quick_keys

        if (.not. html_quick_keys%is_off()) then
            do i = 1, size(html_keys)
                call keymap_set(html_mode_map, trim(html_keys(i)), trim(html_commands(i)), st)
            end do
        end if
    end subroutine set_quick_keys

    ! ------------------------------------------------------------------
    ! HTML skeletons
    ! ------------------------------------------------------------------

    ! The value of an element that is TEXT in XML mode and none else.
    ! Not a port.
    subroutine xml_only(text)
        character(len=*), intent(in) :: text

        if (sgml_xml_mode%value) skeleton_state%value = [skeleton_literal(text)]
    end subroutine xml_only

    ! (if sgml-xml-mode XML OTHER).  Not a port.
    subroutine xml_or(xml, other)
        character(len=*), intent(in) :: xml, other

        if (sgml_xml_mode%value) then
            skeleton_state%value = [skeleton_literal(xml)]
        else
            skeleton_state%value = [skeleton_literal(other)]
        end if
    end subroutine xml_or

    ! (if sgml-xml-mode "</li>").  Not a port.
    recursive subroutine xml_end_li(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call xml_only("</li>")
    end subroutine xml_end_li

    ! (if sgml-xml-mode "</p>").  Not a port.
    recursive subroutine xml_end_paragraph(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call xml_only("</p>")
    end subroutine xml_end_paragraph

    ! (if sgml-xml-mode "</option>").  Not a port.
    recursive subroutine xml_end_option(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call xml_only("</option>")
    end subroutine xml_end_option

    ! (if sgml-xml-mode "</item>").  Not a port.
    recursive subroutine xml_end_item(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call xml_only("</item>")
    end subroutine xml_end_item

    ! (if sgml-xml-mode "</over>").  Not a port.
    recursive subroutine xml_end_over(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call xml_only("</over>")
    end subroutine xml_end_over

    ! (if sgml-xml-mode "</dt>").  Not a port.
    recursive subroutine xml_end_dt(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call xml_only("</dt>")
    end subroutine xml_end_dt

    ! (if sgml-xml-mode "</dd>").  Not a port.
    recursive subroutine xml_end_dd(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call xml_only("</dd>")
    end subroutine xml_end_dd

    ! (if sgml-xml-mode (concat "<" string "></tr>")).  Not a port.
    recursive subroutine xml_end_tr(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(skeleton_state%string)) call xml_only("<"//skeleton_state%string//"></tr>")
    end subroutine xml_end_tr

    ! (if sgml-xml-mode (concat " id=\"" string "\"")).  Not a port.
    recursive subroutine xml_id(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(skeleton_state%string)) call xml_only(' id="'//skeleton_state%string//'"')
    end subroutine xml_id

    ! (if sgml-xml-mode " />" ">").  Not a port.
    recursive subroutine xml_close(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call xml_or(" />", ">")
    end subroutine xml_close

    ! (if sgml-xml-mode "<hr />" "<hr>").  Not a port.
    recursive subroutine xml_hr(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call xml_or("<hr />", "<hr>")
    end subroutine xml_hr

    ! (if sgml-xml-mode "<br />" "<br>").  Not a port.
    recursive subroutine xml_br(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call xml_or("<br />", "<br>")
    end subroutine xml_br

    ! (if (is_beginning_of_line) nil '\n).  Not a port.
    recursive subroutine newline_unless_beginning_of_line(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. is_beginning_of_line()) skeleton_state%value = [SKELETON_NEWLINE]
    end subroutine newline_unless_beginning_of_line

    ! The table's cell kind: (completing-read "Cell kind: " '(("td")
    ! ("th")) nil t "t").  Not a port.
    recursive subroutine cell_kind_interactor(string, status)
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: kinds(:)

        call append_string(kinds, "td", "th")
        call completing_read("Cell kind: ", table_of(kinds), string, status, require_match=REQUIRE_MATCH_YES, initial_input="t")
    end subroutine cell_kind_interactor

    ! (read-string "Title: ") and (setq string (read-string "Title: ")).
    ! Not a port.
    recursive subroutine title_value(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer

        call read_string("Title: ", answer, status)
        if (status%ok()) skeleton_state%value = [skeleton_literal(answer)]
    end subroutine title_value

    ! (setq string (read-string "Title: ")).  Not a port.
    recursive subroutine title_string_value(status)
        type(status_type), intent(out) :: status

        call title_value(status)
        if (status%ok()) skeleton_state%string = skeleton_state%value(1)%text(1:skeleton_state%value(1)%length)
    end subroutine title_string_value

    ! user-mail-address.  Not a port.
    recursive subroutine mail_address_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(user_mail_address)) skeleton_state%value = [skeleton_literal(user_mail_address)]
    end subroutine mail_address_value

    ! (user-full-name).  Not a port.
    recursive subroutine full_name_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        skeleton_state%value = [skeleton_literal(user_full_name())]
    end subroutine full_name_value

    ! (file-relative-name (read-file-name "File name: ") default-directory).
    ! Not a port.
    recursive subroutine file_name_interactor(string, status)
        character(len=:), allocatable, intent(out) :: string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_file_name("File name: ", file, status)
        if (.not. status%ok()) return

        if (allocated(buffers(current)%directory)) then
            string = file_relative_name(file, buffers(current)%directory)
        else
            string = file_relative_name(file)
        end if
    end subroutine file_name_interactor

    ! '(setq v1 nil v2 nil) of the checkboxes and radio buttons.
    ! Not a port.
    recursive subroutine reset_inputs_action(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(input_name)) deallocate (input_name)
        if (allocated(input_newline)) deallocate (input_newline)
        input_checked = .false.
    end subroutine reset_inputs_action

    ! (or v1 (setq v1 (skeleton-read "Name: "))).  Not a port.
    recursive subroutine input_name_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. allocated(input_name)) call skeleton_read(input_name, status, prompt="Name: ")
        if (.not. status%ok()) return
        if (allocated(input_name)) skeleton_state%value = [skeleton_literal(input_name)]
    end subroutine input_name_value

    ! A checkbox's "checked" attribute: (when (y-or-n-p ...) ...).
    ! Not a port.
    recursive subroutine checkbox_checked_value(status)
        type(status_type), intent(out) :: status
        logical :: yes

        call read_y_or_n('Set "checked" attribute? ', yes, status)
        if (status%ok() .and. yes) call checked_text()
    end subroutine checkbox_checked_value

    ! A radio button's "checked" attribute, asked until one is checked.
    ! Not a port.
    recursive subroutine radio_checked_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (input_checked) return
        call read_y_or_n('Set "checked" attribute? ', input_checked, status)
        if (status%ok() .and. input_checked) call checked_text()
    end subroutine radio_checked_value

    ! The text of the checked attribute.  Not a port.
    subroutine checked_text()
        if (sgml_xml_mode%value) then
            skeleton_state%value = [skeleton_literal(transform(' checked="checked"'))]
        else
            skeleton_state%value = [skeleton_literal(transform(" checked"))]
        end if
    end subroutine checked_text

    ! (skeleton-read "Text: " (capitalize string)).  Not a port.
    recursive subroutine input_text_value(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer

        call skeleton_read(answer, status, prompt="Text: ", initial_input=capitalize_string(string_value()))
        if (status%ok() .and. allocated(answer)) skeleton_state%value = [skeleton_literal(answer)]
    end subroutine input_text_value

    ! (or v2 (setq v2 (if (y-or-n-p "Newline after text? ") ... ""))).
    ! Not a port.
    recursive subroutine input_newline_value(status)
        type(status_type), intent(out) :: status
        logical :: yes

        status = status_type()

        if (.not. allocated(input_newline)) then
            call read_y_or_n("Newline after text? ", yes, status)
            if (.not. status%ok()) return
            input_newline = ""

            if (yes) then
                if (sgml_xml_mode%value) then
                    input_newline = transform("<br />")
                else
                    input_newline = transform("<br>")
                end if
            end if
        end if

        skeleton_state%value = [skeleton_literal(input_newline)]
    end subroutine input_newline_value

    ! (skeleton-read "URL: " "#").  Not a port.
    recursive subroutine url_value(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer

        call skeleton_read(answer, status, prompt="URL: ", initial_input="#")
        if (status%ok() .and. allocated(answer)) skeleton_state%value = [skeleton_literal(answer)]
    end subroutine url_value

    ! (skeleton-read "Title: ").  Not a port.
    recursive subroutine link_title_value(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer

        call skeleton_read(answer, status, prompt="Title: ")
        if (status%ok() .and. allocated(answer)) skeleton_state%value = [skeleton_literal(answer)]
    end subroutine link_title_value

    ! (skeleton-read "Page Title: ").  Not a port.
    recursive subroutine page_title_value(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer

        call skeleton_read(answer, status, prompt="Page Title: ")
        if (status%ok() .and. allocated(answer)) skeleton_state%value = [skeleton_literal(answer)]
    end subroutine page_title_value

    ! string, or "".  Not a port.
    function string_value() result(s)
        character(len=:), allocatable :: s

        s = ""
        if (allocated(skeleton_state%string)) s = skeleton_state%string
    end function string_value

    ! A text element (`skeleton_text', shorter to write).  Not a port.
    function text(string) result(element)
        character(len=*), intent(in) :: string
        type(skeleton_element_type) :: element

        element = skeleton_text(string)
    end function text

    ! The skeleton of the HTML skeleton command NAME.  Port of
    ! sgml-mode.el's define-skeleton forms (html-href-anchor ...
    ! html-html5-template).
    subroutine html_skeleton(name, skeleton)
        character(len=*), intent(in) :: name
        type(skeleton_element_type), allocatable, intent(out) :: skeleton(:)
        type(skeleton_element_type), parameter :: N = SKELETON_NEWLINE, P = SKELETON_POINT, S = SKELETON_STRING, &
                                                  I = SKELETON_INDENT, E = SKELETON_END, NONE = SKELETON_NO_INTERACTOR

        select case (name)
        case ("html_href_anchor")
            skeleton = [skeleton_prompt("URL: "), text('<a href="'), S, text('">'), P, text("</a>")]
        case ("html_href_anchor_file")
            skeleton = [skeleton_interactor(file_name_interactor), text('<a href="'), S, text('">'), P, text("</a>")]
        case ("html_name_anchor")
            skeleton = [skeleton_prompt("Name: "), text('<a name="'), S, text('"'), skeleton_value(xml_id), text(">"), P, &
                        text("</a>")]
        case ("html_id_anchor")
            skeleton = [skeleton_prompt("ID: "), text('<a id="'), S, text('">'), P, text("</a>")]
        case ("html_headline_1", "html_headline_2", "html_headline_3", "html_headline_4", "html_headline_5", &
              "html_headline_6")
            skeleton = [NONE, text("<h"//name(15:15)//">"), P, text("</h"//name(15:15)//">")]
        case ("html_horizontal_rule")
            skeleton = [NONE, skeleton_value(xml_hr), N]
        case ("html_image")
            skeleton = [skeleton_prompt("Image URL: "), text('<img src="'), S, text('" alt="'), P, text('"'), &
                        skeleton_value(xml_close)]
        case ("html_line")
            skeleton = [NONE, skeleton_value(xml_br), N]
        case ("html_ordered_list", "html_unordered_list")
            skeleton = [NONE, N, text("<"//name(6:6)//"l>"), N, text("<li>"), P, skeleton_value(xml_end_li), N, &
                        text("</"//name(6:6)//"l>"), I, N]
        case ("html_list_item")
            skeleton = [NONE, skeleton_value(newline_unless_beginning_of_line), text("<li>"), P, skeleton_value(xml_end_li)]
        case ("html_paragraph")
            skeleton = [NONE, N, text("<p>"), P, skeleton_value(xml_end_paragraph)]
        case ("html_div")
            skeleton = [NONE, text("<div>"), I, N, P, N, text("</div>"), I]
        case ("html_span")
            skeleton = [NONE, text("<span>"), I, P, text("</span>")]
        case ("html_checkboxes")
            skeleton = [NONE, skeleton_action(reset_inputs_action), skeleton_prompt("Value: "), text('<input type="'), &
                        skeleton_literal("checkbox"), text('" name="'), skeleton_value(input_name_value), &
                        text('" value="'), S, text('"'), skeleton_value(checkbox_checked_value), skeleton_value(xml_close), &
                        skeleton_value(input_text_value), skeleton_value(input_newline_value), N, E]
        case ("html_radio_buttons")
            skeleton = [NONE, skeleton_action(reset_inputs_action), skeleton_prompt("Value: "), text('<input type="'), &
                        skeleton_literal("radio"), text('" name="'), skeleton_value(input_name_value), &
                        text('" value="'), S, text('"'), skeleton_value(radio_checked_value), skeleton_value(xml_close), &
                        skeleton_value(input_text_value), skeleton_value(input_newline_value), N, E]
        case ("html_navigational_links")
            skeleton = [NONE, text("<nav>"), N, text("<ul>"), N, text('<li><a href="'), skeleton_value(url_value), &
                        text('">'), skeleton_value(link_title_value), text("</a>"), skeleton_value(xml_end_li), N, &
                        text("</ul>"), N, text("</nav>")]
        case default
            skeleton = [NONE, text("<!DOCTYPE html>"), N, text('<html lang="en">'), N, text("<head>"), N, &
                        text('<meta charset="utf-8">'), N, &
                        text('<meta name="viewport" content="width=device-width, initial-scale=1">'), N, text("<title>"), &
                        skeleton_value(page_title_value), text("</title>"), N, text("</head>"), N, text("<body>"), N, &
                        text('<div id="app"></div>'), N, text("</body>"), N, text("</html>")]
        end select
    end subroutine html_skeleton

    ! Insert the HTML skeleton NAME as its command does.  Not a port.
    recursive subroutine html_skeleton_command(name, status, string, argument)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument
        type(skeleton_element_type), allocatable :: skeleton(:)

        call html_skeleton(name, skeleton)
        call skeleton_command(skeleton, status, string, argument)
    end subroutine html_skeleton_command

    ! The tag tables: sgml-tag-alist and html-tag-alist (their values).
    ! Port of sgml-mode.el sgml-tag-alist and html-tag-alist.
    subroutine initialize_tag_tables()
        type(skeleton_element_type), allocatable :: list_item_skeleton(:)
        type(skeleton_element_type), parameter :: N = SKELETON_NEWLINE, P = SKELETON_POINT, S = SKELETON_STRING, &
                                                  E = SKELETON_END

        if (allocated(html_tags%entries)) return
        list_item_skeleton = [N, skeleton_prompt("List item: "), text("<li>"), S, skeleton_value(xml_end_li), N, E]
        call sgml_tags%append("![")
        call sgml_tags%append_attribute("ignore", optional=.true.)
        call sgml_tags%append_attribute("include", optional=.true.)
        call sgml_tags%append("!attlist")
        call sgml_tags%append("!doctype")
        call sgml_tags%append("!element")
        call sgml_tags%append("!entity")
        call html_tags%append("a")
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("href", values=[character(len=9) :: "ftp:", "file:", "finger:", "gopher:", "http:", &
                                               "https:", "mailto:", "news:", "rlogin:", "telnet:", "tn3270:", "wais:", "/cgi-bin/"])
        call html_tags%append_attribute("rel", values=[character(len=11) :: "next", "previous", "parent", "subdocument", "made"])
        call html_tags%append_attribute("rev", values=[character(len=11) :: "next", "previous", "parent", "subdocument", "made"])
        call html_tags%append_attribute("title")
        call html_tags%append("area", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("shape", values=[character(len=7) :: "rect", "circle", "poly", "default"])
        call html_tags%append_attribute("coords")
        call html_tags%append_attribute("href")
        call html_tags%append_attribute("nohref", value="nohref")
        call html_tags%append_attribute("alt")
        call html_tags%append_attribute("tabindex")
        call html_tags%append_attribute("accesskey")
        call html_tags%append_attribute("onfocus")
        call html_tags%append_attribute("onblur")
        call html_tags%append("base", end_tag=SGML_END_TAG_NO)
        call html_tags%append_name("href")
        call html_tags%append_attribute("ftp:")
        call html_tags%append_attribute("file:")
        call html_tags%append_attribute("finger:")
        call html_tags%append_attribute("gopher:")
        call html_tags%append_attribute("http:")
        call html_tags%append_attribute("https:")
        call html_tags%append_attribute("mailto:")
        call html_tags%append_attribute("news:")
        call html_tags%append_attribute("rlogin:")
        call html_tags%append_attribute("telnet:")
        call html_tags%append_attribute("tn3270:")
        call html_tags%append_attribute("wais:")
        call html_tags%append_attribute("/cgi-bin/")
        call html_tags%append("col", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("align", values=[character(len=7) :: "left", "center", "right", "justify", "char"])
        call html_tags%append_attribute("char")
        call html_tags%append_attribute("charoff")
        call html_tags%append_attribute("valign", values=[character(len=8) :: "top", "middle", "bottom", "baseline"])
        call html_tags%append_attribute("span")
        call html_tags%append_attribute("width")
        call html_tags%append("colgroup", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("align", values=[character(len=7) :: "left", "center", "right", "justify", "char"])
        call html_tags%append_attribute("char")
        call html_tags%append_attribute("charoff")
        call html_tags%append_attribute("valign", values=[character(len=8) :: "top", "middle", "bottom", "baseline"])
        call html_tags%append_attribute("span")
        call html_tags%append_attribute("width")
        call html_tags%append("dir", skeleton=list_item_skeleton, skeleton_slot=SGML_END_TAG_DEFAULT)
        call html_tags%append("figcaption")
        call html_tags%append("figure", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("font", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_name("size")
        call html_tags%append_attribute("-1")
        call html_tags%append_attribute("+1")
        call html_tags%append_attribute("-2")
        call html_tags%append_attribute("+2")
        call html_tags%append_attribute("1")
        call html_tags%append_attribute("2")
        call html_tags%append_attribute("3")
        call html_tags%append_attribute("4")
        call html_tags%append_attribute("5")
        call html_tags%append_attribute("6")
        call html_tags%append_attribute("7")
        call html_tags%append("form", skeleton=[P, N, text('<input type="submit" value=""'), &
                                                skeleton_value(xml_close)], skeleton_slot=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("action", values=[character(len=9) :: "ftp:", "file:", "finger:", "gopher:", "http:", &
                                               "https:", "mailto:", "news:", "rlogin:", "telnet:", "tn3270:", "wais:", "/cgi-bin/"])
        call html_tags%append_attribute("method", values=[character(len=4) :: "get", "post"])
        call html_tags%append("h1")
        call html_tags%append_attribute("align", values=[character(len=6) :: "left", "center", "right"])
        call html_tags%append("h2")
        call html_tags%append_attribute("align", values=[character(len=6) :: "left", "center", "right"])
        call html_tags%append("h3")
        call html_tags%append_attribute("align", values=[character(len=6) :: "left", "center", "right"])
        call html_tags%append("h4")
        call html_tags%append_attribute("align", values=[character(len=6) :: "left", "center", "right"])
        call html_tags%append("h5")
        call html_tags%append_attribute("align", values=[character(len=6) :: "left", "center", "right"])
        call html_tags%append("h6")
        call html_tags%append_attribute("align", values=[character(len=6) :: "left", "center", "right"])
        call html_tags%append("hr", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("size", values=[character(len=1) :: "1", "2", "3", "4", "5", "6", "7", "8", "9"])
        call html_tags%append_attribute("width")
        call html_tags%append_attribute("noshade", optional=.true.)
        call html_tags%append_attribute("align", values=[character(len=6) :: "left", "center", "right"])
        call html_tags%append("iframe", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("align", values=[character(len=6) :: "top", "middle", "bottom", "left", "right"])
        call html_tags%append_attribute("longdesc")
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("src")
        call html_tags%append_attribute("frameborder", values=[character(len=1) :: "1", "0"])
        call html_tags%append_attribute("marginwidth")
        call html_tags%append_attribute("marginheight")
        call html_tags%append_attribute("scrolling", values=[character(len=4) :: "yes", "no", "auto"])
        call html_tags%append_attribute("height")
        call html_tags%append_attribute("width")
        call html_tags%append("img", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("align", values=[character(len=9) :: "top", "middle", "bottom", "baseline", "texttop", &
                                                         "absmiddle", "absbottom"])
        call html_tags%append_attribute("src")
        call html_tags%append_attribute("alt")
        call html_tags%append_attribute("width", value="1")
        call html_tags%append_attribute("height", value="1")
        call html_tags%append_attribute("border", value="1")
        call html_tags%append_attribute("vspace", value="1")
        call html_tags%append_attribute("hspace", value="1")
        call html_tags%append_attribute("ismap", optional=.true.)
        call html_tags%append("input", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("accept")
        call html_tags%append_attribute("alt")
        call html_tags%append_attribute("autocomplete", values=[character(len=3) :: "on", "off"])
        call html_tags%append_attribute("autofocus", optional=.true.)
        call html_tags%append_attribute("checked", optional=.true.)
        call html_tags%append_attribute("dirname")
        call html_tags%append_attribute("disabled", optional=.true.)
        call html_tags%append_attribute("form")
        call html_tags%append_attribute("formaction")
        call html_tags%append_attribute("formenctype", values=[character(len=33) :: "application/x-www-form-urlencoded", &
                                                               "multipart/form-data", "text/plain"])
        call html_tags%append_attribute("formmethod", values=[character(len=4) :: "get", "post"])
        call html_tags%append_attribute("formnovalidate", optional=.true.)
        call html_tags%append_attribute("formtarget", values=[character(len=7) :: "_blank", "_self", "_parent", "_top"])
        call html_tags%append_attribute("height")
        call html_tags%append_attribute("inputmode")
        call html_tags%append_attribute("list")
        call html_tags%append_attribute("max")
        call html_tags%append_attribute("maxlength")
        call html_tags%append_attribute("min")
        call html_tags%append_attribute("minlength")
        call html_tags%append_attribute("multiple", optional=.true.)
        call html_tags%append_attribute("pattern")
        call html_tags%append_attribute("placeholder")
        call html_tags%append_attribute("readonly", optional=.true.)
        call html_tags%append_attribute("required", optional=.true.)
        call html_tags%append_attribute("size")
        call html_tags%append_attribute("src")
        call html_tags%append_attribute("step")
        call html_tags%append_attribute("type", values=[character(len=8) :: "hidden", "text", "search", "tel", "url", "email", &
                           "password", "date", "time", "number", "range", "color", "checkbox", "radio", "file", "submit", "image", &
                                                        "reset", "button"])
        call html_tags%append_attribute("value")
        call html_tags%append_attribute("width")
        call html_tags%append("link", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("href", values=[character(len=9) :: "ftp:", "file:", "finger:", "gopher:", "http:", &
                                               "https:", "mailto:", "news:", "rlogin:", "telnet:", "tn3270:", "wais:", "/cgi-bin/"])
        call html_tags%append_attribute("rel", values=[character(len=11) :: "next", "previous", "parent", "subdocument", "made"])
        call html_tags%append_attribute("rev", values=[character(len=11) :: "next", "previous", "parent", "subdocument", "made"])
        call html_tags%append_attribute("title")
        call html_tags%append("menu", skeleton=list_item_skeleton, skeleton_slot=SGML_END_TAG_DEFAULT)
        call html_tags%append("ol", skeleton=list_item_skeleton, skeleton_slot=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("type", values=[character(len=1) :: "A", "a", "I", "i", "1"])
        call html_tags%append("p", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("align", values=[character(len=6) :: "left", "center", "right"])
        call html_tags%append("select", skeleton=[N, skeleton_prompt("Text: "), text("<option>"), S, &
                                                  skeleton_value(xml_end_option), N, E], skeleton_slot=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("size", values=[character(len=1) :: "1", "2", "3", "4", "5", "6", "7", "8", "9"])
        call html_tags%append_attribute("multiple", optional=.true.)
        call html_tags%append("table", skeleton=[N, skeleton_interactor(cell_kind_interactor), text("<tr><"), S, &
                                                text(">"), P, skeleton_value(xml_end_tr), N, E], skeleton_slot=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("border", optional=.true., values=[character(len=1) :: "1", "2", "3", "4", "5", "6", &
                                                                           "7", "8", "9"])
        call html_tags%append_attribute("width", value="10")
        call html_tags%append_attribute("cellpadding")
        call html_tags%append("tbody", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("align", values=[character(len=7) :: "left", "center", "right", "justify", "char"])
        call html_tags%append_attribute("char")
        call html_tags%append_attribute("charoff")
        call html_tags%append_attribute("valign", values=[character(len=8) :: "top", "middle", "bottom", "baseline"])
        call html_tags%append("td", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("align", values=[character(len=6) :: "left", "center", "right"])
        call html_tags%append_attribute("valign", values=[character(len=8) :: "top", "middle", "bottom", "baseline"])
        call html_tags%append_attribute("colspan", values=[character(len=1) :: "1", "2", "3", "4", "5", "6", "7", "8", "9"])
        call html_tags%append_attribute("rowspan", values=[character(len=1) :: "1", "2", "3", "4", "5", "6", "7", "8", "9"])
        call html_tags%append_attribute("nowrap", optional=.true.)
        call html_tags%append("textarea")
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("rows", values=[character(len=1) :: "1", "2", "3", "4", "5", "6", "7", "8", "9"])
        call html_tags%append_attribute("cols", values=[character(len=1) :: "1", "2", "3", "4", "5", "6", "7", "8", "9"])
        call html_tags%append("tfoot", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("align", values=[character(len=7) :: "left", "center", "right", "justify", "char"])
        call html_tags%append_attribute("char")
        call html_tags%append_attribute("charoff")
        call html_tags%append_attribute("valign", values=[character(len=8) :: "top", "middle", "bottom", "baseline"])
        call html_tags%append("th", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("align", values=[character(len=6) :: "left", "center", "right"])
        call html_tags%append_attribute("valign", values=[character(len=8) :: "top", "middle", "bottom", "baseline"])
        call html_tags%append_attribute("colspan", values=[character(len=1) :: "1", "2", "3", "4", "5", "6", "7", "8", "9"])
        call html_tags%append_attribute("rowspan", values=[character(len=1) :: "1", "2", "3", "4", "5", "6", "7", "8", "9"])
        call html_tags%append_attribute("nowrap", optional=.true.)
        call html_tags%append("thead", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("align", values=[character(len=7) :: "left", "center", "right", "justify", "char"])
        call html_tags%append_attribute("char")
        call html_tags%append_attribute("charoff")
        call html_tags%append_attribute("valign", values=[character(len=8) :: "top", "middle", "bottom", "baseline"])
        call html_tags%append("ul", skeleton=list_item_skeleton, skeleton_slot=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("type", values=[character(len=6) :: "disc", "circle", "square"])
        call html_tags%append("![")
        call html_tags%append_attribute("ignore", optional=.true.)
        call html_tags%append_attribute("include", optional=.true.)
        call html_tags%append("!attlist")
        call html_tags%append("!doctype")
        call html_tags%append("!element")
        call html_tags%append("!entity")
        call html_tags%append("abbr")
        call html_tags%append("acronym")
        call html_tags%append("address")
        call html_tags%append("array", skeleton=[N, skeleton_prompt("Item: "), text("<item>"), S, &
                                                 skeleton_value(xml_end_item), N, E], skeleton_slot=SGML_END_TAG_DEFAULT)
        call html_tags%append_name("align")
        call html_tags%append("article", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("aside", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("au")
        call html_tags%append("audio", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("src")
        call html_tags%append_attribute("crossorigin", values=[character(len=15) :: "anonymous", "use-credentials"])
        call html_tags%append_attribute("preload", values=[character(len=8) :: "none", "metadata", "auto"])
        call html_tags%append_attribute("autoplay", value="autoplay")
        call html_tags%append_attribute("mediagroup")
        call html_tags%append_attribute("loop", value="loop")
        call html_tags%append_attribute("muted", value="muted")
        call html_tags%append_attribute("controls", value="controls")
        call html_tags%append("b")
        call html_tags%append("bdi")
        call html_tags%append("bdo", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("lang")
        call html_tags%append_attribute("dir", values=[character(len=3) :: "ltr", "rtl"])
        call html_tags%append("big")
        call html_tags%append("blink")
        call html_tags%append("blockquote", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("cite")
        call html_tags%append("body", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("background", value=".gif")
        call html_tags%append_attribute("bgcolor", value="#")
        call html_tags%append_attribute("text", value="#")
        call html_tags%append_attribute("link", value="#")
        call html_tags%append_attribute("alink", value="#")
        call html_tags%append_attribute("vlink", value="#")
        call html_tags%append("box", skeleton=[P, text("<over>"), P, skeleton_value(xml_end_over)], &
                              skeleton_slot=SGML_END_TAG_DEFAULT)
        call html_tags%append("br", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("clear", values=[character(len=5) :: "left", "right"])
        call html_tags%append("button", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("value")
        call html_tags%append_attribute("type", values=[character(len=6) :: "submit", "reset", "button"])
        call html_tags%append_attribute("disabled", value="disabled")
        call html_tags%append_attribute("tabindex")
        call html_tags%append_attribute("accesskey")
        call html_tags%append_attribute("onfocus")
        call html_tags%append_attribute("onblur")
        call html_tags%append("canvas", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("width")
        call html_tags%append_attribute("height")
        call html_tags%append("caption")
        call html_tags%append_attribute("valign", values=[character(len=6) :: "top", "bottom"])
        call html_tags%append("center", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("cite")
        call html_tags%append("code")
        call html_tags%append("datalist", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("dd", end_tag=SGML_END_TAG_NO)
        call html_tags%append("del", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("cite")
        call html_tags%append_attribute("datetime")
        call html_tags%append("details", skeleton=[text("<summary>"), skeleton_value(title_value), &
                                                   text("</summary>"), N, P], skeleton_slot=SGML_END_TAG_NEWLINES)
        call html_tags%append("dfn")
        call html_tags%append("div", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("id")
        call html_tags%append_attribute("class")
        call html_tags%append("dl", skeleton=[N, skeleton_prompt("Term: "), text("<dt>"), S, skeleton_value(xml_end_dt), &
                                             text("<dd>"), P, skeleton_value(xml_end_dd), N, E], skeleton_slot=SGML_END_TAG_DEFAULT)
        call html_tags%append("dt", skeleton=[P, skeleton_value(xml_end_dt), text("<dd>"), skeleton_value(xml_end_dd), N], &
                              skeleton_slot=SGML_END_TAG_NO)
        call html_tags%append("em")
        call html_tags%append("embed", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("src")
        call html_tags%append_attribute("type")
        call html_tags%append_attribute("width")
        call html_tags%append_attribute("height")
        call html_tags%append("fieldset", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("fn")
        call html_tags%append_name("id")
        call html_tags%append_name("fn")
        call html_tags%append("footer", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("frame", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("longdesc")
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("src")
        call html_tags%append_attribute("frameborder", values=[character(len=1) :: "1", "0"])
        call html_tags%append_attribute("marginwidth")
        call html_tags%append_attribute("marginheight")
        call html_tags%append_attribute("noresize", value="noresize")
        call html_tags%append_attribute("scrolling", values=[character(len=4) :: "yes", "no", "auto"])
        call html_tags%append("frameset", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("rows")
        call html_tags%append_attribute("cols")
        call html_tags%append_attribute("onload")
        call html_tags%append_attribute("onunload")
        call html_tags%append("head", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("header", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("hgroup", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("html", skeleton=[text("<head>"//LF), text("<title>"), skeleton_value(title_string_value), &
                                                text("</title>"//LF), text("</head>"//LF), text("<body>"//LF//"<h1>"), S, &
                                                text("</h1>"//LF), P, text(LF//"<address>"//LF//"<a href=""mailto:"), &
                                                skeleton_value(mail_address_value), text('">'), skeleton_value(full_name_value), &
                                          text("</a>"//LF//"</address>"//LF), text("</body>")], skeleton_slot=SGML_END_TAG_NEWLINES)
        call html_tags%append("i")
        call html_tags%append("ins", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("cite")
        call html_tags%append_attribute("datetime")
        call html_tags%append("isindex", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("action")
        call html_tags%append_attribute("prompt")
        call html_tags%append("kbd")
        call html_tags%append("label", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("for")
        call html_tags%append_attribute("accesskey")
        call html_tags%append_attribute("onfocus")
        call html_tags%append_attribute("onblur")
        call html_tags%append("lang")
        call html_tags%append("legend", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("accesskey")
        call html_tags%append("li", end_tag=SGML_END_TAG_NO)
        call html_tags%append("main", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("map", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("name")
        call html_tags%append("mark")
        call html_tags%append("math", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("meta", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("http-equiv")
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("content")
        call html_tags%append_attribute("scheme")
        call html_tags%append("meter", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("value")
        call html_tags%append_attribute("min")
        call html_tags%append_attribute("max")
        call html_tags%append_attribute("low")
        call html_tags%append_attribute("high")
        call html_tags%append_attribute("optimum")
        call html_tags%append("nav", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("nobr")
        call html_tags%append("noframes", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("noscript", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("object", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("declare", value="declare")
        call html_tags%append_attribute("classid")
        call html_tags%append_attribute("codebase")
        call html_tags%append_attribute("data")
        call html_tags%append_attribute("type")
        call html_tags%append_attribute("codetype")
        call html_tags%append_attribute("archive")
        call html_tags%append_attribute("standby")
        call html_tags%append_attribute("height")
        call html_tags%append_attribute("width")
        call html_tags%append_attribute("usemap")
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("tabindex")
        call html_tags%append("optgroup", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("size")
        call html_tags%append_attribute("multiple", value="multiple")
        call html_tags%append_attribute("disabled", value="disabled")
        call html_tags%append_attribute("tabindex")
        call html_tags%append_attribute("onfocus")
        call html_tags%append_attribute("onblur")
        call html_tags%append_attribute("onchange")
        call html_tags%append("option", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("value")
        call html_tags%append_attribute("label")
        call html_tags%append_attribute("selected", optional=.true.)
        call html_tags%append("output", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("for")
        call html_tags%append_attribute("form")
        call html_tags%append_attribute("name")
        call html_tags%append("over", end_tag=SGML_END_TAG_NO)
        call html_tags%append("param", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("name")
        call html_tags%append_attribute("value")
        call html_tags%append_attribute("valuetype", values=[character(len=6) :: "data", "ref", "object"])
        call html_tags%append_attribute("type")
        call html_tags%append("person")
        call html_tags%append("picture", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("pre", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("progress", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("value")
        call html_tags%append_attribute("max")
        call html_tags%append("q", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("cite")
        call html_tags%append("rev")
        call html_tags%append("rp", end_tag=SGML_END_TAG_NO)
        call html_tags%append("rt", end_tag=SGML_END_TAG_NO)
        call html_tags%append("ruby")
        call html_tags%append("s")
        call html_tags%append("samp")
        call html_tags%append("script", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("charset")
        call html_tags%append_attribute("type")
        call html_tags%append_attribute("src")
        call html_tags%append_attribute("defer", value="defer")
        call html_tags%append("section", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append("small")
        call html_tags%append("source", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("src")
        call html_tags%append_attribute("type")
        call html_tags%append_attribute("media")
        call html_tags%append("span", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("class", values=[character(len=13) :: "builtin", "comment", "constant", &
                                                         "function-name", "keyword", "string", "type", "variable-name", "warning"])
        call html_tags%append("strong")
        call html_tags%append("style", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("type")
        call html_tags%append_attribute("media")
        call html_tags%append_attribute("title")
        call html_tags%append("sub")
        call html_tags%append("summary")
        call html_tags%append("sup")
        call html_tags%append("time", end_tag=SGML_END_TAG_DEFAULT)
        call html_tags%append_attribute("datetime")
        call html_tags%append("title")
        call html_tags%append("tr", end_tag=SGML_END_TAG_NO)
        call html_tags%append("track", end_tag=SGML_END_TAG_NO)
        call html_tags%append_attribute("kind", values=[character(len=12) :: "subtitles", "captions", "descriptions", &
                                                        "chapters", "metadata"])
        call html_tags%append_attribute("src")
        call html_tags%append_attribute("srclang")
        call html_tags%append_attribute("label")
        call html_tags%append_attribute("default")
        call html_tags%append("tt")
        call html_tags%append("u")
        call html_tags%append("var")
        call html_tags%append("video", end_tag=SGML_END_TAG_NEWLINES)
        call html_tags%append_attribute("src")
        call html_tags%append_attribute("crossorigin", values=[character(len=15) :: "anonymous", "use-credentials"])
        call html_tags%append_attribute("poster")
        call html_tags%append_attribute("preload", values=[character(len=8) :: "none", "metadata", "auto"])
        call html_tags%append_attribute("autoplay", value="autoplay")
        call html_tags%append_attribute("mediagroup")
        call html_tags%append_attribute("loop", value="loop")
        call html_tags%append_attribute("muted", value="muted")
        call html_tags%append_attribute("controls", value="controls")
        call html_tags%append_attribute("width")
        call html_tags%append_attribute("height")
        call html_tags%append("wbr", end_tag=SGML_END_TAG_NO)
    end subroutine initialize_tag_tables

    ! The tag descriptions: sgml-tag-help and html-tag-help (their values).
    ! Port of sgml-mode.el sgml-tag-help and html-tag-help.
    subroutine initialize_tag_help()
        if (allocated(html_tag_help)) return
        html_tag_help = [ &
                        sgml_tag_help_type(tag="!", description="Empty declaration for comment"), &
                        sgml_tag_help_type(tag="![", description="Embed declarations with parser directive"), &
                        sgml_tag_help_type(tag="!attlist", description="Tag attributes declaration"), &
                        sgml_tag_help_type(tag="!doctype", description="Document type (DTD) declaration"), &
                        sgml_tag_help_type(tag="!element", description="Tag declaration"), &
                        sgml_tag_help_type(tag="!entity", description="Entity (macro) declaration"), &
                        sgml_tag_help_type(tag="a", description="Anchor of point or link elsewhere"), &
                        sgml_tag_help_type(tag="abbr", description="Abbreviation"), &
                        sgml_tag_help_type(tag="acronym", description="Acronym"), &
                        sgml_tag_help_type(tag="address", description="Formatted mail address"), &
                        sgml_tag_help_type(tag="area", description="Region of an image map"), &
                        sgml_tag_help_type(tag="array", description="Math array"), &
                        sgml_tag_help_type(tag="article", description="An independent part of document or site"), &
                        sgml_tag_help_type(tag="aside", &
                                           description="Secondary content related to surrounding content "// &
                                           "(e.g. page or article)"), &
                        sgml_tag_help_type(tag="au", description="Author"), &
                        sgml_tag_help_type(tag="audio", description="Sound or audio stream"), &
                        sgml_tag_help_type(tag="b", description="Bold face"), &
                        sgml_tag_help_type(tag="base", description="Base address for URLs"), &
                        sgml_tag_help_type(tag="bdi", description="Text isolated for bidirectional formatting"), &
                        sgml_tag_help_type(tag="bdo", description="Override text directionality"), &
                        sgml_tag_help_type(tag="big", description="Font size"), &
                        sgml_tag_help_type(tag="blink", description="Blinking text"), &
                        sgml_tag_help_type(tag="blockquote", description="Indented quotation"), &
                        sgml_tag_help_type(tag="body", description="Document body"), &
                        sgml_tag_help_type(tag="box", description="Math fraction"), &
                        sgml_tag_help_type(tag="br", description="Line break"), &
                        sgml_tag_help_type(tag="button", description="Clickable button"), &
                        sgml_tag_help_type(tag="canvas", description="Script generated graphics canvas"), &
                        sgml_tag_help_type(tag="caption", description="Table caption"), &
                        sgml_tag_help_type(tag="center", description="Centered text"), &
                        sgml_tag_help_type(tag="changed", description="Change bars"), &
                        sgml_tag_help_type(tag="cite", description="Citation of a document"), &
                        sgml_tag_help_type(tag="code", description="Formatted source code"), &
                        sgml_tag_help_type(tag="col", description="Group of attribute specifications for table columns"), &
                        sgml_tag_help_type(tag="colgroup", description="Group of columns"), &
                        sgml_tag_help_type(tag="datalist", description="A set of predefined options"), &
                        sgml_tag_help_type(tag="dd", description="Definition of term"), &
                        sgml_tag_help_type(tag="del", description="Deleted text"), &
                        sgml_tag_help_type(tag="details", description="Details disclosure"), &
                        sgml_tag_help_type(tag="dfn", description="Defining instance of a term"), &
                        sgml_tag_help_type(tag="dir", description="Directory list (obsolete)"), &
                        sgml_tag_help_type(tag="div", description="Generic block-level container"), &
                        sgml_tag_help_type(tag="dl", description="Definition list"), &
                        sgml_tag_help_type(tag="dt", description="Term to be defined"), &
                        sgml_tag_help_type(tag="em", description="Emphasized"), &
                        sgml_tag_help_type(tag="embed", description="Embedded data in foreign format"), &
                        sgml_tag_help_type(tag="fieldset", description="Group of related controls and labels"), &
                        sgml_tag_help_type(tag="fig", description="Figure"), &
                        sgml_tag_help_type(tag="figa", description="Figure anchor"), &
                        sgml_tag_help_type(tag="figcaption", description="Caption for a figure"), &
                        sgml_tag_help_type(tag="figd", description="Figure description"), &
                        sgml_tag_help_type(tag="figt", description="Figure text"), &
                        sgml_tag_help_type(tag="figure", description="Self-contained content, often with a caption"), &
                        sgml_tag_help_type(tag="fn", description="Footnote"), &
                        sgml_tag_help_type(tag="font", description="Font size"), &
                        sgml_tag_help_type(tag="footer", description="Footer of a section"), &
                        sgml_tag_help_type(tag="form", description="Form with input fields"), &
                        sgml_tag_help_type(tag="frame", description="Frame in which another HTML document can be displayed"), &
                        sgml_tag_help_type(tag="frameset", description="Container for frames"), &
                        sgml_tag_help_type(tag="group", description="Document grouping"), &
                        sgml_tag_help_type(tag="h1", description="Most important section headline"), &
                        sgml_tag_help_type(tag="h2", description="Important section headline"), &
                        sgml_tag_help_type(tag="h3", description="Section headline"), &
                        sgml_tag_help_type(tag="h4", description="Minor section headline"), &
                        sgml_tag_help_type(tag="h5", description="Unimportant section headline"), &
                        sgml_tag_help_type(tag="h6", description="Least important section headline"), &
                        sgml_tag_help_type(tag="head", description="Document header"), &
                        sgml_tag_help_type(tag="header", description="Header of a section"), &
                        sgml_tag_help_type(tag="hgroup", description="Group of headings - h1-h6 elements"), &
                        sgml_tag_help_type(tag="hr", description="Horizontal rule"), &
                        sgml_tag_help_type(tag="html", description="HTML Document"), &
                        sgml_tag_help_type(tag="i", description="Italic face"), &
                        sgml_tag_help_type(tag="iframe", description="Inline frame with a nested browsing context"), &
                        sgml_tag_help_type(tag="img", description="Graphic image"), &
                        sgml_tag_help_type(tag="input", description="Form input field"), &
                        sgml_tag_help_type(tag="ins", description="Inserted text"), &
                        sgml_tag_help_type(tag="isindex", description="Input field for index search"), &
                        sgml_tag_help_type(tag="kbd", description="Keyboard example face"), &
                        sgml_tag_help_type(tag="label", description="Caption for a user interface item"), &
                        sgml_tag_help_type(tag="lang", description="Natural language"), &
                        sgml_tag_help_type(tag="legend", description="Caption for a fieldset"), &
                        sgml_tag_help_type(tag="li", description="List item"), &
                        sgml_tag_help_type(tag="link", description="Link relationship"), &
                        sgml_tag_help_type(tag="main", description="Main content of the document body"), &
                        sgml_tag_help_type(tag="map", description="Image map (a clickable link area"), &
                        sgml_tag_help_type(tag="mark", description="Highlighted text"), &
                        sgml_tag_help_type(tag="math", description="Math formula"), &
                        sgml_tag_help_type(tag="menu", description="List of commands"), &
                        sgml_tag_help_type(tag="meta", description="Document properties"), &
                        sgml_tag_help_type(tag="meter", description="Scalar measurement within a known range"), &
                        sgml_tag_help_type(tag="mh", description="Form mail header"), &
                        sgml_tag_help_type(tag="nav", description="Group of navigational links"), &
                        sgml_tag_help_type(tag="nextid", description="Allocate new id"), &
                        sgml_tag_help_type(tag="nobr", description="Text without line break"), &
                        sgml_tag_help_type(tag="noframes", description="Content for user agents that don't support frames"), &
                        sgml_tag_help_type(tag="noscript", description="Alternate content for when a script isn't executed"), &
                        sgml_tag_help_type(tag="object", description="External resource"), &
                        sgml_tag_help_type(tag="ol", description="Ordered list"), &
                        sgml_tag_help_type(tag="optgroup", description="Group of options"), &
                        sgml_tag_help_type(tag="option", description="Selection list item"), &
                        sgml_tag_help_type(tag="output", description="Result of a calculation or user action"), &
                        sgml_tag_help_type(tag="over", description="Math fraction rule"), &
                        sgml_tag_help_type(tag="p", description="Paragraph start"), &
                        sgml_tag_help_type(tag="panel", description="Floating panel"), &
                        sgml_tag_help_type(tag="param", description="Parameters for an object"), &
                        sgml_tag_help_type(tag="person", description="Person's name"), &
                        sgml_tag_help_type(tag="picture", description="Picture"), &
                        sgml_tag_help_type(tag="pre", description="Preformatted fixed width text"), &
                        sgml_tag_help_type(tag="progress", description="Completion progress of a task"), &
                        sgml_tag_help_type(tag="q", description="Quotation"), &
                        sgml_tag_help_type(tag="rev", description="Reverse video"), &
                        sgml_tag_help_type(tag="rp", description="Fallback text for when ruby annotations aren't supported"), &
                        sgml_tag_help_type(tag="rt", description="Ruby text component of a ruby annotation"), &
                        sgml_tag_help_type(tag="ruby", description="Ruby annotation"), &
                        sgml_tag_help_type(tag="s", description="Strikeout"), &
                        sgml_tag_help_type(tag="samp", description="Sample text"), &
                        sgml_tag_help_type(tag="script", description="Executable script within a document"), &
                        sgml_tag_help_type(tag="section", description="Section of a document"), &
                        sgml_tag_help_type(tag="select", description="Selection list"), &
                        sgml_tag_help_type(tag="small", description="Font size"), &
                        sgml_tag_help_type(tag="source", description="Media resource for media elements"), &
                        sgml_tag_help_type(tag="sp", description="Nobreak space"), &
                        sgml_tag_help_type(tag="span", description="Generic inline container"), &
                        sgml_tag_help_type(tag="strong", description="Standout text"), &
                        sgml_tag_help_type(tag="style", description="Style information"), &
                        sgml_tag_help_type(tag="sub", description="Subscript"), &
                        sgml_tag_help_type(tag="summary", description="Summary, caption, or legend"), &
                        sgml_tag_help_type(tag="sup", description="Superscript"), &
                        sgml_tag_help_type(tag="table", description="Table with rows and columns"), &
                        sgml_tag_help_type(tag="tb", description="Table vertical break"), &
                        sgml_tag_help_type(tag="tbody", description="Table body"), &
                        sgml_tag_help_type(tag="td", description="Table data cell"), &
                        sgml_tag_help_type(tag="textarea", description="Form multiline edit area"), &
                        sgml_tag_help_type(tag="tfoot", description="Table foot"), &
                        sgml_tag_help_type(tag="th", description="Table header cell"), &
                        sgml_tag_help_type(tag="thead", description="Table head"), &
                        sgml_tag_help_type(tag="time", description="Content with optional machine-readable timestamp"), &
                        sgml_tag_help_type(tag="title", description="Document title"), &
                        sgml_tag_help_type(tag="tr", description="Table row separator"), &
                        sgml_tag_help_type(tag="track", description="Timed text track for media elements"), &
                        sgml_tag_help_type(tag="tt", description="Typewriter face"), &
                        sgml_tag_help_type(tag="u", description="Underlined text"), &
                        sgml_tag_help_type(tag="ul", description="Unordered list"), &
                        sgml_tag_help_type(tag="var", description="Math variable face"), &
                        sgml_tag_help_type(tag="video", description="Video or movie"), &
                        sgml_tag_help_type(tag="wbr", description="Enable <br> within <nobr>")]
        if (.not. allocated(sgml_tag_help_variable)) sgml_tag_help_variable = html_tag_help(1:6)
    end subroutine initialize_tag_help

    !> HTML anchor tag with href attribute.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-href-anchor.
    recursive subroutine html_href_anchor(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_href_anchor", status, string, argument)
    end subroutine html_href_anchor

    recursive subroutine execute_html_href_anchor(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_href_anchor(status, argument=current_prefix_argument)
    end subroutine execute_html_href_anchor

    !> HTML anchor tag with href attribute (from a local file).
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-href-anchor-file.
    recursive subroutine html_href_anchor_file(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_href_anchor_file", status, string, argument)
    end subroutine html_href_anchor_file

    recursive subroutine execute_html_href_anchor_file(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_href_anchor_file(status, argument=current_prefix_argument)
    end subroutine execute_html_href_anchor_file

    !> HTML anchor tag with name attribute.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-name-anchor.
    recursive subroutine html_name_anchor(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_name_anchor", status, string, argument)
    end subroutine html_name_anchor

    recursive subroutine execute_html_name_anchor(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_name_anchor(status, argument=current_prefix_argument)
    end subroutine execute_html_name_anchor

    !> HTML anchor tag with id attribute.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-id-anchor.
    recursive subroutine html_id_anchor(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_id_anchor", status, string, argument)
    end subroutine html_id_anchor

    recursive subroutine execute_html_id_anchor(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_id_anchor(status, argument=current_prefix_argument)
    end subroutine execute_html_id_anchor

    !> HTML level 1 headline tags.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-headline-1.
    recursive subroutine html_headline_1(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_headline_1", status, string, argument)
    end subroutine html_headline_1

    recursive subroutine execute_html_headline_1(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_headline_1(status, argument=current_prefix_argument)
    end subroutine execute_html_headline_1

    !> HTML level 2 headline tags.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-headline-2.
    recursive subroutine html_headline_2(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_headline_2", status, string, argument)
    end subroutine html_headline_2

    recursive subroutine execute_html_headline_2(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_headline_2(status, argument=current_prefix_argument)
    end subroutine execute_html_headline_2

    !> HTML level 3 headline tags.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-headline-3.
    recursive subroutine html_headline_3(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_headline_3", status, string, argument)
    end subroutine html_headline_3

    recursive subroutine execute_html_headline_3(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_headline_3(status, argument=current_prefix_argument)
    end subroutine execute_html_headline_3

    !> HTML level 4 headline tags.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-headline-4.
    recursive subroutine html_headline_4(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_headline_4", status, string, argument)
    end subroutine html_headline_4

    recursive subroutine execute_html_headline_4(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_headline_4(status, argument=current_prefix_argument)
    end subroutine execute_html_headline_4

    !> HTML level 5 headline tags.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-headline-5.
    recursive subroutine html_headline_5(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_headline_5", status, string, argument)
    end subroutine html_headline_5

    recursive subroutine execute_html_headline_5(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_headline_5(status, argument=current_prefix_argument)
    end subroutine execute_html_headline_5

    !> HTML level 6 headline tags.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-headline-6.
    recursive subroutine html_headline_6(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_headline_6", status, string, argument)
    end subroutine html_headline_6

    recursive subroutine execute_html_headline_6(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_headline_6(status, argument=current_prefix_argument)
    end subroutine execute_html_headline_6

    !> HTML horizontal rule tag.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-horizontal-rule.
    recursive subroutine html_horizontal_rule(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_horizontal_rule", status, string, argument)
    end subroutine html_horizontal_rule

    recursive subroutine execute_html_horizontal_rule(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_horizontal_rule(status, argument=current_prefix_argument)
    end subroutine execute_html_horizontal_rule

    !> HTML image tag.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-image.
    recursive subroutine html_image(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_image", status, string, argument)
    end subroutine html_image

    recursive subroutine execute_html_image(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_image(status, argument=current_prefix_argument)
    end subroutine execute_html_image

    !> HTML line break tag.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-line.
    recursive subroutine html_line(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_line", status, string, argument)
    end subroutine html_line

    recursive subroutine execute_html_line(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_line(status, argument=current_prefix_argument)
    end subroutine execute_html_line

    !> HTML ordered list tags.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-ordered-list.
    recursive subroutine html_ordered_list(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_ordered_list", status, string, argument)
    end subroutine html_ordered_list

    recursive subroutine execute_html_ordered_list(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_ordered_list(status, argument=current_prefix_argument)
    end subroutine execute_html_ordered_list

    !> HTML unordered list tags.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-unordered-list.
    recursive subroutine html_unordered_list(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_unordered_list", status, string, argument)
    end subroutine html_unordered_list

    recursive subroutine execute_html_unordered_list(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_unordered_list(status, argument=current_prefix_argument)
    end subroutine execute_html_unordered_list

    !> HTML list item tag.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-list-item.
    recursive subroutine html_list_item(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_list_item", status, string, argument)
    end subroutine html_list_item

    recursive subroutine execute_html_list_item(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_list_item(status, argument=current_prefix_argument)
    end subroutine execute_html_list_item

    !> HTML paragraph tag.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-paragraph.
    recursive subroutine html_paragraph(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_paragraph", status, string, argument)
    end subroutine html_paragraph

    recursive subroutine execute_html_paragraph(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_paragraph(status, argument=current_prefix_argument)
    end subroutine execute_html_paragraph

    !> HTML div tag.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-div.
    recursive subroutine html_div(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_div", status, string, argument)
    end subroutine html_div

    recursive subroutine execute_html_div(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_div(status, argument=current_prefix_argument)
    end subroutine execute_html_div

    !> HTML span tag.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-span.
    recursive subroutine html_span(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_span", status, string, argument)
    end subroutine html_span

    recursive subroutine execute_html_span(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_span(status, argument=current_prefix_argument)
    end subroutine execute_html_span

    !> Group of connected checkbox inputs.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-checkboxes.
    recursive subroutine html_checkboxes(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_checkboxes", status, string, argument)
    end subroutine html_checkboxes

    recursive subroutine execute_html_checkboxes(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_checkboxes(status, argument=current_prefix_argument)
    end subroutine execute_html_checkboxes

    !> Group of connected radio button inputs.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-radio-buttons.
    recursive subroutine html_radio_buttons(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_radio_buttons", status, string, argument)
    end subroutine html_radio_buttons

    recursive subroutine execute_html_radio_buttons(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_radio_buttons(status, argument=current_prefix_argument)
    end subroutine execute_html_radio_buttons

    !> Group of navigational links.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-navigational-links.
    recursive subroutine html_navigational_links(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_navigational_links", status, string, argument)
    end subroutine html_navigational_links

    recursive subroutine execute_html_navigational_links(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_navigational_links(status, argument=current_prefix_argument)
    end subroutine execute_html_navigational_links

    !> Initial HTML5 template.
    !>
    !> This is a skeleton command (see `skeleton_insert').
    !> Normally the skeleton text is inserted at point, with nothing "inside".
    !> If there is a highlighted region, the skeleton text is wrapped
    !> around the region text.
    !>
    !> A prefix argument ARGUMENT says to wrap the skeleton around the next ARGUMENT words.
    !> A prefix argument of -1 says to wrap around region, even if not highlighted.
    !> A prefix argument of zero says to wrap around zero words---that is, nothing.
    !> This is a way of overriding the use of a highlighted region.  Port of
    !> sgml-mode.el:html-html5-template.
    recursive subroutine html_html5_template(status, string, argument)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: string
        type(prefix_argument_type), intent(in), optional :: argument

        call html_skeleton_command("html_html5_template", status, string, argument)
    end subroutine html_html5_template

    recursive subroutine execute_html_html5_template(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call html_html5_template(status, argument=current_prefix_argument)
    end subroutine execute_html_html5_template
end module sgml_mode_mod

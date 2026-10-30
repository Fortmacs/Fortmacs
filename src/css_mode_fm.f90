! css_mode_fm.f90 -- major mode to edit CSS files.
! Ported from GNU Emacs 31.1 lisp/textmodes/css-mode.el.
!
! Copyright (C) 2006-2026 Free Software Foundation, Inc.
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

!> CSS mode (`css_mode', `css_base_mode') and SCSS mode (`scss_mode'):
!> the syntax tables, Font Lock keywords with color fontification
!> (`css_fontify_colors'), SMIE indentation, completion at point of
!> properties, values, pseudo-classes and -elements, at-rules, bang-rules
!> and selectors, `css_cycle_color_format' (C-c C-f), `css_fill_paragraph'
!> and `css_current_defun_name'.
!>
!> Deferred: `css_lookup_symbol' (it needs eww; `css__mdn_find_symbol'
!> is ported), Imenu (its index functions are ported, for
!> `css_current_defun_name'), `add_log_current_defun_function', the menu,
!> `css_ts_mode', `less_css_mode' (less-css-mode.el) and the
!> `:company_kind' completion properties.
module css_mode_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_rint
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, list, make_string, make_list, make_fixnum
    use lread_fm, only: sym
    use data_fm, only: integer_setting, logical_setting, string_setting, forward_setting, add_local_variables_killer
    use commands_fm, only: commands
    use buffer_fm, only: buffers, cur, buffer_serial_live, current_buffer, buffer_list, set_buffer, buffer_live_p
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes
    use keymap_fm, only: keymap_set, define_key
    use syntax_fm, only: make_syntax_table, modify_syntax_entry, syntax_table_live_p, syntax_ppss, parse_state_type, &
                         forward_comment, skip_chars_forward, skip_chars_backward, char_syntax
    use editfns_fm, only: point, point_min, point_max, goto_char, bolp, eolp, char_after, char_before, &
                          buffer_substring, line_beginning_position, line_end_position, narrow_to_region, widen, &
                          following_char, preceding_char
    use cmds_fm, only: forward_char
    use insdel_fm, only: insert
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use indent_fm, only: indent_region
    use search_fm, only: looking_at, re_search_forward, re_search_backward, search_backward, string_match, &
                         match_beginning, match_end, replace_match, case_fold_search, SEARCH_NOERROR
    use subr_fm, only: match_string_no_properties, looking_back, silent_modifications_type, silent_modifications_begin, &
                       silent_modifications_end
    use simple_fm, only: newline, backward_word
    use textprop_fm, only: put_text_property
    use xdisp_fm, only: message
    use faces_fm, only: named_face_type, display_when, face_attrs, readable_foreground_color
    use color_fm, only: color_name_to_rgb, color_rgb_to_hex, color_hsl_to_rgb
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                            FONT_LOCK_OVERRIDE_T, FONT_LOCK_OVERRIDE_KEEP
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight, font_lock_default_fontify_region, &
                            set_local_font_lock_fontify_region_function
    use lisp_cmds_fm, only: forward_sexp, backward_sexp, down_list, kill_sexp
    use newcomment_fm, only: comment_start, comment_start_skip, comment_end, comment_end_skip, comment_continue, &
                             comment_normalize_vars, comment_search_forward, comment_beginning
    use paragraphs_fm, only: paragraph_start, paragraph_separate
    use fill_fm, only: fill_paragraph, fill_paragraph_function, adaptive_fill_function
    use electric_fm, only: electric_indent_chars_value, set_local_electric_indent_chars
    use syntax_propertize_fm, only: syntax_propertize_function, syntax_propertize_rules_type, syntax_highlight_type
    use smie_fm, only: smie_prec_type, smie_precs_type, smie_prec2_type, smie_grammar_type, smie_rule_result_type, &
                       smie_indent_type, smie_sexp_type, smie_precs_to_prec2, smie_prec2_to_grammar, smie_setup, &
                       smie_default_forward_token, smie_default_backward_token, smie_backward_sexp, &
                       smie_rule_hanging_p, smie_rule_bolp, smie_rule_parent, smie_indent_virtual, SMIE_ASSOC, SMIE_LEFT, &
                       SMIE_AFTER, SMIE_BEFORE, SMIE_ELEM, SMIE_LIST_INTRO, SMIE_NIL, SMIE_RULE_OFFSET, SMIE_RULE_COLUMN, &
                       SMIE_RULE_T, SMIE_INDENT_COLUMN
    use minibuf_table_fm, only: capf_data_type, completion_table_type, completion_predicate, TABLE_FUNCTION, &
                                COMPLETE_TRY, COMPLETE_ALL, COMPLETE_TEST, COMPLETION_FINISHED
    use minibuf_fm, only: try_completion, all_completions, test_completion
    use minibuffer_fm, only: completion_at_point_functions
    use regexp_opt_fm, only: regexp_opt
    implicit none(type, external)
    private

    public :: syms_of_css_mode, css_base_mode, css_mode, scss_mode, css_value_class_lookup, css_property_values, &
              css_property_value_cache_get, css_foreign_completions, css_completion_at_point, css_color_to_4_dpc, &
              css_format_hex, css_named_color_to_hex, css_format_rgba_alpha, css_hex_to_rgb, &
              css_rgb_to_named_color_or_hex, css_cycle_color_format, css_join_nested_selectors, &
              css_prev_index_position, css_extract_index_name, css_current_defun_name, css_mdn_find_symbol, &
              css_rgb_color, css_hsl_color, css_hex_color, css_hex_alpha, css_named_color, css_compute_color, &
              css_fontify_region, css_smie_forward_token, css_smie_backward_token, css_smie_rules, css_fill_paragraph, &
              css_adaptive_fill, scss_smie_not_interpolation_p, css_font_lock_keywords_of, scss_font_lock_keywords
    public :: css_list_proc

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    abstract interface
        !> A `css_class_list_function' or `css_id_list_function': the class
        !> names or IDs the current buffer suggests.
        subroutine css_list_proc(names)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: names(:)
        end subroutine css_list_proc
    end interface

    ! One buffer's own value of a `css_list_function_type'.
    type :: css_list_entry_type
        integer(int64) :: serial = 0
        procedure(css_list_proc), pointer, nopass :: fn => null()
    end type css_list_entry_type

    !> The type of `css_class_list_function' and `css_id_list_function': a
    !> default `value' (assign with `=>'; null is `ignore') and buffers' own
    !> values (`set_local').
    type, public :: css_list_function_type
        procedure(css_list_proc), pointer, nopass :: value => null()
        type(css_list_entry_type), allocatable :: entries(:)
    contains
        !> Make FN the current buffer's function (setq-local).
        procedure :: set_local => list_function_set_local
        !> The current buffer's function (null for `ignore').
        procedure :: get => list_function_get
        !> Make the current buffer use the default again.
        procedure :: kill_local => list_function_kill_local
    end type css_list_function_type

    !> Called to provide completions of class names.
    !> This can be bound by buffers that are able to suggest class name
    !> completions, such as HTML mode buffers.  Port of css-mode.el
    !> css-class-list-function.
    type(css_list_function_type), public :: css_class_list_function

    !> Called to provide completions of IDs.
    !> This can be bound by buffers that are able to suggest ID
    !> completions, such as HTML mode buffers.  Port of css-mode.el
    !> css-id-list-function.
    type(css_list_function_type), public :: css_id_list_function

    !> Self inserting keys which should trigger re-indentation.  Port of
    !> css-mode.el css-electric-keys (characters).
    integer, allocatable, public :: css_electric_keys(:)

    !> Whether CSS colors should be fontified using the color as the
    !> background.  When true, a text representing CSS color will be
    !> fontified such that its background is the color itself.  E.g.,
    !> #ff0000 will be fontified with a red background.  Port of
    !> css-mode.el css-fontify-colors.
    type(logical_setting), target, public :: css_fontify_colors = logical_setting(name="css_fontify_colors", value=.true.)

    !> Basic size of one indentation step.  Port of css-mode.el
    !> css-indent-offset.
    type(integer_setting), target, public :: css_indent_offset = integer_setting(name="css_indent_offset", value=4)

    !> Format for a URL where CSS documentation can be found.
    !> The format should include a single "%s" substitution.
    !> The name of the CSS property, @-id, pseudo-class, or pseudo-element
    !> to look up will be substituted there.  Port of css-mode.el
    !> css-lookup-url-format.
    type(string_setting), target, public :: css_lookup_url_format = string_setting(name="css_lookup_url_format")

    !> Face to use for selectors.  Port of css-mode.el:css-selector.
    type(named_face_type), target, public :: css_selector
    !> Face to use for properties.  Port of css-mode.el:css-property.
    type(named_face_type), target, public :: css_property
    !> Face to use for vendor-specific properties.  Port of
    !> css-mode.el:css-proprietary-property.
    type(named_face_type), target, public :: css_proprietary_property

    !> Hook run after entering `css_base_mode'.  Port of the
    !> css-base-mode-hook define-derived-mode defines.
    type(hook_type), target, public :: css_base_mode_hook = hook_type(name="css_base_mode_hook")
    !> Hook run after entering `css_mode'.  Port of the css-mode-hook
    !> define-derived-mode defines.
    type(hook_type), target, public :: css_mode_hook = hook_type(name="css_mode_hook")
    !> Hook run after entering `scss_mode'.  Port of the scss-mode-hook
    !> define-derived-mode defines.
    type(hook_type), target, public :: scss_mode_hook = hook_type(name="scss_mode_hook")

    !> Keymap used in `css_mode'.  Port of css-mode.el css-mode-map.
    type(handle_type), public :: css_mode_map
    !> Keymap for `css_base_mode'.
    type(handle_type), public :: css_base_mode_map
    !> Keymap for `scss_mode'.
    type(handle_type), public :: scss_mode_map
    !> Syntax table used in `css_mode'.  Port of css-mode.el
    !> css-mode-syntax-table.
    type(handle_type), public :: css_mode_syntax_table
    !> Syntax table used in `scss_mode'.  Port of css-mode.el
    !> scss-mode-syntax-table.
    type(handle_type), public :: scss_mode_syntax_table

    !> The Font Lock keywords of `css_mode'.  Port of css-mode.el
    !> css-font-lock-keywords.
    type(font_lock_keyword_type), allocatable, public :: css_font_lock_keywords(:)

    !> The SMIE grammar of CSS mode.  Port of css-mode.el css-smie-grammar.
    type(smie_grammar_type), target, public :: css_smie_grammar

    ! One buffer's css--at-ids, css--bang-ids (true: SCSS's too) and
    ! css--nested-selectors-allowed.
    type :: css_local_type
        integer(int64) :: serial = 0
        logical :: scss = .false.
    end type css_local_type

    type(css_local_type), allocatable :: locals(:)

    ! css--property-value-cache: the values computed for each property.
    type :: value_cache_type
        character(len=36) :: property = ""
        character(len=36), allocatable :: values(:)
    end type value_cache_type

    type(value_cache_type), allocatable :: value_cache(:)

    ! css-syntax-propertize-function.
    type(syntax_propertize_rules_type) :: propertize_rules

    ! The tables of the completion `css_completion_at_point' returns: the
    ! properties (if `merge_prop'), and the selectors after MERGE_SEL_CHAR
    ! (if `merge_sel').
    logical :: merge_prop = .false., merge_sel = .false.
    integer :: merge_sel_char = 0

    !> Identifiers for pseudo-classes.  Port of css-mode.el css-pseudo-class-ids.
    character(len=*), parameter, public :: css_pseudo_class_ids(39) = [character(len=20) :: &
                                           "active", "checked", "default", "disabled", "empty", "enabled", "first", "first-child", &
                                 "first-of-type", "focus", "focus-within", "has", "hover", "in-range", "indeterminate", "invalid", &
                                       "lang", "last-child", "last-of-type", "left", "link", "not", "nth-child", "nth-last-child", &
                                      "nth-last-of-type", "nth-of-type", "only-child", "only-of-type", "optional", "out-of-range", &
                                      "read-only", "read-write", "required", "right", "root", "scope", "target", "valid", "visited"]
    !> Identifiers for pseudo-elements.  Port of css-mode.el
    !> css-pseudo-element-ids.
    character(len=*), parameter, public :: css_pseudo_element_ids(5) = [character(len=20) :: &
                                                                       "after", "before", "first-letter", "first-line", "selection"]
    !> Identifiers that appear in the form @foo.  Port of css-mode.el
    !> css-at-ids.
    character(len=*), parameter, public :: css_at_ids(8) = [character(len=20) :: &
                                            "charset", "font-face", "import", "keyframes", "media", "namespace", "page", "supports"]
    !> Additional identifiers that appear in the form @foo in SCSS.  Port of
    !> css-mode.el scss-at-ids.
    character(len=*), parameter, public :: scss_at_ids(18) = [character(len=20) :: &
                                   "at-root", "content", "debug", "each", "else", "else if", "error", "extend", "for", "function", &
                                                              "if", "import", "include", "mixin", "return", "use", "warn", "while"]
    !> Identifiers that appear in the form !foo.  Port of css-mode.el
    !> css-bang-ids.
    character(len=*), parameter, public :: css_bang_ids(1) = [character(len=20) :: &
                                                              "important"]
    !> Additional identifiers that appear in the form !foo in SCSS.  Port of
    !> css-mode.el scss-bang-ids.
    character(len=*), parameter, public :: scss_bang_ids(3) = [character(len=20) :: &
                                                               "default", "global", "optional"]
    !> Identifiers for font descriptors.  Port of css-mode.el
    !> css-descriptor-ids.
    character(len=*), parameter, public :: css_descriptor_ids(24) = [character(len=20) :: &
                                            "ascent", "baseline", "bbox", "cap-height", "centerline", "definition-src", "descent", &
                                          "font-family", "font-size", "font-stretch", "font-style", "font-variant", "font-weight", &
                                             "mathline", "panose-1", "slope", "src", "stemh", "stemv", "topline", "unicode-range", &
                                                                     "units-per-em", "widths", "x-height"]
    !> Identifiers for types of media.  Port of css-mode.el css-media-ids.
    character(len=*), parameter, public :: css_media_ids(9) = [character(len=20) :: &
                                             "all", "aural", "bitmap", "continuous", "grid", "paged", "static", "tactile", "visual"]
    !> Identifiers for properties.  Port of css-mode.el css-property-ids.
    character(len=*), parameter, public :: css_property_ids(277) = [character(len=36) :: &
                                     "azimuth", "border-collapse", "border-spacing", "bottom", "caption-side", "clear", "content", &
                                   "counter-increment", "counter-reset", "cue", "cue-after", "cue-before", "display", "elevation", &
                                        "empty-cells", "float", "height", "left", "line-height", "list-style", "list-style-image", &
                               "list-style-position", "list-style-type", "margin", "margin-bottom", "margin-left", "margin-right", &
                                  "margin-top", "max-height", "max-width", "min-height", "min-width", "padding", "padding-bottom", &
                                          "padding-left", "padding-right", "padding-top", "page-break-after", "page-break-before", &
                               "page-break-inside", "pause", "pause-after", "pause-before", "pitch", "pitch-range", "play-during", &
                                              "position", "quotes", "richness", "right", "speak", "speak-header", "speak-numeral", &
                                            "speak-punctuation", "speech-rate", "stress", "table-layout", "top", "vertical-align", &
                                       "visibility", "voice-family", "volume", "width", "z-index", "animation", "animation-delay", &
                                  "animation-direction", "animation-duration", "animation-fill-mode", "animation-iteration-count", &
                                              "animation-name", "animation-play-state", "animation-timing-function", "background", &
                                               "background-attachment", "background-clip", "background-color", "background-image", &
                                     "background-origin", "background-position", "background-repeat", "background-size", "border", &
                                "border-bottom", "border-bottom-color", "border-bottom-left-radius", "border-bottom-right-radius", &
                                                     "border-bottom-style", "border-bottom-width", "border-color", "border-image", &
                                        "border-image-outset", "border-image-repeat", "border-image-slice", "border-image-source", &
                               "border-image-width", "border-left", "border-left-color", "border-left-style", "border-left-width", &
                                "border-radius", "border-right", "border-right-color", "border-right-style", "border-right-width", &
                                                       "border-style", "border-top", "border-top-color", "border-top-left-radius", &
                                  "border-top-right-radius", "border-top-style", "border-top-width", "border-width", "box-shadow", &
                                  "box-sizing", "caret-color", "cursor", "nav-down", "nav-left", "nav-right", "nav-up", "outline", &
                                   "outline-color", "outline-offset", "outline-style", "outline-width", "resize", "text-overflow", &
                                                  "all", "color", "opacity", "contain", "content-visibility", "grid", "grid-area", &
                                        "grid-auto-columns", "grid-auto-flow", "grid-auto-rows", "grid-column", "grid-column-end", &
                                   "grid-column-gap", "grid-column-start", "grid-gap", "grid-row", "grid-row-end", "grid-row-gap", &
                                                "grid-row-start", "grid-template", "grid-template-areas", "grid-template-columns", &
                                          "grid-template-rows", "align-content", "align-items", "align-self", "column-gap", "gap", &
                                 "justify-content", "justify-items", "justify-self", "place-content", "place-items", "place-self", &
                                       "row-gap", "flex", "flex-basis", "flex-direction", "flex-flow", "flex-grow", "flex-shrink", &
                                             "flex-wrap", "order", "font", "font-family", "font-feature-settings", "font-kerning", &
                                          "font-language-override", "font-size", "font-size-adjust", "font-stretch", "font-style", &
                                                 "font-synthesis", "font-variant", "font-variant-alternates", "font-variant-caps", &
                                                      "font-variant-east-asian", "font-variant-ligatures", "font-variant-numeric", &
                                    "font-variant-position", "font-weight", "box-decoration-break", "break-after", "break-before", &
                                         "break-inside", "orphans", "widows", "clip-path", "clip-rule", "mask-image", "mask-mode", &
                                "mask-repeat", "mask-position", "mask-clip", "mask-origin", "mask-size", "mask-composite", "mask", &
                                               "mask-border-source", "mask-border-mode", "mask-border-slice", "mask-border-width", &
                                   "mask-border-outset", "mask-border-repeat", "mask-border", "mask-type", "clip", "column-count", &
                                      "column-fill", "column-rule", "column-rule-color", "column-rule-style", "column-rule-width", &
                                    "column-span", "column-width", "columns", "max-lines", "overflow", "overflow-x", "overflow-y", &
                                       "text-decoration", "text-decoration-color", "text-decoration-line", "text-decoration-skip", &
                                        "text-decoration-style", "text-emphasis", "text-emphasis-color", "text-emphasis-position", &
                                "text-emphasis-style", "text-shadow", "text-underline-position", "hanging-punctuation", "hyphens", &
                                     "letter-spacing", "line-break", "overflow-wrap", "tab-size", "text-align", "text-align-last", &
                                     "text-indent", "text-justify", "text-transform", "white-space", "word-break", "word-spacing", &
                                             "word-wrap", "backface-visibility", "perspective", "perspective-origin", "transform", &
                                   "transform-origin", "transform-style", "transition", "transition-delay", "transition-duration", &
                                                  "transition-property", "transition-timing-function", "will-change", "direction", &
                                                       "text-combine-upright", "text-orientation", "unicode-bidi", "writing-mode", &
                                        "color-interpolation-filters", "filter", "flood-color", "flood-opacity", "lighting-color", &
                                                                    "touch-action"]
    ! css-property-alist: the properties (css_property_ids) and their
    ! possible values, PROPERTY_VALUES(PROPERTY_FIRST(i):PROPERTY_FIRST(i+1)-1)
    ! for property i.  String values represent literal values, while symbols
    ! (PROPERTY_VALUE_SYMBOL) represent one of the value classes found in
    ! css-value-class-alist, or else a reference back to one of the
    ! properties.
    integer, parameter :: PROPERTY_FIRST(278) = [ &
                          1, 14, 16, 17, 20, 22, 26, 36, 39, 42, 44, 46, 48, 68, 74, 76, 79, 82, 85, 89, 92, 94, 96, 111, &
                          112, 113, 114, 115, 116, 119, 122, 124, 126, 127, 128, 129, 130, 131, 136, 141, 143, 145, 147, 149, &
                          155, 156, 161, 165, 167, 168, 171, 174, 176, 178, 180, 188, 189, 191, 194, 204, 207, 211, 219, 222, &
                          224, 231, 232, 233, 234, 235, 236, 237, 238, 239, 241, 242, 243, 244, 245, 246, 247, 248, 249, 252, &
                          255, 256, 258, 260, 261, 262, 263, 268, 270, 274, 277, 279, 283, 286, 287, 288, 289, 291, 294, 295, &
                          296, 297, 298, 301, 302, 304, 306, 307, 308, 309, 311, 313, 315, 354, 359, 364, 369, 374, 377, 379, &
                          380, 382, 383, 387, 390, 390, 391, 392, 399, 402, 409, 410, 411, 414, 415, 416, 417, 418, 419, 421, &
                          422, 423, 424, 425, 433, 435, 439, 443, 447, 452, 458, 460, 462, 468, 477, 485, 487, 489, 491, 493, &
                          497, 500, 504, 506, 507, 508, 511, 512, 525, 527, 529, 532, 534, 538, 540, 549, 552, 555, 582, 590, &
                          597, 601, 607, 613, 616, 629, 631, 643, 655, 660, 661, 662, 666, 668, 669, 670, 671, 672, 674, 675, &
                          676, 677, 678, 680, 682, 685, 689, 691, 695, 701, 703, 705, 707, 710, 713, 714, 715, 716, 718, 720, &
                          722, 724, 733, 742, 751, 754, 755, 760, 766, 771, 773, 774, 778, 787, 790, 794, 799, 802, 804, 808, &
                          810, 812, 819, 826, 828, 832, 837, 842, 845, 848, 850, 852, 854, 861, 863, 870, 872, 873, 874, 875, &
                          878, 879, 881, 883, 885, 888, 894, 897, 900, 902, 903, 905, 906, 911]
    character(len=*), parameter :: PROPERTY_VALUES(910) = [character(len=36) :: &
                                       "angle", "left-side", "far-left", "left", "center-left", "center", "center-right", "right", &
                                 "far-right", "right-side", "behind", "leftwards", "rightwards", "collapse", "separate", "length", &
                               "length", "percentage", "auto", "top", "bottom", "none", "left", "right", "both", "normal", "none", &
                                               "string", "uri", "counter", "attr()", "open-quote", "close-quote", "no-open-quote", &
                                 "no-close-quote", "identifier", "integer", "none", "identifier", "integer", "none", "cue-before", &
                               "cue-after", "uri", "none", "uri", "none", "inline", "block", "list-item", "inline-block", "table", &
                                       "inline-table", "table-row-group", "table-header-group", "table-footer-group", "table-row", &
                               "table-column-group", "table-column", "table-cell", "table-caption", "none", "flex", "inline-flex", &
                                  "grid", "inline-grid", "subgrid", "angle", "below", "level", "above", "higher", "lower", "show", &
                                  "hide", "left", "right", "none", "length", "percentage", "auto", "length", "percentage", "auto", &
                                             "normal", "number", "length", "percentage", "list-style-type", "list-style-position", &
                                    "list-style-image", "uri", "none", "inside", "outside", "disc", "circle", "square", "decimal", &
                                "decimal-leading-zero", "lower-roman", "upper-roman", "lower-greek", "lower-latin", "upper-latin", &
                                     "armenian", "georgian", "lower-alpha", "upper-alpha", "none", "margin-width", "margin-width", &
                                         "margin-width", "margin-width", "margin-width", "length", "percentage", "none", "length", &
                                            "percentage", "none", "length", "percentage", "length", "percentage", "padding-width", &
                                    "padding-width", "padding-width", "padding-width", "padding-width", "auto", "always", "avoid", &
                               "left", "right", "auto", "always", "avoid", "left", "right", "avoid", "auto", "time", "percentage", &
                                        "time", "percentage", "time", "percentage", "frequency", "x-low", "low", "medium", "high", &
                                     "x-high", "number", "uri", "mix", "repeat", "auto", "none", "static", "relative", "absolute", &
                               "fixed", "string", "none", "number", "length", "percentage", "auto", "normal", "none", "spell-out", &
                                   "once", "always", "digits", "continuous", "code", "none", "number", "x-slow", "slow", "medium", &
                                  "fast", "x-fast", "faster", "slower", "number", "auto", "fixed", "length", "percentage", "auto", &
                                   "baseline", "sub", "super", "top", "text-top", "middle", "bottom", "text-bottom", "percentage", &
                                   "length", "visible", "hidden", "collapse", "specific-voice", "generic-voice", "specific-voice", &
                                  "generic-voice", "number", "percentage", "silent", "x-soft", "soft", "medium", "loud", "x-loud", &
                                               "length", "percentage", "auto", "auto", "integer", "single-animation-name", "time", &
                                       "single-timing-function", "single-animation-iteration-count", "single-animation-direction", &
                                "single-animation-fill-mode", "single-animation-play-state", "time", "single-animation-direction", &
                                "time", "single-animation-fill-mode", "single-animation-iteration-count", "single-animation-name", &
                                            "single-animation-play-state", "single-timing-function", "bg-layer", "final-bg-layer", &
                                           "attachment", "box", "color", "bg-image", "box", "position", "repeat-style", "bg-size", &
                                      "line-width", "line-style", "color", "line-width", "line-style", "color", "color", "length", &
                                 "percentage", "length", "percentage", "line-style", "line-width", "color", "border-image-source", &
                               "border-image-slice", "border-image-width", "border-image-outset", "border-image-repeat", "length", &
                                 "number", "stretch", "repeat", "round", "space", "number", "percentage", "fill", "none", "image", &
                                           "length", "percentage", "number", "auto", "line-width", "line-style", "color", "color", &
                                 "line-style", "line-width", "length", "percentage", "line-width", "line-style", "color", "color", &
                                 "line-style", "line-width", "line-style", "line-width", "line-style", "color", "color", "length", &
                                 "percentage", "length", "percentage", "line-style", "line-width", "line-width", "none", "shadow", &
                                         "content-box", "border-box", "auto", "color", "uri", "x", "y", "auto", "default", "none", &
                                               "context-menu", "help", "pointer", "progress", "wait", "cell", "crosshair", "text", &
                               "vertical-text", "alias", "copy", "move", "no-drop", "not-allowed", "grab", "grabbing", "e-resize", &
                                           "n-resize", "ne-resize", "nw-resize", "s-resize", "se-resize", "sw-resize", "w-resize", &
                                 "ew-resize", "ns-resize", "nesw-resize", "nwse-resize", "col-resize", "row-resize", "all-scroll", &
                                   "zoom-in", "zoom-out", "auto", "id", "current", "root", "target-name", "auto", "id", "current", &
                                   "root", "target-name", "auto", "id", "current", "root", "target-name", "auto", "id", "current", &
                                      "root", "target-name", "outline-color", "outline-style", "outline-width", "color", "invert", &
                               "length", "auto", "border-style", "border-width", "none", "both", "horizontal", "vertical", "clip", &
                                       "ellipsis", "string", "color", "alphavalue", "none", "strict", "content", "size", "layout", &
                                "style", "paint", "visible", "auto", "hidden", "grid-template", "grid-template-rows", "auto-flow", &
                               "dense", "grid-auto-columns", "grid-auto-rows", "grid-template-columns", "grid-line", "track-size", &
                               "row", "column", "dense", "track-size", "grid-line", "grid-line", "length-percentage", "grid-line", &
                                    "grid-row-gap", "grid-column-gap", "grid-line", "grid-line", "length-percentage", "grid-line", &
                                      "none", "grid-template-rows", "grid-template-columns", "line-names", "string", "track-size", &
                                   "line-names", "explicit-track-list", "none", "string", "none", "track-list", "auto-track-list", &
                                               "subgrid", "none", "track-list", "auto-track-list", "subgrid", "baseline-position", &
                                             "content-distribution", "overflow-position", "content-position", "normal", "stretch", &
                                           "baseline-position", "overflow-position", "self-position", "auto", "normal", "stretch", &
                                         "baseline-position", "overflow-position", "self-position", "normal", "length-percentage", &
                               "row-gap", "column-gap", "normal", "content-distribution", "overflow-position", "content-position", &
                                  "left", "right", "normal", "stretch", "baseline-position", "overflow-position", "self-position", &
                                            "left", "right", "legacy", "center", "auto", "normal", "stretch", "baseline-position", &
                                        "overflow-position", "self-position", "left", "right", "align-content", "justify-content", &
                                      "align-items", "justify-items", "justify-self", "align-self", "normal", "length-percentage", &
                               "none", "flex-grow", "flex-shrink", "flex-basis", "auto", "content", "width", "row", "row-reverse", &
                                  "column", "column-reverse", "flex-direction", "flex-wrap", "number", "number", "nowrap", "wrap", &
                                     "wrap-reverse", "integer", "font-style", "font-variant-css21", "font-weight", "font-stretch", &
                                              "font-size", "line-height", "font-family", "caption", "icon", "menu", "message-box", &
                                    "small-caption", "status-bar", "family-name", "generic-family", "normal", "feature-tag-value", &
                                         "auto", "normal", "none", "normal", "string", "absolute-size", "relative-size", "length", &
                                      "percentage", "none", "number", "normal", "ultra-condensed", "extra-condensed", "condensed", &
                                      "semi-condensed", "semi-expanded", "expanded", "extra-expanded", "ultra-expanded", "normal", &
                                            "italic", "oblique", "none", "weight", "style", "normal", "none", "common-lig-values", &
                                      "discretionary-lig-values", "historical-lig-values", "contextual-alt-values", "stylistic()", &
                                "historical-forms", "styleset()", "character-variant()", "swash()", "ornaments()", "annotation()", &
                                      "small-caps", "all-small-caps", "petite-caps", "all-petite-caps", "unicase", "titling-caps", &
                                          "numeric-figure-values", "numeric-spacing-values", "numeric-fraction-values", "ordinal", &
                                         "slashed-zero", "east-asian-variant-values", "east-asian-width-values", "ruby", "normal", &
                                 "stylistic()", "historical-forms", "styleset()", "character-variant()", "swash()", "ornaments()", &
                                       "annotation()", "normal", "small-caps", "all-small-caps", "petite-caps", "all-petite-caps", &
                                      "unicase", "titling-caps", "normal", "east-asian-variant-values", "east-asian-width-values", &
                               "ruby", "normal", "none", "common-lig-values", "discretionary-lig-values", "historical-lig-values", &
                                             "contextual-alt-values", "normal", "numeric-figure-values", "numeric-spacing-values", &
                                 "numeric-fraction-values", "ordinal", "slashed-zero", "normal", "sub", "super", "normal", "bold", &
                                      "bolder", "lighter", "100", "200", "300", "400", "500", "600", "700", "800", "900", "slice", &
                                "clone", "auto", "avoid", "avoid-page", "page", "left", "right", "recto", "verso", "avoid-column", &
                                       "column", "avoid-region", "region", "auto", "avoid", "avoid-page", "page", "left", "right", &
                                            "recto", "verso", "avoid-column", "column", "avoid-region", "region", "auto", "avoid", &
                                 "avoid-page", "avoid-column", "avoid-region", "integer", "integer", "clip-source", "basic-shape", &
                                   "geometry-box", "none", "nonzero", "evenodd", "mask-reference", "masking-mode", "repeat-style", &
                                         "position", "geometry-box", "no-clip", "geometry-box", "bg-size", "compositing-operator", &
                                    "mask-layer", "none", "image", "luminance", "alpha", "number", "percentage", "fill", "length", &
                                        "percentage", "number", "auto", "length", "number", "stretch", "repeat", "round", "space", &
                                             "mask-border-source", "mask-border-slice", "mask-border-width", "mask-border-outset", &
                                         "mask-border-repeat", "mask-border-mode", "luminance", "alpha", "rect()", "auto", "auto", &
                                            "integer", "auto", "balance", "balance-all", "column-rule-width", "column-rule-style", &
                                        "column-rule-color", "color", "line-style", "line-width", "none", "all", "auto", "length", &
                                         "column-width", "column-count", "none", "integer", "visible", "hidden", "scroll", "auto", &
                                   "paged-x", "paged-y", "paged-x-controls", "paged-y-controls", "fragments", "visible", "hidden", &
                                      "scroll", "auto", "paged-x", "paged-y", "paged-x-controls", "paged-y-controls", "fragments", &
                                                  "visible", "hidden", "scroll", "auto", "paged-x", "paged-y", "paged-x-controls", &
                                                 "paged-y-controls", "fragments", "text-decoration-line", "text-decoration-style", &
                               "text-decoration-color", "color", "none", "underline", "overline", "line-through", "blink", "none", &
                                     "objects", "spaces", "ink", "edges", "box-decoration", "solid", "double", "dotted", "dashed", &
                                  "wavy", "text-emphasis-style", "text-emphasis-color", "color", "over", "under", "right", "left", &
                               "none", "filled", "open", "dot", "circle", "double-circle", "triangle", "sesame", "string", "none", &
                                   "length", "color", "auto", "under", "left", "right", "none", "first", "force-end", "allow-end", &
                                        "last", "none", "manual", "auto", "normal", "length", "auto", "loose", "normal", "strict", &
                                "normal", "break-word", "integer", "length", "start", "end", "left", "right", "center", "justify", &
                                           "match-parent", "auto", "start", "end", "left", "right", "center", "justify", "length", &
                                      "percentage", "auto", "none", "inter-word", "distribute", "none", "capitalize", "uppercase", &
                               "lowercase", "full-width", "normal", "pre", "nowrap", "pre-wrap", "pre-line", "normal", "keep-all", &
                               "break-all", "normal", "length", "percentage", "normal", "break-word", "visible", "hidden", "none", &
                                             "length", "left", "center", "right", "top", "bottom", "percentage", "length", "none", &
                                     "transform-list", "left", "center", "right", "top", "bottom", "percentage", "length", "flat", &
                                  "preserve-3d", "single-transition", "time", "time", "none", "single-transition-property", "all", &
                                  "single-transition-timing-function", "auto", "animateable-feature", "ltr", "rtl", "none", "all", &
                                "mixed", "upright", "sideways", "normal", "embed", "isolate", "bidi-override", "isolate-override", &
                                  "plaintext", "horizontal-tb", "vertical-rl", "vertical-lr", "auto", "sRGB", "linearRGB", "none", &
                               "filter-function-list", "color", "number", "percentage", "color", "auto", "none", "pan-x", "pan-y", &
                                                           "manipulation"]
    logical, parameter :: PROPERTY_VALUE_SYMBOL(910) = [ &
                          .true., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .true., .true., .true., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .true., .true., .true., .false., .false., .false., &
                          .false., .false., .true., .true., .false., .true., .true., .false., .true., .true., .true., &
                          .false., .true., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .true., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .true., .true., .false., .true., .true., .false., .false., .true., .true., .true., .true., &
                          .true., .true., .true., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .true., &
                          .true., .true., .true., .true., .true., .true., .false., .true., .true., .false., .true., .true., &
                          .true., .true., .true., .true., .true., .true., .true., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .true., .true., .true., &
                          .true., .true., .true., .true., .false., .false., .false., .false., .false., .true., .true., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .true., .false., .true., &
                          .true., .true., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .true., .false., .false., .false., .false., .false., .false., .false., .true., .false., &
                          .false., .true., .true., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .true., .true., .false., .false., .false., .true., .true., .true., .true., .true., .true., &
                          .false., .false., .false., .false., .false., .false., .true., .true., .false., .false., .true., &
                          .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                          .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                          .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                          .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .false., .false., &
                          .false., .false., .true., .true., .false., .false., .true., .true., .true., .true., .false., &
                          .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                          .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., .true., &
                          .true., .true., .false., .true., .false., .false., .false., .true., .true., .true., .true., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .true., .false., .false., .true., .false., .true., .false., &
                          .false., .true., .false., .true., .false., .false., .true., .false., .true., .false., .false., &
                          .true., .true., .true., .true., .true., .false., .true., .false., .true., .true., .false., .false., &
                          .false., .false., .false., .false., .true., .true., .true., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .true., .true., .false., .false., .true., &
                          .true., .true., .true., .true., .false., .false., .false., .true., .true., .true., .true., .true., &
                          .true., .true., .true., .true., .true., .true., .false., .true., .true., .true., .true., .true., &
                          .true., .true., .false., .true., .false., .true., .true., .false., .false., .true., .true., &
                          .false., .true., .true., .true., .true., .false., .false., .true., .true., .true., .false., &
                          .false., .false., .true., .true., .true., .false., .true., .true., .true., .false., .true., .true., &
                          .true., .false., .false., .false., .false., .true., .true., .true., .false., .false., .false., &
                          .false., .false., .false., .false., .true., .true., .true., .false., .false., .true., .true., &
                          .true., .true., .true., .true., .false., .true., .false., .true., .true., .true., .false., .false., &
                          .true., .false., .false., .false., .false., .true., .true., .true., .true., .false., .false., &
                          .false., .true., .true., .true., .true., .true., .true., .true., .true., .false., .false., .false., &
                          .false., .false., .false., .true., .true., .false., .true., .false., .false., .false., .false., &
                          .true., .true., .true., .true., .true., .false., .true., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .true., .true., .true., .true., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .true., .true., .true., &
                          .false., .false., .true., .true., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .true., &
                          .true., .false., .false., .false., .true., .true., .true., .true., .false., .true., .true., .true., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .true., .true., .true., .true., .true., .false., &
                          .false., .false., .true., .true., .true., .true., .true., .false., .true., .true., .true., .true., &
                          .false., .true., .false., .false., .true., .true., .false., .true., .true., .true., .false., &
                          .true., .true., .false., .false., .false., .false., .true., .true., .true., .true., .true., .true., &
                          .false., .false., .false., .false., .false., .true., .false., .false., .false., .true., .true., &
                          .true., .true., .true., .true., .false., .false., .false., .true., .true., .true., .false., .true., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .true., .true., .true., .true., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .true., .true., .true., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .true., .false., .true., .true., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .true., .false., .false., .false., .false., .false., .false., .true., .true., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .true., .true., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .true., .true., .false., .false., .false., .false., .false., .true., .false., .false., &
                          .false., .false., .false., .true., .true., .false., .true., .false., .false., .false., .false., &
                          .false., .true., .true., .false., .false., .true., .true., .true., .false., .true., .false., &
                          .true., .false., .true., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .true., .true., .true., .true., .true., .false., .false., .false., .false., .false.]
    ! css-value-class-alist: property value classes and their values, in
    ! the same form; symbols always refer to other classes.
    character(len=*), parameter :: CLASS_NAMES(93) = [character(len=36) :: &
                                       "absolute-size", "alphavalue", "angle", "animateable-feature", "attachment", "auto-repeat", &
                               "auto-track-list", "basic-shape", "bg-image", "bg-layer", "bg-size", "box", "clip-source", "color", &
                                                  "common-lig-values", "compositing-operator", "contextual-alt-values", "counter", &
                                               "discretionary-lig-values", "east-asian-variant-values", "east-asian-width-values", &
                                                     "explicit-track-list", "family-name", "feature-tag-value", "filter-function", &
                                          "filter-function-list", "final-bg-layer", "fixed-breadth", "fixed-repeat", "fixed-size", &
                                 "font-variant-css21", "frequency", "generic-family", "generic-voice", "geometry-box", "gradient", &
                                     "grid-line", "historical-lig-values", "image", "image-list", "inflexible-breadth", "integer", &
                                             "length", "line-height", "line-names", "line-style", "line-width", "linear-gradient", &
                                     "margin-width", "mask-layer", "mask-reference", "mask-source", "masking-mode", "named-color", &
                                           "number", "numeric-figure-values", "numeric-fraction-values", "numeric-spacing-values", &
                                    "padding-width", "position", "baseline-position", "content-distribution", "overflow-position", &
                                          "content-position", "self-position", "radial-gradient", "relative-size", "repeat-style", &
                                                  "repeating-linear-gradient", "repeating-radial-gradient", "shadow", "shape-box", &
                                   "single-animation-direction", "single-animation-fill-mode", "single-animation-iteration-count", &
                                                 "single-animation-name", "single-animation-play-state", "single-timing-function", &
                                           "single-transition", "single-transition-property", "single-transition-timing-function", &
                                           "specific-voice", "target-name", "time", "track-breadth", "track-list", "track-repeat", &
                                                      "track-size", "transform-list", "uri", "width", "x", "y"]
    integer, parameter :: CLASS_FIRST(94) = [ &
                          1, 8, 9, 10, 13, 16, 17, 21, 25, 27, 32, 37, 40, 41, 48, 50, 54, 56, 58, 60, 66, 68, 70, 73, 77, &
                          87, 89, 95, 96, 97, 99, 101, 102, 107, 110, 114, 118, 122, 124, 128, 129, 133, 134, 136, 140, 141, &
                          151, 155, 156, 159, 167, 170, 171, 174, 322, 323, 325, 327, 329, 331, 338, 341, 345, 347, 352, 359, &
                          360, 362, 368, 369, 370, 373, 375, 379, 383, 385, 387, 389, 390, 394, 396, 405, 406, 407, 408, 413, &
                          416, 417, 420, 441, 442, 445, 446, 447]
    character(len=*), parameter :: CLASS_VALUES(446) = [character(len=36) :: &
                                     "xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large", "number", "calc()", &
                                            "scroll-position", "contents", "custom-ident", "scroll", "fixed", "local", "repeat()", &
                                    "line-names", "fixed-size", "fixed-repeat", "auto-repeat", "inset()", "circle()", "ellipse()", &
                                        "polygon()", "image", "none", "bg-image", "position", "repeat-style", "attachment", "box", &
                                   "length", "percentage", "auto", "cover", "contain", "border-box", "padding-box", "content-box", &
                                        "uri", "rgb()", "rgba()", "hsl()", "hsla()", "named-color", "transparent", "currentColor", &
                               "common-ligatures", "no-common-ligatures", "add", "subtract", "intersect", "exclude", "contextual", &
                                                        "no-contextual", "counter()", "counters()", "discretionary-ligatures", &
                                    "no-discretionary-ligatures", "jis78", "jis83", "jis90", "jis04", "simplified", "traditional", &
                                  "full-width", "proportional-width", "line-names", "track-size", "Courier", "Helvetica", "Times", &
                                        "string", "integer", "on", "off", "blur()", "brightness()", "contrast()", "drop-shadow()", &
                               "grayscale()", "hue-rotate()", "invert()", "opacity()", "sepia()", "saturate()", "filter-function", &
                                 "uri", "bg-image", "position", "repeat-style", "attachment", "box", "color", "length-percentage", &
                                 "repeat()", "fixed-breadth", "minmax()", "normal", "small-caps", "calc()", "serif", "sans-serif", &
                                            "cursive", "fantasy", "monospace", "male", "female", "child", "shape-box", "fill-box", &
                                      "stroke-box", "view-box", "linear-gradient", "radial-gradient", "repeating-linear-gradient", &
                                   "repeating-radial-gradient", "auto", "custom-ident", "integer", "span", "historical-ligatures", &
                                       "no-historical-ligatures", "uri", "image-list", "element-reference", "gradient", "image()", &
                                "length-percentage", "min-content", "max-content", "auto", "calc()", "calc()", "number", "normal", &
                                  "number", "length", "percentage", "custom-ident", "none", "hidden", "dotted", "dashed", "solid", &
                                              "double", "groove", "ridge", "inset", "outset", "length", "thin", "medium", "thick", &
                                "linear-gradient()", "auto", "length", "percentage", "mask-reference", "masking-mode", "position", &
                                    "bg-size", "repeat-style", "geometry-box", "no-clip", "compositing-operator", "none", "image", &
                                 "mask-source", "uri", "alpha", "luminance", "auto", "black", "silver", "gray", "white", "maroon", &
                                "red", "purple", "fuchsia", "magenta", "green", "lime", "olive", "yellow", "navy", "blue", "teal", &
                                  "aqua", "cyan", "orange", "aliceblue", "antiquewhite", "aquamarine", "azure", "beige", "bisque", &
                                     "blanchedalmond", "blueviolet", "brown", "burlywood", "cadetblue", "chartreuse", "chocolate", &
                                        "coral", "cornflowerblue", "cornsilk", "crimson", "darkblue", "darkcyan", "darkgoldenrod", &
                                  "darkgray", "darkgreen", "darkgrey", "darkkhaki", "darkmagenta", "darkolivegreen", "darkorange", &
                                          "darkorchid", "darkred", "darksalmon", "darkseagreen", "darkslateblue", "darkslategray", &
                                  "darkslategrey", "darkturquoise", "darkviolet", "deeppink", "deepskyblue", "dimgray", "dimgrey", &
                                       "dodgerblue", "firebrick", "floralwhite", "forestgreen", "gainsboro", "ghostwhite", "gold", &
                               "goldenrod", "greenyellow", "grey", "honeydew", "hotpink", "indianred", "indigo", "ivory", "khaki", &
                                 "lavender", "lavenderblush", "lawngreen", "lemonchiffon", "lightblue", "lightcoral", "lightcyan", &
                                       "lightgoldenrodyellow", "lightgray", "lightgreen", "lightgrey", "lightpink", "lightsalmon", &
                                            "lightseagreen", "lightskyblue", "lightslategray", "lightslategrey", "lightsteelblue", &
                                            "lightyellow", "limegreen", "linen", "mediumaquamarine", "mediumblue", "mediumorchid", &
                                      "mediumpurple", "mediumseagreen", "mediumslateblue", "mediumspringgreen", "mediumturquoise", &
                                "mediumvioletred", "midnightblue", "mintcream", "mistyrose", "moccasin", "navajowhite", "oldlace", &
                               "olivedrab", "orangered", "orchid", "palegoldenrod", "palegreen", "paleturquoise", "palevioletred", &
                                        "papayawhip", "peachpuff", "peru", "pink", "plum", "powderblue", "rosybrown", "royalblue", &
                                  "saddlebrown", "salmon", "sandybrown", "seagreen", "seashell", "sienna", "skyblue", "slateblue", &
                                         "slategray", "slategrey", "snow", "springgreen", "steelblue", "tan", "thistle", "tomato", &
                                           "turquoise", "violet", "wheat", "whitesmoke", "yellowgreen", "rebeccapurple", "calc()", &
                                   "lining-nums", "oldstyle-nums", "diagonal-fractions", "stacked-fractions", "proportional-nums", &
                                 "tabular-nums", "length", "percentage", "left", "center", "right", "top", "bottom", "percentage", &
                                "length", "left", "right", "baseline", "space-between", "space-around", "space-evenly", "stretch", &
                                   "unsafe", "safe", "center", "start", "end", "flex-start", "flex-end", "center", "start", "end", &
                                     "self-start", "self-end", "flex-start", "flex-end", "radial-gradient()", "larger", "smaller", &
                                   "repeat-x", "repeat-y", "repeat", "space", "round", "no-repeat", "repeating-linear-gradient()", &
                                         "repeating-radial-gradient()", "inset", "length", "color", "box", "margin-box", "normal", &
                                 "reverse", "alternate", "alternate-reverse", "none", "forwards", "backwards", "both", "infinite", &
                                 "number", "none", "identifier", "running", "paused", "single-transition-timing-function", "none", &
                                   "single-transition-property", "time", "single-transition-timing-function", "all", "identifier", &
                                      "ease", "linear", "ease-in", "ease-out", "ease-in-out", "step-start", "step-end", "steps()", &
                                   "cubic-bezier()", "identifier", "string", "calc()", "length-percentage", "flex", "min-content", &
                                   "max-content", "auto", "line-names", "track-size", "track-repeat", "repeat()", "track-breadth", &
                                "minmax()", "fit-content()", "matrix()", "translate()", "translateX()", "translateY()", "scale()", &
                                "scaleX()", "scaleY()", "rotate()", "skew()", "skewX()", "skewY()", "matrix3d()", "translate3d()", &
                                     "translateZ()", "scale3d()", "scaleZ()", "rotate3d()", "rotateX()", "rotateY()", "rotateZ()", &
                                                       "perspective()", "url()", "length", "percentage", "auto", "number", "number"]
    logical, parameter :: CLASS_VALUE_SYMBOL(446) = [ &
                          .false., .false., .false., .false., .false., .false., .false., .true., .false., .false., .false., &
                          .true., .false., .false., .false., .false., .true., .true., .true., .true., .false., .false., &
                          .false., .false., .true., .false., .true., .true., .true., .true., .true., .true., .true., .false., &
                          .false., .false., .false., .false., .false., .true., .false., .false., .false., .false., .true., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .true., .true., .false., .false., .false., .true., .true., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .true., .true., .true., &
                          .true., .true., .true., .true., .true., .true., .false., .true., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .true., .false., &
                          .false., .false., .true., .true., .true., .true., .false., .true., .true., .false., .false., &
                          .false., .true., .true., .true., .true., .false., .true., .false., .false., .false., .false., &
                          .false., .true., .false., .true., .true., .true., .true., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .true., .false., .false., .false., .false., &
                          .false., .true., .true., .true., .true., .true., .true., .true., .true., .false., .true., .false., &
                          .true., .true., .true., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .true., .true., .false., .false., &
                          .false., .false., .false., .true., .true., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .true., .true., .true., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .true., .false., .true., .false., &
                          .false., .true., .false., .true., .true., .true., .false., .true., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .true., .true., .false., .true., .true., &
                          .false., .false., .false., .true., .true., .true., .false., .true., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., &
                          .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .true., &
                          .true., .false., .true., .true.]
    ! css--color-map: CSS named colors and their hex RGB values.
    character(len=*), parameter :: COLOR_NAMES(148) = [character(len=20) :: &
                                     "black", "silver", "gray", "white", "maroon", "red", "purple", "fuchsia", "magenta", "green", &
                                         "lime", "olive", "yellow", "navy", "blue", "teal", "aqua", "cyan", "orange", "aliceblue", &
                                "antiquewhite", "aquamarine", "azure", "beige", "bisque", "blanchedalmond", "blueviolet", "brown", &
                                       "burlywood", "cadetblue", "chartreuse", "chocolate", "coral", "cornflowerblue", "cornsilk", &
                                          "crimson", "darkblue", "darkcyan", "darkgoldenrod", "darkgray", "darkgreen", "darkgrey", &
                                "darkkhaki", "darkmagenta", "darkolivegreen", "darkorange", "darkorchid", "darkred", "darksalmon", &
                                 "darkseagreen", "darkslateblue", "darkslategray", "darkslategrey", "darkturquoise", "darkviolet", &
                                        "deeppink", "deepskyblue", "dimgray", "dimgrey", "dodgerblue", "firebrick", "floralwhite", &
                                 "forestgreen", "gainsboro", "ghostwhite", "gold", "goldenrod", "greenyellow", "grey", "honeydew", &
                                     "hotpink", "indianred", "indigo", "ivory", "khaki", "lavender", "lavenderblush", "lawngreen", &
                                      "lemonchiffon", "lightblue", "lightcoral", "lightcyan", "lightgoldenrodyellow", "lightgray", &
                                           "lightgreen", "lightgrey", "lightpink", "lightsalmon", "lightseagreen", "lightskyblue", &
                                        "lightslategray", "lightslategrey", "lightsteelblue", "lightyellow", "limegreen", "linen", &
                                               "mediumaquamarine", "mediumblue", "mediumorchid", "mediumpurple", "mediumseagreen", &
                                     "mediumslateblue", "mediumspringgreen", "mediumturquoise", "mediumvioletred", "midnightblue", &
                               "mintcream", "mistyrose", "moccasin", "navajowhite", "oldlace", "olivedrab", "orangered", "orchid", &
                                "palegoldenrod", "palegreen", "paleturquoise", "palevioletred", "papayawhip", "peachpuff", "peru", &
                                    "pink", "plum", "powderblue", "rosybrown", "royalblue", "saddlebrown", "salmon", "sandybrown", &
                                       "seagreen", "seashell", "sienna", "skyblue", "slateblue", "slategray", "slategrey", "snow", &
                                           "springgreen", "steelblue", "tan", "thistle", "tomato", "turquoise", "violet", "wheat", &
                                                       "whitesmoke", "yellowgreen", "rebeccapurple"]
    character(len=*), parameter :: COLOR_HEX(148) = [character(len=7) :: &
                                "#000000", "#c0c0c0", "#808080", "#ffffff", "#800000", "#ff0000", "#800080", "#ff00ff", "#ff00ff", &
                                "#008000", "#00ff00", "#808000", "#ffff00", "#000080", "#0000ff", "#008080", "#00ffff", "#00ffff", &
                                "#ffa500", "#f0f8ff", "#faebd7", "#7fffd4", "#f0ffff", "#f5f5dc", "#ffe4c4", "#ffebcd", "#8a2be2", &
                                "#a52a2a", "#deb887", "#5f9ea0", "#7fff00", "#d2691e", "#ff7f50", "#6495ed", "#fff8dc", "#dc143c", &
                                "#00008b", "#008b8b", "#b8860b", "#a9a9a9", "#006400", "#a9a9a9", "#bdb76b", "#8b008b", "#556b2f", &
                                "#ff8c00", "#9932cc", "#8b0000", "#e9967a", "#8fbc8f", "#483d8b", "#2f4f4f", "#2f4f4f", "#00ced1", &
                                "#9400d3", "#ff1493", "#00bfff", "#696969", "#696969", "#1e90ff", "#b22222", "#fffaf0", "#228b22", &
                                "#dcdcdc", "#f8f8ff", "#ffd700", "#daa520", "#adff2f", "#808080", "#f0fff0", "#ff69b4", "#cd5c5c", &
                                "#4b0082", "#fffff0", "#f0e68c", "#e6e6fa", "#fff0f5", "#7cfc00", "#fffacd", "#add8e6", "#f08080", &
                                "#e0ffff", "#fafad2", "#d3d3d3", "#90ee90", "#d3d3d3", "#ffb6c1", "#ffa07a", "#20b2aa", "#87cefa", &
                                "#778899", "#778899", "#b0c4de", "#ffffe0", "#32cd32", "#faf0e6", "#66cdaa", "#0000cd", "#ba55d3", &
                                "#9370db", "#3cb371", "#7b68ee", "#00fa9a", "#48d1cc", "#c71585", "#191970", "#f5fffa", "#ffe4e1", &
                                "#ffe4b5", "#ffdead", "#fdf5e6", "#6b8e23", "#ff4500", "#da70d6", "#eee8aa", "#98fb98", "#afeeee", &
                                "#db7093", "#ffefd5", "#ffdab9", "#cd853f", "#ffc0cb", "#dda0dd", "#b0e0e6", "#bc8f8f", "#4169e1", &
                                "#8b4513", "#fa8072", "#f4a460", "#2e8b57", "#fff5ee", "#a0522d", "#87ceeb", "#6a5acd", "#708090", &
                                "#708090", "#fffafa", "#00ff7f", "#4682b4", "#d2b48c", "#d8bfd8", "#ff6347", "#40e0d0", "#ee82ee", &
                                                     "#f5deb3", "#f5f5f5", "#9acd32", "#663399"]
    ! css--html-tags: the HTML tags (the names of sgml-mode.el's
    ! html-tag-alist), completed in selectors.
    character(len=*), parameter :: HTML_TAGS(134) = [character(len=10) :: &
                                "a", "area", "base", "col", "colgroup", "dir", "figcaption", "figure", "font", "form", "h1", "h2", &
                                      "h3", "h4", "h5", "h6", "hr", "iframe", "img", "input", "link", "menu", "ol", "p", "select", &
                                   "table", "tbody", "td", "textarea", "tfoot", "th", "thead", "ul", "![", "!attlist", "!doctype", &
                                  "!element", "!entity", "abbr", "acronym", "address", "array", "article", "aside", "au", "audio", &
                                         "b", "bdi", "bdo", "big", "blink", "blockquote", "body", "box", "br", "button", "canvas", &
                                "caption", "center", "cite", "code", "datalist", "dd", "del", "details", "dfn", "div", "dl", "dt", &
                               "em", "embed", "fieldset", "fn", "footer", "frame", "frameset", "head", "header", "hgroup", "html", &
                                     "i", "ins", "isindex", "kbd", "label", "lang", "legend", "li", "main", "map", "mark", "math", &
                                 "meta", "meter", "nav", "nobr", "noframes", "noscript", "object", "optgroup", "option", "output", &
                                     "over", "param", "person", "picture", "pre", "progress", "q", "rev", "rp", "rt", "ruby", "s", &
                               "samp", "script", "section", "small", "source", "span", "strong", "style", "sub", "summary", "sup", &
                                                     "time", "title", "tr", "track", "tt", "u", "var", "video", "wbr"]
    ! css--uri-re.
    character(len=*), parameter :: URI_RE = &
                                   "url\((\)[[:space:]]*\(?:\\.\|[^()[:space:]"//LF//"'""]\)+[" &
                                   //"[:space:]]*\()\)"
    !> Port of css-mode.el css-escapes-re.
    character(len=*), parameter, public :: css_escapes_re = &
                       "\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]]+[ "//LF//TAB//achar(13)//achar(12)//"]?\)"
    !> Port of css-mode.el css-nmchar-re.
    character(len=*), parameter, public :: css_nmchar_re = &
                                     "\(?:[-_[:alnum:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]]+[ "//LF &
                                           //TAB//achar(13)//achar(12)//"]?\)\)"
    !> Port of css-mode.el css-nmstart-re.
    character(len=*), parameter, public :: css_nmstart_re = &
                       "\(?:[[:alpha:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]]+[ "//LF//TAB//achar(13) &
                                           //achar(12)//"]?\)\)"
    !> Port of css-mode.el css-ident-re.
    character(len=*), parameter, public :: css_ident_re = &
                                     "\(?:[-_[:alnum:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]]+[ "//LF &
                                           //TAB//achar(13)//achar(12)//"]?\)\)+"
    !> Vendor-specific properties.  Port of css-mode.el
    !> css-proprietary-nmstart-re.
    character(len=*), parameter, public :: css_proprietary_nmstart_re = &
                                           "[-_]\(?:khtml\|m\(?:oz\|s\)\|o\|webkit\)-"
    !> Port of css-mode.el css-name-re.
    character(len=*), parameter, public :: css_name_re = &
                                     "\(?:[-_[:alnum:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]]+[ "//LF &
                                           //TAB//achar(13)//achar(12)//"]?\)\)+"
    ! scss--hash-re.
    character(len=*), parameter :: SCSS_HASH_RE = &
                                   "#\(?:{[$-_[:alnum:]]+}\|[[:alnum:]]+\)"
    ! The regexps of (css--font-lock-keywords), then of (css--font-lock-keywords
    ! t) (the first is rebuilt from css--bang-ids).
    character(len=*), parameter :: KW1_RE = &
                                   "!\s-*\(?:important\)"
    character(len=*), parameter :: KW2_RE = &
                                   "@\(?:[-_[:alnum:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]]+[ " &
                                   //LF//TAB//achar(13)//achar(12)//"]?\)\)+"
    character(len=*), parameter :: KW3_RE = &
                                   "^[ "//TAB//"]*\(:root\)["//LF//" "//TAB//"]*{"
    character(len=*), parameter :: KW4_RE = &
                                   "^[ "//TAB//"]*\(\(?:[-_%*#.>[:alnum:]]+\(?:\(:\(a\(?:ctive" &
                                   //"\|fter\)\|before\|checked\|d\(?:efault\|isabled\)\" &
                                   //"|e\(?:mpty\|nabled\)\|f\(?:irst\(?:-\(?:child\|l\(" &
                                   //"?:etter\|ine\)\|of-type\)\)?\|ocus\(?:-within\)?\)" &
                                   //"\|h\(?:as\|over\)\|in\(?:-range\|determinate\|vali" &
                                   //"d\)\|l\(?:a\(?:ng\|st-\(?:child\|of-type\)\)\|eft\" &
                                   //"|ink\)\|n\(?:ot\|th-\(?:child\|last-\(?:child\|of-" &
                                   //"type\)\|of-type\)\)\|o\(?:nly-\(?:child\|of-type\)" &
                                   //"\|ptional\|ut-of-range\)\|r\(?:e\(?:ad-\(?:only\|w" &
                                   //"rite\)\|quired\)\|\(?:igh\|oo\)t\)\|s\(?:cope\|ele" &
                                   //"ction\)\|target\|v\(?:\(?:ali\|isite\)d\)\)\|::\(a" &
                                   //"fter\|before\|first-l\(?:etter\|ine\)\|selection\)" &
                                   //"\)\)?\(?:\[[^]"//LF//"]+\]\)?\(?:([^)]+)\)?\|\[[^]"//LF//"]+\]\|" &
                                   //"::?[[:alnum:]]+\(?:([^)]+)\)?\)\(?:[ "//LF//TAB//",+~>]+\(?:[" &
                                   //"-_%*#.>[:alnum:]]+\(?:\(:\(a\(?:ctive\|fter\)\|bef" &
                                   //"ore\|checked\|d\(?:efault\|isabled\)\|e\(?:mpty\|n" &
                                   //"abled\)\|f\(?:irst\(?:-\(?:child\|l\(?:etter\|ine\" &
                                   //")\|of-type\)\)?\|ocus\(?:-within\)?\)\|h\(?:as\|ov" &
                                   //"er\)\|in\(?:-range\|determinate\|valid\)\|l\(?:a\(" &
                                   //"?:ng\|st-\(?:child\|of-type\)\)\|eft\|ink\)\|n\(?:" &
                                   //"ot\|th-\(?:child\|last-\(?:child\|of-type\)\|of-ty" &
                                   //"pe\)\)\|o\(?:nly-\(?:child\|of-type\)\|ptional\|ut" &
                                   //"-of-range\)\|r\(?:e\(?:ad-\(?:only\|write\)\|quire" &
                                   //"d\)\|\(?:igh\|oo\)t\)\|s\(?:cope\|election\)\|targ" &
                                   //"et\|v\(?:\(?:ali\|isite\)d\)\)\|::\(after\|before\" &
                                   //"|first-l\(?:etter\|ine\)\|selection\)\)\)?\(?:\[[^" &
                                   //"]"//LF//"]+\]\)?\(?:([^)]+)\)?\|\[[^]"//LF//"]+\]\|::?[[:alnum:]" &
                                   //"]+\(?:([^)]+)\)?\)\)*\)[ "//LF//TAB//"]*{"
    character(len=*), parameter :: KW5_RE = &
                                   "^[ "//TAB//"]*{"
    character(len=*), parameter :: KW6_RE = &
                                   "\_<--\(?:[-_[:alnum:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]" &
                                   //"]+[ "//LF//TAB//achar(13)//achar(12)//"]?\)\)+"
    character(len=*), parameter :: KW7_RE = &
                                   "\(?:[{;]\|^\)[ "//TAB//"]*\(\(?:\([-_]\(?:khtml\|m\(?:oz\|" &
                                   //"s\)\|o\|webkit\)-\)\|\(?:[[:alpha:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127) &
                                   //"]\|[[:xdigit:]]+[ "//LF//TAB//achar(13)//achar(12)//"]?\)\)\)\(?:[-_[:alnum:]]\|\" &
            //"\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]]+[ "//LF//TAB//achar(13)//achar(12)//"]?\)\)*\)\s-*:"
    character(len=*), parameter :: KW8_RE = &
                                   "url\((\)[[:space:]]*\(?:\\.\|[^()[:space:]"//LF//"'""]\)+[" &
                                   //"[:space:]]*\()\)"
    character(len=*), parameter :: SASSY_KW1_RE = &
                                   "!\s-*\(?:important\)"
    character(len=*), parameter :: SASSY_KW2_RE = &
                                   "@\(?:[-_[:alnum:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]]+[ " &
                                   //LF//TAB//achar(13)//achar(12)//"]?\)\)+"
    character(len=*), parameter :: SASSY_KW3_RE = &
                                   "^[ "//TAB//"]*\(:root\)["//LF//" "//TAB//"]*{"
    character(len=*), parameter :: SASSY_KW4_RE = &
                                   "^[ "//TAB//"]*\(\(?:\(?:[-_%*#.>&+~[:alnum:]]*#\(?:{[$-_[:" &
                                   //"alnum:]]+}\|[[:alnum:]]+\)\|[-_%*#.>&+~[:alnum:]]+" &
                                   //"\)\(?:\(:\(a\(?:ctive\|fter\)\|before\|checked\|d\" &
                                   //"(?:efault\|isabled\)\|e\(?:mpty\|nabled\)\|f\(?:ir" &
                                   //"st\(?:-\(?:child\|l\(?:etter\|ine\)\|of-type\)\)?\" &
                                   //"|ocus\(?:-within\)?\)\|h\(?:as\|over\)\|in\(?:-ran" &
                                   //"ge\|determinate\|valid\)\|l\(?:a\(?:ng\|st-\(?:chi" &
                                   //"ld\|of-type\)\)\|eft\|ink\)\|n\(?:ot\|th-\(?:child" &
                                   //"\|last-\(?:child\|of-type\)\|of-type\)\)\|o\(?:nly" &
                                   //"-\(?:child\|of-type\)\|ptional\|ut-of-range\)\|r\(" &
                                   //"?:e\(?:ad-\(?:only\|write\)\|quired\)\|\(?:igh\|oo" &
                                   //"\)t\)\|s\(?:cope\|election\)\|target\|v\(?:\(?:ali" &
                                   //"\|isite\)d\)\)\|::\(after\|before\|first-l\(?:ette" &
                                   //"r\|ine\)\|selection\)\)\)?\(?:\[[^]"//LF//"]+\]\)?\(?:([^" &
                                   //")]+)\)?\|\[[^]"//LF//"]+\]\|::?[[:alnum:]]+\(?:([^)]+)\)?" &
                                   //"\)\(?:[ "//LF//TAB//",+~>]+\(?:\(?:[-_%*#.>&+~[:alnum:]]*#\(?" &
                                   //":{[$-_[:alnum:]]+}\|[[:alnum:]]+\)\|[-_%*#.>&+~[:a" &
                                   //"lnum:]]+\)\(?:\(:\(a\(?:ctive\|fter\)\|before\|che" &
                                   //"cked\|d\(?:efault\|isabled\)\|e\(?:mpty\|nabled\)\" &
                                   //"|f\(?:irst\(?:-\(?:child\|l\(?:etter\|ine\)\|of-ty" &
                                   //"pe\)\)?\|ocus\(?:-within\)?\)\|h\(?:as\|over\)\|in" &
                                   //"\(?:-range\|determinate\|valid\)\|l\(?:a\(?:ng\|st" &
                                   //"-\(?:child\|of-type\)\)\|eft\|ink\)\|n\(?:ot\|th-\" &
                                   //"(?:child\|last-\(?:child\|of-type\)\|of-type\)\)\|" &
                                   //"o\(?:nly-\(?:child\|of-type\)\|ptional\|ut-of-rang" &
                                   //"e\)\|r\(?:e\(?:ad-\(?:only\|write\)\|quired\)\|\(?" &
                                   //":igh\|oo\)t\)\|s\(?:cope\|election\)\|target\|v\(?" &
                                   //":\(?:ali\|isite\)d\)\)\|::\(after\|before\|first-l" &
                                   //"\(?:etter\|ine\)\|selection\)\)\)?\(?:\[[^]"//LF//"]+\]\)" &
                                   //"?\(?:([^)]+)\)?\|\[[^]"//LF//"]+\]\|::?[[:alnum:]]+\(?:([" &
                                   //"^)]+)\)?\)\)*\)[ "//LF//TAB//"]*{"
    character(len=*), parameter :: SASSY_KW5_RE = &
                                   "^[ "//TAB//"]*{"
    character(len=*), parameter :: SASSY_KW6_RE = &
                                   "\_<--\(?:[-_[:alnum:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]" &
                                   //"]+[ "//LF//TAB//achar(13)//achar(12)//"]?\)\)+"
    character(len=*), parameter :: SASSY_KW7_RE = &
                                   "\(?:[{;]\|^\)[ "//TAB//"]*\(\(?:\([-_]\(?:khtml\|m\(?:oz\|" &
                                   //"s\)\|o\|webkit\)-\)\|\(?:[[:alpha:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127) &
                                   //"]\|[[:xdigit:]]+[ "//LF//TAB//achar(13)//achar(12)//"]?\)\)\)\(?:[-_[:alnum:]]\|\" &
            //"\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]]+[ "//LF//TAB//achar(13)//achar(12)//"]?\)\)*\)\s-*:"
    character(len=*), parameter :: SASSY_KW8_RE = &
                                   "url\((\)[[:space:]]*\(?:\\.\|[^()[:space:]"//LF//"'""]\)+[" &
                                   //"[:space:]]*\()\)"
    ! The first and last regexps of scss-font-lock-keywords.
    character(len=*), parameter :: SCSS_VARIABLE_RE = &
                                   "$\(?:[-_[:alnum:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:xdigit:]]+[ " &
                                   //LF//TAB//achar(13)//achar(12)//"]?\)\)+"
    character(len=*), parameter :: SCSS_MIXIN_RE = &
                                "@mixin[ "//TAB//"]+\(\(?:[-_[:alnum:]]\|\\\(?:[^"//achar(0)//"-"//achar(31)//achar(127)//"]\|[[:" &
                                   //"xdigit:]]+[ "//LF//TAB//achar(13)//achar(12)//"]?\)\)+\)[ "//TAB//"]*("
    ! css--number-regexp, css--percent-regexp, css--number-or-percent-regexp,
    ! css--angle-regexp and css--colors-regexp.
    character(len=*), parameter :: NUMBER_RE = &
                                   "\(\(?:[0-9]*\.[0-9]+\(?:[eE][0-9]+\)?\)\|[0-9]+\)"
    character(len=*), parameter :: PERCENT_RE = &
                                   "\([0-9]+\)%"
    character(len=*), parameter :: NUMBER_OR_PERCENT_RE = &
                                   "\(?:\([0-9]+\)%\)\|\(?:\(\(?:[0-9]*\.[0-9]+\(?:[eE" &
                                   //"][0-9]+\)?\)\|[0-9]+\)\)"
    character(len=*), parameter :: ANGLE_RE = &
                                   "\(\(?:[0-9]*\.[0-9]+\(?:[eE][0-9]+\)?\)\|[0-9]+\)\" &
                                   //"(deg\|grad\|rad\|turn\)?"
    character(len=*), parameter :: COLORS_RE = &
                                   "\_<\(a\(?:liceblue\|ntiquewhite\|qua\(?:marine\)?\" &
                                   //"|zure\)\|b\(?:eige\|isque\|l\(?:a\(?:ck\|nchedalmo" &
                                   //"nd\)\|ue\(?:violet\)?\)\|rown\|urlywood\)\|c\(?:ad" &
                                   //"etblue\|h\(?:\(?:artreus\|ocolat\)e\)\|or\(?:al\|n" &
                                   //"\(?:flowerblue\|silk\)\)\|\(?:rimso\|ya\)n\)\|d\(?" &
                                   //":ark\(?:blue\|cyan\|g\(?:oldenrod\|r\(?:ay\|e\(?:e" &
                                   //"n\|y\)\)\)\|khaki\|magenta\|o\(?:livegreen\|r\(?:a" &
                                   //"nge\|chid\)\)\|red\|s\(?:almon\|eagreen\|late\(?:b" &
                                   //"lue\|gr\(?:[ae]y\)\)\)\|turquoise\|violet\)\|eep\(" &
                                   //"?:pink\|skyblue\)\|imgr\(?:[ae]y\)\|odgerblue\)\|f" &
                                   //"\(?:irebrick\|loralwhite\|orestgreen\|uchsia\)\|g\" &
                                   //"(?:ainsboro\|hostwhite\|old\(?:enrod\)?\|r\(?:ay\|" &
                                   //"e\(?:en\(?:yellow\)?\|y\)\)\)\|ho\(?:neydew\|tpink" &
                                   //"\)\|i\(?:ndi\(?:anred\|go\)\|vory\)\|khaki\|l\(?:a" &
                                   //"\(?:vender\(?:blush\)?\|wngreen\)\|emonchiffon\|i\" &
                                   //"(?:ght\(?:blue\|c\(?:oral\|yan\)\|g\(?:oldenrodyel" &
                                   //"low\|r\(?:ay\|e\(?:en\|y\)\)\)\|pink\|s\(?:almon\|" &
                                   //"eagreen\|kyblue\|lategr\(?:[ae]y\)\|teelblue\)\|ye" &
                                   //"llow\)\|me\(?:green\)?\|nen\)\)\|m\(?:a\(?:genta\|" &
                                   //"roon\)\|edium\(?:aquamarine\|blue\|orchid\|purple\" &
                                   //"|s\(?:eagreen\|lateblue\|pringgreen\)\|turquoise\|" &
                                   //"violetred\)\|i\(?:dnightblue\|ntcream\|styrose\)\|" &
                                   //"occasin\)\|nav\(?:ajowhite\|y\)\|o\(?:l\(?:dlace\|" &
                                   //"ive\(?:drab\)?\)\|r\(?:ange\(?:red\)?\|chid\)\)\|p" &
                                   //"\(?:a\(?:le\(?:g\(?:oldenrod\|reen\)\|turquoise\|v" &
                                   //"ioletred\)\|payawhip\)\|e\(?:achpuff\|ru\)\|ink\|l" &
                                   //"um\|\(?:owderblu\|urpl\)e\)\|r\(?:e\(?:beccapurple" &
                                   //"\|d\)\|o\(?:sybrown\|yalblue\)\)\|s\(?:a\(?:\(?:dd" &
                                   //"lebrow\|lmo\|ndybrow\)n\)\|ea\(?:green\|shell\)\|i" &
                                   //"\(?:enna\|lver\)\|kyblue\|late\(?:blue\|gr\(?:[ae]" &
                                   //"y\)\)\|now\|pringgreen\|teelblue\)\|t\(?:an\|eal\|" &
                                   //"histle\|omato\|urquoise\)\|violet\|wh\(?:eat\|ite\" &
                                   //"(?:smoke\)?\)\|yellow\(?:green\)?\)\_>\|\(#[[:xdig" &
                                   //"it:]]\{3,4\}\b\)\|\(#\(?:[[:xdigit:]][[:xdigit:]]\" &
                                   //")\{3,4\}\b\)\|\(\_<rgba?(\)\|\(\_<hsla?(\)"
    ! css--mdn-symbol-regexp and css--mdn-property-regexp.
    character(len=*), parameter :: MDN_SYMBOL_RE = &
                                   "\(\(@\(?:charset\|font-face\|import\|keyframes\|me" &
                                   //"dia\|namespace\|page\|supports\)\)\|\(a\(?:l\(?:ig" &
                                   //"n-\(?:content\|items\|self\)\|l\)\|nimation\(?:-\(" &
                                   //"?:d\(?:elay\|\(?:irec\|ura\)tion\)\|fill-mode\|ite" &
                                   //"ration-count\|name\|play-state\|timing-function\)\" &
                                   //")?\|zimuth\)\|b\(?:ack\(?:face-visibility\|ground\" &
                                   //"(?:-\(?:attachment\|c\(?:lip\|olor\)\|image\|origi" &
                                   //"n\|position\|repeat\|size\)\)?\)\|o\(?:rder\(?:-\(" &
                                   //"?:bottom\(?:-\(?:color\|left-radius\|right-radius\" &
                                   //"|style\|width\)\)?\|col\(?:lapse\|or\)\|image\(?:-" &
                                   //"\(?:outset\|repeat\|s\(?:\(?:li\|our\)ce\)\|width\" &
                                   //")\)?\|left\(?:-\(?:color\|style\|width\)\)?\|r\(?:" &
                                   //"adius\|ight\(?:-\(?:color\|style\|width\)\)?\)\|s\" &
                                   //"(?:pacing\|tyle\)\|top\(?:-\(?:color\|left-radius\" &
                                   //"|right-radius\|style\|width\)\)?\|width\)\)?\|ttom" &
                                   //"\|x-\(?:decoration-break\|s\(?:hadow\|izing\)\)\)\" &
                                   //"|reak-\(?:after\|\(?:befor\|insid\)e\)\)\|c\(?:a\(" &
                                   //"?:ption-side\|ret-color\)\|l\(?:ear\|ip\(?:-\(?:pa" &
                                   //"th\|rule\)\)?\)\|o\(?:l\(?:or\(?:-interpolation-fi" &
                                   //"lters\)?\|umn\(?:-\(?:count\|fill\|gap\|rule\(?:-\" &
                                   //"(?:color\|style\|width\)\)?\|span\|width\)\|s\)\)\" &
                                   //"|nt\(?:ain\|ent\(?:-visibility\)?\)\|unter-\(?:\(?" &
                                   //":incremen\|rese\)t\)\)\|u\(?:e\(?:-\(?:after\|befo" &
                                   //"re\)\)?\|rsor\)\)\|di\(?:rection\|splay\)\|e\(?:le" &
                                   //"vation\|mpty-cells\)\|f\(?:ilter\|l\(?:ex\(?:-\(?:" &
                                   //"basis\|direction\|flow\|grow\|shrink\|wrap\)\)?\|o" &
                                   //"\(?:at\|od-\(?:color\|opacity\)\)\)\|ont\(?:-\(?:f" &
                                   //"\(?:amily\|eature-settings\)\|kerning\|language-ov" &
                                   //"erride\|s\(?:ize\(?:-adjust\)?\|t\(?:retch\|yle\)\" &
                                   //"|ynthesis\)\|variant\(?:-\(?:alternates\|caps\|eas" &
                                   //"t-asian\|ligatures\|numeric\|position\)\)?\|weight" &
                                   //"\)\)?\)\|g\(?:ap\|rid\(?:-\(?:a\(?:rea\|uto-\(?:co" &
                                   //"lumns\|flow\|rows\)\)\|column\(?:-\(?:end\|gap\|st" &
                                   //"art\)\)?\|gap\|row\(?:-\(?:end\|gap\|start\)\)?\|t" &
                                   //"emplate\(?:-\(?:\(?:area\|column\|row\)s\)\)?\)\)?" &
                                   //"\)\|h\(?:anging-punctuation\|eight\|yphens\)\|just" &
                                   //"ify-\(?:content\|items\|self\)\|l\(?:e\(?:ft\|tter" &
                                   //"-spacing\)\|i\(?:ghting-color\|ne-\(?:break\|heigh" &
                                   //"t\)\|st-style\(?:-\(?:image\|position\|type\)\)?\)" &
                                   //"\)\|m\(?:a\(?:rgin\(?:-\(?:bottom\|left\|right\|to" &
                                   //"p\)\)?\|sk\(?:-\(?:border\(?:-\(?:mode\|outset\|re" &
                                   //"peat\|s\(?:\(?:li\|our\)ce\)\|width\)\)?\|c\(?:lip" &
                                   //"\|omposite\)\|image\|mode\|origin\|position\|repea" &
                                   //"t\|\(?:siz\|typ\)e\)\)?\|x-\(?:height\|lines\|widt" &
                                   //"h\)\)\|in-\(?:height\|width\)\)\|nav-\(?:down\|lef" &
                                   //"t\|right\|up\)\|o\(?:pacity\|r\(?:der\|phans\)\|ut" &
                                   //"line\(?:-\(?:color\|offset\|style\|width\)\)?\|ver" &
                                   //"flow\(?:-\(?:wrap\|[xy]\)\)?\)\|p\(?:a\(?:dding\(?" &
                                   //":-\(?:bottom\|left\|right\|top\)\)?\|ge-break-\(?:" &
                                   //"after\|\(?:befor\|insid\)e\)\|use\(?:-\(?:after\|b" &
                                   //"efore\)\)?\)\|erspective\(?:-origin\)?\|itch\(?:-r" &
                                   //"ange\)?\|la\(?:ce-\(?:content\|items\|self\)\|y-du" &
                                   //"ring\)\|osition\)\|quotes\|r\(?:esize\|i\(?:chness" &
                                   //"\|ght\)\|ow-gap\)\|s\(?:pe\(?:ak\(?:-\(?:header\|n" &
                                   //"umeral\|punctuation\)\)?\|ech-rate\)\|tress\)\|t\(" &
                                   //"?:ab\(?:-size\|le-layout\)\|ext-\(?:align\(?:-last" &
                                   //"\)?\|combine-upright\|decoration\(?:-\(?:color\|li" &
                                   //"ne\|s\(?:kip\|tyle\)\)\)?\|emphasis\(?:-\(?:color\" &
                                   //"|position\|style\)\)?\|indent\|justify\|o\(?:rient" &
                                   //"ation\|verflow\)\|shadow\|transform\|underline-pos" &
                                   //"ition\)\|o\(?:p\|uch-action\)\|rans\(?:form\(?:-\(" &
                                   //"?:origin\|style\)\)?\|ition\(?:-\(?:d\(?:elay\|ura" &
                                   //"tion\)\|property\|timing-function\)\)?\)\)\|unicod" &
                                   //"e-bidi\|v\(?:ertical-align\|isibility\|o\(?:ice-fa" &
                                   //"mily\|lume\)\)\|w\(?:hite-space\|i\(?:d\(?:ows\|th" &
                                   //"\)\|ll-change\)\|ord-\(?:break\|spacing\|wrap\)\|r" &
                                   //"iting-mode\)\|z-index\)\|\(:\(?:active\|checked\|d" &
                                   //"\(?:efault\|isabled\)\|e\(?:mpty\|nabled\)\|f\(?:i" &
                                   //"rst\(?:-\(?:child\|of-type\)\)?\|ocus\(?:-within\)" &
                                   //"?\)\|h\(?:as\|over\)\|in\(?:-range\|determinate\|v" &
                                   //"alid\)\|l\(?:a\(?:ng\|st-\(?:child\|of-type\)\)\|e" &
                                   //"ft\|ink\)\|n\(?:ot\|th-\(?:child\|last-\(?:child\|" &
                                   //"of-type\)\|of-type\)\)\|o\(?:nly-\(?:child\|of-typ" &
                                   //"e\)\|ptional\|ut-of-range\)\|r\(?:e\(?:ad-\(?:only" &
                                   //"\|write\)\|quired\)\|\(?:igh\|oo\)t\)\|scope\|targ" &
                                   //"et\|v\(?:\(?:ali\|isite\)d\)\)\)\|\(::?\(?:after\|" &
                                   //"before\|first-l\(?:etter\|ine\)\|selection\)\)\)"
    character(len=*), parameter :: MDN_PROPERTY_RE = &
                                   "\_<\(a\(?:l\(?:ign-\(?:content\|items\|self\)\|l\)" &
                                   //"\|nimation\(?:-\(?:d\(?:elay\|\(?:irec\|ura\)tion\" &
                                   //")\|fill-mode\|iteration-count\|name\|play-state\|t" &
                                   //"iming-function\)\)?\|zimuth\)\|b\(?:ack\(?:face-vi" &
                                   //"sibility\|ground\(?:-\(?:attachment\|c\(?:lip\|olo" &
                                   //"r\)\|image\|origin\|position\|repeat\|size\)\)?\)\" &
                                   //"|o\(?:rder\(?:-\(?:bottom\(?:-\(?:color\|left-radi" &
                                   //"us\|right-radius\|style\|width\)\)?\|col\(?:lapse\" &
                                   //"|or\)\|image\(?:-\(?:outset\|repeat\|s\(?:\(?:li\|" &
                                   //"our\)ce\)\|width\)\)?\|left\(?:-\(?:color\|style\|" &
                                   //"width\)\)?\|r\(?:adius\|ight\(?:-\(?:color\|style\" &
                                   //"|width\)\)?\)\|s\(?:pacing\|tyle\)\|top\(?:-\(?:co" &
                                   //"lor\|left-radius\|right-radius\|style\|width\)\)?\" &
                                   //"|width\)\)?\|ttom\|x-\(?:decoration-break\|s\(?:ha" &
                                   //"dow\|izing\)\)\)\|reak-\(?:after\|\(?:befor\|insid" &
                                   //"\)e\)\)\|c\(?:a\(?:ption-side\|ret-color\)\|l\(?:e" &
                                   //"ar\|ip\(?:-\(?:path\|rule\)\)?\)\|o\(?:l\(?:or\(?:" &
                                   //"-interpolation-filters\)?\|umn\(?:-\(?:count\|fill" &
                                   //"\|gap\|rule\(?:-\(?:color\|style\|width\)\)?\|span" &
                                   //"\|width\)\|s\)\)\|nt\(?:ain\|ent\(?:-visibility\)?" &
                                   //"\)\|unter-\(?:\(?:incremen\|rese\)t\)\)\|u\(?:e\(?" &
                                   //":-\(?:after\|before\)\)?\|rsor\)\)\|di\(?:rection\" &
                                   //"|splay\)\|e\(?:levation\|mpty-cells\)\|f\(?:ilter\" &
                                   //"|l\(?:ex\(?:-\(?:basis\|direction\|flow\|grow\|shr" &
                                   //"ink\|wrap\)\)?\|o\(?:at\|od-\(?:color\|opacity\)\)" &
                                   //"\)\|ont\(?:-\(?:f\(?:amily\|eature-settings\)\|ker" &
                                   //"ning\|language-override\|s\(?:ize\(?:-adjust\)?\|t" &
                                   //"\(?:retch\|yle\)\|ynthesis\)\|variant\(?:-\(?:alte" &
                                   //"rnates\|caps\|east-asian\|ligatures\|numeric\|posi" &
                                   //"tion\)\)?\|weight\)\)?\)\|g\(?:ap\|rid\(?:-\(?:a\(" &
                                   //"?:rea\|uto-\(?:columns\|flow\|rows\)\)\|column\(?:" &
                                   //"-\(?:end\|gap\|start\)\)?\|gap\|row\(?:-\(?:end\|g" &
                                   //"ap\|start\)\)?\|template\(?:-\(?:\(?:area\|column\" &
                                   //"|row\)s\)\)?\)\)?\)\|h\(?:anging-punctuation\|eigh" &
                                   //"t\|yphens\)\|justify-\(?:content\|items\|self\)\|l" &
                                   //"\(?:e\(?:ft\|tter-spacing\)\|i\(?:ghting-color\|ne" &
                                   //"-\(?:break\|height\)\|st-style\(?:-\(?:image\|posi" &
                                   //"tion\|type\)\)?\)\)\|m\(?:a\(?:rgin\(?:-\(?:bottom" &
                                   //"\|left\|right\|top\)\)?\|sk\(?:-\(?:border\(?:-\(?" &
                                   //":mode\|outset\|repeat\|s\(?:\(?:li\|our\)ce\)\|wid" &
                                   //"th\)\)?\|c\(?:lip\|omposite\)\|image\|mode\|origin" &
                                   //"\|position\|repeat\|\(?:siz\|typ\)e\)\)?\|x-\(?:he" &
                                   //"ight\|lines\|width\)\)\|in-\(?:height\|width\)\)\|" &
                                   //"nav-\(?:down\|left\|right\|up\)\|o\(?:pacity\|r\(?" &
                                   //":der\|phans\)\|utline\(?:-\(?:color\|offset\|style" &
                                   //"\|width\)\)?\|verflow\(?:-\(?:wrap\|[xy]\)\)?\)\|p" &
                                   //"\(?:a\(?:dding\(?:-\(?:bottom\|left\|right\|top\)\" &
                                   //")?\|ge-break-\(?:after\|\(?:befor\|insid\)e\)\|use" &
                                   //"\(?:-\(?:after\|before\)\)?\)\|erspective\(?:-orig" &
                                   //"in\)?\|itch\(?:-range\)?\|la\(?:ce-\(?:content\|it" &
                                   //"ems\|self\)\|y-during\)\|osition\)\|quotes\|r\(?:e" &
                                   //"size\|i\(?:chness\|ght\)\|ow-gap\)\|s\(?:pe\(?:ak\" &
                                   //"(?:-\(?:header\|numeral\|punctuation\)\)?\|ech-rat" &
                                   //"e\)\|tress\)\|t\(?:ab\(?:-size\|le-layout\)\|ext-\" &
                                   //"(?:align\(?:-last\)?\|combine-upright\|decoration\" &
                                   //"(?:-\(?:color\|line\|s\(?:kip\|tyle\)\)\)?\|emphas" &
                                   //"is\(?:-\(?:color\|position\|style\)\)?\|indent\|ju" &
                                   //"stify\|o\(?:rientation\|verflow\)\|shadow\|transfo" &
                                   //"rm\|underline-position\)\|o\(?:p\|uch-action\)\|ra" &
                                   //"ns\(?:form\(?:-\(?:origin\|style\)\)?\|ition\(?:-\" &
                                   //"(?:d\(?:elay\|uration\)\|property\|timing-function" &
                                   //"\)\)?\)\)\|unicode-bidi\|v\(?:ertical-align\|isibi" &
                                   //"lity\|o\(?:ice-family\|lume\)\)\|w\(?:hite-space\|" &
                                   //"i\(?:d\(?:ows\|th\)\|ll-change\)\|ord-\(?:break\|s" &
                                   //"pacing\|wrap\)\|riting-mode\)\|z-index\)\s-*\(?:\=" &
                                   //"\|:\)"

contains

    ! ------------------------------------------------------------------
    ! Buffer-local values
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine list_function_set_local(self, fn)
        class(css_list_function_type), intent(inout) :: self
        procedure(css_list_proc) :: fn
        type(css_list_entry_type), allocatable :: grown(:)
        integer :: i, n

        if (cur == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%fn => fn
                return
            end if
        end do

        ! Reuse the entry of a dead buffer.
        do i = 1, size(self%entries)
            if (.not. buffer_serial_live(self%entries(i)%serial)) then
                self%entries(i)%serial = buffers(cur)%serial
                self%entries(i)%fn => fn
                return
            end if
        end do

        n = size(self%entries)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i)%serial = self%entries(i)%serial
            grown(i)%fn => self%entries(i)%fn
        end do

        grown(n + 1)%serial = buffers(cur)%serial
        grown(n + 1)%fn => fn
        call move_alloc(grown, self%entries)
    end subroutine list_function_set_local

    ! Not a port.
    function list_function_get(self) result(fn)
        class(css_list_function_type), intent(in) :: self
        procedure(css_list_proc), pointer :: fn
        integer :: i

        fn => self%value
        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                if (associated(self%entries(i)%fn)) fn => self%entries(i)%fn
            end if
        end do
    end function list_function_get

    ! Not a port.
    subroutine list_function_kill_local(self)
        class(css_list_function_type), intent(inout) :: self
        integer :: i

        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%serial = 0
                self%entries(i)%fn => null()
            end if
        end do
    end subroutine list_function_kill_local

    ! The index of the current buffer's entry in `locals' (made if MAKE),
    ! 0 if none.
    ! Not a port.
    integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(css_local_type), allocatable :: grown(:)
        integer :: n

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
            grown(1:n) = locals
            call move_alloc(grown, locals)
            k = n + 1
        end if

        locals(k) = css_local_type(serial=buffers(cur)%serial)
    end function local_index

    ! Whether the current buffer has SCSS's at-rules and bang-rules and
    ! allows nested selectors.
    ! Not a port.
    logical function scss_buffer()
        integer :: k

        scss_buffer = .false.
        k = local_index(.false.)
        if (k > 0) scss_buffer = locals(k)%scss
    end function scss_buffer

    ! kill-all-local-variables: forget the current buffer's css-mode.el
    ! variables.
    ! Not a port.
    subroutine kill_css_locals()
        integer :: k

        call css_class_list_function%kill_local()
        call css_id_list_function%kill_local()
        k = local_index(.false.)
        if (k > 0) locals(k)%serial = 0
    end subroutine kill_css_locals

    ! css--at-ids.
    ! Port of css-mode.el:css--at-ids (its value).
    function at_ids() result(ids)
        character(len=20), allocatable :: ids(:)

        if (scss_buffer()) then
            ids = [css_at_ids, scss_at_ids]
        else
            ids = css_at_ids
        end if
    end function at_ids

    ! css--bang-ids.
    ! Port of css-mode.el:css--bang-ids (its value).
    function bang_ids() result(ids)
        character(len=20), allocatable :: ids(:)

        if (scss_buffer()) then
            ids = [css_bang_ids, scss_bang_ids]
        else
            ids = css_bang_ids
        end if
    end function bang_ids

    ! ------------------------------------------------------------------
    ! Property values
    ! ------------------------------------------------------------------

    ! The index of the value class NAME, 0 if none.
    ! Not a port.
    integer function class_index(name) result(k)
        character(len=*), intent(in) :: name

        do k = 1, size(CLASS_NAMES)
            if (trim(CLASS_NAMES(k)) == name) return
        end do

        k = 0
    end function class_index

    ! The index of the property NAME, 0 if none.
    ! Not a port.
    integer function property_index(name) result(k)
        character(len=*), intent(in) :: name

        do k = 1, size(css_property_ids)
            if (trim(css_property_ids(k)) == name) return
        end do

        k = 0
    end function property_index

    ! Append the elements of ITEMS that LIST doesn't have yet (seq-uniq of
    ! seq-mapcat).
    ! Not a port.
    subroutine append_new(list, items)
        character(len=36), allocatable, intent(inout) :: list(:)
        character(len=36), intent(in) :: items(:)
        integer :: i

        do i = 1, size(items)
            if (.not. any(list == items(i))) list = [character(len=36) :: list, items(i)]
        end do
    end subroutine append_new

    !> Return a list of value completion candidates for VALUE_CLASS.
    !> Completion candidates are looked up in `css_value_class_alist' by
    !> the name VALUE_CLASS.  Port of css-mode.el:css--value-class-lookup.
    recursive function css_value_class_lookup(value_class) result(values)
        character(len=*), intent(in) :: value_class
        character(len=36), allocatable :: values(:)
        integer :: k, i

        allocate (values(0))
        k = class_index(value_class)
        if (k == 0) return

        do i = CLASS_FIRST(k), CLASS_FIRST(k + 1) - 1
            if (CLASS_VALUE_SYMBOL(i)) then
                call append_new(values, css_value_class_lookup(trim(CLASS_VALUES(i))))
            else
                call append_new(values, [CLASS_VALUES(i)])
            end if
        end do
    end function css_value_class_lookup

    !> Return a list of value completion candidates for PROPERTY.
    !> Completion candidates are looked up in `css_property_alist' by
    !> the string PROPERTY.  Port of css-mode.el:css--property-values.
    recursive function css_property_values(property) result(values)
        character(len=*), intent(in) :: property
        character(len=36), allocatable :: values(:), class_values(:)
        type(value_cache_type), allocatable :: grown(:)
        logical :: found
        integer :: k, i, n

        call css_property_value_cache_get(property, values, found)
        if (found) return
        if (allocated(values)) deallocate (values)
        allocate (values(0))
        k = property_index(property)

        if (k > 0) then
            do i = PROPERTY_FIRST(k), PROPERTY_FIRST(k + 1) - 1
                if (PROPERTY_VALUE_SYMBOL(i)) then
                    class_values = css_value_class_lookup(trim(PROPERTY_VALUES(i)))
                    if (size(class_values) == 0) class_values = css_property_values(trim(PROPERTY_VALUES(i)))
                    call append_new(values, class_values)
                else
                    call append_new(values, [PROPERTY_VALUES(i)])
                end if
            end do
        end if

        ! puthash.
        if (.not. allocated(value_cache)) allocate (value_cache(0))

        do i = 1, size(value_cache)
            if (trim(value_cache(i)%property) == property) then
                value_cache(i)%values = values
                return
            end if
        end do

        n = size(value_cache)
        allocate (grown(n + 1))

        do i = 1, n
            call move_alloc(value_cache(i)%values, grown(i)%values)
            grown(i)%property = value_cache(i)%property
        end do

        grown(n + 1)%property = property
        grown(n + 1)%values = values
        call move_alloc(grown, value_cache)
    end function css_property_values

    !> The values of PROPERTY in the cache of previously completed property
    !> values (`found' false when it has none: gethash's nil).  Port of
    !> css-mode.el css--property-value-cache.
    subroutine css_property_value_cache_get(property, values, found)
        character(len=*), intent(in) :: property
        character(len=36), allocatable, intent(out) :: values(:)
        logical, intent(out) :: found
        integer :: i

        found = .false.
        allocate (values(0))
        if (.not. allocated(value_cache)) return

        do i = 1, size(value_cache)
            if (trim(value_cache(i)%property) == property) then
                values = value_cache(i)%values
                found = size(values) > 0
                return
            end if
        end do
    end subroutine css_property_value_cache_get

    ! ------------------------------------------------------------------
    ! Completion
    ! ------------------------------------------------------------------

    ! A list of strings: the trimmed elements of STRS, each with PREFIX.
    ! Not a port.
    function string_list(strs, prefix) result(v)
        character(len=*), intent(in) :: strs(:)
        character(len=*), intent(in), optional :: prefix
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        integer :: i

        allocate (elts(size(strs)))

        do i = 1, size(strs)
            if (present(prefix)) then
                elts(i) = make_string(prefix//trim(strs(i)))
            else
                elts(i) = make_string(trim(strs(i)))
            end if
        end do

        v = make_list(elts)
    end function string_list

    ! Append V to ELTS.
    ! Not a port.
    subroutine push(elts, v)
        type(value_type), allocatable, intent(inout) :: elts(:)
        type(value_type), intent(in) :: v
        type(value_type), allocatable :: grown(:)
        integer :: i, n

        n = size(elts)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i) = elts(i)
        end do

        grown(n + 1) = v
        call move_alloc(grown, elts)
    end subroutine push

    ! Append the elements of MORE to ELTS.
    ! Not a port.
    subroutine push_all(elts, more)
        type(value_type), allocatable, intent(inout) :: elts(:)
        type(value_type), intent(in) :: more(:)
        integer :: i

        do i = 1, size(more)
            call push(elts, more(i))
        end do
    end subroutine push_all

    ! Complete property at point.  Port of css-mode.el:css--complete-property.
    subroutine complete_property(data)
        type(capf_data_type), intent(out) :: data
        type(status_type) :: st
        integer(int64) :: opoint, pos, start, moved

        opoint = point()
        pos = point()
        moved = skip_chars_backward("-[:alnum:]", st)
        start = point()
        moved = skip_chars_backward(" "//TAB//achar(13)//LF, st)

        if (preceding_char() == iachar("{") .or. preceding_char() == iachar(";")) then
            data%found = .true.
            data%start = start
            data%end = pos
            data%table%list = string_list(css_property_ids)
        end if

        call goto_char(opoint)
    end subroutine complete_property

    ! Complete bang-rule at point.  Port of css-mode.el:css--complete-bang-rule.
    subroutine complete_bang_rule(data)
        type(capf_data_type), intent(out) :: data
        type(status_type) :: st
        integer(int64) :: opoint, moved

        opoint = point()
        moved = skip_chars_backward("-[:alnum:]", st)

        if (preceding_char() == iachar("!")) then
            data%found = .true.
            data%start = point()
            data%end = opoint
            data%table%list = string_list(bang_ids())
        end if

        call goto_char(opoint)
    end subroutine complete_bang_rule

    ! Complete pseudo-element or pseudo-class at point.  Port of
    ! css-mode.el:css--complete-pseudo-element-or-class.
    subroutine complete_pseudo_element_or_class(data)
        type(capf_data_type), intent(out) :: data
        type(value_type), allocatable :: elts(:), more(:)
        type(status_type) :: st
        integer(int64) :: opoint, moved
        logical :: double_colon
        integer :: i

        opoint = point()
        moved = skip_chars_backward("-[:alnum:]", st)

        if (preceding_char() == iachar(":")) then
            double_colon = before_char_at(point() - 1) == iachar(":")
            data%found = .true.

            if (double_colon) then
                data%start = point() - 2
            else
                data%start = point() - 1
            end if

            data%end = opoint
            allocate (elts(0))

            if (.not. double_colon) then
                allocate (more(size(css_pseudo_class_ids)))

                do i = 1, size(css_pseudo_class_ids)
                    more(i) = make_string(":"//trim(css_pseudo_class_ids(i)))
                end do

                call push_all(elts, more)
                deallocate (more)
            end if

            allocate (more(size(css_pseudo_element_ids)))

            do i = 1, size(css_pseudo_element_ids)
                more(i) = make_string("::"//trim(css_pseudo_element_ids(i)))
            end do

            call push_all(elts, more)
            data%table%list = make_list(elts)
        end if

        call goto_char(opoint)
    end subroutine complete_pseudo_element_or_class

    ! Complete at-rule (statement beginning with `@') at point.  Port of
    ! css-mode.el:css--complete-at-rule.
    subroutine complete_at_rule(data)
        type(capf_data_type), intent(out) :: data
        type(status_type) :: st
        integer(int64) :: opoint, moved

        opoint = point()
        moved = skip_chars_backward("-[:alnum:]", st)

        if (preceding_char() == iachar("@")) then
            data%found = .true.
            data%start = point()
            data%end = opoint
            data%table%list = string_list(at_ids())
        end if

        call goto_char(opoint)
    end subroutine complete_at_rule

    ! Complete property value at point.  Port of
    ! css-mode.el:css--complete-property-value.
    subroutine complete_property_value(data)
        type(capf_data_type), intent(out) :: data
        type(parse_state_type) :: ppss
        type(status_type) :: st
        character(len=:), allocatable :: property
        integer(int64) :: opoint, limit, moved
        character(len=36), allocatable :: values(:)

        opoint = point()
        call syntax_ppss(ppss)
        limit = point()
        if (ppss%innermost_start > 0) limit = ppss%innermost_start
        if (.not. looking_back("\([[:alnum:]-]+\):[^/][^;]*", st, limit, greedy=.true.)) return
        property = match_string_no_properties(1)
        if (property_index(property) == 0) return
        moved = skip_chars_backward("[:graph:]", st)
        data%found = .true.
        data%start = point()
        data%end = opoint
        values = css_property_values(property)
        data%table%list = string_list([character(len=36) :: "inherit", "initial", "unset", values])
        call goto_char(opoint)
    end subroutine complete_property_value

    !> Return a list of completions provided by other buffers.
    !> EXTRACTOR should be a function variable that may be defined in one
    !> or more buffers.  In each of the buffers where EXTRACTOR is defined,
    !> EXTRACTOR is called and the results are accumulated into a list of
    !> completions.  Port of css-mode.el:css--foreign-completions.
    subroutine css_foreign_completions(extractor, completions)
        type(css_list_function_type), intent(in) :: extractor
        character(len=:), allocatable, intent(out) :: completions(:)
        type(handle_type), allocatable :: bufs(:)
        type(handle_type) :: saved
        type(status_type) :: st
        procedure(css_list_proc), pointer :: fn
        character(len=:), allocatable :: names(:)
        type(value_type), allocatable :: found(:)
        integer :: i, j, n, width

        saved = current_buffer()
        bufs = buffer_list()
        allocate (found(0))

        do i = 1, size(bufs)
            if (.not. buffer_live_p(bufs(i))) cycle
            call set_buffer(bufs(i), st)
            fn => extractor%get()

            if (associated(fn)) then
                call fn(names)

                do j = 1, size(names)
                    call push(found, make_string(trim(names(j))))
                end do
            end if
        end do

        call set_buffer(saved, st)
        ! delete-dups.
        width = 0

        do i = 1, size(found)
            width = max(width, len(string_of(found(i))))
        end do

        allocate (character(len=width) :: completions(0))
        n = 0

        do i = 1, size(found)
            if (.not. any(completions == string_of(found(i)))) then
                completions = [character(len=width) :: completions, string_of(found(i))]
                n = n + 1
            end if
        end do
    end subroutine css_foreign_completions

    ! The string V holds.
    ! Not a port.
    function string_of(v) result(s)
        use lisp_fm, only: xstring
        type(value_type), intent(in) :: v
        character(len=:), allocatable :: s

        s = xstring(v)
    end function string_of

    ! The candidates of the selector table (completion-table-dynamic of
    ! css--complete-selector): class names after ".", IDs after "#", HTML
    ! tags otherwise.
    ! Port of css-mode.el:css--complete-selector (in part).
    function selector_candidates() result(v)
        type(value_type) :: v
        character(len=:), allocatable :: names(:)

        if (merge_sel_char == iachar(".")) then
            call css_foreign_completions(css_class_list_function, names)
            v = string_list(names)
        else if (merge_sel_char == iachar("#")) then
            call css_foreign_completions(css_id_list_function, names)
            v = string_list(names)
        else
            v = string_list(HTML_TAGS)
        end if
    end function selector_candidates

    ! The tables merged in `css_completion_at_point' (completion-table-merge
    ! of the property and selector tables).
    ! Port of css-mode.el:css-completion-at-point (in part: its merged table).
    recursive subroutine merged_table(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: tables(2), retvals
        type(value_type) :: v, all(2)
        type(value_type), allocatable :: elts(:), more(:)
        logical :: have(2)
        integer :: i

        status = status_type()
        result = nil()
        have = [merge_prop, merge_sel]
        if (merge_prop) tables(1)%list = string_list(css_property_ids)
        if (merge_sel) tables(2)%list = selector_candidates()

        select case (action)
        case (COMPLETE_TRY)
            allocate (elts(0))

            do i = 1, 2
                if (.not. have(i)) cycle
                call try_completion(string, tables(i), v, status, pred)
                if (.not. status%ok()) return

                if (is_t(v)) then
                    call push(elts, make_string(string))
                else if (.not. is_nil(v)) then
                    if (string_of(v) == string) then
                        result = make_string(string)
                        return
                    end if

                    call push(elts, v)
                end if
            end do

            retvals%list = make_list(elts)
            call try_completion(string, retvals, result, status, pred)
        case (COMPLETE_ALL)
            allocate (elts(0))

            do i = 1, 2
                if (.not. have(i)) cycle
                call all_completions(string, tables(i), all(i), status, pred)
                if (.not. status%ok()) return
                call list_to_array(all(i), more)
                call push_all(elts, more)
            end do

            result = make_list(elts)
        case (COMPLETE_TEST)
            do i = 1, 2
                if (.not. have(i)) cycle

                if (test_completion(string, tables(i), pred)) then
                    result = sym("t")
                    return
                end if
            end do
        case default
            continue
        end select
    end subroutine merged_table

    ! Whether V is t.
    ! Not a port.
    logical function is_t(v)
        use lisp_fm, only: symbolp, xsymbol, symbols
        type(value_type), intent(in) :: v

        is_t = .false.
        if (symbolp(v)) is_t = symbols(xsymbol(v))%name == "t"
    end function is_t

    ! Whether V is nil.
    ! Not a port.
    logical function is_nil(v)
        use lisp_fm, only: nilp
        type(value_type), intent(in) :: v

        is_nil = nilp(v)
    end function is_nil

    ! The elements of the list V.
    ! Not a port.
    subroutine list_to_array(v, elts)
        use lisp_fm, only: list_elements
        type(value_type), intent(in) :: v
        type(value_type), allocatable, intent(out) :: elts(:)

        call list_elements(v, elts)
    end subroutine list_to_array

    ! The :exit-function of `css_completion_at_point': after a property
    ! name, insert ": ;" and move before the semicolon.
    ! Port of css-mode.el:css-completion-at-point (in part: its exit function).
    recursive subroutine property_exit_function(string, finished)
        character(len=*), intent(in) :: string
        integer, intent(in) :: finished
        type(completion_table_type) :: prop_table, sel_table
        type(status_type) :: st

        if (finished /= COMPLETION_FINISHED) return
        if (.not. eolp()) return
        if (.not. merge_prop) return
        prop_table%list = string_list(css_property_ids)
        if (.not. test_completion(string, prop_table)) return

        if (merge_sel) then
            sel_table%list = selector_candidates()
            if (test_completion(string, sel_table)) return
        end if

        call insert(": ;", st)
        call forward_char(st, -1_int64)
    end subroutine property_exit_function

    ! Complete part of a CSS selector at point (whether it applies, and
    ! where).  Port of css-mode.el:css--complete-selector.
    subroutine complete_selector(found, start, end, start_char)
        logical, intent(out) :: found
        integer(int64), intent(out) :: start, end
        integer, intent(out) :: start_char
        type(parse_state_type) :: ppss
        type(status_type) :: st
        integer(int64) :: opoint, moved

        found = .false.
        start = 0
        end = 0
        start_char = 0
        opoint = point()
        call syntax_ppss(ppss)
        if (ppss%depth /= 0 .and. .not. scss_buffer()) return
        end = point()
        moved = skip_chars_backward("-[:alnum:]", st)
        start_char = preceding_char()
        start = point()
        found = .true.
        call goto_char(opoint)
    end subroutine complete_selector

    !> Complete current symbol at point.
    !> Currently supports completion of CSS properties, property values,
    !> pseudo-elements, pseudo-classes, at-rules, bang-rules, and HTML
    !> tags, classes and IDs.  Port of css-mode.el:css-completion-at-point.
    recursive subroutine css_completion_at_point(data, status)
        type(capf_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        type(capf_data_type) :: prop
        integer(int64) :: sel_beg, sel_end
        logical :: sel_found
        integer :: sel_char

        status = status_type()
        call complete_bang_rule(data)
        if (data%found) return
        call complete_property_value(data)
        if (data%found) return
        call complete_pseudo_element_or_class(data)
        if (data%found) return
        call complete_at_rule(data)
        if (data%found) return
        call complete_property(prop)
        call complete_selector(sel_found, sel_beg, sel_end, sel_char)
        if (.not. prop%found .and. .not. sel_found) return
        merge_prop = prop%found
        merge_sel = sel_found
        merge_sel_char = sel_char
        data%found = .true.

        if (prop%found) then
            data%start = prop%start
            data%end = prop%end
        else
            data%start = sel_beg
            data%end = sel_end
        end if

        data%table%kind = TABLE_FUNCTION
        data%table%fn => merged_table
        data%exit_function => property_exit_function
    end subroutine css_completion_at_point

    ! ------------------------------------------------------------------
    ! Colors
    ! ------------------------------------------------------------------

    ! The number STR denotes (string-to-number).
    ! Not a port.
    real(real64) function to_number(str)
        character(len=*), intent(in) :: str
        integer :: ios

        to_number = 0
        if (len(str) == 0) return
        read (str, *, iostat=ios) to_number
        if (ios /= 0) to_number = 0
    end function to_number

    ! N rounded to the nearest integer, ties to even (round).
    ! Not a port.
    integer function round_int(n)
        real(real64), intent(in) :: n

        round_int = int(ieee_rint(n))
    end function round_int

    ! "#" and the two-digit hex forms of COMPONENTS.
    ! Not a port.
    function hex_string(components) result(s)
        integer, intent(in) :: components(:)
        character(len=:), allocatable :: s
        character(len=2) :: buf
        integer :: i

        s = "#"

        do i = 1, size(components)
            write (buf, "(z2.2)") components(i)
            s = s//buf
        end do

        s = lower(s)
    end function hex_string

    ! S with its ASCII letters in lower case.
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

    ! The text from START to END.
    ! Not a port.
    function text_between(start, end) result(text)
        integer(int64), intent(in) :: start, end
        character(len=:), allocatable :: text
        type(status_type) :: st

        call buffer_substring(start, end, text, st)
        if (.not. allocated(text)) text = ""
    end function text_between

    ! The character after POS, -1 if none.
    ! Not a port.
    integer function after_char_at(pos)
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
    integer function before_char_at(pos)
        integer(int64), intent(in) :: pos
        integer(int32) :: c
        logical :: found

        before_char_at = -1
        if (pos <= point_min() .or. pos > point_max()) return
        call char_before(c, found, pos)
        if (found) before_char_at = c
    end function before_char_at

    ! Skip blanks and comments.  Port of css-mode.el:css--color-skip-blanks.
    subroutine color_skip_blanks()
        do
            if (.not. forward_comment(1_int64)) exit
        end do
    end subroutine color_skip_blanks

    !> Parse a CSS rgb() or rgba() color.
    !> Point should be just after the open paren.
    !> Returns a hex RGB color, or `found' false if the color could not be
    !> recognized.  This recognizes CSS-color-4 extensions.
    !> When INCLUDE_ALPHA is true, the alpha component is included in
    !> the returned hex string.  Port of css-mode.el:css--rgb-color.
    subroutine css_rgb_color(color, found, include_alpha)
        character(len=:), allocatable, intent(out) :: color
        logical, intent(out) :: found
        logical, intent(in), optional :: include_alpha
        type(status_type) :: st
        integer :: result(4), iter, n
        logical :: alpha, is_percent
        real(real64) :: number
        character(len=:), allocatable :: str

        color = ""
        found = .false.
        alpha = .false.
        if (present(include_alpha)) alpha = include_alpha
        n = 0
        iter = 0

        do while (iter < 4)
            call color_skip_blanks()
            if (.not. looking_at(NUMBER_OR_PERCENT_RE, st)) return
            is_percent = mb(1) > 0

            if (is_percent) then
                str = match_string_no_properties(1)
            else
                str = match_string_no_properties(2)
            end if

            number = to_number(str)

            if (is_percent) then
                number = 255*(number/100.0_real64)
            else if (alpha .and. iter == 3) then
                number = number*255
            end if

            n = n + 1
            result(n) = min(max(0, round_int(number)), 255)
            call goto_char(me(0))
            call color_skip_blanks()
            iter = iter + 1

            ! Accept a superset of the CSS syntax since I'm feeling lazy.
            if (skip_chars_forward(",/", st) == 0 .and. iter == 3) then
                ! The alpha is optional.
                iter = iter + 1
            end if

            call color_skip_blanks()
        end do

        if (looking_at(")", st)) then
            call forward_char(st, 1_int64)
            found = .true.

            if (alpha .and. n == 4) then
                color = hex_string(result(1:4))
            else
                color = hex_string(result(1:3))
            end if
        end if
    end subroutine css_rgb_color

    !> Parse a CSS hsl() or hsla() color.
    !> Point should be just after the open paren.
    !> Returns a hex RGB color, or `found' false if the color could not be
    !> recognized.  This recognizes CSS-color-4 extensions.  Port of
    !> css-mode.el:css--hsl-color.
    subroutine css_hsl_color(color, found)
        character(len=:), allocatable, intent(out) :: color
        logical, intent(out) :: found
        real(real64), parameter :: PI = 4*atan(1.0_real64)
        type(status_type) :: st
        real(real64) :: hue, number, result(3), rgb(3)
        character(len=:), allocatable :: unit
        integer(int64) :: moved
        integer :: i

        color = ""
        found = .false.
        ! First parse the hue.
        call color_skip_blanks()
        if (.not. looking_at(ANGLE_RE, st)) return
        hue = to_number(match_string_no_properties(1))
        unit = match_string_no_properties(2)
        call goto_char(me(0))

        ! Note that here "turn" is just passed through.
        if (unit == "" .or. unit == "deg") then
            ! Degrees.
            hue = hue/360.0_real64
        else if (unit == "grad") then
            hue = hue/400.0_real64
        else if (unit == "rad") then
            hue = hue/(2*PI)
        end if

        result(1) = modulo(hue, 1.0_real64)

        do i = 2, 3
            moved = skip_chars_forward(",", st)
            call color_skip_blanks()
            if (.not. looking_at(PERCENT_RE, st)) return
            number = to_number(match_string_no_properties(1))
            number = number/100.0_real64
            result(i) = min(max(number, 0.0_real64), 1.0_real64)
            call goto_char(me(0))
            call color_skip_blanks()
        end do

        call color_skip_blanks()

        ! Accept a superset of the CSS syntax since I'm feeling lazy.
        if (skip_chars_forward(",/", st) > 0) then
            call color_skip_blanks()
            if (.not. looking_at(NUMBER_OR_PERCENT_RE, st)) return
            call goto_char(me(0))
            call color_skip_blanks()
        end if

        if (looking_at(")", st)) then
            call forward_char(st, 1_int64)
            rgb = color_hsl_to_rgb(result(1), result(2), result(3))
            color = color_rgb_to_hex(rgb(1), rgb(2), rgb(3), 2)
            found = .true.
        end if
    end subroutine css_hsl_color

    !> Convert a CSS hex color to an Emacs hex color.
    !> STR is the incoming CSS hex color.
    !> This function simply drops any transparency.  Port of
    !> css-mode.el:css--hex-color.
    function css_hex_color(str) result(hex)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: hex

        ! Either #RGB or #RRGGBB, drop the "A" or "AA".
        if (len(str) > 5) then
            hex = str(1:min(7, len(str)))
        else
            hex = str(1:min(4, len(str)))
        end if
    end function css_hex_color

    !> Return the alpha component of CSS color HEX.
    !> HEX can either be in the #RGBA or #RRGGBBAA format.  `found' is
    !> false if the color doesn't have an alpha component.  Port of
    !> css-mode.el:css--hex-alpha.
    subroutine css_hex_alpha(hex, alpha, found)
        character(len=*), intent(in) :: hex
        character(len=:), allocatable, intent(out) :: alpha
        logical, intent(out) :: found

        alpha = ""
        found = .true.

        select case (len(hex))
        case (5)
            alpha = hex(5:5)
        case (9)
            alpha = hex(8:9)
        case default
            found = .false.
        end select
    end subroutine css_hex_alpha

    ! The index of the named color NAME, 0 if none.
    ! Not a port.
    integer function color_index(name) result(k)
        character(len=*), intent(in) :: name

        do k = 1, size(COLOR_NAMES)
            if (trim(COLOR_NAMES(k)) == name) return
        end do

        k = 0
    end function color_index

    !> Check whether STR, seen at point, is CSS named color.
    !> Returns its hex value (`found') if it is a valid color.  Special care
    !> is taken to exclude some SCSS constructs.  Port of
    !> css-mode.el:css--named-color.
    subroutine css_named_color(start_point, str, color, found)
        integer(int64), intent(in) :: start_point
        character(len=*), intent(in) :: str
        character(len=:), allocatable, intent(out) :: color
        logical, intent(out) :: found
        type(status_type) :: st
        integer(int64) :: opoint, moved
        logical :: moved_any
        integer :: k

        color = ""
        found = .false.
        k = color_index(str)
        if (k == 0) return
        opoint = point()
        call goto_char(start_point)
        moved_any = forward_comment(-point())
        moved = skip_chars_backward("@[:alpha:]", st)

        if (.not. looking_at("@\(mixin\|include\)", st, inhibit_modify=.true.)) then
            color = trim(COLOR_HEX(k))
            found = .true.
        end if

        call goto_char(opoint)
    end subroutine css_named_color

    !> Return the CSS color at point.
    !> Point should be just after the start of a CSS color, as recognized
    !> by `css__colors_regexp'.  START_POINT is the start of the color,
    !> and MATCH is the string matched by the regexp.
    !>
    !> This function will either return the color, as a hex RGB string;
    !> or `found' false if no color could be recognized.  When this function
    !> returns, point will be at the end of the recognized color.  Port of
    !> css-mode.el:css--compute-color.
    subroutine css_compute_color(start_point, match, color, found)
        integer(int64), intent(in) :: start_point
        character(len=*), intent(in) :: match
        character(len=:), allocatable, intent(out) :: color
        logical, intent(out) :: found

        color = ""
        found = .false.
        if (len(match) == 0) return

        if (match(1:1) == "#") then
            color = css_hex_color(match)
            found = .true.
        else if (match == "rgb(" .or. match == "rgba(") then
            call css_rgb_color(color, found)
        else if (match == "hsl(" .or. match == "hsla(") then
            call css_hsl_color(color, found)
        else
            ! Evaluate to the color if the name is found.
            call css_named_color(start_point, match, color, found)
        end if
    end subroutine css_compute_color

    !> Fontify a CSS buffer between BEG and END.
    !> BEG and END are buffer positions.  Port of
    !> css-mode.el:css--fontify-region.
    recursive subroutine css_fontify_region(beg, end, loudly, status, done_beg, done_end)
        integer(int64), intent(in) :: beg, end
        logical, intent(in) :: loudly
        type(status_type), intent(out) :: status
        integer(int64), intent(out) :: done_beg, done_end
        type(silent_modifications_type) :: silent
        type(parse_state_type) :: ppss
        type(status_type) :: st
        character(len=:), allocatable :: color
        integer(int64) :: opoint, found_at, start
        logical :: saved_fold, found

        call font_lock_default_fontify_region(beg, end, loudly, status, done_beg, done_end)
        if (.not. status%ok()) return
        if (.not. css_fontify_colors%value) return
        opoint = point()
        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        call goto_char(done_beg)

        do
            call re_search_forward(COLORS_RE, found_at, st, bound=done_end, noerror=SEARCH_NOERROR)
            if (found_at == 0) exit
            ! Skip comments and strings.
            call syntax_ppss(ppss)

            if (ppss%comstr_start == 0) then
                start = mb(0)
                call css_compute_color(start, match_string_no_properties(0), color, found)

                if (found) then
                    call silent_modifications_begin(silent)
                    ! Use the color as the background, to make it more
                    ! clear.  Use a contrasting color as the foreground, to
                    ! make it readable.  Finally, have a small box using the
                    ! existing foreground color, to make sure it stands out
                    ! a bit from any other text; in particular this is nice
                    ! when the color matches the buffer's background color.
                    call put_text_property(start, point(), sym("face"), &
                                           list(sym(":background"), make_string(color), sym(":foreground"), &
                                                make_string(readable_foreground_color(color)), sym(":box"), &
                                                list(sym(":line-width"), make_fixnum(-1_int64))), st)
                    call silent_modifications_end(silent)
                end if
            end if
        end do

        case_fold_search%value = saved_fold
        call goto_char(opoint)
    end subroutine css_fontify_region

    ! ------------------------------------------------------------------
    ! Color formats
    ! ------------------------------------------------------------------

    !> Convert the CSS color HEX to four digits per component.
    !> CSS colors use one or two digits per component for RGB hex
    !> values.  Convert the given color to four digits per component.
    !>
    !> Note that this function handles CSS colors specifically, and
    !> should not be mixed with those in color_fm.  Port of
    !> css-mode.el:css--color-to-4-dpc.
    function css_color_to_4_dpc(hex) result(out)
        character(len=*), intent(in) :: hex
        character(len=:), allocatable :: out
        integer :: width, i, j

        out = "#"

        if (len(hex) == 7) then
            width = 2
        else
            width = 1
        end if

        do i = 2, len(hex), width
            do j = 1, 4/width
                out = out//hex(i:min(i + width - 1, len(hex)))
            end do
        end do
    end function css_color_to_4_dpc

    !> Format a CSS hex color by shortening it if possible.  Port of
    !> css-mode.el:css--format-hex.
    function css_format_hex(hex) result(out)
        character(len=*), intent(in) :: hex
        character(len=:), allocatable :: out
        integer :: i
        logical :: same

        out = hex
        if (len(hex) < 6) return
        same = .true.

        do i = 2, len(hex), 2
            if (i + 1 > len(hex)) then
                same = .false.
            else if (hex(i:i) /= hex(i + 1:i + 1)) then
                same = .false.
            end if
        end do

        if (.not. same) return
        out = "#"

        do i = 2, len(hex), 2
            out = out//hex(i:i)
        end do
    end function css_format_hex

    ! The word around point (thing-at-point 'word), "" for none.
    ! Not a port.
    function word_at_point() result(word)
        character(len=:), allocatable :: word
        type(status_type) :: st
        integer(int64) :: opoint, b, e, moved

        opoint = point()
        moved = skip_chars_backward("[:alnum:]", st)
        b = point()
        call goto_char(opoint)
        moved = skip_chars_forward("[:alnum:]", st)
        e = point()
        call goto_char(opoint)
        word = text_between(b, e)
    end function word_at_point

    !> Convert named CSS color at point to hex format.
    !> Return true if a conversion was made.
    !>
    !> Note that this function handles CSS colors specifically, and
    !> should not be mixed with those in color_fm.  Port of
    !> css-mode.el:css--named-color-to-hex.
    logical function css_named_color_to_hex() result(converted)
        type(status_type) :: st
        type(handle_type) :: mark
        character(len=:), allocatable :: color
        logical :: found

        converted = .false.
        call excursion_begin(mark)

        if (.not. looking_at(COLORS_RE, st)) then
            if (preceding_char() /= iachar("#")) call backward_word(1_int64)
        end if

        if (color_index(word_at_point()) > 0) then
            if (looking_at(COLORS_RE, st)) continue
            call css_compute_color(point(), match_string_no_properties(0), color, found)
            call replace_match(css_format_hex(color), st)
            converted = .true.
        end if

        call excursion_end(mark)
    end function css_named_color_to_hex

    !> Return ALPHA component formatted for use in rgba().  Port of
    !> css-mode.el:css--format-rgba-alpha.
    function css_format_rgba_alpha(alpha) result(s)
        real(real64), intent(in) :: alpha
        character(len=:), allocatable :: s
        character(len=32) :: buf
        real(real64) :: a

        write (buf, "(f0.2)") alpha
        a = to_number(trim(buf))

        if (a == 0 .or. a == 1) then
            write (buf, "(i0)") int(a)
            s = trim(buf)
            return
        end if

        ! (number-to-string of a number of two decimals.)
        write (buf, "(f0.2)") a
        s = trim(buf)
        if (s(1:1) == ".") s = "0"//s

        if (len(s) > 2) then
            if (s(1:2) == "-.") s = "-0"//s(2:)
        end if

        do while (s(len(s):len(s)) == "0")
            s = s(1:len(s) - 1)
        end do
    end function css_format_rgba_alpha

    !> Convert CSS hex color at point to RGB format.
    !> Return true if a conversion was made.
    !>
    !> Note that this function handles CSS colors specifically, and
    !> should not be mixed with those in color_fm.  Port of
    !> css-mode.el:css--hex-to-rgb.
    logical function css_hex_to_rgb() result(converted)
        type(status_type) :: st
        type(handle_type) :: mark
        character(len=:), allocatable :: hex, rgb, alpha, text
        character(len=64) :: buf
        real(real64) :: comps(3)
        integer :: r, g, b, digits, value
        logical :: found, has_alpha

        converted = .false.
        call excursion_begin(mark)

        if (following_char() /= iachar("#") .and. preceding_char() /= iachar("(")) then
            call backward_sexp(1_int64, st)
        end if

        if (looking_at(COLORS_RE, st)) then
            hex = match_string_no_properties(0)

            if (len(hex) > 0) then
                if (hex(1:1) == "#") then
                    rgb = css_hex_color(hex)
                    call color_name_to_rgb(css_color_to_4_dpc(rgb), comps, found)

                    if (found) then
                        r = round_int(comps(1)*255)
                        g = round_int(comps(2)*255)
                        b = round_int(comps(3)*255)
                        call css_hex_alpha(hex, alpha, has_alpha)

                        if (has_alpha) then
                            value = hex_value(alpha)
                            digits = len(alpha)
                            write (buf, '("rgba(",i0,", ",i0,", ",i0,", ")') r, g, b
                            text = trim(buf)//" "//css_format_rgba_alpha(value/real(16**digits - 1, real64))//")"
                        else
                            write (buf, '("rgb(",i0,", ",i0,", ",i0,")")') r, g, b
                            text = trim(buf)
                        end if

                        call replace_match(text, st, fixedcase=.true.)
                        converted = .true.
                    end if
                end if
            end if
        end if

        call excursion_end(mark)
    end function css_hex_to_rgb

    !> Convert CSS RGB color at point to a named color or hex format.
    !> Convert to a named color if the color at point has a name, else
    !> convert to hex format.  Return true if a conversion was made.
    !>
    !> Note that this function handles CSS colors specifically, and
    !> should not be mixed with those in color_fm.  Port of
    !> css-mode.el:css--rgb-to-named-color-or-hex.
    logical function css_rgb_to_named_color_or_hex() result(converted)
        type(parse_state_type) :: ppss
        type(status_type) :: st
        type(handle_type) :: mark
        character(len=:), allocatable :: color
        integer(int64) :: open_paren_pos, start, moved, here
        logical :: found, rgba_before
        integer :: k

        converted = .false.
        call excursion_begin(mark)
        call syntax_ppss(ppss)
        open_paren_pos = ppss%innermost_start

        if (open_paren_pos > 0) then
            here = point()
            call goto_char(open_paren_pos)
            rgba_before = looking_back("rgba?", st, point() - 4)
            call goto_char(here)

            if (rgba_before) then
                call syntax_ppss(ppss)
                call goto_char(ppss%innermost_start)
            end if
        end if

        if (preceding_char() == iachar(")")) call backward_sexp(1_int64, st)
        moved = skip_chars_backward("rgba", st)

        if (looking_at(COLORS_RE, st)) then
            start = me(0)
            here = point()
            call goto_char(start)
            call css_rgb_color(color, found, include_alpha=.true.)
            call goto_char(here)

            if (found) then
                call kill_sexp(1_int64, st)
                call kill_sexp(1_int64, st)
                k = 0

                do k = 1, size(COLOR_HEX)
                    if (trim(COLOR_HEX(k)) == color) exit
                end do

                if (k <= size(COLOR_HEX)) then
                    call insert(trim(COLOR_NAMES(k)), st)
                else
                    call insert(css_format_hex(color), st)
                end if

                converted = .true.
            end if
        end if

        call excursion_end(mark)
    end function css_rgb_to_named_color_or_hex

    !> Cycle the color at point between different CSS color formats.
    !> Supported formats are by name (if possible), hexadecimal, and
    !> rgb()/rgba().  Port of css-mode.el:css-cycle-color-format.
    recursive subroutine css_cycle_color_format(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (css_named_color_to_hex()) return
        if (css_hex_to_rgb()) return
        if (css_rgb_to_named_color_or_hex()) return
        call message("It doesn't look like a color at point")
    end subroutine css_cycle_color_format

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

    ! The number the hexadecimal digits S denote (string-to-number S 16).
    ! Not a port.
    integer function hex_value(s) result(n)
        character(len=*), intent(in) :: s
        integer :: i, d

        n = 0

        do i = 1, len(s)
            d = index("0123456789abcdef", lower(s(i:i))) - 1
            if (d < 0) return
            n = n*16 + d
        end do
    end function hex_value

    ! ------------------------------------------------------------------
    ! Defun names
    ! ------------------------------------------------------------------

    ! STR with every occurrence of FROM replaced by TO.
    ! Not a port.
    function replace_all(str, from, to) result(out)
        character(len=*), intent(in) :: str, from, to
        character(len=:), allocatable :: out
        integer :: i

        out = ""
        i = 1

        do while (i <= len(str))
            if (i + len(from) - 1 <= len(str)) then
                if (str(i:i + len(from) - 1) == from) then
                    out = out//to
                    i = i + len(from)
                    cycle
                end if
            end if

            out = out//str(i:i)
            i = i + 1
        end do
    end function replace_all

    !> Join a list of nested CSS selectors (the trimmed elements of
    !> SELECTORS).  Port of css-mode.el:css--join-nested-selectors.
    function css_join_nested_selectors(selectors) result(joined)
        character(len=*), intent(in) :: selectors(:)
        character(len=:), allocatable :: joined
        character(len=:), allocatable :: sel, prev, last
        integer :: i

        joined = ""
        prev = ""
        last = ""

        do i = 1, size(selectors)
            sel = trim(selectors(i))

            if (index(sel, "&") > 0) then
                sel = replace_all(sel, "&", prev)
                ! (pop processed): drop the previous selector.
                joined = joined(1:len(joined) - len(last))
            else if (i > 1) then
                ! Unless this is the first selector, separate this one and
                ! the previous one by a space.
                joined = joined//" "
            end if

            joined = joined//sel
            last = sel
            prev = sel
        end do
    end function css_join_nested_selectors

    !> Move to the start of the CSS section before point, returning
    !> whether there is one (imenu-prev-index-position-function).  Port of
    !> css-mode.el:css--prev-index-position.
    recursive logical function css_prev_index_position() result(found)
        type(parse_state_type) :: ppss
        type(status_type) :: st
        integer(int64) :: pos
        logical :: moved

        found = .false.
        call syntax_ppss(ppss)
        if (ppss%incomment /= 0) call goto_char(comment_beginning())
        moved = forward_comment(-point())
        call search_backward("{", pos, st, bound=point_min(), noerror=SEARCH_NOERROR)
        if (pos == 0) return
        call re_search_backward("}\|;\|{", pos, st, bound=point_min(), noerror=SEARCH_NOERROR)

        if (pos > 0) then
            call forward_char(st, 1_int64)
        else
            call goto_char(point_min())
        end if

        moved = forward_comment(point_max())
        pos = point()
        call re_search_forward("[^{;]*", pos, st, noerror=SEARCH_NOERROR)
        found = pos > 0
        call goto_char(point() - (me(0) - mb(0)))
        ! (The match data stays that of the name, for
        ! `css_extract_index_name'.)
    end function css_prev_index_position

    ! STR with each run of newlines and spaces made one space, trimmed
    ! (string-trim of replace-regexp-in-string).
    ! Not a port.
    function squeeze(str) result(out)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: out
        integer :: i, b, e

        out = ""

        do i = 1, len(str)
            if (str(i:i) == LF .or. str(i:i) == " ") then
                if (len(out) > 0) then
                    if (out(len(out):len(out)) == " ") cycle
                end if

                out = out//" "
            else
                out = out//str(i:i)
            end if
        end do

        b = verify(out, " "//TAB//LF//achar(13))
        e = verify(out, " "//TAB//LF//achar(13), back=.true.)

        if (b == 0) then
            out = ""
        else
            out = out(b:e)
        end if
    end function squeeze

    !> The name of the CSS section whose name the last search matched,
    !> with those it is nested in (imenu-extract-index-name-function).
    !> Port of css-mode.el:css--extract-index-name.
    recursive function css_extract_index_name() result(name)
        character(len=:), allocatable :: name
        type(parse_state_type) :: ppss
        type(status_type) :: st
        character(len=200), allocatable :: res(:)
        integer(int64) :: opoint, pos
        logical :: moved

        opoint = point()
        res = [character(len=200) :: match_string_no_properties(0)]

        do
            call syntax_ppss(ppss)
            if (ppss%innermost_start == 0) exit
            call goto_char(ppss%innermost_start)
            call re_search_backward("}\|;\|{", pos, st, bound=point_min(), noerror=SEARCH_NOERROR)

            if (pos > 0) then
                call forward_char(st, 1_int64)
            else
                call goto_char(point_min())
            end if

            moved = forward_comment(point_max())
            pos = point()
            call re_search_forward("[^{;]*", pos, st, noerror=SEARCH_NOERROR)
            if (pos > 0) res = [character(len=200) :: match_string_no_properties(0), res]
            call goto_char(mb(0))
        end do

        name = join_squeezed(res)
        call goto_char(opoint)
    end function css_extract_index_name

    ! css--join-nested-selectors of the squeezed elements of RES.
    ! Not a port.
    function join_squeezed(res) result(name)
        character(len=*), intent(in) :: res(:)
        character(len=:), allocatable :: name
        character(len=200), allocatable :: parts(:)
        integer :: i

        allocate (parts(size(res)))

        do i = 1, size(res)
            parts(i) = squeeze(res(i))
        end do

        name = css_join_nested_selectors(parts)
    end function join_squeezed

    !> Return the name of the CSS section at point (`found' false for
    !> none).  Port of css-mode.el:css-current-defun-name.
    recursive subroutine css_current_defun_name(name, found)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: found
        integer(int64) :: opoint

        name = ""
        opoint = point()
        found = css_prev_index_position()
        if (found) name = css_extract_index_name()
        call goto_char(opoint)
    end subroutine css_current_defun_name

    !> A helper for `css_lookup_symbol' that finds the symbol at point.
    !> Returns the symbol, a string, or `found' false if none found.  Port of
    !> css-mode.el:css--mdn-find-symbol.
    subroutine css_mdn_find_symbol(symbol, found)
        character(len=:), allocatable, intent(out) :: symbol
        logical, intent(out) :: found
        type(status_type) :: st
        integer(int64) :: opoint, moved, bound, pos

        symbol = ""
        found = .false.
        opoint = point()
        ! Skip any whitespace between the word and point.
        moved = skip_chars_backward("- "//TAB, st)
        ! Skip backward over a word.
        moved = skip_chars_backward("-[:alnum:]", st)
        ! Now skip ":" or "@" to see if it's a pseudo-element or at-id.
        moved = skip_chars_backward("@:", st)

        if (looking_at(MDN_SYMBOL_RE, st)) then
            symbol = match_string_no_properties(0)
            found = .true.
        else
            bound = line_beginning_position()
            call re_search_backward(MDN_PROPERTY_RE, pos, st, bound=bound, noerror=SEARCH_NOERROR)

            if (pos > 0) then
                symbol = match_string_no_properties(1)
                found = .true.
            end if
        end if

        call goto_char(opoint)
    end subroutine css_mdn_find_symbol

    ! ------------------------------------------------------------------
    ! Font Lock
    ! ------------------------------------------------------------------

    ! A highlight of subexpression N in FACE.
    ! Not a port.
    function h(n, face, override) result(hl)
        integer, intent(in) :: n
        character(len=*), intent(in) :: face
        integer, intent(in), optional :: override
        type(font_lock_highlight_type) :: hl

        hl = font_lock_highlight(n, face=face, override=override)
    end function h

    ! The FACE of "^[ \t]*{": mark the text from the end of the selector
    ! before it as a multiline element; no face.
    ! Port of css-mode.el:css--font-lock-keywords (in part).
    subroutine brace_multiline(face)
        character(len=:), allocatable, intent(out) :: face
        type(status_type) :: st
        integer(int64) :: opoint, moved

        face = ""
        opoint = point()
        call goto_char(mb(0))
        moved = skip_chars_backward(" "//LF//TAB, st)
        call put_text_property(point(), me(0), sym("font_lock_multiline"), sym("t"), st)
        call goto_char(opoint)
    end subroutine brace_multiline

    ! The FACE of a property name: vendor-specific or not.
    ! Port of css-mode.el:css--font-lock-keywords (in part).
    subroutine property_face(face)
        character(len=:), allocatable, intent(out) :: face

        if (me(2) > 0) then
            face = "css_proprietary_property"
        else
            face = "css_property"
        end if
    end subroutine property_face

    !> The Font Lock keywords of CSS mode, of SCSS if SASSY.  Port of
    !> css-mode.el:css--font-lock-keywords.
    function css_font_lock_keywords_of(sassy) result(kw)
        logical, intent(in) :: sassy
        type(font_lock_keyword_type) :: kw(8)
        type(font_lock_highlight_type) :: h1(1), h2(2)
        character(len=20), allocatable :: ids(:)

        ids = bang_ids()
        h1(1) = h(0, "font_lock_builtin_face")
        kw(1) = font_lock_keyword("!\s-*"//regexp_opt(ids), highlights=h1)

        ! Atrules keywords.  IDs not in css-at-ids are valid (ignored).
        if (sassy) then
            kw(2) = font_lock_keyword(SASSY_KW2_RE, highlights=h1)
        else
            kw(2) = font_lock_keyword(KW2_RE, highlights=h1)
        end if

        ! Selectors.
        ! Allow plain ":root" as a selector.
        h1(1) = h(1, "css_selector", FONT_LOCK_OVERRIDE_KEEP)
        kw(3) = font_lock_keyword(KW3_RE, highlights=h1)

        if (sassy) then
            kw(4) = font_lock_keyword(SASSY_KW4_RE, highlights=h1)
        else
            kw(4) = font_lock_keyword(KW4_RE, highlights=h1)
        end if

        ! In the above rule, we allow the open-brace to be on some
        ! subsequent line.  This will only work if we properly mark the
        ! intervening text as being part of a multiline element (and even
        ! then, this only ensures proper refontification, but not proper
        ! discovery).
        h1(1) = font_lock_highlight(0, face_function=brace_multiline)
        kw(5) = font_lock_keyword(KW5_RE, highlights=h1)
        ! Variables.
        h1(1) = h(0, "font_lock_variable_name_face")
        kw(6) = font_lock_keyword(KW6_RE, highlights=h1)
        ! Properties.  Again, we don't limit ourselves to css-property-ids.
        h1(1) = font_lock_highlight(1, face_function=property_face)
        kw(7) = font_lock_keyword(KW7_RE, highlights=h1)
        ! Make sure the parens in a url(...) expression receive the
        ! default face. This is done because the parens may sometimes
        ! receive generic string delimiter syntax (see
        ! `css_syntax_propertize_function').
        h2(1) = h(1, "default", FONT_LOCK_OVERRIDE_T)
        h2(2) = h(2, "default", FONT_LOCK_OVERRIDE_T)
        kw(8) = font_lock_keyword(KW8_RE, highlights=h2)
    end function css_font_lock_keywords_of

    !> The Font Lock keywords of SCSS mode.  Port of
    !> css-mode.el:scss-font-lock-keywords.
    function scss_font_lock_keywords() result(kw)
        type(font_lock_keyword_type) :: kw(10)
        type(font_lock_highlight_type) :: h1(1)

        h1(1) = h(0, "font_lock_variable_name_face")
        kw(1) = font_lock_keyword(SCSS_VARIABLE_RE, highlights=h1)
        kw(2:9) = css_font_lock_keywords_of(.true.)
        h1(1) = h(1, "font_lock_function_name_face")
        kw(10) = font_lock_keyword(SCSS_MIXIN_RE, highlights=h1)
    end function scss_font_lock_keywords

    ! css-syntax-propertize-function.
    ! Port of css-mode.el:css-syntax-propertize-function.
    recursive subroutine css_syntax_propertize_function(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call goto_char(start)
        call propertize_rules%apply(start, end, status)
    end subroutine css_syntax_propertize_function

    ! ------------------------------------------------------------------
    ! Indentation
    ! ------------------------------------------------------------------

    ! Return true if point looks to be inside a CSS selector.
    ! This function is intended to be good enough to help SMIE during
    ! tokenization, but should not be regarded as a reliable function for
    ! determining whether point is within a selector.  Port of
    ! css-mode.el:css--colon-inside-selector-p.
    logical function colon_inside_selector_p()
        type(status_type) :: st
        integer(int64) :: opoint, pos

        opoint = point()
        call re_search_forward("[{};]", pos, st, noerror=SEARCH_NOERROR)
        colon_inside_selector_p = preceding_char() == iachar("{")
        call goto_char(opoint)
    end function colon_inside_selector_p

    ! Return true if point is inside a function call.  Port of
    ! css-mode.el:css--colon-inside-funcall.
    logical function colon_inside_funcall()
        type(parse_state_type) :: ppss

        colon_inside_funcall = .false.
        call syntax_ppss(ppss)
        if (ppss%innermost_start == 0) return
        colon_inside_funcall = after_char_at(ppss%innermost_start) == iachar("(")
    end function colon_inside_funcall

    !> Whether the "}" before point doesn't end an interpolation #{$...}.
    !> Port of css-mode.el:scss-smie--not-interpolation-p.
    logical function scss_smie_not_interpolation_p()
        type(status_type) :: st
        integer(int64) :: opoint

        opoint = point()
        call forward_char(st, -1_int64)
        scss_smie_not_interpolation_p = skip_chars_backward("-[:alnum:]", st) == 0

        if (.not. scss_smie_not_interpolation_p) then
            scss_smie_not_interpolation_p = .not. looking_back("#{\$", st, point() - 3)
        end if

        call goto_char(opoint)
    end function scss_smie_not_interpolation_p

    ! The token of a colon after point: in a selector or function call, or
    ! of a property.
    ! Port of css-mode.el:css-smie--forward-token (in part).
    function colon_token() result(token)
        character(len=:), allocatable :: token

        if (colon_inside_selector_p()) then
            token = ":"
        else if (colon_inside_funcall()) then
            token = ":"
        else
            token = ":-property"
        end if
    end function colon_token

    !> The SMIE forward token function of CSS mode.  Port of
    !> css-mode.el:css-smie--forward-token.
    recursive subroutine css_smie_forward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        logical :: moved, hanging
        integer :: c

        status = status_type()
        hanging = .false.

        if (preceding_char() == iachar("}")) then
            if (scss_smie_not_interpolation_p()) then
                ! FIXME: If the next char is not whitespace, what should we do?
                c = following_char()

                if (c == iachar(" ") .or. c == 9 .or. c == 10) then
                    hanging = .true.
                    call forward_char(status, 1_int64)
                else if (allocated(comment_start_skip%value)) then
                    if (looking_at(comment_start_skip%value, status)) then
                        hanging = .true.
                        moved = forward_comment(1_int64)
                    end if
                end if
            end if
        end if

        if (hanging) then
            token = ";"
            return
        end if

        moved = forward_comment(point_max())

        if (looking_at("[;,:]", status)) then
            call forward_char(status, 1_int64)

            if (match_string_no_properties(0) == ":") then
                token = colon_token()
            else
                token = match_string_no_properties(0)
            end if
        else
            call smie_default_forward_token(token, status)
        end if
    end subroutine css_smie_forward_token

    !> The SMIE backward token function of CSS mode.  Port of
    !> css-mode.el:css-smie--backward-token.
    recursive subroutine css_smie_backward_token(token, status)
        character(len=:), allocatable, intent(out) :: token
        type(status_type), intent(out) :: status
        integer(int64) :: pos
        logical :: moved
        integer :: c

        status = status_type()
        pos = point()
        moved = forward_comment(-point())
        c = preceding_char()

        if (c == iachar("}")) then
            ! FIXME: If the next char is not whitespace, what should we do?
            if (scss_smie_not_interpolation_p()) then
                if (pos > point()) then
                    token = ";"
                    return
                end if
            end if
        end if

        if (c == iachar(";") .or. c == iachar(",") .or. c == iachar(":")) then
            call forward_char(status, -1_int64)

            if (following_char() == iachar(":")) then
                token = colon_token()
            else
                token = achar(following_char())
            end if
        else
            call smie_default_backward_token(token, status)
        end if
    end subroutine css_smie_backward_token

    !> The SMIE rules of CSS mode.  Port of css-mode.el:css-smie-rules.
    recursive subroutine css_smie_rules(method, arg, result, status)
        integer, intent(in) :: method
        character(len=*), intent(in) :: arg
        type(smie_rule_result_type), intent(out) :: result
        type(status_type), intent(out) :: status
        type(smie_sexp_type) :: res
        type(smie_indent_type) :: indent

        status = status_type()
        result = smie_rule_result_type()

        select case (method)
        case (SMIE_ELEM)
            if (arg == "basic") then
                result = smie_rule_result_type(kind=SMIE_RULE_OFFSET, value=int(css_indent_offset%value))
            else if (arg == "arg") then
                result = smie_rule_result_type(kind=SMIE_RULE_OFFSET, value=0)
            end if
        case (SMIE_LIST_INTRO)
            ! "" stands for BOB (bug#15467).
            if (arg == ";" .or. arg == "" .or. arg == ":-property") result = smie_rule_result_type(kind=SMIE_RULE_T)
        case (SMIE_BEFORE)
            if (arg == "{") then
                if (smie_rule_hanging_p(status) .or. smie_rule_bolp()) then
                    call smie_backward_sexp(res, status, token=";")

                    if (following_char() /= iachar("{")) then
                        indent = smie_indent_virtual(status)

                        if (indent%kind == SMIE_INDENT_COLUMN) then
                            result = smie_rule_result_type(kind=SMIE_RULE_COLUMN, value=indent%column)
                        end if
                    end if
                end if
            else if (arg == "(") then
                if (smie_rule_hanging_p(status)) then
                    result = smie_rule_parent(status, 0)
                else if (.not. smie_rule_bolp()) then
                    result = smie_rule_result_type(kind=SMIE_RULE_OFFSET, value=0)
                end if
            end if
        case (SMIE_AFTER)
            if (arg == ":-property") then
                if (smie_rule_hanging_p(status)) then
                    result = smie_rule_result_type(kind=SMIE_RULE_OFFSET, value=int(css_indent_offset%value))
                end if
            end if
        case default
            continue
        end select
    end subroutine css_smie_rules

    ! ------------------------------------------------------------------
    ! Filling
    ! ------------------------------------------------------------------

    ! The paragraph regexps of filling a comment whose comment-continue is
    ! CONTINUE: START and SEPARATE.
    ! Port of css-mode.el:css-fill-paragraph (in part).
    subroutine comment_paragraph_regexps(start, separate)
        character(len=:), allocatable, intent(inout) :: start, separate
        type(status_type) :: st
        character(len=:), allocatable :: cont, skip, end_skip

        if (.not. allocated(comment_continue%value)) return
        cont = comment_continue%value
        if (string_match("[^ "//TAB//"]", cont, st) == 0) return
        skip = ""
        if (allocated(comment_start_skip%value)) skip = comment_start_skip%value
        end_skip = ""
        if (allocated(comment_end_skip%value)) end_skip = comment_end_skip%value
        separate = "\(?:[ "//TAB//"]*\(?:"//regexp_quote(cont)//"\|"//skip//"\|"//end_skip//"\)\)?"// &
                   "\(?:"//separate//"\)"
        start = "\(?:[ "//TAB//"]*"//regexp_quote(cont)//"\)?\(?:"//start//"\)"
    end subroutine comment_paragraph_regexps

    ! S with the characters special in regexps quoted (regexp-quote).
    ! Not a port.
    function regexp_quote(s) result(q)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: q
        integer :: i

        q = ""

        do i = 1, len(s)
            if (index("[*.\?+^$", s(i:i)) > 0) then
                q = q//"\"//s(i:i)
            else
                q = q//s(i:i)
            end if
        end do
    end function regexp_quote

    !> Fill the comment or reformat the declaration block at point (the
    !> `fill_paragraph_function' of CSS mode; JUSTIFY as in
    !> `fill_paragraph').  Port of css-mode.el:css-fill-paragraph.
    recursive subroutine css_fill_paragraph(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify
        type(handle_type) :: mark, end_marker, begv_marker, zv_marker
        type(parse_state_type) :: ppss
        type(status_type) :: st
        character(len=:), allocatable :: saved_start, saved_separate, start, separate
        integer(int64) :: eol, cstart, pos, begv, zv, end, here, opening
        logical :: moved, ok, has_end
        integer :: c

        status = status_type()
        done = .false.
        call excursion_begin(mark)
        ! Fill succeeding comment when invoked right before a multi-line
        ! comment.
        here = point()
        call goto_char(line_beginning_position())
        cstart = comment_search_forward(line_end_position(), .true., st)
        call goto_char(here)
        if (cstart > 0) call goto_char(me(0))
        call syntax_ppss(ppss)
        eol = line_end_position()
        ok = .false.

        if (ppss%incomment /= 0) then
            here = point()
            call goto_char(ppss%comstr_start)
            moved = forward_comment(1_int64)
            ok = .not. bolp()
            eol = point()
            call goto_char(here)
        end if

        if (ok) then
            ! Filling inside a comment whose comment-end marker is not \n.
            ! This code is meant to be generic, so that it works not only
            ! for css-mode but for all modes.
            ! save-restriction.
            call copy_marker_at(buffers(cur)%begv, begv_marker, .false., st)
            call copy_marker_at(buffers(cur)%zv, zv_marker, .true., st)
            call narrow_to_region(ppss%comstr_start, eol, st)
            ! Will define comment-continue.
            call comment_normalize_vars(st)
            saved_start = paragraph_start%value
            saved_separate = paragraph_separate%value
            start = saved_start
            separate = saved_separate
            call comment_paragraph_regexps(start, separate)
            paragraph_start%value = start
            paragraph_separate%value = separate
            call fill_without_function(status, justify)
            paragraph_start%value = saved_start
            paragraph_separate%value = saved_separate
            call widen()
            call marker_position(begv_marker, begv, st)
            call marker_position(zv_marker, zv, st)
            call narrow_to_region(begv, zv, st)
            call free_marker(begv_marker, st)
            call free_marker(zv_marker, st)
            ! Don't try filling again.
            done = .true.
        else if (ppss%comstr_start == 0) then
            ok = ppss%innermost_start > 0

            if (.not. ok) then
                here = point()
                call down_list(1_int64, st)

                if (st%ok() .and. point() <= eol) then
                    call syntax_ppss(ppss)
                    ok = .true.
                else
                    call goto_char(here)
                end if
            end if

            if (ok) then
                opening = ppss%innermost_start
                call goto_char(opening)
                here = point()
                call forward_sexp(1_int64, st)
                has_end = st%ok()
                if (has_end) call copy_marker_at(point(), end_marker, .true., st)
                call goto_char(here)

                if (has_end) then
                    do
                        call marker_position(end_marker, end, st)
                        call re_search_forward("[{;}]", pos, st, bound=end, noerror=SEARCH_NOERROR)
                        if (pos == 0) exit
                        call reformat_after_delimiter()
                    end do

                    call goto_char(opening)
                    call marker_position(end_marker, end, st)
                    call indent_region(line_beginning_position(2_int64), end, st)
                    call free_marker(end_marker, st)
                    ! Don't use the default filling code.
                    done = .true.
                end if
            end if
        end if

        call excursion_end(mark)

    contains

        ! The step of the declaration block loop after a "{", ";" or "}".
        subroutine reformat_after_delimiter()
            type(parse_state_type) :: p
            type(handle_type) :: saved
            integer(int64) :: line_end, back
            logical :: again

            call syntax_ppss(p)
            ! This is a false positive inside a string or comment.
            if (p%comstr_start > 0) return
            ! This is a false positive when encountering an interpolated
            ! variable (bug#19751).
            if (before_char_at(point() - 1) == iachar("#")) return
            c = preceding_char()

            if (c == iachar("}")) then
                call excursion_begin(saved)
                call forward_char(st, -1_int64)
                back = skip_chars_backward(" "//TAB, st)

                if (.not. bolp()) then
                    if (scss_smie_not_interpolation_p()) call newline(st)
                end if

                call excursion_end(saved)
            else
                do
                    line_end = line_end_position()
                    if (.not. forward_comment(1_int64)) exit
                    if (.not. point() > line_end) exit
                    ! A multi-line comment should be on its own line.
                    call excursion_begin(saved)
                    moved = forward_comment(-1_int64)
                    again = point() < line_end
                    if (again) call newline(st)
                    call excursion_end(saved)
                    if (.not. again) exit
                end do

                if (point() < line_end) call newline(st)
            end if
        end subroutine reformat_after_delimiter
    end subroutine css_fill_paragraph

    ! fill-paragraph with fill-paragraph-function bound to nil.
    ! Not a port.
    recursive subroutine fill_without_function(status, justify)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: justify

        call fill_paragraph_function%set_local(no_fill_function)
        call fill_paragraph(status, justify)
        call fill_paragraph_function%set_local(css_fill_paragraph)
    end subroutine fill_without_function

    ! A `fill_paragraph_function' that leaves filling to `fill_paragraph'.
    ! Not a port.
    recursive subroutine no_fill_function(status, done, justify)
        type(status_type), intent(out) :: status
        logical, intent(out) :: done
        integer, intent(in), optional :: justify

        status = status_type()
        done = .false.
        if (present(justify)) continue
    end subroutine no_fill_function

    !> The `adaptive_fill_function' of CSS mode: the fill prefix " *" for
    !> the start of a comment.  Port of css-mode.el:css-adaptive-fill.
    subroutine css_adaptive_fill(prefix, found)
        character(len=:), allocatable, intent(out) :: prefix
        logical, intent(out) :: found
        type(status_type) :: st
        character(len=:), allocatable :: str
        integer(int64) :: k

        prefix = ""
        found = .false.
        if (.not. looking_at("[ "//TAB//"]*/\*[ "//TAB//"]*", st)) return
        str = match_string_no_properties(0)
        k = string_match("/\*", str, st)
        if (k == 0) return
        call replace_match(" *", st, fixedcase=.true., literal=.true., string=str, result=prefix)
        found = .true.
    end subroutine css_adaptive_fill

    ! ------------------------------------------------------------------
    ! Modes
    ! ------------------------------------------------------------------

    ! The faces.
    ! Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => css_selector
        call f%define("css_selector", "Face to use for selectors.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_function_name_face"))
        f => css_property
        call f%define("css_property", "Face to use for properties.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_keyword_face"))
        f => css_proprietary_property
        call f%define("css_proprietary_property", "Face to use for vendor-specific properties.")
        call f%spec_entry(display_when(), face_attrs(inherits=[character(len=12) :: "css_property", "italic"]))
    end subroutine define_faces

    ! css-mode-syntax-table and scss-mode-syntax-table.
    ! Not a port.
    subroutine init_syntax_tables()
        type(status_type) :: st
        type(handle_type) :: t
        integer :: i
        character(len=*), parameter :: PUNCT = "!$%&+,<>=?"

        call make_syntax_table(css_mode_syntax_table, st)
        t = css_mode_syntax_table
        ! C-style comments.
        call modify_syntax_entry(iachar("/"), iachar("/"), ". 14", st, t)
        call modify_syntax_entry(iachar("*"), iachar("*"), ". 23b", st, t)
        ! Strings.
        call modify_syntax_entry(iachar(""""), iachar(""""), """", st, t)
        call modify_syntax_entry(iachar("'"), iachar("'"), """", st, t)
        ! Blocks.
        call modify_syntax_entry(iachar("{"), iachar("{"), "(}", st, t)
        call modify_syntax_entry(iachar("}"), iachar("}"), "){", st, t)
        ! Args in url(...) thingies and other "function calls".
        call modify_syntax_entry(iachar("("), iachar("("), "()", st, t)
        call modify_syntax_entry(iachar(")"), iachar(")"), ")(", st, t)
        ! To match attributes in selectors.
        call modify_syntax_entry(iachar("["), iachar("["), "(]", st, t)
        call modify_syntax_entry(iachar("]"), iachar("]"), ")[", st, t)
        ! Special chars that sometimes come at the beginning of words.
        ! We'll treat them as symbol constituents.
        call modify_syntax_entry(iachar("@"), iachar("@"), "_", st, t)
        call modify_syntax_entry(iachar("#"), iachar("#"), "_", st, t)
        call modify_syntax_entry(iachar("."), iachar("."), "_", st, t)
        ! Distinction between words and symbols.
        call modify_syntax_entry(iachar("-"), iachar("-"), "_", st, t)

        do i = 1, len(PUNCT)
            call modify_syntax_entry(iachar(PUNCT(i:i)), iachar(PUNCT(i:i)), ".", st, t)
        end do

        call make_syntax_table(scss_mode_syntax_table, st, css_mode_syntax_table)
        t = scss_mode_syntax_table
        call modify_syntax_entry(iachar("/"), iachar("/"), ". 124", st, t)
        call modify_syntax_entry(10, 10, ">", st, t)
        ! Variable names are prefixed by $.
        call modify_syntax_entry(iachar("$"), iachar("$"), "_", st, t)
        call modify_syntax_entry(iachar("%"), iachar("%"), "_", st, t)
    end subroutine init_syntax_tables

    ! css-syntax-propertize-function's rules and css-smie-grammar.
    ! Not a port.
    subroutine init_rules()
        type(syntax_highlight_type) :: hl(2)
        type(smie_precs_type) :: precs
        type(smie_prec2_type) :: prec2
        type(status_type) :: st

        allocate (propertize_rules%rules(1))
        propertize_rules%rules(1)%regexp = URI_RE
        hl(1) = syntax_highlight_type(group=1, syntax="|")
        hl(2) = syntax_highlight_type(group=2, syntax="|")
        propertize_rules%rules(1)%highlights = hl
        precs = smie_precs_type()
        call precs%add(SMIE_ASSOC, [character(len=1) :: ";"])
        ! Colons that belong to a CSS property.  These get a higher
        ! precedence than other colons, such as colons in selectors, which
        ! are represented by a plain ":" token.
        call precs%add(SMIE_LEFT, [character(len=10) :: ":-property"])
        call precs%add(SMIE_ASSOC, [character(len=1) :: ","])
        call precs%add(SMIE_ASSOC, [character(len=1) :: ":"])
        call smie_precs_to_prec2(precs, prec2)
        call smie_prec2_to_grammar(prec2, css_smie_grammar, st)
    end subroutine init_rules

    !> Generic mode to edit Cascading Style Sheets (CSS).
    !>
    !> This is a generic major mode intended to be inherited by a concrete
    !> implementation.  Currently there is one concrete implementation:
    !> `css_mode'.  Port of css-mode.el:css-base-mode.
    recursive subroutine css_base_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. syntax_table_live_p(css_mode_syntax_table)) call syms_of_css_mode()
        call major_modes%css_base_mode%begin(saved, status)
        if (.not. status%ok()) return
        call comment_start%set_local("/*")
        call comment_start_skip%set_local("/\*+[ "//TAB//"]*")
        call comment_end%set_local("*/")
        call comment_end_skip%set_local("[ "//TAB//"]*\*+/")
        call set_local_electric_indent_chars([css_electric_keys, electric_indent_chars_value()])
        call major_modes%css_base_mode%finish(saved, status)
    end subroutine css_base_mode

    !> Major mode to edit Cascading Style Sheets (CSS).
    !> This mode provides syntax highlighting, indentation, completion, and
    !> documentation lookup for CSS.
    !>
    !> Use `\[completion_at_point]' to complete CSS properties, property
    !> values, pseudo-elements, pseudo-classes, at-rules, bang-rules, and
    !> HTML tags, classes and IDs.  Completion candidates for HTML class
    !> names and IDs are found by looking through open HTML mode buffers.
    !>
    !> Use `\[fill_paragraph]' to reformat CSS declaration blocks.  It can
    !> also be used to fill comments.  Port of css-mode.el:css-mode.
    recursive subroutine css_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. syntax_table_live_p(css_mode_syntax_table)) call syms_of_css_mode()
        call major_modes%css_mode%begin(saved, status)
        if (.not. status%ok()) return
        call font_lock_defaults%set_local(keywords=css_font_lock_keywords, case_fold=.true.)
        call syntax_propertize_function%set_local(css_syntax_propertize_function)
        call fill_paragraph_function%set_local(css_fill_paragraph)
        call adaptive_fill_function%set_local(css_adaptive_fill)
        call smie_setup(css_smie_grammar, css_smie_rules, css_smie_forward_token, css_smie_backward_token)
        call set_local_font_lock_fontify_region_function(css_fontify_region)
        call completion_at_point_functions%add(css_completion_at_point, local=.true.)
        call major_modes%css_mode%finish(saved, status)
    end subroutine css_mode

    !> Major mode to edit "Sassy CSS" files.  Port of css-mode.el:scss-mode.
    recursive subroutine scss_mode(status)
        type(status_type), intent(out) :: status
        integer :: k
        logical :: saved

        if (.not. syntax_table_live_p(css_mode_syntax_table)) call syms_of_css_mode()
        call major_modes%scss_mode%begin(saved, status)
        if (.not. status%ok()) return
        call comment_start%set_local("// ")
        call comment_end%set_local("")
        call comment_continue%set_local(" *")
        call comment_start_skip%set_local("/[*/]+[ "//TAB//"]*")
        call comment_end_skip%set_local("[ "//TAB//"]*\(?:"//LF//"\|\*+/\)")
        ! css--at-ids, css--bang-ids and css--nested-selectors-allowed.
        k = local_index(.true.)
        if (k > 0) locals(k)%scss = .true.
        call font_lock_defaults%set_local(keywords=scss_font_lock_keywords(), case_fold=.true.)
        call major_modes%scss_mode%finish(saved, status)
    end subroutine scss_mode

    ! The command `css_cycle_color_format'.
    recursive subroutine cmd_css_cycle_color_format(status)
        type(status_type), intent(out) :: status

        call css_cycle_color_format(status)
    end subroutine cmd_css_cycle_color_format

    ! Bind [remap FROM] to COMMAND in `css_mode_map'.
    ! Not a port.
    subroutine bind_remap(from, command)
        use lisp_fm, only: list_elements
        character(len=*), intent(in) :: from, command
        type(value_type), allocatable :: events(:)
        type(status_type) :: st

        call list_elements(list(sym("remap"), sym(from)), events)
        call define_key(css_mode_map, events, sym(command), st)
    end subroutine bind_remap

    !> Define CSS mode and SCSS mode: their variables, tables, keymaps and
    !> commands.
    !> Not a port.
    recursive subroutine syms_of_css_mode()
        type(status_type) :: st

        if (syntax_table_live_p(css_mode_syntax_table)) return
        call forward_setting(css_fontify_colors)
        call forward_setting(css_indent_offset)
        call forward_setting(css_lookup_url_format)

        if (.not. allocated(css_lookup_url_format%value)) then
            css_lookup_url_format%value = "https://developer.mozilla.org/en-US/docs/Web/CSS/%s?raw&macros"
        end if

        if (.not. allocated(css_electric_keys)) css_electric_keys = [iachar("}"), iachar(";")]
        call define_faces()
        call init_syntax_tables()
        call init_rules()
        css_font_lock_keywords = css_font_lock_keywords_of(.false.)
        call add_local_variables_killer(kill_css_locals)
        call major_modes%css_base_mode%define("css_base_mode", "CSS", css_base_mode, css_base_mode_hook, &
                                              parent=major_modes%prog_mode, syntax_table=css_mode_syntax_table)
        css_base_mode_map = major_modes%css_base_mode%map
        call major_modes%css_mode%define("css_mode", "CSS", css_mode, css_mode_hook, parent=major_modes%css_base_mode)
        css_mode_map = major_modes%css_mode%map
        call major_modes%scss_mode%define("scss_mode", "SCSS", scss_mode, scss_mode_hook, parent=major_modes%css_mode, &
                                          syntax_table=scss_mode_syntax_table)
        scss_mode_map = major_modes%scss_mode%map
        ! `info_complete_symbol' is not used.
        call bind_remap("complete_symbol", "completion_at_point")
        call keymap_set(css_mode_map, "C-c C-f", "css_cycle_color_format", st)
        commands%css_base_mode%run => css_base_mode
        commands%css_mode%run => css_mode
        commands%scss_mode%run => scss_mode
        commands%css_cycle_color_format%run => cmd_css_cycle_color_format
    end subroutine syms_of_css_mode
end module css_mode_fm

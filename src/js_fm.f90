! js_fm.f90 -- major mode for editing JavaScript.
! Ported from GNU Emacs 31.1 lisp/progmodes/js.el.
!
! Copyright (C) 2008-2026 Free Software Foundation, Inc.
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

!> JavaScript mode (`js_mode', `js_base_mode', `javascript_mode') and
!> `js_json_mode': the syntax table and regexp literals
!> (`js_syntax_propertize'), the three Font Lock levels (with the class
!> declarations of `js_enabled_frameworks', variable declarations and
!> formal parameters), the parse cache of functions, macros and classes
!> (`js__ensure_cache') with `js_syntactic_context' and
!> `js_beginning_of_subprogram'/`js_end_of_subprogram', and indentation
!> (`js_indent_line', with C comments lined up by `js_comment_lineup_func').
!>
!> Deferred: JSX (`js_jsx_syntax', its detection, `js_jsx_mode' and its
!> indentation and keywords), filling (`js_fill_paragraph',
!> `js_do_auto_fill' and the CC Mode paragraph variables they need),
!> `comment_line_break_function' (`c_indent_new_comment_line'), Imenu,
!> `which_func', `prettify_symbols_table', `electric_layout_rules', the
!> hideshow variables, `js_use_syntactic_mode_name', the obsolete MozRepl
!> variables and `js_ts_mode'.
module js_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, list, make_list, make_fixnum, fixnump, xfixnum, list_elements, consp
    use lread_fm, only: sym
    use data_fm, only: integer_setting, logical_setting, forward_setting, add_local_variables_killer
    use commands_fm, only: commands
    use buffer_fm, only: buffers, cur, buffer_serial_live
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes
    use syntax_fm, only: make_syntax_table, modify_syntax_entry, syntax_table_live_p, syntax_ppss, parse_state_type, &
                         parse_partial_expression, forward_comment, skip_chars_forward, skip_chars_backward, skip_syntax_forward, &
                         skip_syntax_backward, parse_expression_ignore_comments, ST_STRING_STYLE, COMMENTSTOP_SYNTAX_TABLE
    use editfns_fm, only: point, point_min, point_max, goto_char, bolp, eolp, bobp, eobp, char_after, char_before, &
                          line_beginning_position, line_end_position, narrow_to_region, widen, buffer_substring
    use cmds_fm, only: forward_line, forward_char, beginning_of_line, end_of_line
    use insdel_fm, only: insert, del_range, before_change_functions
    use indent_fm, only: current_column, current_indentation, indent_line_to, indent_line_function
    use search_fm, only: looking_at, re_search_forward, re_search_backward, search_backward, string_match, &
                         match_beginning, match_end, case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use subr_fm, only: match_string_no_properties, looking_back, silent_modifications_type, silent_modifications_begin, &
                       silent_modifications_end
    use simple_fm, only: back_to_indentation
    use textprop_fm, only: put_text_property, get_text_property, next_single_property_change, &
                           previous_single_property_change, remove_text_properties
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                            font_lock_keywords_type, font_lock_keywords_level, font_lock_match_highlight_type, &
                            FONT_LOCK_OVERRIDE_T
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight, font_lock_anchored, cpp_font_lock_keywords, &
                            font_lock_apply_highlight, CPP_FONT_LOCK_KEYWORDS_SOURCE_DIRECTIVES
    use expression_motion_fm, only: forward_expression, backward_expression, forward_group, backward_group, up_group, &
                                    beginning_of_subprogram_function, end_of_subprogram_function
    use newcomment_fm, only: comment_start, comment_start_skip, comment_end
    use electric_fm, only: electric_indent_chars_value, set_local_electric_indent_chars
    use prog_mode_fm, only: prog_first_column
    use syntax_propertize_fm, only: syntax_propertize_function, syntax_propertize_rules_type, syntax_highlight_type, &
                                    syntax_property_type, &
                                    syntax_propertize_extend_region_functions, syntax_propertize_multiline
    use cc_align_fm, only: c_lineup_c_comments, c_lineup_proc
    implicit none(type, external)
    private

    public :: syms_of_js, js_base_mode, js_mode, js_json_mode, js_indent_line, js_beginning_of_subprogram, &
              js_end_of_subprogram, js_syntactic_context, js_syntax_propertize, js_font_lock_syntactic_face_function, &
              js_re_search_forward, js_re_search_backward, js_continued_expression_p, js_proper_indentation, &
              js_flush_caches, js_ensure_cache

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    !> Values of `js_indent_first_init': nil, t and `dynamic'.
    integer, parameter, public :: JS_INDENT_FIRST_INIT_NIL = 0, JS_INDENT_FIRST_INIT_T = 1, &
                                  JS_INDENT_FIRST_INIT_DYNAMIC = 2

    !> The syntactic contexts of `js_syntactic_context'.
    integer, parameter, public :: JS_CONTEXT_TOPLEVEL = 0, JS_CONTEXT_FUNCTION = 1, JS_CONTEXT_MACRO = 2, &
                                  JS_CONTEXT_CLASS = 3

    !> Number of spaces for each indentation step in `js_mode'.  Port of
    !> js.el js-indent-level.
    type(integer_setting), target, public :: js_indent_level = integer_setting(name="js_indent_level", value=4)

    !> Number of additional spaces for indenting continued expressions.
    !> The value must be no less than minus `js_indent_level'.  Port of
    !> js.el js-expr-indent-offset.
    type(integer_setting), target, public :: js_expr_indent_offset = &
                                             integer_setting(name="js_expr_indent_offset", value=0)

    !> Number of additional spaces for indenting expressions in
    !> parentheses.  The value must be no less than minus
    !> `js_indent_level'.  Port of js.el js-paren-indent-offset.
    type(integer_setting), target, public :: js_paren_indent_offset = &
                                             integer_setting(name="js_paren_indent_offset", value=0)

    !> Number of additional spaces for indenting expressions in square
    !> braces.  The value must be no less than minus `js_indent_level'.
    !> Port of js.el js-square-indent-offset.
    type(integer_setting), target, public :: js_square_indent_offset = &
                                             integer_setting(name="js_square_indent_offset", value=0)

    !> Number of additional spaces for indenting expressions in curly
    !> braces.  The value must be no less than minus `js_indent_level'.
    !> Port of js.el js-curly-indent-offset.
    type(integer_setting), target, public :: js_curly_indent_offset = &
                                             integer_setting(name="js_curly_indent_offset", value=0)

    !> Number of additional spaces for indenting the contents of a switch
    !> block.  The value must not be negative.  Port of js.el
    !> js-switch-indent-offset.
    type(integer_setting), target, public :: js_switch_indent_offset = &
                                             integer_setting(name="js_switch_indent_offset", value=0)

    !> Treat nested functions as top-level functions in `js_mode'.
    !> This applies to function movement, marking, and so on.  Port of
    !> js.el js-flat-functions.
    type(logical_setting), target, public :: js_flat_functions = logical_setting(name="js_flat_functions", value=.false.)

    !> Align continuation of non-empty ([{ lines in `js_mode'.  Port of
    !> js.el js-indent-align-list-continuation.
    type(logical_setting), target, public :: js_indent_align_list_continuation = &
                                             logical_setting(name="js_indent_align_list_continuation", value=.true.)

    !> Lineup function for `cc_mode_style', for C comments in `js_mode'.
    !> Port of js.el js-comment-lineup-func (its default is
    !> `c_lineup_c_comments').
    procedure(c_lineup_proc), pointer, public :: js_comment_lineup_func => null()

    !> Frameworks recognized by `js_mode'.
    !> To improve performance, you may turn off some frameworks you seldom
    !> use, either globally or on a per-buffer basis.  Port of js.el
    !> js-enabled-frameworks (the names of `js__available_frameworks').
    character(len=12), allocatable, public :: js_enabled_frameworks(:)

    !> True means specially indent the first variable declaration's
    !> initializer.  Normally, the first declaration's initializer is
    !> unindented, and subsequent declarations have their identifiers
    !> aligned with it:
    !>
    !>     var o = {
    !>         foo: 3
    !>     };
    !>
    !>     var o = {
    !>         foo: 3
    !>     },
    !>         bar = 2;
    !>
    !> If this option has the value JS_INDENT_FIRST_INIT_T, indent the
    !> first declaration's initializer by an additional level:
    !>
    !>     var o = {
    !>             foo: 3
    !>         };
    !>
    !>     var o = {
    !>             foo: 3
    !>         },
    !>         bar = 2;
    !>
    !> If this option has the value JS_INDENT_FIRST_INIT_DYNAMIC, if there
    !> is only one declaration, don't indent the first one's initializer;
    !> otherwise, indent it.
    !>
    !>     var o = {
    !>         foo: 3
    !>     };
    !>
    !>     var o = {
    !>             foo: 3
    !>         },
    !>         bar = 2;
    !>
    !> Port of js.el js-indent-first-init.
    type(integer_setting), target, public :: js_indent_first_init = &
                                             integer_setting(name="js_indent_first_init", value=JS_INDENT_FIRST_INIT_NIL)

    !> Use "chained" indentation.
    !> Chained indentation applies when the current line starts with ".".
    !> If the previous expression also contains a "." at the same level,
    !> then the "."s will be lined up:
    !>
    !>     let x = svg.mumble()
    !>                .chained;
    !>
    !> Port of js.el js-chain-indent.
    type(logical_setting), target, public :: js_chain_indent = logical_setting(name="js_chain_indent", value=.false.)

    !> Hook run after entering `js_base_mode'.  Port of the
    !> js-base-mode-hook define-derived-mode defines.
    type(hook_type), target, public :: js_base_mode_hook = hook_type(name="js_base_mode_hook")
    !> Hook run after entering `js_mode'.  Port of the js-mode-hook
    !> define-derived-mode defines.
    type(hook_type), target, public :: js_mode_hook = hook_type(name="js_mode_hook")
    !> Hook run after entering `js_json_mode'.  Port of the
    !> js-json-mode-hook define-derived-mode defines.
    type(hook_type), target, public :: js_json_mode_hook = hook_type(name="js_json_mode_hook")

    !> Keymap for `js_mode'.
    type(handle_type), public :: js_mode_map
    !> Keymap for `js_base_mode'.
    type(handle_type), public :: js_base_mode_map
    !> Keymap for `js_json_mode'.
    type(handle_type), public :: js_json_mode_map
    !> Syntax table for `js_mode'.  Port of js.el js-mode-syntax-table.
    type(handle_type), public :: js_mode_syntax_table

    ! js--available-frameworks.
    character(len=12), parameter :: AVAILABLE_FRAMEWORKS(6) = [character(len=12) :: "javascript", "mochikit", &
                                                                                       "prototype", "dojo", "extjs", "merrillpress"]

    ! js--class-styles: the name, class declaration regexp, :prototype and
    ! framework of each style (all have the context `toplevel').
    integer, parameter :: NSTYLES = 10

    ! The types of a pitem: toplevel, function, macro, or a class (the index
    ! of its style plus PITEM_CLASS).
    integer, parameter :: PITEM_TOPLEVEL = 0, PITEM_FUNCTION = 1, PITEM_MACRO = 2, PITEM_CLASS = 100

    ! js--pitem: a construct of the parse cache (see js.el): its paren
    ! depth after h-end, type, h-begin, name (its dot-separated parts;
    ! `anonymous' for t) and b-end (0 for nil).  Children (for Imenu) are
    ! not kept.
    type :: pitem_type
        integer(int64) :: paren_depth = 0
        integer :: type = PITEM_TOPLEVEL
        integer(int64) :: h_begin = 0
        logical :: anonymous = .false.
        character(len=:), allocatable :: name
        integer(int64) :: b_end = 0
    end type pitem_type

    ! One buffer's parse cache and frameworks: js--cache-end, the pitems
    ! (identified by their index; 1 is js--initial-pitem), js--quick-match-re
    ! and js--quick-match-re-func, and its own `js_enabled_frameworks' (if
    ! `has_frameworks').
    type :: js_local_type
        integer(int64) :: serial = 0
        integer(int64) :: cache_end = 1
        type(pitem_type), allocatable :: pitems(:)
        integer :: npitems = 0
        character(len=:), allocatable :: quick_match_re, quick_match_re_func
        logical :: has_frameworks = .false.
        character(len=12), allocatable :: frameworks(:)
    end type js_local_type

    type(js_local_type), allocatable, target :: locals(:)

    ! js--guess-function-name-start (0 for nil).
    integer(int64) :: guess_function_name_start = 0

    ! js--tmp-location (0 for nil).
    integer(int64) :: tmp_location = 0

    ! The syntax-propertize rules of `js_syntax_propertize'.
    type(syntax_propertize_rules_type) :: propertize_rules

    ! The END of the current `js_syntax_propertize'.
    integer(int64) :: propertize_end = 0

    ! The keywords of the three levels.
    type(font_lock_keyword_type), allocatable :: keywords_1(:), keywords_2(:), keywords_3(:)

    ! The regexps of js.el.
    character(len=*), parameter :: JS_NAME_START_RE = &
                                   "[[:alpha:]_$]"
    character(len=*), parameter :: JS_NAME_RE = &
                                   "[[:alpha:]_$]\(?:\s_\|\sw\)*"
    character(len=*), parameter :: JS_OBJFIELD_RE = &
                                   "[[:alpha:]_$]\(?:\s_\|\sw\)*:"
    character(len=*), parameter :: JS_DOTTED_NAME_RE = &
                                   "[[:alpha:]_$]\(?:\s_\|\sw\)*\(?:\.[[:alpha:]_$]\(?" &
                                   //":\s_\|\sw\)*\)*"
    character(len=*), parameter :: JS_OPT_CPP_START = &
                                   "^\s-*#\s-*\([[:alnum:]]+\)"
    character(len=*), parameter :: JS_PLAIN_METHOD_RE = &
                                   "^\s-*?\([[:alpha:]_$]\(?:\s_\|\sw\)*\(?:\.[[:alpha" &
                                   //":]_$]\(?:\s_\|\sw\)*\)*\)\.prototype\.\([[:alpha:]" &
                                   //"_$]\(?:\s_\|\sw\)*\)\s-*?=\s-*?\(\(?:async[ "//TAB//LF//"]+\)" &
                                   //"function\)\_>"
    character(len=*), parameter :: JS_PLAIN_CLASS_RE = &
                                   "^\s-*\([[:alpha:]_$]\(?:\s_\|\sw\)*\(?:\.[[:alpha:" &
                                   //"]_$]\(?:\s_\|\sw\)*\)*\)\.prototype\s-*=\s-*{"
    character(len=*), parameter :: JS_MP_CLASS_DECL_RE = &
                                   "^\s-*var\s-+\([[:alpha:]_$]\(?:\s_\|\sw\)*\)\s-*=\" &
                                   //"s-*\([[:alpha:]_$]\(?:\s_\|\sw\)*\(?:\.[[:alpha:]_" &
                                   //"$]\(?:\s_\|\sw\)*\)*\)\.extend\(?:Final\)?\s-*(\s-" &
                                   //"*{?\s-*$"
    character(len=*), parameter :: JS_PROTOTYPE_OBSOLETE_CLASS_DECL_RE = &
                                   "^\s-*\(?:var\s-+\)?\([[:alpha:]_$]\(?:\s_\|\sw\)*\" &
                                   //"(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)\s-*=\s-*Cla" &
                                   //"ss\.create()"
    character(len=*), parameter :: JS_PROTOTYPE_OBJEXTEND_CLASS_DECL_RE_1 = &
                                   "^\s-*Object\.extend\s-*(\([[:alpha:]_$]\(?:\s_\|\s" &
                                   //"w\)*\(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)\s-*,\s" &
                                   //"-*{"
    character(len=*), parameter :: JS_PROTOTYPE_OBJEXTEND_CLASS_DECL_RE_2 = &
                                   "^\s-*\(?:var\s-+\)?\([[:alpha:]_$]\(?:\s_\|\sw\)*\" &
                                   //"(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)\s-*=\s-*Obj" &
                                   //"ect\.extend\s-*("
    character(len=*), parameter :: JS_PROTOTYPE_CLASS_DECL_RE = &
                                   "^\s-*\(?:var\s-+\)?\([[:alpha:]_$]\(?:\s_\|\sw\)*\" &
                                   //")\s-*=\s-*Class\.create\s-*(\s-*\(?:\([[:alpha:]_$" &
                                   //"]\(?:\s_\|\sw\)*\(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*" &
                                   //"\)*\)\s-*,\s-*\)?{?"
    character(len=*), parameter :: JS_DOJO_CLASS_DECL_RE = &
                                   "^\s-*dojo\.declare\s-*(""\([[:alpha:]_$]\(?:\s_\|\s" &
                                   //"w\)*\(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)"
    character(len=*), parameter :: JS_EXTJS_CLASS_DECL_RE_1 = &
                                   "^\s-*Ext\.extend\s-*(\s-*\([[:alpha:]_$]\(?:\s_\|\" &
                                   //"sw\)*\(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)\s-*,\" &
                                   //"s-*\([[:alpha:]_$]\(?:\s_\|\sw\)*\(?:\.[[:alpha:]_" &
                                   //"$]\(?:\s_\|\sw\)*\)*\)"
    character(len=*), parameter :: JS_EXTJS_CLASS_DECL_RE_2 = &
                                   "^\s-*\(?:var\s-+\)?\([[:alpha:]_$]\(?:\s_\|\sw\)*\" &
                                   //")\s-*=\s-*Ext\.extend\s-*(\s-*\([[:alpha:]_$]\(?:\" &
                                   //"s_\|\sw\)*\(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)"
    character(len=*), parameter :: JS_MOCHIKIT_CLASS_RE = &
                                   "^\s-*MochiKit\.Base\.update\s-*(\s-*\([[:alpha:]_$" &
                                   //"]\(?:\s_\|\sw\)*\(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*" &
                                   //"\)*\)"
    character(len=*), parameter :: JS_FUNCTION_HEADING_1_RE = &
                                   "^\s-*function\(?:\s-\|\*\)+\([[:alpha:]_$]\(?:\s_\" &
                                   //"|\sw\)*\)"
    character(len=*), parameter :: JS_FUNCTION_HEADING_2_RE = &
                                   "^\s-*\([[:alpha:]_$]\(?:\s_\|\sw\)*\)\s-*:\s-*func" &
                                   //"tion\_>"
    character(len=*), parameter :: JS_FUNCTION_HEADING_3_RE = &
                                   "^\s-*\(?:var\s-+\)?\([[:alpha:]_$]\(?:\s_\|\sw\)*\" &
                                   //"(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)\s-*=\s-*fun" &
                                   //"ction\_>"
    character(len=*), parameter :: JS_MACRO_DECL_RE = &
                                   "^\s-*#\s-*define\s-+\([[:alpha:]_$]\(?:\s_\|\sw\)*" &
                                   //"\)\s-*("
    character(len=*), parameter :: JS_KEYWORD_RE = &
                                   "\_<\(a\(?:bstract\|sync\|wait\)\|break\|c\(?:a\(?:" &
                                   //"se\|tch\)\|lass\|on\(?:st\|tinue\)\)\|d\(?:e\(?:bu" &
                                   //"gger\|fault\|lete\)\|o\)\|e\(?:lse\|num\|x\(?:port" &
                                   //"\|tends\)\)\|f\(?:inal\(?:ly\)?\|or\|unction\)\|go" &
                                   //"to\|i\(?:mp\(?:lements\|ort\)\|n\(?:stanceof\|terf" &
                                   //"ace\)\|[fn]\)\|let\|n\(?:ative\|ew\)\|of\|p\(?:ack" &
                                   //"age\|r\(?:ivate\|otected\)\|ublic\)\|return\|s\(?:" &
                                   //"tatic\|uper\|witch\|ynchronized\)\|t\(?:hrows?\|r\" &
                                   //"(?:ansient\|y\)\|ypeof\)\|using\|v\(?:ar\|o\(?:id\" &
                                   //"|latile\)\)\|w\(?:hile\|ith\)\|yield\)\_>"
    character(len=*), parameter :: JS_BASIC_TYPE_RE = &
                                   "\_<\(b\(?:oolean\|yte\)\|char\|double\|float\|int\" &
                                   //"|long\|short\|void\)\_>"
    character(len=*), parameter :: JS_CONSTANT_RE = &
                                   "\_<\(Infinity\|NaN\|arguments\|false\|null\|t\(?:h" &
                                   //"is\|rue\)\|undefined\)\_>"
    character(len=*), parameter :: REGEXP_LITERAL_RE = &
                                   "/\(?:[^/[\]\|\\.\|\[\(?:[^]\]\|\\.\)*]\)*\(/?\)"
    character(len=*), parameter :: UNARY_KEYWORD_RE = &
                                   "\_<\(await\|delete\|typeof\|\(?:voi\|yiel\)d\)\_>"
    character(len=*), parameter :: POSSIBLY_BRACELESS_KEYWORD_RE = &
                                   "\_<\(catch\|do\|e\(?:ach\|lse\)\|f\(?:inally\|or\)" &
                                   //"\|if\|try\|w\(?:hile\|ith\)\)\_>"
    character(len=*), parameter :: DECLARATION_KEYWORD_RE = &
                                   "\<\(const\|let\|using\|var\)\>"
    character(len=*), parameter :: INDENT_OPERATOR_RE = &
                                   "[-+*/%<>&^|?:.]\([^-+*/.]\|$\)\|!?=\|\_<\(in\(?:st" &
                                   //"anceof\)?\)\_>"
    character(len=*), parameter :: LINE_TERMINATING_ARROW_RE = &
                                   "=>\s-*\(/[/*]\|$\)"
    character(len=*), parameter :: CLASS_PART_RE = &
                                   "\([[:alpha:]_$]\(?:\s_\|\sw\)*\)\(?:\.\|.*$\)"
    character(len=*), parameter :: DOJO_MATCH_1_RE = &
                                   "^\s-*dojo\.declare\s-*(""\([[:alpha:]_$]\(?:\s_\|\s" &
                                   //"w\)*\(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)\(?:""\s" &
                                   //"-*,\s-*\([[:alpha:]_$]\(?:\s_\|\sw\)*\(?:\.[[:alph" &
                                   //"a:]_$]\(?:\s_\|\sw\)*\)*\)\)?"
    character(len=*), parameter :: DOJO_MATCH_2_RE = &
                                   "^\s-*dojo\.declare\s-*(""\([[:alpha:]_$]\(?:\s_\|\s" &
                                   //"w\)*\(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)""\s-*,\" &
                                   //"s-*\["
    character(len=*), parameter :: DOJO_BASE_RE = &
                                   "[[,]\s-*\([[:alpha:]_$]\(?:\s_\|\sw\)*\(?:\.[[:alp" &
                                   //"ha:]_$]\(?:\s_\|\sw\)*\)*\)\s-*\(?:\].*$\)?"
    character(len=*), parameter :: DOJO_MATCH_3_RE = &
                                   "^\s-*[[:alpha:]_$]\(?:\s_\|\sw\)*\(?:\.[[:alpha:]_" &
                                   //"$]\(?:\s_\|\sw\)*\)*\s-*[],]"
    character(len=*), parameter :: DOJO_CONTINUED_RE = &
                                   "\([[:alpha:]_$]\(?:\s_\|\sw\)*\(?:\.[[:alpha:]_$]\" &
                                   //"(?:\s_\|\sw\)*\)*\)\s-*\(?:\].*$\)?"
    character(len=*), parameter :: VARIABLE_DECL_RE = &
                                   "\_<\(const\|var\|let\|using\)\_>\|\_<\(b\(?:oolean" &
                                   //"\|yte\)\|char\|double\|float\|int\|long\|short\|vo" &
                                   //"id\)\_>"
    character(len=*), parameter :: NEW_RE = &
                                   "\_<new\_>\s-+\([[:alpha:]_$]\(?:\s_\|\sw\)*\(?:\.[" &
                                   //"[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)"
    character(len=*), parameter :: INSTANCEOF_RE = &
                                   "\_<instanceof\_>\s-+\([[:alpha:]_$]\(?:\s_\|\sw\)*" &
                                   //"\(?:\.[[:alpha:]_$]\(?:\s_\|\sw\)*\)*\)"
    character(len=*), parameter :: FORMAL_PARAMS_RE = &
                                   "\_<function\_>\(\s-+[[:alpha:]_$]\(?:\s_\|\sw\)*\)" &
                                   //"?\s-*(\s-*[[:alpha:]_$]"
    character(len=*), parameter :: FORMAL_PARAM_RE = &
                                   "\([[:alpha:]_$]\(?:\s_\|\sw\)*\)\(\s-*).*\)?"
    character(len=*), parameter :: CONTINUED_PARAMS_RE = &
                                   "^\s-*[[:alpha:]_$]\(?:\s_\|\sw\)*\s-*[,)]"
    character(len=*), parameter :: PROPERTIZE_SLASH_RE = &
                                   "\(?:^\|[=([{,:;|&!]\|\_<return\_>\)\(?:[ "//TAB//"]\)*\(/\" &
                                   //")[^/*]"
    character(len=*), parameter :: GENERATOR_RE = &
                                   "\* *\(?:\[\|[[:alpha:]_$]\(?:\s_\|\sw\)* *(\)"

contains

    ! ------------------------------------------------------------------
    ! Buffer-local state
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `locals' (made if MAKE),
    ! 0 if none.
    ! Not a port.
    recursive integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(js_local_type), allocatable :: grown(:)
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
                call move_local(locals(i), grown(i))
            end do

            call move_alloc(grown, locals)
            k = n + 1
        end if

        call reset_local(locals(k))
        locals(k)%serial = buffers(cur)%serial
    end function local_index

    ! Move FROM to TO.
    ! Not a port.
    subroutine move_local(from, to)
        type(js_local_type), intent(inout) :: from
        type(js_local_type), intent(out) :: to

        to%serial = from%serial
        to%cache_end = from%cache_end
        to%npitems = from%npitems
        if (allocated(from%pitems)) call move_alloc(from%pitems, to%pitems)
        if (allocated(from%quick_match_re)) call move_alloc(from%quick_match_re, to%quick_match_re)
        if (allocated(from%quick_match_re_func)) call move_alloc(from%quick_match_re_func, to%quick_match_re_func)
        to%has_frameworks = from%has_frameworks
        if (allocated(from%frameworks)) call move_alloc(from%frameworks, to%frameworks)
    end subroutine move_local

    ! Give L its initial state: an empty cache holding js--initial-pitem.
    ! Not a port.
    subroutine reset_local(l)
        type(js_local_type), intent(inout) :: l

        l%serial = 0
        l%cache_end = 1
        if (allocated(l%pitems)) deallocate (l%pitems)
        allocate (l%pitems(16))
        l%npitems = 1
        ! js--initial-pitem.
        l%pitems(1)%paren_depth = -huge(1_int64)
        l%pitems(1)%type = PITEM_TOPLEVEL
        l%has_frameworks = .false.
        if (allocated(l%frameworks)) deallocate (l%frameworks)
        if (allocated(l%quick_match_re)) deallocate (l%quick_match_re)
        if (allocated(l%quick_match_re_func)) deallocate (l%quick_match_re_func)
    end subroutine reset_local

    ! kill-all-local-variables: forget the current buffer's js.el
    ! variables.
    ! Not a port.
    subroutine kill_js_locals()
        integer :: k

        k = local_index(.false.)
        if (k > 0) call reset_local(locals(k))
    end subroutine kill_js_locals

    ! The current buffer's local state (made if needed).
    ! Not a port.
    recursive integer function here() result(k)
        k = local_index(.true.)
    end function here

    ! Whether FRAMEWORK is in the current buffer's `js_enabled_frameworks'.
    ! Not a port.
    recursive logical function framework_enabled(framework)
        character(len=*), intent(in) :: framework
        integer :: k, i

        framework_enabled = .false.
        k = here()

        if (locals(k)%has_frameworks) then
            do i = 1, size(locals(k)%frameworks)
                if (trim(locals(k)%frameworks(i)) == framework) framework_enabled = .true.
            end do
        else if (allocated(js_enabled_frameworks)) then
            do i = 1, size(js_enabled_frameworks)
                if (trim(js_enabled_frameworks(i)) == framework) framework_enabled = .true.
            end do
        end if
    end function framework_enabled

    ! Whether any framework is enabled in the current buffer.
    ! Not a port.
    recursive logical function any_framework_enabled()
        integer :: i

        any_framework_enabled = .false.

        do i = 1, size(AVAILABLE_FRAMEWORKS)
            if (framework_enabled(trim(AVAILABLE_FRAMEWORKS(i)))) any_framework_enabled = .true.
        end do
    end function any_framework_enabled

    ! The name, class declaration regexp, :prototype and framework of the
    ! class style I (js--class-styles).
    ! Not a port.
    subroutine class_style(i, name, decl, prototype, framework)
        integer, intent(in) :: i
        character(len=:), allocatable, intent(out) :: name, decl, framework
        logical, intent(out) :: prototype

        prototype = .false.

        select case (i)
        case (1)
            name = "Plain"
            decl = JS_PLAIN_CLASS_RE
            prototype = .true.
            framework = "javascript"
        case (2)
            name = "MochiKit"
            decl = JS_MOCHIKIT_CLASS_RE
            prototype = .true.
            framework = "mochikit"
        case (3)
            name = "Prototype (Obsolete)"
            decl = JS_PROTOTYPE_OBSOLETE_CLASS_DECL_RE
            framework = "prototype"
        case (4)
            name = "Prototype (Modern)"
            decl = JS_PROTOTYPE_CLASS_DECL_RE
            framework = "prototype"
        case (5)
            name = "Prototype (Object.extend)"
            decl = JS_PROTOTYPE_OBJEXTEND_CLASS_DECL_RE_1
            prototype = .true.
            framework = "prototype"
        case (6)
            name = "Prototype (Object.extend) 2"
            decl = JS_PROTOTYPE_OBJEXTEND_CLASS_DECL_RE_2
            prototype = .true.
            framework = "prototype"
        case (7)
            name = "Dojo"
            decl = JS_DOJO_CLASS_DECL_RE
            framework = "dojo"
        case (8)
            name = "ExtJS (style 1)"
            decl = JS_EXTJS_CLASS_DECL_RE_1
            prototype = .true.
            framework = "extjs"
        case (9)
            name = "ExtJS (style 2)"
            decl = JS_EXTJS_CLASS_DECL_RE_2
            framework = "extjs"
        case default
            name = "Merrill Press"
            decl = JS_MP_CLASS_DECL_RE
            framework = "merrillpress"
        end select
    end subroutine class_style

    ! Helper function for `js__update_quick_match_re': the non-empty
    ! elements of LIST joined by SEPARATOR, with PREFIX and SUFFIX ("" if
    ! there are none).  Port of js.el:js--maybe-join.
    function maybe_join(prefix, separator, suffix, list) result(s)
        character(len=*), intent(in) :: prefix, separator, suffix
        character(len=*), intent(in) :: list(:)
        character(len=:), allocatable :: s
        integer :: i

        s = ""

        do i = 1, size(list)
            if (len_trim(list(i)) == 0) cycle
            if (len(s) > 0) s = s//separator
            s = s//trim(list(i))
        end do

        if (len(s) > 0) s = prefix//s//suffix
    end function maybe_join

    ! STR if FRAMEWORK is enabled, "" otherwise.
    ! Not a port.
    recursive function when_enabled(framework, str) result(s)
        character(len=*), intent(in) :: framework, str
        character(len=:), allocatable :: s

        s = ""
        if (framework_enabled(framework)) s = str
    end function when_enabled

    ! Internal function used by `js_mode' for caching buffer constructs.
    ! This updates js--quick-match-re, based on the current set of enabled
    ! frameworks.  Port of js.el:js--update-quick-match-re.
    recursive subroutine update_quick_match_re()
        character(len=200) :: inner(3), proto(2), outer(7)
        integer :: k

        k = here()
        ! var mumble = THING (
        inner = [character(len=200) :: when_enabled("prototype", "Class\.create"), when_enabled("extjs", "Ext\.extend"), &
                 when_enabled("merrillpress", "[a-zA-Z_$0-9]+\.extend\(?:Final\)?")]
        proto = [character(len=200) :: when_enabled("javascript", "\.[a-zA-Z_$0-9]+[ "//TAB//"]*=[ "//TAB &
                                                    //"]*function[ "//TAB//"]*("), &
                 when_enabled("javascript", "[ "//TAB//"]*=[ "//TAB//"]*{")]
        ! #define mumble
        outer(1) = "#define[ "//TAB//"]+[a-zA-Z_]"
        outer(2) = when_enabled("extjs", "Ext\.extend")
        outer(3) = when_enabled("prototype", "Object\.extend")
        outer(4) = maybe_join("\(?:var[ "//TAB//"]+\)?[a-zA-Z_$0-9.]+[ "//TAB//"]*=[ "//TAB//"]*\(?:", "\|", &
                              "\)[ "//TAB//"]*(", inner)
        outer(5) = when_enabled("dojo", "dojo\.declare[ "//TAB//"]*(")
        outer(6) = when_enabled("mochikit", "MochiKit\.Base\.update[ "//TAB//"]*(")
        ! mumble.prototypeTHING
        outer(7) = maybe_join("[a-zA-Z_$0-9.]+\.prototype\(?:", "\|", "\)", proto)
        locals(k)%quick_match_re = maybe_join("^[ "//TAB//"]*\(?:", "\|", "\)", outer)
        locals(k)%quick_match_re_func = "function\|"//locals(k)%quick_match_re
    end subroutine update_quick_match_re

    ! ------------------------------------------------------------------
    ! Pitems and text properties
    ! ------------------------------------------------------------------

    ! A new pitem of the current buffer: its index.
    ! Not a port.
    recursive integer function make_pitem(paren_depth, type, h_begin, name, anonymous) result(id)
        integer(int64), intent(in) :: paren_depth, h_begin
        integer, intent(in) :: type
        character(len=*), intent(in) :: name
        logical, intent(in) :: anonymous
        type(pitem_type), allocatable :: grown(:)
        integer :: k, i

        k = here()

        if (locals(k)%npitems >= size(locals(k)%pitems)) then
            allocate (grown(2*size(locals(k)%pitems)))

            do i = 1, locals(k)%npitems
                grown(i)%paren_depth = locals(k)%pitems(i)%paren_depth
                grown(i)%type = locals(k)%pitems(i)%type
                grown(i)%h_begin = locals(k)%pitems(i)%h_begin
                grown(i)%anonymous = locals(k)%pitems(i)%anonymous
                if (allocated(locals(k)%pitems(i)%name)) call move_alloc(locals(k)%pitems(i)%name, grown(i)%name)
                grown(i)%b_end = locals(k)%pitems(i)%b_end
            end do

            call move_alloc(grown, locals(k)%pitems)
        end if

        locals(k)%npitems = locals(k)%npitems + 1
        id = locals(k)%npitems
        locals(k)%pitems(id)%paren_depth = paren_depth
        locals(k)%pitems(id)%type = type
        locals(k)%pitems(id)%h_begin = h_begin
        locals(k)%pitems(id)%name = name
        locals(k)%pitems(id)%anonymous = anonymous
        locals(k)%pitems(id)%b_end = 0
    end function make_pitem

    ! The type of pitem ID.
    ! Not a port.
    recursive integer function pitem_type_of(id)
        integer, intent(in) :: id

        pitem_type_of = locals(here())%pitems(id)%type
    end function pitem_type_of

    ! The h-begin of pitem ID.
    ! Not a port.
    recursive integer(int64) function pitem_h_begin(id)
        integer, intent(in) :: id

        pitem_h_begin = locals(here())%pitems(id)%h_begin
    end function pitem_h_begin

    ! The value of a js--pstate property: the list of the pitems ITEMS.
    ! Not a port.
    function pstate_value(items) result(v)
        integer, intent(in) :: items(:)
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        integer :: i

        allocate (elts(size(items)))

        do i = 1, size(items)
            elts(i) = make_fixnum(int(items(i), int64))
        end do

        v = make_list(elts)
    end function pstate_value

    ! The pitems of the js--pstate value V.
    ! Not a port.
    function pstate_items(v) result(items)
        type(value_type), intent(in) :: v
        integer, allocatable :: items(:)
        type(value_type), allocatable :: elts(:)
        integer :: i

        allocate (items(0))
        if (nilp(v)) return
        call list_elements(v, elts)
        deallocate (items)
        allocate (items(size(elts)))

        do i = 1, size(elts)
            items(i) = int(xfixnum(elts(i)))
        end do
    end function pstate_items

    ! The text property PROPNAME at POS.
    ! Not a port.
    recursive function prop_at(pos, propname) result(v)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: propname
        type(value_type) :: v
        type(status_type) :: st

        call get_text_property(pos, sym(propname), v, st)
    end function prop_at

    ! Move over the next value of PROPNAME in the buffer.
    ! If found, return that value and leave point after the character
    ! having that value; otherwise, return nil and leave point at EOB.
    ! Port of js.el:js--forward-text-property.
    recursive function forward_text_property(propname) result(next_value)
        character(len=*), intent(in) :: propname
        type(value_type) :: next_value
        type(status_type) :: st
        integer(int64) :: pos
        logical :: found

        next_value = prop_at(point(), propname)

        if (.not. nilp(next_value)) then
            call forward_char(st, 1_int64)
        else
            call next_single_property_change(point(), sym(propname), pos, found, st, limit=point_max())
            if (.not. found) pos = point_max()
            call goto_char(pos)

            if (.not. eobp()) then
                next_value = prop_at(point(), propname)
                call forward_char(st, 1_int64)
            end if
        end if
    end function forward_text_property

    ! Move over the previous value of PROPNAME in the buffer.
    ! If found, return that value and leave point just before the
    ! character that has that value, otherwise return nil and leave point
    ! at BOB.  Port of js.el:js--backward-text-property.
    recursive function backward_text_property(propname) result(prev_value)
        character(len=*), intent(in) :: propname
        type(value_type) :: prev_value
        type(status_type) :: st
        integer(int64) :: pos
        logical :: found

        prev_value = nil()
        if (bobp()) return
        prev_value = prop_at(point() - 1, propname)

        if (.not. nilp(prev_value)) then
            call forward_char(st, -1_int64)
        else
            call previous_single_property_change(point(), sym(propname), pos, found, st, limit=point_min())
            if (.not. found) pos = point_min()
            call goto_char(pos)

            if (.not. bobp()) then
                call forward_char(st, -1_int64)
                prev_value = prop_at(point(), propname)
            end if
        end if
    end function backward_text_property

    ! js--forward-pstate.
    ! Port of js.el:js--forward-pstate.
    recursive function forward_pstate() result(items)
        integer, allocatable :: items(:)

        items = pstate_items(forward_text_property("js__pstate"))
    end function forward_pstate

    ! js--backward-pstate.
    ! Port of js.el:js--backward-pstate.
    recursive function backward_pstate() result(items)
        integer, allocatable :: items(:)

        items = pstate_items(backward_text_property("js__pstate"))
    end function backward_pstate

    ! Go to the h-end of pitem ID.  Port of js.el:js--pitem-goto-h-end.
    recursive subroutine pitem_goto_h_end(id)
        integer, intent(in) :: id
        integer, allocatable :: items(:)

        call goto_char(pitem_h_begin(id))
        items = forward_pstate()
    end subroutine pitem_goto_h_end

    ! ------------------------------------------------------------------
    ! Searching
    ! ------------------------------------------------------------------

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

    ! The character after point, -1 at the end.
    ! Not a port.
    recursive integer function next_char()
        next_char = after_char_at(point())
    end function next_char

    ! The character before point, -1 at the beginning.
    ! Not a port.
    recursive integer function prev_char()
        prev_char = before_char_at(point())
    end function prev_char

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
    recursive logical function at_p(regexp)
        character(len=*), intent(in) :: regexp
        type(status_type) :: st

        at_p = looking_at(regexp, st, inhibit_modify=.true.)
    end function at_p

    ! The parse state at point (syntax-ppss).
    ! Not a port.
    recursive function ppss() result(state)
        type(parse_state_type) :: state

        call syntax_ppss(state)
    end function ppss

    ! The parse state at POS (syntax-ppss POS, which moves point there).
    ! Not a port.
    recursive function ppss_at(pos) result(state)
        integer(int64), intent(in) :: pos
        type(parse_state_type) :: state

        call goto_char(pos)
        call syntax_ppss(state)
    end function ppss_at

    ! Helper function for `js_re_search_forward': search COUNT times for
    ! REGEXP before BOUND (0 for none), ignoring strings, comments and
    ! macros; FAILED when a search fails (search-failed).  Port of
    ! js.el:js--re-search-forward-inner.
    recursive subroutine re_search_forward_inner(regexp, bound, count_in, failed)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in) :: bound
        integer, intent(in) :: count_in
        logical, intent(out) :: failed
        type(parse_state_type) :: parse
        type(status_type) :: st
        integer(int64) :: orig_macro_end, opoint, pos, shortage
        integer :: count, str_terminator

        failed = .false.
        count = count_in
        opoint = point()
        orig_macro_end = 0

        if (beginning_of_macro()) then
            call c_end_of_macro()
            orig_macro_end = point()
        end if

        call goto_char(opoint)

        do while (count > 0)
            call search(regexp)
            if (failed) return
            parse = ppss()

            if (parse%instring >= 0) then
                str_terminator = parse%instring
                if (str_terminator == ST_STRING_STYLE) str_terminator = iachar("/")
                call re_search_forward("\([^\]\|^\)"//achar(str_terminator), pos, st, bound=line_end_position(), &
                                       noerror=SEARCH_NOERROR)
            else if (parse%comment_style /= 0 .and. parse%incomment /= 0) then
                ! (nth 7 parse): a comment of style b (a line comment).
                shortage = forward_line(1_int64)
            else if (parse%incomment /= 0 .or. (prev_char() == iachar("/") .and. next_char() == iachar("*"))) then
                call search("\*/")
                if (failed) return
            else if (in_new_macro()) then
                call c_end_of_macro()
            else
                count = count - 1
            end if
        end do

    contains

        ! re-search-forward RE before BOUND, failing if not found.
        recursive subroutine search(re)
            character(len=*), intent(in) :: re

            if (bound > 0) then
                call re_search_forward(re, pos, st, bound=bound, noerror=SEARCH_NOERROR)
            else
                call re_search_forward(re, pos, st, noerror=SEARCH_NOERROR)
            end if

            failed = pos == 0
        end subroutine search

        ! (and (not (and orig-macro-end (<= (point) orig-macro-end)))
        !      (js--beginning-of-macro))
        recursive logical function in_new_macro()
            in_new_macro = .false.

            if (orig_macro_end > 0) then
                if (point() <= orig_macro_end) return
            end if

            in_new_macro = beginning_of_macro()
        end function in_new_macro
    end subroutine re_search_forward_inner

    ! Auxiliary function for `js_re_search_backward'.  Port of
    ! js.el:js--re-search-backward-inner.
    recursive subroutine re_search_backward_inner(regexp, bound, count_in, failed)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in) :: bound
        integer, intent(in) :: count_in
        logical, intent(out) :: failed
        type(parse_state_type) :: parse
        type(status_type) :: st
        integer(int64) :: orig_macro_start, opoint, pos
        integer :: count
        logical :: slash

        failed = .false.
        count = count_in
        opoint = point()
        orig_macro_start = 0
        if (beginning_of_macro()) orig_macro_start = point()
        call goto_char(opoint)

        do while (count > 0)
            call search(regexp)
            if (failed) return

            if (point() > point_min()) then
                call goto_char(point() - 1)
                slash = at("/[/*]")
                call goto_char(point() + 1)
                if (slash) call forward_char(st, 1_int64)
            end if

            parse = ppss()

            if (parse%comstr_start > 0) then
                call goto_char(parse%comstr_start)
            else if (parse%incomment /= 0 .or. (prev_char() == iachar("/") .and. next_char() == iachar("*"))) then
                call search("/\*")
                if (failed) return
            else if (in_new_macro()) then
                continue
            else
                count = count - 1
            end if
        end do

    contains

        ! re-search-backward RE after BOUND, failing if not found.
        recursive subroutine search(re)
            character(len=*), intent(in) :: re

            if (bound > 0) then
                call re_search_backward(re, pos, st, bound=bound, noerror=SEARCH_NOERROR)
            else
                call re_search_backward(re, pos, st, noerror=SEARCH_NOERROR)
            end if

            failed = pos == 0
        end subroutine search

        ! (and (not (and orig-macro-start (>= (point) orig-macro-start)))
        !      (js--beginning-of-macro))
        recursive logical function in_new_macro()
            in_new_macro = .false.

            if (orig_macro_start > 0) then
                if (point() >= orig_macro_start) return
            end if

            in_new_macro = beginning_of_macro()
        end function in_new_macro
    end subroutine re_search_backward_inner

    !> Search forward, ignoring strings, cpp macros, and comments.
    !> This function invokes `re_search_forward', but treats the buffer as
    !> if strings, cpp macros, and comments have been removed.  BOUND (0
    !> for none) and COUNT (negative to search backward) are as there; the
    !> result is whether it found the match (on failure point does not
    !> move).
    !>
    !> If invoked while inside a macro, it treats the contents of the
    !> macro as normal text.  Port of js.el:js--re-search-forward (with
    !> NOERROR).
    recursive logical function js_re_search_forward(regexp, bound, count) result(found)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in), optional :: bound
        integer, intent(in), optional :: count
        integer(int64) :: saved_point, b
        integer :: n
        logical :: failed

        n = 1
        if (present(count)) n = count
        b = 0
        if (present(bound)) b = bound
        saved_point = point()
        failed = .false.

        if (n < 0) then
            call re_search_backward_inner(regexp, b, -n, failed)
        else if (n > 0) then
            call re_search_forward_inner(regexp, b, n, failed)
        end if

        found = .not. failed
        if (failed) call goto_char(saved_point)
    end function js_re_search_forward

    !> Search backward, ignoring strings, preprocessor macros, and comments.
    !>
    !> This function invokes `re_search_backward' but treats the buffer as
    !> if strings, preprocessor macros, and comments have been removed.
    !>
    !> If invoked while inside a macro, treat the macro as normal text.
    !> Port of js.el:js--re-search-backward (with NOERROR).
    recursive logical function js_re_search_backward(regexp, bound, count) result(found)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in), optional :: bound
        integer, intent(in), optional :: count
        integer :: n

        n = 1
        if (present(count)) n = count
        found = js_re_search_forward(regexp, bound, -n)
    end function js_re_search_backward

    ! Go to the end of a preprocessor directive: the end of the closest
    ! following line that doesn't end with a line continuation backslash.
    ! Port of cc-engine.el:c-end-of-macro (no LIM; its cache is not
    ! needed here).
    recursive subroutine c_end_of_macro()
        type(parse_state_type) :: s, after
        type(status_type) :: st
        integer(int64) :: safe_pos
        logical :: again

        ! A point outside any literal.
        safe_pos = point()

        ! Move over stuff followed by a multiline block comment lacking
        ! escaped newlines each time around this loop.
        do
            do
                call end_of_line()
                if (.not. (prev_char() == iachar("\") .and. .not. eobp())) exit
                call forward_char(st, 1_int64)
            end do

            call parse_partial_expression(safe_pos, point(), s, st)
            again = .false.

            ! Are we in a block comment?
            if (s%incomment /= 0) then
                if (s%comment_style == 0) then
                    ! Move to after the block comment.
                    call parse_partial_expression(point(), point_max(), after, st, oldstate=s, &
                                                  commentstop=COMMENTSTOP_SYNTAX_TABLE)
                    safe_pos = point()
                    again = .true.
                end if
            end if

            if (.not. again) exit
        end do
    end subroutine c_end_of_macro

    ! Whether point is in a cpp macro: if so, go to its start.  Port of
    ! js.el:js--beginning-of-macro (no LIM).
    recursive logical function beginning_of_macro() result(found)
        integer(int64) :: start, shortage

        start = point()
        call beginning_of_line()

        do while (before_char_at(point() - 1) == iachar("\"))
            shortage = forward_line(-1_int64)
        end do

        call back_to_indentation()
        found = .false.

        if (point() <= start) then
            if (at(JS_OPT_CPP_START)) found = .true.
        end if

        if (.not. found) call goto_char(start)
    end function beginning_of_macro

    ! Simple implementation of `c_backward_syntactic_ws' for `js_mode'.
    ! Port of js.el:js--backward-syntactic-ws (no LIM).
    recursive subroutine backward_syntactic_ws()
        integer(int64) :: opoint, pos
        logical :: in_macro, moved

        opoint = point()
        in_macro = beginning_of_macro()
        call goto_char(opoint)
        pos = point()

        do
            if (.not. in_macro) moved = beginning_of_macro()
            moved = forward_comment(-huge(1_int64))
            if (point() == pos) exit
            pos = point()
        end do
    end subroutine backward_syntactic_ws

    ! Simple implementation of `c_forward_syntactic_ws' for `js_mode'.
    ! Port of js.el:js--forward-syntactic-ws (no LIM).
    recursive subroutine forward_syntactic_ws()
        integer(int64) :: pos
        logical :: moved

        pos = point()

        do
            moved = forward_comment(huge(1_int64))
            if (next_char() == iachar("#")) call c_end_of_macro()
            if (point() == pos) exit
            pos = point()
        end do
    end subroutine forward_syntactic_ws

    ! Like (up-list -1), but only considers lists that end nearby.
    ! Port of js.el:js--up-nearby-list.
    recursive subroutine up_nearby_list(status)
        type(status_type), intent(out) :: status
        integer(int64) :: begv, zv

        begv = buffers(cur)%begv
        zv = buffers(cur)%zv
        ! Look at a very small region so our computation time doesn't
        ! explode in pathological cases.
        call narrow_to_region(max(point_min(), point() - 500), point(), status)
        call up_group(-1_int64, status)
        call widen()
        call narrow_to_region(begv, zv, status)
    end subroutine up_nearby_list

    ! Move forward over symbol ARG times (backward if negative).  Port of
    ! thingatpt.el:forward-symbol.
    recursive subroutine forward_symbol(arg)
        integer, intent(in) :: arg
        type(status_type) :: st
        integer(int64) :: pos, moved
        integer :: n

        n = arg

        if (n >= 0) then
            call re_search_forward("\(\sw\|\s_\)+", pos, st, noerror=SEARCH_MOVE, count=int(n, int64))
            return
        end if

        do while (n < 0)
            call re_search_backward("\(\sw\|\s_\)+", pos, st, noerror=SEARCH_MOVE)
            if (pos > 0) moved = skip_syntax_backward("w_")
            n = n + 1
        end do
    end subroutine forward_symbol

    ! ------------------------------------------------------------------
    ! Functions and the parse cache
    ! ------------------------------------------------------------------

    ! Move forward over a whole JavaScript expression.
    ! This function doesn't move over expressions continued across lines.
    ! Port of js.el:js--forward-expression.
    recursive subroutine js_forward_expression(status)
        type(status_type), intent(out) :: status
        logical :: moved, continued
        integer :: c

        status = status_type()

        do
            ! non-continued case; simplistic, but good enough?
            do
                if (eolp()) exit
                moved = forward_comment(huge(1_int64))
                c = next_char()
                if (c == iachar(",") .or. c == iachar(";") .or. c == iachar("]") .or. c == iachar(")") &
                    .or. c == iachar("}")) exit
                call forward_expression(1_int64, status)
                if (.not. status%ok()) return
            end do

            if (next_char() /= 10) exit
            call goto_char(point() + 1)
            continued = js_continued_expression_p()
            call goto_char(point() - 1)
            if (.not. continued) exit
        end do
    end subroutine js_forward_expression

    ! Move forward over a JavaScript function declaration.
    ! This puts point at the `function' keyword.
    !
    ! If this is a syntactically-correct non-expression function, the
    ! result is true with the name of the function in NAME, or ANONYMOUS
    ! if the name could not be determined.  Otherwise, false.  Port of
    ! js.el:js--forward-function-decl.
    recursive logical function forward_function_decl(name, anonymous) result(ok)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: anonymous
        type(status_type) :: st
        logical :: moved

        ok = .false.
        name = ""
        anonymous = .true.
        if (.not. at("\(\_<async\_>[ "//TAB//LF//"]+\)?\_<function\_>")) return
        call goto_char(me(0))
        moved = forward_comment(huge(1_int64))

        if (next_char() == iachar("*")) then
            call forward_char(st, 1_int64)
            moved = forward_comment(huge(1_int64))
        end if

        if (at(JS_NAME_RE)) then
            name = match_string_no_properties(0)
            anonymous = .false.
            call goto_char(me(0))
        end if

        moved = forward_comment(huge(1_int64))
        if (next_char() /= iachar("(")) return
        call forward_group(1_int64, st)
        if (.not. st%ok()) return
        moved = forward_comment(huge(1_int64))
        ok = next_char() == iachar("{")
    end function forward_function_decl

    ! Return the start of the JavaScript function prologue containing POS
    ! (default: point).  A function prologue is everything from start of
    ! the definition up to and including the opening brace.  If POS is not
    ! in a function prologue, return 0.  Port of
    ! js.el:js--function-prologue-beginning.
    recursive integer(int64) function function_prologue_beginning(pos_in) result(begin)
        integer(int64), intent(in), optional :: pos_in
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int64) :: opoint, pos, prologue_begin, start, moved, here_pos
        logical :: anonymous, heading, found

        begin = 0
        opoint = point()

        if (present(pos_in)) then
            call goto_char(pos_in)
            pos = pos_in
        else
            pos = point()
        end if

        prologue_begin = 0
        here_pos = point()
        call goto_char(line_beginning_position())
        heading = at(JS_FUNCTION_HEADING_2_RE)
        if (.not. heading) heading = at(JS_FUNCTION_HEADING_3_RE)
        call goto_char(here_pos)

        if (heading) then
            prologue_begin = mb(1)
            if (prologue_begin <= pos) call goto_char(me(0))
        end if

        moved = skip_syntax_backward("w_")
        found = at("\_<function\_>")
        if (.not. found) found = js_re_search_backward("\_<function\_>")

        if (found) then
            start = mb(0)
            call goto_char(start)
            if (looking_back("\_<async\_>[ "//TAB//LF//"]+", st, point() - 30)) start = mb(0)

            if (forward_function_decl(name, anonymous)) then
                if (pos <= point()) then
                    if (prologue_begin > 0) then
                        begin = prologue_begin
                    else
                        begin = start
                    end if
                end if
            end if
        end if

        call goto_char(opoint)
    end function function_prologue_beginning

    ! Helper function for `js_beginning_of_subprogram'.
    ! Go to previous defun-beginning and return the parse state for it, or
    ! none if we went all the way back to bob and don't find anything.
    ! Port of js.el:js--beginning-of-defun-raw.
    recursive function beginning_of_subprogram_raw() result(pstate)
        integer, allocatable :: pstate(:)

        call js_ensure_cache()

        do
            pstate = backward_pstate()
            if (size(pstate) == 0) exit
            if (pitem_type_of(pstate(1)) == PITEM_FUNCTION) exit
        end do

        if (bobp()) then
            deallocate (pstate)
            allocate (pstate(0))
        end if
    end function beginning_of_subprogram_raw

    ! Helper function for `js__beginning_of_subprogram_nested'.
    ! If PSTATE represents a non-empty top-level subprogram, return the top-most
    ! pitem.  Otherwise, return 0.  Port of
    ! js.el:js--pstate-is-toplevel-defun.
    recursive integer function pstate_is_toplevel_subprogram(pstate) result(func_pitem)
        integer, intent(in) :: pstate(:)
        integer :: i, func_depth

        func_depth = 0
        func_pitem = 0

        do i = 1, size(pstate)
            if (pitem_type_of(pstate(i)) == PITEM_FUNCTION) then
                func_depth = func_depth + 1
                func_pitem = pstate(i)
            end if
        end do

        if (func_depth /= 1) func_pitem = 0
    end function pstate_is_toplevel_subprogram

    ! Helper function for `js__beginning_of_subprogram'.
    ! Return the pitem of the function we went to the beginning of (0 for
    ! none).  Port of js.el:js--beginning-of-defun-nested.
    recursive integer function beginning_of_subprogram_nested() result(pitem)
        integer, allocatable :: pstate(:)
        integer :: i

        ! Look for the smallest function that encloses point...
        pstate = parse_state_at_point()

        do i = 1, size(pstate)
            if (pitem_type_of(pstate(i)) == PITEM_FUNCTION) then
                if (inside_pitem_p(pstate(i))) then
                    call goto_char(pitem_h_begin(pstate(i)))
                    pitem = pstate(i)
                    return
                end if
            end if
        end do

        ! ...and if that isn't found, look for the previous top-level subprogram
        pitem = 0

        do
            pstate = backward_pstate()
            if (size(pstate) == 0) exit
            pitem = pstate_is_toplevel_subprogram(pstate)

            if (pitem > 0) then
                call goto_char(pitem_h_begin(pitem))
                return
            end if
        end do
    end function beginning_of_subprogram_nested

    ! Helper function for `js_beginning_of_subprogram'.  Port of
    ! js.el:js--beginning-of-defun-flat.
    recursive logical function beginning_of_subprogram_flat() result(found)
        integer, allocatable :: pstate(:)

        pstate = beginning_of_subprogram_raw()
        found = size(pstate) > 0
        if (found) call goto_char(pitem_h_begin(pstate(1)))
    end function beginning_of_subprogram_flat

    !> Value of `beginning_of_subprogram_function' for `js_mode': move to the
    !> beginning of the ARGth function (default 1; backward), returning
    !> whether one was found.  Port of js.el:js-beginning-of-defun.
    recursive subroutine js_beginning_of_subprogram(arg, found)
        integer(int64), intent(in) :: arg
        logical, intent(out) :: found
        type(status_type) :: st
        integer(int64) :: n, prologue_begin

        n = arg
        found = .false.

        do while (.not. eobp() .and. n < 0)
            n = n + 1

            if (.not. js_flat_functions%value) then
                if (js_syntactic_context() == JS_CONTEXT_FUNCTION) then
                    call js_end_of_subprogram(1_int64)
                else if (function_prologue_beginning() > 0) then
                    call js_end_of_subprogram(1_int64)
                end if
            end if

            if (js_re_search_forward("\_<function\_>")) then
                call goto_char(function_prologue_beginning())
                found = .true.
            else
                call goto_char(point_max())
                found = .false.
            end if
        end do

        do while (n > 0)
            n = n - 1
            ! If we're just past the end of a function, the user probably
            ! wants to go to the beginning of *that* function
            if (prev_char() == iachar("}")) call forward_char(st, -1_int64)
            prologue_begin = function_prologue_beginning()

            if (prologue_begin > 0 .and. prologue_begin < point()) then
                call goto_char(prologue_begin)
                found = .true.
            else if (js_flat_functions%value) then
                found = beginning_of_subprogram_flat()
            else if (beginning_of_subprogram_nested() > 0) then
                found = .true.
            end if
        end do
    end subroutine js_beginning_of_subprogram

    ! `js_beginning_of_subprogram' as a `beginning_of_subprogram_function'.
    ! Not a port.
    recursive subroutine beginning_of_subprogram_proc(arg, found, status)
        integer(int64), intent(in) :: arg
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        status = status_type()
        call js_beginning_of_subprogram(arg, found)
    end subroutine beginning_of_subprogram_proc

    !> Flush the `js_mode' syntax cache after position BEG.
    !> BEG defaults to `point_min', meaning to flush the entire cache.
    !> Port of js.el:js--flush-caches.
    recursive subroutine js_flush_caches(beg)
        integer(int64), intent(in), optional :: beg
        integer(int64) :: b
        integer :: k

        k = local_index(.false.)
        if (k == 0) return
        b = 1
        if (present(beg)) b = beg
        locals(k)%cache_end = min(locals(k)%cache_end, b)
    end subroutine js_flush_caches

    ! `js_flush_caches' in `before_change_functions'.
    ! Not a port.
    recursive subroutine flush_caches_hook(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status

        status = status_type()
        if (end < 0) return
        call js_flush_caches(beg)
    end subroutine flush_caches_hook

    ! The pitems OPEN_ITEMS with the top one popped if it ended at
    ! PAREN_DEPTH (putting js--pend on the character before point).
    ! Port of js.el:js--ensure-cache--pop-if-ended (the children are not kept).
    recursive subroutine pop_if_ended(open_items, paren_depth)
        integer, allocatable, intent(inout) :: open_items(:)
        integer(int64), intent(in) :: paren_depth
        type(status_type) :: st
        integer :: top, k

        k = here()
        top = open_items(1)

        if (paren_depth <= locals(k)%pitems(top)%paren_depth) then
            call put_text_property(point() - 1, point(), sym("js__pend"), make_fixnum(int(top, int64)), st)
            locals(k)%pitems(top)%b_end = point()
            ! open-items must contain at least two items for this to work,
            ! but because we push a dummy item to start with, that
            ! assumption holds.
            open_items = open_items(2:)
        end if
    end subroutine pop_if_ended

    ! Split a JavaScript name into its dot-separated parts.  This also
    ! removes any prototype parts from the split name (unless the name is
    ! just "prototype" to start with).  The parts are joined by ".".
    ! Port of js.el:js--split-name.
    function split_name(string) result(name)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: name
        character(len=:), allocatable :: part
        integer :: i, j, count

        if (string == "prototype") then
            name = string
            return
        end if

        name = ""
        count = 0
        i = 1

        do while (i <= len(string))
            j = index(string(i:), ".")

            if (j == 0) then
                part = string(i:)
                i = len(string) + 1
            else
                part = string(i:i + j - 2)
                i = i + j
            end if

            if (len(part) == 0 .or. part == "prototype") cycle

            if (count > 0) then
                name = name//"."//part
            else
                name = part
            end if

            count = count + 1
        end do
    end function split_name

    ! Guess the name of the JavaScript function at POSITION.
    ! POSITION should be just after the end of the word "function".  The
    ! result is false if the name could not be guessed.
    !
    ! If we find the preamble begins earlier than expected while guessing
    ! the function name, set js--guess-function-name-start to that
    ! position; otherwise, set that variable to 0.  Port of
    ! js.el:js--guess-function-name.
    recursive logical function guess_function_name(position, name) result(found)
        integer(int64), intent(in) :: position
        character(len=:), allocatable, intent(out) :: name
        integer(int64) :: opoint

        guess_function_name_start = 0
        found = .false.
        name = ""
        opoint = point()
        call goto_char(position)
        call goto_char(line_beginning_position())

        if (at(JS_FUNCTION_HEADING_3_RE)) then
            if (me(0) == position) then
                guess_function_name_start = mb(1)
                name = match_string_no_properties(1)
                found = .true.
            end if
        else if (at(JS_FUNCTION_HEADING_2_RE)) then
            if (me(0) == position) then
                guess_function_name_start = mb(1)
                name = match_string_no_properties(1)
                found = .true.
            end if
        end if

        call goto_char(opoint)
    end function guess_function_name

    ! Clear the cache after point.  Port of js.el:js--clear-stale-cache.
    recursive subroutine clear_stale_cache()
        type(value_type) :: end_prop
        type(status_type) :: st
        integer(int64) :: opoint
        logical :: changed
        integer :: k

        k = here()
        ! Clear any endings that occur after point
        opoint = point()

        do
            end_prop = forward_text_property("js__pend")
            if (nilp(end_prop)) exit
            locals(k)%pitems(int(xfixnum(end_prop)))%b_end = 0
        end do

        call goto_char(opoint)
        ! Remove any cache properties after this point
        call remove_text_properties(point(), point_max(), list(sym("js__pstate"), sym("t"), sym("js__pend"), sym("t")), &
                                    changed, st)
    end subroutine clear_stale_cache

    !> Ensures brace cache is valid up to the character before LIMIT.
    !> LIMIT defaults to point.  Port of js.el:js--ensure-cache.
    recursive subroutine js_ensure_cache(limit_in)
        integer(int64), intent(in), optional :: limit_in
        type(silent_modifications_type) :: silent
        type(parse_state_type) :: parse
        type(status_type) :: st
        integer, allocatable :: open_items(:)
        character(len=:), allocatable :: name, style_name, decl, framework
        integer(int64) :: limit, prev_parse_point, goal_point, orig_match_start, orig_match_end, orig_depth, pos, &
                          opoint, begv, zv
        logical :: saved_fold, found, anonymous, prototype, add
        integer :: k, item, context, i
        type(value_type) :: v

        limit = point()
        if (present(limit_in)) limit = limit_in
        k = here()
        if (.not. allocated(locals(k)%quick_match_re)) call update_quick_match_re()
        if (.not. locals(k)%cache_end < limit) return
        ! c-save-buffer-state.
        call silent_modifications_begin(silent)
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        opoint = point()
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv
        call widen()
        ! Find last known good position
        call goto_char(locals(k)%cache_end)
        allocate (open_items(0))

        if (.not. bobp()) then
            open_items = pstate_items(prop_at(point() - 1, "js__pstate"))

            if (size(open_items) == 0) then
                call previous_single_property_change(point(), sym("js__pstate"), pos, found, st, limit=point_min())
                if (.not. found) pos = point_min()
                call goto_char(pos)
                if (.not. bobp()) open_items = pstate_items(prop_at(point() - 1, "js__pstate"))
            end if
        end if

        ! Make a placeholder for the top-level definition
        if (size(open_items) == 0) open_items = [1]
        ! (When the whole cache is flushed, its pitems are forgotten.)
        if (point() == point_min()) locals(k)%npitems = 1
        parse = ppss()
        prev_parse_point = point()
        call clear_stale_cache()
        call narrow_to_region(point_min(), limit, st)

        do
            call re_search_forward(locals(k)%quick_match_re_func, pos, st, noerror=SEARCH_NOERROR)
            if (pos == 0) exit
            orig_match_start = mb(0)
            orig_match_end = me(0)
            call goto_char(orig_match_start)
            call update_parse()
            orig_depth = parse%depth
            add = .false.

            ! Each of these conditions should return true if we should add
            ! a new item and leave point at the end of the new item's
            ! header (h-end in the js--pitem diagram).  This point is the
            ! one after the last character we need to unambiguously detect
            ! this construct.  If one of these evaluates to false, the
            ! location of the point is ignored.
            if (parse%comstr_start > 0) then
                ! In comment or string
                continue
            else if (at("\_<function\_>")) then
                ! Regular function declaration
                if (forward_function_decl(name, anonymous)) then
                    if (anonymous) then
                        found = guess_function_name(orig_match_end, name)

                        if (found) then
                            anonymous = .false.
                            if (guess_function_name_start > 0) orig_match_start = guess_function_name_start
                        end if
                    end if

                    call forward_char(st, 1_int64)
                    pos = point()
                    call goto_char(orig_match_start)
                    if (looking_back("\_<async\_>[ "//TAB//LF//"]+", st, point() - 30)) orig_match_start = mb(0)
                    call goto_char(pos)

                    if (anonymous) then
                        item = make_pitem(orig_depth, PITEM_FUNCTION, orig_match_start, "", .true.)
                    else
                        item = make_pitem(orig_depth, PITEM_FUNCTION, orig_match_start, split_name(name), .false.)
                    end if

                    add = .true.
                end if
            else if (at(JS_MACRO_DECL_RE)) then
                ! Macro
                name = match_string_no_properties(1)
                ! Macros often contain unbalanced parentheses.  Make sure
                ! that h-end is at the textual end of the macro no matter
                ! what the parenthesis say.
                call c_end_of_macro()
                call update_parse()
                item = make_pitem(parse%depth, PITEM_MACRO, orig_match_start, name, .false.)
                add = .true.
            else if (at(JS_PLAIN_METHOD_RE)) then
                ! "Prototype function" declaration
                name = split_name(match_string_no_properties(1))//"."//match_string_no_properties(2)
                call goto_char(mb(3))

                if (forward_function_decl(style_name, anonymous)) then
                    call forward_char(st, 1_int64)
                    item = make_pitem(orig_depth, PITEM_FUNCTION, orig_match_start, name, .false.)
                    add = .true.
                end if
            else
                ! Class definition
                context = context_from_pstate(open_items)

                if (context == JS_CONTEXT_TOPLEVEL) then
                    do i = 1, NSTYLES
                        call class_style(i, style_name, decl, prototype, framework)
                        if (.not. framework_enabled(framework)) cycle

                        if (at(decl)) then
                            call goto_char(me(0))
                            item = make_pitem(orig_depth, PITEM_CLASS + i, orig_match_start, &
                                              split_name(match_string_no_properties(1)), .false.)
                            add = .true.
                            exit
                        end if
                    end do
                end if
            end if

            if (add) then
                call update_parse()
                open_items = [item, open_items]
                v = pstate_value(open_items)
                call put_text_property(point() - 1, point(), sym("js__pstate"), v, st)
            else
                call goto_char(orig_match_end)
            end if
        end do

        call goto_char(limit)
        call update_parse()
        locals(k)%cache_end = limit
        call widen()
        call narrow_to_region(begv, zv, st)
        call goto_char(opoint)
        case_fold_search%value = saved_fold
        call silent_modifications_end(silent)

    contains

        ! Update parsing information up to point.  Port of
        ! js.el:js--ensure-cache--update-parse.
        recursive subroutine update_parse()
            type(parse_state_type) :: old

            goal_point = point()
            call goto_char(prev_parse_point)

            do
                call pop_if_ended(open_items, parse%depth)
                ! Make sure parse-partial-sexp doesn't stop because we
                ! *entered* the given depth -- i.e., make sure we're deeper
                ! than the target depth.
                old = parse
                call parse_partial_expression(prev_parse_point, goal_point, parse, st, &
                                              targetdepth=locals(k)%pitems(open_items(1))%paren_depth, oldstate=old)
                prev_parse_point = point()
                if (.not. point() < goal_point) exit
            end do

            call pop_if_ended(open_items, parse%depth)
        end subroutine update_parse
    end subroutine js_ensure_cache

    ! Helper function for `js_end_of_subprogram'.  Port of
    ! js.el:js--end-of-defun-flat.
    recursive subroutine end_of_subprogram_flat()
        type(value_type) :: v

        do while (js_re_search_forward("}"))
            call js_ensure_cache()
            v = prop_at(point() - 1, "js__pend")

            if (.not. nilp(v)) then
                if (pitem_type_of(int(xfixnum(v))) == PITEM_FUNCTION) return
            end if
        end do

        call goto_char(point_max())
    end subroutine end_of_subprogram_flat

    ! Helper function for `js_end_of_subprogram'.  Port of
    ! js.el:js--end-of-defun-nested.
    recursive subroutine end_of_subprogram_nested()
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int64) :: opoint, this_end
        integer :: pitem
        logical :: found, anonymous

        opoint = point()
        this_end = 0
        pitem = beginning_of_subprogram_nested()

        if (pitem > 0) then
            call pitem_goto_h_end(pitem)
            call forward_char(st, -1_int64)
            call forward_group(1_int64, st)
            this_end = point()
        end if

        call goto_char(opoint)

        if (this_end > 0 .and. point() < this_end) then
            ! We're already inside a function; just go to its end.
            call goto_char(this_end)
            return
        end if

        ! Otherwise, go to the end of the next function...
        found = .false.

        do while (js_re_search_forward("\_<function\_>"))
            call goto_char(mb(0))
            found = forward_function_decl(name, anonymous)
            if (found) exit
        end do

        if (found) then
            call forward_group(1_int64, st)
        else
            ! ... or eob.
            call goto_char(point_max())
        end if
    end subroutine end_of_subprogram_nested

    !> Value of `end_of_subprogram_function' for `js_mode': move to the end of
    !> the ARGth function.  Port of js.el:js-end-of-defun.
    recursive subroutine js_end_of_subprogram(arg)
        integer(int64), intent(in) :: arg
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int64) :: n, prologue_begin, pos
        logical :: found, anonymous

        n = arg

        do while (.not. bobp() .and. n < 0)
            n = n + 1
            call js_beginning_of_subprogram(1_int64, found)
            call js_beginning_of_subprogram(1_int64, found)
            if (.not. bobp()) call js_end_of_subprogram(1_int64)
        end do

        do while (n > 0)
            n = n - 1

            ! look for function backward. if we're inside it, go to that
            ! function's end. otherwise, search for the next function's end
            ! and go there
            if (js_flat_functions%value) then
                call end_of_subprogram_flat()
            else
                ! if we're doing nested functions, see whether we're in the
                ! prologue. If we are, go to the end of the function;
                ! otherwise, call js--end-of-defun-nested to do the real
                ! work
                prologue_begin = function_prologue_beginning()

                if (prologue_begin > 0 .and. prologue_begin <= point()) then
                    call goto_char(prologue_begin)
                    call re_search_forward("\_<function", pos, st)
                    call goto_char(mb(0))
                    found = forward_function_decl(name, anonymous)
                    call forward_group(1_int64, st)
                else
                    call end_of_subprogram_nested()
                end if
            end if
        end do
    end subroutine js_end_of_subprogram

    ! `js_end_of_subprogram' as an `end_of_subprogram_function'.
    ! Not a port.
    recursive subroutine end_of_subprogram_proc(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call js_end_of_subprogram(1_int64)
    end subroutine end_of_subprogram_proc

    ! Return whether point is inside the given pitem's header or body.
    ! Port of js.el:js--inside-pitem-p.
    recursive logical function inside_pitem_p(pitem)
        integer, intent(in) :: pitem
        integer :: k

        call js_ensure_cache()
        k = here()
        inside_pitem_p = .false.
        if (.not. point() > locals(k)%pitems(pitem)%h_begin) return
        inside_pitem_p = locals(k)%pitems(pitem)%b_end == 0
        if (.not. inside_pitem_p) inside_pitem_p = locals(k)%pitems(pitem)%b_end > point()
    end function inside_pitem_p

    ! Parse the JavaScript program state at point.
    ! Return a list of pitems that apply to point, most specific first.
    ! In the worst case, the current toplevel instance will be returned.
    ! Port of js.el:js--parse-state-at-point.
    recursive function parse_state_at_point() result(pstate)
        integer, allocatable :: pstate(:)
        type(status_type) :: st
        integer(int64) :: opoint, begv, zv

        opoint = point()
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv
        call widen()
        call js_ensure_cache()
        pstate = backward_pstate()
        call goto_char(opoint)
        if (size(pstate) == 0) pstate = [1]

        ! Loop until we either hit a pitem at BOB or pitem ends after point
        ! (or at point if we're at eob)
        do
            if (pitem_type_of(pstate(1)) == PITEM_TOPLEVEL) exit
            if (inside_pitem_p(pstate(1))) exit
            pstate = pstate(2:)
        end do

        call narrow_to_region(begv, zv, st)
        call goto_char(opoint)
    end function parse_state_at_point

    ! Return the JavaScript syntactic context corresponding to PSTATE.
    ! Port of js.el:js--syntactic-context-from-pstate.
    recursive integer function context_from_pstate(pstate) result(context)
        integer, intent(in) :: pstate(:)
        integer :: type

        type = pitem_type_of(pstate(1))

        select case (type)
        case (PITEM_FUNCTION)
            context = JS_CONTEXT_FUNCTION
        case (PITEM_MACRO)
            context = JS_CONTEXT_MACRO
        case (PITEM_TOPLEVEL)
            context = JS_CONTEXT_TOPLEVEL
        case default
            context = JS_CONTEXT_CLASS
        end select
    end function context_from_pstate

    !> Return the JavaScript syntactic context at point (a JS_CONTEXT_*
    !> value).  Port of js.el:js-syntactic-context.
    recursive integer function js_syntactic_context() result(context)
        context = context_from_pstate(parse_state_at_point())
    end function js_syntactic_context

    ! The command `js_syntactic_context': also display a message with the
    ! context.
    recursive subroutine cmd_js_syntactic_context(status)
        use xdisp_fm, only: message
        type(status_type), intent(out) :: status
        character(len=*), parameter :: NAMES(0:3) = [character(len=8) :: "toplevel", "function", "macro", "class"]

        status = status_type()
        call message("Syntactic context: "//trim(NAMES(js_syntactic_context())))
    end subroutine cmd_js_syntactic_context

    ! ------------------------------------------------------------------
    ! Font Lock
    ! ------------------------------------------------------------------

    ! Return non-nil if point is in a function parameter list.  Port of
    ! js.el:js--inside-param-list-p.
    recursive logical function inside_param_list_p()
        type(status_type) :: st
        integer(int64) :: opoint

        inside_param_list_p = .false.
        opoint = point()
        call up_nearby_list(st)

        if (st%ok()) then
            if (at("(")) then
                call forward_symbol(-1)
                inside_param_list_p = at("function")

                if (.not. inside_param_list_p) then
                    call forward_symbol(-1)
                    inside_param_list_p = at("function")
                end if
            end if
        end if

        call goto_char(opoint)
    end function inside_param_list_p

    ! Return non-nil if point is in a Dojo multiple-inheritance class
    ! block.  Port of js.el:js--inside-dojo-class-list-p.
    recursive logical function inside_dojo_class_list_p()
        type(status_type) :: st
        integer(int64) :: opoint, list_begin

        inside_dojo_class_list_p = .false.
        opoint = point()
        call up_nearby_list(st)

        if (st%ok()) then
            list_begin = point()
            call goto_char(line_beginning_position())

            if (at(JS_DOJO_CLASS_DECL_RE)) then
                call goto_char(me(0))
                if (at("""\s-*,\s-*\[")) inside_dojo_class_list_p = me(0) == list_begin + 1
            end if
        end if

        call goto_char(opoint)
    end function inside_dojo_class_list_p

    ! Move forward over a JavaScript destructuring spec.
    ! If APPLY, highlight every variable name in the spec.  Return true if
    ! this was actually a spec.  Port of
    ! js.el:js--forward-destructuring-spec.
    recursive logical function forward_destructuring_spec(apply) result(spec)
        logical, intent(in) :: apply
        type(status_type) :: st
        logical :: moved, more
        integer :: c

        spec = .false.
        c = next_char()

        if (c == iachar("[")) then
            call forward_char(st, 1_int64)

            do
                moved = forward_comment(huge(1_int64))
                c = next_char()

                if (c == iachar("[") .or. c == iachar("{")) then
                    more = forward_destructuring_spec(apply)
                else if (c == iachar(",")) then
                    call forward_char(st, 1_int64)
                    more = .true.
                else if (at(JS_NAME_RE)) then
                    if (apply) call highlight_variable()
                    call goto_char(me(0))
                    more = .true.
                else
                    more = .false.
                end if

                if (.not. more) exit
            end do

            if (next_char() == iachar("]")) then
                call forward_char(st, 1_int64)
                spec = .true.
            end if
        else if (c == iachar("{")) then
            call forward_char(st, 1_int64)
            moved = forward_comment(huge(1_int64))

            do
                if (.not. at(JS_OBJFIELD_RE)) exit
                call goto_char(me(0))
                moved = forward_comment(huge(1_int64))
                c = next_char()

                if (c == iachar("[") .or. c == iachar("{")) then
                    more = forward_destructuring_spec(apply)
                else if (at(JS_NAME_RE)) then
                    if (apply) call highlight_variable()
                    call goto_char(me(0))
                    more = .true.
                else
                    more = .false.
                end if

                if (.not. more) exit
                moved = forward_comment(huge(1_int64))
                if (next_char() /= iachar(",")) exit
                call forward_char(st, 1_int64)
                moved = forward_comment(huge(1_int64))
            end do

            if (next_char() == iachar("}")) then
                call forward_char(st, 1_int64)
                spec = .true.
            end if
        end if
    end function forward_destructuring_spec

    ! (font-lock-apply-highlight '(0 font-lock-variable-name-face)).
    ! Port of js.el:js--font-lock-keywords-3 (in part).
    recursive subroutine highlight_variable()
        type(font_lock_highlight_type) :: hl
        type(status_type) :: st

        hl = font_lock_highlight(0, "font_lock_variable_name_face")
        call font_lock_apply_highlight(hl%match, st)
    end subroutine highlight_variable

    ! Font-lock matcher for variable names in a variable declaration.
    ! This is a cc-mode-style matcher that *always* fails, from the point
    ! of view of font-lock.  It applies highlighting directly with
    ! `font_lock_apply_highlight'.  Port of js.el:js--variable-decl-matcher.
    recursive subroutine variable_decl_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: begv, zv, opoint
        logical :: first, moved, ok, spec

        status = status_type()
        found = .false.
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv
        call narrow_to_region(point_min(), limit, status)
        first = .true.
        moved = forward_comment(huge(1_int64))

        do
            ok = first

            if (.not. ok) then
                if (next_char() == iachar(",")) then
                    call forward_char(status, 1_int64)
                    moved = forward_comment(huge(1_int64))
                    ok = .true.
                end if
            end if

            if (.not. ok) exit

            if (at(JS_NAME_RE)) then
                call highlight_variable()
                call goto_char(me(0))
            else
                opoint = point()
                spec = forward_destructuring_spec(.false.)
                call goto_char(opoint)
                if (.not. spec) exit
                spec = forward_destructuring_spec(.true.)
            end if

            moved = forward_comment(huge(1_int64))

            if (next_char() == iachar("=")) then
                call forward_char(status, 1_int64)
                call js_forward_expression(status)
                ! (scan-error: stop.)
                if (.not. status%ok()) exit
                moved = forward_comment(huge(1_int64))
            end if

            first = .false.
        end do

        status = status_type()
        call widen()
        call narrow_to_region(begv, zv, status)
        ! Matcher always "fails"
        found = .false.
    end subroutine variable_decl_matcher

    ! Font lock function used by `js_mode'.
    ! This performs fontification according to js--class-styles.  Port of
    ! js.el:js--class-decl-matcher.
    recursive subroutine class_decl_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name, decl, framework
        integer(int64) :: pos, orig_end
        logical :: prototype
        integer :: i, k

        status = status_type()
        found = .false.
        if (.not. any_framework_enabled()) return
        call js_ensure_cache(limit)
        k = here()

        do
            call re_search_forward(locals(k)%quick_match_re, pos, status, bound=limit, noerror=SEARCH_NOERROR)
            if (pos == 0) return
            orig_end = me(0)
            call goto_char(mb(0))

            do i = 1, NSTYLES
                call class_style(i, name, decl, prototype, framework)
                if (.not. framework_enabled(framework)) cycle
                if (js_syntactic_context() /= JS_CONTEXT_TOPLEVEL) cycle

                if (at(decl)) then
                    call goto_char(me(0))
                    found = .true.
                    return
                end if
            end do

            call goto_char(orig_end)
        end do
    end subroutine class_decl_matcher

    ! A matcher for REGEXP when FRAMEWORK is enabled (js--make-framework-matcher).
    ! Port of js.el:js--make-framework-matcher.
    recursive subroutine framework_search(framework, regexp, limit, found, status)
        character(len=*), intent(in) :: framework, regexp
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: pos

        status = status_type()
        found = .false.
        if (.not. framework_enabled(framework)) return
        call re_search_forward(regexp, pos, status, bound=limit, noerror=SEARCH_NOERROR)
        found = pos > 0
    end subroutine framework_search

    ! The three Dojo matchers.
    ! Not a port.
    recursive subroutine dojo_matcher_1(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        call framework_search("dojo", DOJO_MATCH_1_RE, limit, found, status)
    end subroutine dojo_matcher_1

    ! Not a port.
    recursive subroutine dojo_matcher_2(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        call framework_search("dojo", DOJO_MATCH_2_RE, limit, found, status)
    end subroutine dojo_matcher_2

    ! Not a port.
    recursive subroutine dojo_matcher_3(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        call framework_search("dojo", DOJO_MATCH_3_RE, limit, found, status)
    end subroutine dojo_matcher_3

    ! The PRE-MATCH-FORM of the class parts: (goto-char (match-beginning 1)).
    ! Port of js.el:js--font-lock-keywords-3 (in part).
    recursive subroutine class_part_pre(limit)
        integer(int64), intent(out) :: limit

        call goto_char(mb(1))
        limit = 0
    end subroutine class_part_pre

    ! The PRE-MATCH-FORM of the parent class parts: insert a "=" after the
    ! parent class to stop the search there.
    ! Port of js.el:js--font-lock-keywords-3 (in part).
    recursive subroutine parent_part_pre(limit)
        integer(int64), intent(out) :: limit
        type(status_type) :: st

        limit = 0

        if (mb(2) > 0) then
            tmp_location = me(2)
            call goto_char(tmp_location)
            call insert("=", st)
            call goto_char(mb(2))
        else
            tmp_location = 0
            call goto_char(line_end_position())
        end if
    end subroutine parent_part_pre

    ! The POST-MATCH-FORM of the parent class parts: delete the "=".
    ! Port of js.el:js--font-lock-keywords-3 (in part).
    recursive subroutine parent_part_post(limit)
        integer(int64), intent(out) :: limit
        type(status_type) :: st
        integer(int64) :: opoint

        limit = 0

        if (tmp_location > 0) then
            opoint = point()
            call del_range(tmp_location, tmp_location + 1, st)
            call goto_char(opoint)
        end if
    end subroutine parent_part_post

    ! (backward-char) and (end-of-line) as anchored forms.
    ! Port of js.el:js--font-lock-keywords-3 (in part).
    recursive subroutine backward_char_form(limit)
        integer(int64), intent(out) :: limit
        type(status_type) :: st

        limit = 0
        call forward_char(st, -1_int64)
    end subroutine backward_char_form

    ! Port of js.el:js--font-lock-keywords-3 (in part).
    recursive subroutine end_of_line_form(limit)
        integer(int64), intent(out) :: limit

        limit = 0
        call end_of_line()
    end subroutine end_of_line_form

    ! The PRE-MATCH-FORM of the continued Dojo base-class list.
    ! Port of js.el:js--font-lock-keywords-3 (in part).
    recursive subroutine dojo_continued_pre(limit)
        integer(int64), intent(out) :: limit
        logical :: inside

        limit = 0
        call goto_char(point() - 1)
        inside = inside_dojo_class_list_p()
        call goto_char(point() + 1)

        if (inside) then
            call forward_symbol(-1)
        else
            call end_of_line()
        end if
    end subroutine dojo_continued_pre

    ! The PRE-MATCH-FORM of the continued formal parameter list.
    ! Port of js.el:js--font-lock-keywords-3 (in part).
    recursive subroutine params_continued_pre(limit)
        integer(int64), intent(out) :: limit
        logical :: inside

        limit = 0
        call goto_char(point() - 1)
        inside = inside_param_list_p()
        call goto_char(point() + 1)

        if (inside) then
            call forward_symbol(-1)
        else
            call end_of_line()
        end if
    end subroutine params_continued_pre

    ! A highlight of subexpression N in FACE.
    ! Not a port.
    function h(n, face, override, lax) result(hl)
        integer, intent(in) :: n
        character(len=*), intent(in) :: face
        integer, intent(in), optional :: override
        logical, intent(in), optional :: lax
        type(font_lock_highlight_type) :: hl

        hl = font_lock_highlight(n, face=face, override=override, laxmatch=lax)
    end function h

    ! The keywords of the three levels (js--font-lock-keywords-1, -2 and -3).
    ! Not a port.
    subroutine init_keywords()
        character(len=*), parameter :: TY = "font_lock_type_face", VA = "font_lock_variable_name_face"
        type(font_lock_keyword_type), allocatable :: cpp(:), grown(:)
        type(font_lock_keyword_type) :: kw(14)
        integer :: n
        type(font_lock_highlight_type) :: h1(1), h2(2), a(1)
        type(font_lock_highlight_type), allocatable :: none(:)

        allocate (none(0))
        kw(1) = font_lock_keyword("\_<import\_>")
        h1(1) = h(1, "font_lock_function_name_face")
        kw(2) = font_lock_keyword(JS_FUNCTION_HEADING_1_RE, highlights=h1)
        kw(3) = font_lock_keyword(JS_FUNCTION_HEADING_2_RE, highlights=h1)
        keywords_1 = kw(1:3)
        h1(1) = h(1, "font_lock_keyword_face")
        kw(4) = font_lock_keyword(JS_KEYWORD_RE, highlights=h1)
        h1(1) = h(0, TY)
        kw(5) = font_lock_keyword(JS_BASIC_TYPE_RE, highlights=h1)
        h1(1) = h(0, "font_lock_constant_face")
        kw(6) = font_lock_keyword(JS_CONSTANT_RE, highlights=h1)
        keywords_2 = kw(1:6)
        ! This goes before keywords-2 so it gets used preferentially
        ! instead of the keywords in keywords-2.  Don't use override
        ! because that will override syntactic fontification too, which
        ! will fontify commented-out directives as if they weren't
        ! commented out.
        cpp = cpp_font_lock_keywords()
        allocate (keywords_3(size(cpp) + size(keywords_2)))
        keywords_3(1:size(cpp)) = cpp
        keywords_3(size(cpp) + 1:) = keywords_2
        h1(1) = h(1, "font_lock_constant_face")
        kw(1) = font_lock_keyword("\.\(prototype\)\_>", highlights=h1)
        ! Highlights class being declared, in parts
        h1(1) = h(1, TY)
        a(1) = font_lock_anchored(h1, regexp=CLASS_PART_RE, pre_match_form=class_part_pre)
        kw(2) = font_lock_keyword(matcher=class_decl_matcher, highlights=a)
        ! Highlights parent class, in parts, if available
        a(1) = font_lock_anchored(h1, regexp=CLASS_PART_RE, pre_match_form=parent_part_pre, &
                                  post_match_form=parent_part_post)
        kw(3) = font_lock_keyword(matcher=class_decl_matcher, highlights=a)
        ! Highlights parent class
        h1(1) = h(2, TY, lax=.true.)
        kw(4) = font_lock_keyword(matcher=class_decl_matcher, highlights=h1)
        ! Dojo needs its own matcher to override the string highlighting
        h2(1) = h(1, TY, FONT_LOCK_OVERRIDE_T)
        h2(2) = h(2, TY, lax=.true.)
        kw(5) = font_lock_keyword(matcher=dojo_matcher_1, highlights=h2)
        ! Match Dojo base classes. Of course Mojo has to be different from
        ! everything else under the sun...
        h1(1) = h(1, TY)
        a(1) = font_lock_anchored(h1, regexp=DOJO_BASE_RE, pre_match_form=backward_char_form, &
                                  post_match_form=end_of_line_form)
        kw(6) = font_lock_keyword(matcher=dojo_matcher_2, highlights=a)
        ! continued Dojo base-class list
        a(1) = font_lock_anchored(h1, regexp=DOJO_CONTINUED_RE, pre_match_form=dojo_continued_pre, &
                                  post_match_form=end_of_line_form)
        kw(7) = font_lock_keyword(matcher=dojo_matcher_3, highlights=a)
        ! variable declarations
        a(1) = font_lock_anchored(none, matcher=variable_decl_matcher)
        kw(8) = font_lock_keyword(VARIABLE_DECL_RE, highlights=a)
        ! class instantiation
        h1(1) = h(1, TY)
        kw(9) = font_lock_keyword(NEW_RE, highlights=h1)
        ! instanceof
        kw(10) = font_lock_keyword(INSTANCEOF_RE, highlights=h1)
        ! formal parameters
        h1(1) = h(1, VA)
        a(1) = font_lock_anchored(h1, regexp=FORMAL_PARAM_RE, pre_match_form=backward_char_form, &
                                  post_match_form=end_of_line_form)
        kw(11) = font_lock_keyword(FORMAL_PARAMS_RE, highlights=a)
        ! continued formal parameter list
        h1(1) = h(0, VA)
        a(1) = font_lock_anchored(h1, regexp=JS_NAME_RE, pre_match_form=params_continued_pre, &
                                  post_match_form=end_of_line_form)
        kw(12) = font_lock_keyword(CONTINUED_PARAMS_RE, highlights=a)
        n = size(keywords_3)
        allocate (grown(n + 12))
        grown(1:n) = keywords_3
        grown(n + 1:) = kw(1:12)
        call move_alloc(grown, keywords_3)
    end subroutine init_keywords

    !> Return syntactic face given STATE.  Port of
    !> js.el:js-font-lock-syntactic-face-function.
    subroutine js_font_lock_syntactic_face_function(state, face)
        type(parse_state_type), intent(in) :: state
        character(len=:), allocatable, intent(out) :: face
        integer(int64) :: opoint
        logical :: doc

        if (state%instring >= 0) then
            face = "font_lock_string_face"
            return
        end if

        opoint = point()
        call goto_char(state%comstr_start)
        doc = at("/\*\*")
        call goto_char(opoint)

        if (doc) then
            face = "font_lock_doc_face"
        else
            face = "font_lock_comment_face"
        end if
    end subroutine js_font_lock_syntactic_face_function

    ! ------------------------------------------------------------------
    ! Syntax properties
    ! ------------------------------------------------------------------

    ! If point is in a /.../ regexp, put the string fence syntax on its
    ! closing "/" (not beyond END).  Port of
    ! js.el:js-syntax-propertize-regexp.
    recursive subroutine syntax_propertize_regexp(end_in)
        integer(int64), intent(in) :: end_in
        type(parse_state_type) :: state
        type(status_type) :: st
        integer(int64) :: end

        end = end_in
        state = ppss()
        if (state%instring /= iachar("/")) return
        ! A /.../ regexp.
        call goto_char(state%comstr_start)
        if (.not. at(REGEXP_LITERAL_RE)) return
        ! Don't touch text after END.
        if (end > me(1)) end = me(1)
        call put_text_property(mb(1), end, sym("syntax_table"), string_fence(), st)
        call goto_char(end)
    end subroutine syntax_propertize_regexp

    ! (string-to-syntax "\"/"): a string quote matching "/".
    ! Not a port.
    recursive function string_fence() result(v)
        use syntax_fm, only: syntax_descriptor_value, syntax_descriptor_type, string_to_syntax
        type(value_type) :: v
        type(syntax_descriptor_type) :: d
        type(status_type) :: st

        call string_to_syntax(""""//"/", d, st)
        v = syntax_descriptor_value(d)
    end function string_fence

    ! The value of the regexp literal rule's highlight: treat a "/" as the
    ! start of a regexp after an operator or keyword that can precede one.
    ! Port of js.el:js-syntax-propertize (in part: its regexp literal rule).
    recursive subroutine slash_value(property, status)
        type(syntax_property_type), intent(out) :: property
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: opoint, end
        logical :: regexp, moved
        integer :: c

        status = status_type()
        property%apply = .false.
        end = propertize_end
        call forward_char(st, -1_int64)
        c = after_char_at(mb(0))
        regexp = c /= iachar(" ") .and. c /= 9

        if (.not. regexp) then
            ! If the / is at the beginning of line, we have to check the
            ! end of the previous text.
            opoint = point()
            call goto_char(mb(0))
            moved = forward_comment(-point())
            c = prev_char()

            if (c == -1) then
                regexp = .true.
            else if (c < 128) then
                regexp = index("=({[,:;", achar(c)) > 0
            end if

            call goto_char(opoint)
        end if

        if (regexp) then
            call put_text_property(mb(1), me(1), sym("syntax_table"), string_fence(), st)
            call syntax_propertize_regexp(end)
        end if
    end subroutine slash_value

    !> The `syntax_propertize_function' of JavaScript mode: JavaScript
    !> allows immediate regular expression objects, written /.../.  Port of
    !> js.el:js-syntax-propertize.
    recursive subroutine js_syntax_propertize(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        status = status_type()
        call goto_char(start)
        call syntax_propertize_regexp(end)
        propertize_end = end
        call propertize_rules%apply(point(), end, status)
    end subroutine js_syntax_propertize

    ! ------------------------------------------------------------------
    ! Indentation
    ! ------------------------------------------------------------------

    ! Return non-nil if point is on a JavaScript operator, other than a
    ! comma.  Port of js.el:js--looking-at-operator-p.
    recursive logical function looking_at_operator_p() result(op)
        type(status_type) :: st
        type(parse_state_type) :: state
        integer(int64) :: opoint
        integer, allocatable :: saved(:)
        logical :: question, generator
        integer :: c

        op = .false.
        call save_match(saved)
        opoint = point()

        if (.not. at(INDENT_OPERATOR_RE)) then
            call restore_match(saved)
            return
        end if

        c = next_char()

        if (c == iachar(":")) then
            call backward_syntactic_ws()
            if (prev_char() == iachar(")")) call backward_group(1_int64, st)
            question = .false.
            if (js_re_search_backward("[?:{]\|\_<case\_>")) question = next_char() == iachar("?")
            call goto_char(opoint)

            if (.not. question) then
                call restore_match(saved)
                return
            end if
        end if

        if (c == iachar("/")) then
            state = ppss()
            call goto_char(opoint)

            if (state%instring == iachar("/")) then
                call restore_match(saved)
                return
            end if
        end if

        if (c == iachar("*")) then
            ! Generator method (possibly using computed property).
            generator = at(GENERATOR_RE)

            if (generator) then
                call backward_syntactic_ws()
                ! We might misindent some expressions that would return NaN
                ! anyway.  Shouldn't be a problem.
                c = prev_char()
                generator = c == iachar(",") .or. c == iachar("}") .or. c == iachar("{")
                call goto_char(opoint)
            end if

            if (generator) then
                call restore_match(saved)
                return
            end if
        end if

        op = .true.
        call restore_match(saved)
    end function looking_at_operator_p

    ! save-match-data: the match data, in SAVED.
    ! Not a port.
    subroutine save_match(saved)
        use search_fm, only: match_data
        integer, allocatable, intent(out) :: saved(:)
        integer(int64), allocatable :: data(:)

        data = match_data()
        saved = int(data)
    end subroutine save_match

    ! Restore the match data SAVED.
    ! Not a port.
    subroutine restore_match(saved)
        use search_fm, only: set_match_data
        integer, intent(in) :: saved(:)
        integer(int64), allocatable :: data(:)

        data = int(saved, int64)
        call set_match_data(data)
    end subroutine restore_match

    ! Move backward to the nearest newline that is not in a block comment.
    ! Port of js.el:js--find-newline-backward.
    recursive logical function find_newline_backward() result(found)
        type(parse_state_type) :: parse
        type(status_type) :: st
        integer(int64) :: pos
        logical :: again

        found = .true.

        do
            again = .false.
            call search_backward(LF, pos, st, noerror=SEARCH_NOERROR)

            if (pos > 0) then
                parse = ppss()

                ! We match the end of a // comment but not a newline in a
                ! block comment.
                if (parse%incomment /= 0) then
                    call goto_char(parse%comstr_start)
                    ! If we saw a block comment, keep trying.
                    if (parse%comment_style == 0) again = .true.
                end if
            else
                found = .false.
            end if

            if (.not. again) exit
        end do
    end function find_newline_backward

    !> Return true if the current line continues an expression.  Port of
    !> js.el:js--continued-expression-p.
    recursive logical function js_continued_expression_p() result(continued)
        type(parse_state_type) :: state
        integer(int64) :: opoint
        type(status_type) :: st
        logical :: moved, ok
        integer :: c

        opoint = point()
        call back_to_indentation()
        continued = .false.

        if (looking_at_operator_p()) then
            if (next_char() == iachar("/")) then
                state = ppss_at(point() + 1)
                continued = state%instring < 0
            else
                c = next_char()
                continued = .not. (c == iachar("-") .or. c == iachar("+"))

                if (.not. continued) then
                    moved = forward_comment(-point())
                    c = prev_char()
                    continued = .not. (c == iachar(",") .or. c == iachar("[") .or. c == iachar("("))
                end if
            end if
        else if (find_newline_backward()) then
            moved = skip_chars_backward(" "//TAB, st) < 0
            if (.not. bobp()) call goto_char(point() - 1)

            if (point() > point_min()) then
                call goto_char(point() - 1)
                ok = .not. at("[/*]/\|=>")
                call goto_char(point() + 1)

                if (ok) then
                    if (looking_at_operator_p()) then
                        call goto_char(point() - 1)
                        continued = .not. at("\+\+\|--\|/[/*]")
                    end if
                end if
            end if
        end if

        call goto_char(opoint)
    end function js_continued_expression_p

    ! Skip a term before point; return t if a term was skipped.  Port of
    ! js.el:js--skip-term-backward.
    recursive logical function skip_term_backward() result(dot)
        type(status_type) :: st
        integer(int64) :: save_point, moved
        logical :: progress, term_skipped
        integer :: c

        term_skipped = .false.
        dot = .false.
        ! Skip backward over balanced parens.
        progress = .true.

        do while (progress)
            progress = .false.
            ! First skip whitespace.
            moved = skip_syntax_backward(" ")
            ! Now if we're looking at closing paren, skip to the opener.
            ! This doesn't strictly follow JS syntax, in that we might skip
            ! something nonsensical like "()[]{}", but it is enough if it
            ! works ok for valid input.
            c = prev_char()

            if (c == iachar("]") .or. c == iachar(")") .or. c == iachar("}")) then
                progress = .true.
                term_skipped = .true.
                call backward_group(1_int64, st)
            end if
        end do

        ! Maybe skip over a symbol.
        save_point = point()

        if (skip_syntax_backward("w_") < 0 .and. at(JS_NAME_RE)) then
            ! Skipped.
            term_skipped = .true.
            moved = skip_syntax_backward(" ")
        else
            ! Did not skip, so restore point.
            call goto_char(save_point)
        end if

        if (term_skipped .and. point() > point_min()) then
            call forward_char(st, -1_int64)
            dot = next_char() == iachar(".")
        end if
    end function skip_term_backward

    ! Skip any number of terms backward.
    ! Move point to the earliest "." without changing paren levels.
    ! Returns t if successful, nil if no term was found.  Port of
    ! js.el:js--skip-terms-backward.
    recursive logical function skip_terms_backward() result(found)
        integer(int64) :: last_point

        found = .false.
        if (.not. skip_term_backward()) return
        ! Found at least one.
        last_point = point()

        do while (skip_term_backward())
            last_point = point()
        end do

        call goto_char(last_point)
        found = .true.
    end function skip_terms_backward

    ! Helper for `js_proper_indentation' that handles chained expressions:
    ! the indentation of the current line if it is a chained expression
    ! line (`found'), when `js_chain_indent' is true.  Port of
    ! js.el:js--chained-expression-p.
    recursive subroutine chained_expression_p(column, found)
        integer, intent(out) :: column
        logical, intent(out) :: found
        integer(int64) :: opoint

        column = 0
        found = .false.
        if (.not. js_chain_indent%value) return
        opoint = point()

        if (next_char() == iachar(".")) then
            if (js_continued_expression_p()) then
                if (find_newline_backward()) then
                    if (skip_terms_backward()) then
                        column = int(current_column())
                        found = .true.
                    end if
                end if
            end if
        end if

        call goto_char(opoint)
    end subroutine chained_expression_p

    ! Return true if point is on the "while" of a do-while statement.  A
    ! braceless do-while statement spanning several lines requires that
    ! the start of the loop is indented to the same column as the current
    ! line.  Port of js.el:js--end-of-do-while-loop-p.
    recursive logical function end_of_do_while_loop_p() result(found)
        type(status_type) :: st
        integer(int64) :: opoint, moved, saved_indent, pos
        integer, allocatable :: saved(:)
        logical :: brace

        found = .false.
        opoint = point()
        call save_match(saved)

        if (at("\s-*\_<while\_>")) then
            moved = skip_chars_backward(" "//TAB//LF//"}", st)
            brace = at("[ "//TAB//LF//"]*}")
            call goto_char(opoint)

            if (brace) then
                call backward_group(1_int64, st)
                call forward_symbol(-1)
                found = at("\_<do\_>")
            else
                pos = line_beginning_position()
                if (js_re_search_backward("\_<do\_>", pos)) continue
                found = at("\_<do\_>")

                if (.not. found) then
                    saved_indent = current_indentation()

                    do
                        if (.not. js_re_search_backward("^\s-*\_<")) exit
                        if (current_indentation() == saved_indent) exit
                    end do

                    if (at("\s-*\_<do\_>")) then
                        pos = point()
                        found = .not. js_re_search_forward("\_<while\_>", line_end_position())
                        call goto_char(pos)
                        if (found) found = current_indentation() == saved_indent
                    end if
                end if
            end if
        end if

        call goto_char(opoint)
        call restore_match(saved)
    end function end_of_do_while_loop_p

    ! Helper function for `js_proper_indentation'.
    ! Return the proper indentation of the current line if it starts the
    ! body of a control statement without braces (`found').  Port of
    ! js.el:js--ctrl-statement-indentation.
    recursive subroutine ctrl_statement_indentation(column, found)
        integer, intent(out) :: column
        logical, intent(out) :: found
        type(status_type) :: st
        integer(int64) :: opoint, start, moved
        logical :: ok
        integer :: c

        column = 0
        found = .false.
        opoint = point()
        call back_to_indentation()
        start = point()
        ok = line_beginning_position() /= point_min()
        if (ok) ok = .not. at("[{]")
        if (ok) ok = js_re_search_backward("[[:graph:]]")

        if (ok) then
            if (.not. eobp()) call forward_char(st, 1_int64)
            if (prev_char() == iachar(")")) call backward_group(1_int64, st)
            moved = skip_syntax_backward(" ")
            moved = skip_syntax_backward("w_")
            ok = at(POSSIBLY_BRACELESS_KEYWORD_RE)
        end if

        if (ok) then
            c = prev_char()
            ok = c == iachar(" ") .or. c == 9 .or. c == 10 .or. c == iachar("}")
        end if

        if (ok) then
            call goto_char(start)
            ok = .not. end_of_do_while_loop_p()
        end if

        if (ok) then
            call goto_char(mb(0))
            column = int(current_indentation() + js_indent_level%value)
            found = .true.
        end if

        call goto_char(opoint)
    end subroutine ctrl_statement_indentation

    ! Whether POS is on the current line.  Port of js.el:js--same-line.
    recursive logical function same_line(pos)
        integer(int64), intent(in) :: pos

        same_line = pos >= line_beginning_position() .and. pos <= line_end_position()
    end function same_line

    ! Helper function for `js_proper_indentation'.
    ! Return the proper indentation of the current line if it belongs to a
    ! declaration statement spanning multiple lines (`found').  Port of
    ! js.el:js--multi-line-declaration-indentation.
    recursive subroutine multi_line_declaration_indentation(column, found)
        integer, intent(out) :: column
        logical, intent(out) :: found
        type(parse_state_type) :: state
        type(status_type) :: st
        integer(int64) :: opoint, pt, pos, moved
        logical :: at_opening_bracket, more

        column = 0
        found = .false.
        at_opening_bracket = .false.
        opoint = point()
        call back_to_indentation()

        if (.not. at(DECLARATION_KEYWORD_RE)) then
            pt = point()
            if (at(INDENT_OPERATOR_RE)) call goto_char(me(0))
            ! The "operator" is probably a regexp literal opener.
            state = ppss()
            if (state%instring >= 0) call goto_char(pt)

            do
                if (at_opening_bracket .or. bobp()) exit
                pos = point()

                backward_check: block
                    call backward_syntactic_ws()

                    if (prev_char() == iachar(",")) then
                        more = .true.
                        exit backward_check
                    end if

                    more = .false.

                    if (prev_char() /= iachar(";")) then
                        moved = skip_syntax_backward(".")
                        more = at(INDENT_OPERATOR_RE)
                        call backward_syntactic_ws()
                        if (more) more = prev_char() /= iachar(";")
                    end if

                    if (.not. more) more = same_line(pos)
                end block backward_check

                call goto_char(pos)
                if (.not. more) exit
                call backward_expression(1_int64, st)
                if (.not. st%ok()) at_opening_bracket = .true.
            end do

            if (at(DECLARATION_KEYWORD_RE)) then
                call goto_char(me(0))
                column = int(current_column()) + 1
                found = .true.
            end if
        end if

        call goto_char(opoint)
    end subroutine multi_line_declaration_indentation

    ! Return the position of the first `for' if we think we're in an array
    ! comprehension starting at BRACKET (0 for none).  Port of
    ! js.el:js--indent-in-array-comp.
    recursive integer(int64) function indent_in_array_comp(bracket) result(for_kwd)
        integer(int64), intent(in) :: bracket
        type(parse_state_type) :: status_state
        type(status_type) :: st
        integer(int64) :: end, opoint, pos

        for_kwd = 0
        end = point()
        opoint = point()
        call goto_char(bracket)

        if (at("\[")) then
            call forward_char(st, 1_int64)
            call forward_syntactic_ws()

            if (at("[[{]")) then
                ! Skip destructuring form.
                call forward_expression(1_int64, st)

                if (st%ok()) then
                    call forward_syntactic_ws()

                    ! Regular array.
                    if (next_char() /= iachar(",")) then
                        if (at("for")) for_kwd = mb(0)
                    end if
                end if
            else if (end > point()) then
                ! To skip arbitrary expressions we need the parser, so
                ! we'll just guess at it.
                call re_search_forward("[^,]]* \(for\_>\)", pos, st, bound=end, noerror=SEARCH_NOERROR)

                if (pos > 0) then
                    ! Not inside comment or string literal.
                    call parse_partial_expression(bracket, point(), status_state, st)
                    if (status_state%depth == 1 .and. status_state%comstr_start == 0) for_kwd = mb(1)
                end if
            end if
        end if

        call goto_char(opoint)
    end function indent_in_array_comp

    ! The indentation of a line of the array comprehension at BRACKET
    ! whose first `for' is at FOR_KWD.  Port of
    ! js.el:js--array-comp-indentation.
    recursive integer function array_comp_indentation(bracket, for_kwd) result(column)
        integer(int64), intent(in) :: bracket, for_kwd
        type(status_type) :: st
        integer(int64) :: opoint, moved

        opoint = point()

        if (same_line(for_kwd)) then
            ! First continuation line.
            call goto_char(bracket)
            call forward_char(st, 1_int64)
            moved = skip_chars_forward(" "//TAB, st)
            column = int(current_column())
        else
            call goto_char(for_kwd)
            column = int(current_column())
        end if

        call goto_char(opoint)
    end function array_comp_indentation

    ! Helper function for `js_proper_indentation'.
    ! Depending on the value of `js_indent_first_init', move point to the
    ! end of a variable declaration keyword so that indentation is aligned
    ! to that column.  Port of js.el:js--maybe-goto-declaration-keyword-end.
    recursive subroutine maybe_goto_declaration_keyword_end(parse_status)
        type(parse_state_type), intent(in) :: parse_status
        type(status_type) :: st
        integer(int64) :: declaration_keyword_end, opoint
        logical :: comma_p, moved

        select case (int(js_indent_first_init%value))
        case (JS_INDENT_FIRST_INIT_T)
            if (at(DECLARATION_KEYWORD_RE)) call goto_char(me(0) + 1)
        case (JS_INDENT_FIRST_INIT_DYNAMIC)
            if (.not. at(DECLARATION_KEYWORD_RE)) return
            declaration_keyword_end = me(0)
            opoint = point()
            call goto_char(parse_status%innermost_start)
            call forward_expression(1_int64, st)
            comma_p = .false.

            if (st%ok()) then
                do
                    if (.not. forward_comment(1_int64)) exit
                end do

                comma_p = at_p(",")
            end if

            call goto_char(opoint)
            if (comma_p) call goto_char(declaration_keyword_end + 1)
        case default
            continue
        end select

        if (.false.) moved = .true.
    end subroutine maybe_goto_declaration_keyword_end

    ! Helper function for `js_proper_indentation'.
    ! Return true if the last non-comment, non-whitespace token of the
    ! current line is the "=>" token (of an arrow function).  Port of
    ! js.el:js--broken-arrow-terminates-line-p.
    recursive logical function broken_arrow_terminates_line_p()
        type(status_type) :: st
        integer(int64) :: from, pos

        from = point()
        call end_of_line()
        call re_search_backward(LINE_TERMINATING_ARROW_RE, pos, st, bound=from, noerror=SEARCH_NOERROR)
        broken_arrow_terminates_line_p = pos > 0
    end function broken_arrow_terminates_line_p

    ! The column of a line inside a comment starting at START
    ! (js--get-c-offset 'c START): the comment line-up function's.  Port of
    ! js.el:js--get-c-offset (c-get-syntactic-indentation with the
    ! syntactic symbol `c' alone, whose line-up function gives an absolute
    ! column).
    recursive integer function get_c_offset(start)
        integer(int64), intent(in) :: start
        procedure(c_lineup_proc), pointer :: fn

        fn => js_comment_lineup_func
        if (.not. associated(fn)) fn => c_lineup_c_comments
        ! (c-comment-prefix-regexp as js-mode sets it.)
        get_c_offset = fn(start, "//+\|\**")
    end function get_c_offset

    !> Return the proper indentation for the current line, whose line
    !> beginning has the parse state PARSE_STATUS.  Port of
    !> js.el:js--proper-indentation (JSX is deferred).
    recursive integer function js_proper_indentation(parse_status) result(column)
        type(parse_state_type), intent(in) :: parse_status
        type(status_type) :: st
        integer(int64) :: opoint, bracket, beg, moved
        logical :: found, same_indent_p, switch_keyword_p, continued_expr_p, in_switch_p, nothing_after, macro
        integer :: c, indent

        opoint = point()
        call back_to_indentation()

        if (parse_status%incomment /= 0) then
            ! inside comment
            column = get_c_offset(parse_status%comstr_start)
            call goto_char(opoint)
            return
        end if

        if (parse_status%instring >= 0) then
            ! inside string
            column = 0
            call goto_char(opoint)
            return
        end if

        if (next_char() == iachar("#")) then
            call forward_char(st, 1_int64)
            found = at_p(CPP_FONT_LOCK_KEYWORDS_SOURCE_DIRECTIVES)
            call forward_char(st, -1_int64)

            if (found) then
                column = 0
                call goto_char(opoint)
                return
            end if
        end if

        beg = point()
        macro = beginning_of_macro()
        call goto_char(beg)

        if (macro) then
            column = 4
            call goto_char(opoint)
            return
        end if

        ! Indent array comprehension continuation lines specially.
        bracket = parse_status%innermost_start

        if (bracket > 0) then
            if (.not. same_line(bracket)) then
                beg = indent_in_array_comp(bracket)

                ! At or after the first loop?
                if (beg > 0 .and. point() >= beg) then
                    column = array_comp_indentation(bracket, beg)
                    call goto_char(opoint)
                    return
                end if
            end if
        end if

        call chained_expression_p(column, found)
        if (.not. found) call ctrl_statement_indentation(column, found)
        if (.not. found) call multi_line_declaration_indentation(column, found)

        if (found) then
            call goto_char(opoint)
            return
        end if

        if (bracket > 0) then
            ! A single closing paren/bracket should be indented at the same
            ! level as the opening statement.  Same goes for "case" and
            ! "default".
            same_indent_p = at("[]})]")
            switch_keyword_p = at("default\_>\|case\_>[^:]")
            continued_expr_p = js_continued_expression_p()
            ! go to the opening char
            call goto_char(bracket)
            nothing_after = .not. js_indent_align_list_continuation%value
            if (.not. nothing_after) nothing_after = at("[({[]\s-*\(/[/*]\|$\)")

            if (.not. nothing_after) then
                call forward_char(st, 1_int64)
                nothing_after = broken_arrow_terminates_line_p()
                call goto_char(bracket)
            end if

            if (nothing_after) then
                ! nothing following the opening paren/bracket
                moved = skip_syntax_backward(" ")
                if (prev_char() == iachar(")")) call backward_group(1_int64, st)
                call back_to_indentation()
                call maybe_goto_declaration_keyword_end(parse_status)
                in_switch_p = .false.
                if (.not. same_indent_p) in_switch_p = at("\_<switch\_>")
                same_indent_p = same_indent_p .or. (switch_keyword_p .and. in_switch_p)
                indent = int(current_column())

                if (same_indent_p) then
                    continue
                else if (continued_expr_p) then
                    indent = indent + int(2*js_indent_level%value + js_expr_indent_offset%value)
                else
                    indent = indent + int(js_indent_level%value)
                    c = after_char_at(bracket)

                    if (c == iachar("(")) then
                        indent = indent + int(js_paren_indent_offset%value)
                    else if (c == iachar("[")) then
                        indent = indent + int(js_square_indent_offset%value)
                    else if (c == iachar("{")) then
                        indent = indent + int(js_curly_indent_offset%value)
                    end if
                end if

                if (in_switch_p) indent = indent + int(js_switch_indent_offset%value)
                column = indent
            else
                ! If there is something following the opening paren/bracket,
                ! everything else should be indented at the same level.
                if (.not. same_indent_p) then
                    call forward_char(st, 1_int64)
                    moved = skip_chars_forward(" "//TAB, st)
                end if

                column = int(current_column())
            end if
        else if (js_continued_expression_p()) then
            column = int(js_indent_level%value + js_expr_indent_offset%value)
        else
            column = int(prog_first_column())
        end if

        call goto_char(opoint)
    end function js_proper_indentation

    !> Indent the current line as JavaScript.  Port of js.el:js-indent-line.
    recursive subroutine js_indent_line(status)
        type(status_type), intent(out) :: status
        type(parse_state_type) :: parse_status
        integer(int64) :: opoint, offset, bol

        status = status_type()
        opoint = point()
        call back_to_indentation()
        offset = opoint - point()
        bol = line_beginning_position()
        parse_status = ppss_at(bol)
        call goto_char(opoint)
        if (parse_status%instring >= 0) return
        call indent_line_to(int(js_proper_indentation(parse_status), int64), status)
        if (offset > 0) call forward_char(status, offset)
    end subroutine js_indent_line

    ! ------------------------------------------------------------------
    ! Modes
    ! ------------------------------------------------------------------

    ! js-mode-syntax-table: C's syntax (c-populate-syntax-table) with $ a
    ! symbol constituent and ` a string quote.
    ! Not a port.
    subroutine init_syntax_table()
        type(status_type) :: st
        type(handle_type) :: t
        character(len=*), parameter :: PUNCT = "%&+-<=>|"
        integer :: i

        call make_syntax_table(js_mode_syntax_table, st)
        t = js_mode_syntax_table

        do i = 1, len(PUNCT)
            call modify_syntax_entry(iachar(PUNCT(i:i)), iachar(PUNCT(i:i)), ".", st, t)
        end do

        call modify_syntax_entry(10, 10, "> b", st, t)
        call modify_syntax_entry(13, 13, "> b", st, t)
        call modify_syntax_entry(iachar("'"), iachar("'"), """", st, t)
        call modify_syntax_entry(iachar("*"), iachar("*"), ". 23", st, t)
        call modify_syntax_entry(iachar("/"), iachar("/"), ". 124b", st, t)
        call modify_syntax_entry(iachar("$"), iachar("$"), "_", st, t)
        call modify_syntax_entry(iachar("`"), iachar("`"), """", st, t)
    end subroutine init_syntax_table

    ! js-syntax-propertize's rules.
    ! Not a port.
    subroutine init_rules()
        type(syntax_highlight_type) :: hl

        allocate (propertize_rules%rules(2))
        ! Distinguish /-division from /-regexp chars (and from
        ! /-comment-starter).
        propertize_rules%rules(1)%regexp = PROPERTIZE_SLASH_RE
        hl = syntax_highlight_type(group=1)
        hl%value => slash_value
        propertize_rules%rules(1)%highlights = [hl]
        propertize_rules%rules(2)%regexp = "\`\(#\)!"
        hl = syntax_highlight_type(group=1, syntax="< b")
        propertize_rules%rules(2)%highlights = [hl]
    end subroutine init_rules

    !> Generic major mode for editing JavaScript.
    !>
    !> This mode is intended to be inherited by concrete major modes.
    !> Currently there is `js_mode'.  Port of js.el:js-base-mode (the
    !> hideshow variables are deferred).
    recursive subroutine js_base_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. syntax_table_live_p(js_mode_syntax_table)) call syms_of_js()
        call major_modes%js_base_mode%begin(saved, status)
        if (.not. status%ok()) return
        call major_modes%js_base_mode%finish(saved, status)
    end subroutine js_base_mode

    ! The setup js-mode and js-json-mode share.  Port of js.el:js--mode-setup.
    recursive subroutine mode_setup()
        type(font_lock_keywords_type) :: levels(4)
        integer :: k

        k = here()
        call indent_line_function%set_local(indent_line_proc)
        call beginning_of_subprogram_function%set_local(beginning_of_subprogram_proc)
        call end_of_subprogram_function%set_local(end_of_subprogram_proc)
        levels(1) = font_lock_keywords_level(keywords_3)
        levels(2) = font_lock_keywords_level(keywords_1)
        levels(3) = font_lock_keywords_level(keywords_2)
        levels(4) = font_lock_keywords_level(keywords_3)
        call font_lock_defaults%set_local(levels=levels, syntactic_face_function=js_font_lock_syntactic_face_function)
        call syntax_propertize_function%set_local(js_syntax_propertize)
        call syntax_propertize_extend_region_functions%add(syntax_propertize_multiline, depth=90, local=.true.)
        call parse_expression_ignore_comments%set_local(.true.)
        ! Comments
        call comment_start%set_local("// ")
        call comment_start_skip%set_local("\(?://+\|/\*+\)\s *")
        call comment_end%set_local("")
        ! Parse cache
        call before_change_functions%add(flush_caches_hook, depth=90, local=.true.)
        ! Frameworks
        call update_quick_match_re()
        call set_local_electric_indent_chars([iachar("{"), iachar("}"), iachar("("), iachar(")"), iachar(":"), &
                                              iachar(";"), iachar(","), electric_indent_chars_value()])
    end subroutine mode_setup

    !> Major mode for editing JavaScript.  Port of js.el:js-mode.
    recursive subroutine js_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. syntax_table_live_p(js_mode_syntax_table)) call syms_of_js()
        call major_modes%js_mode%begin(saved, status)
        if (.not. status%ok()) return
        call mode_setup()
        call major_modes%js_mode%finish(saved, status)
    end subroutine js_mode

    !> Major mode for editing JSON.  Port of js.el:js-json-mode
    !> (`editorconfig_indent_size_vars' is deferred).
    recursive subroutine js_json_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        integer :: k

        if (.not. syntax_table_live_p(js_mode_syntax_table)) call syms_of_js()
        call major_modes%js_json_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! Reuse most of `js-mode', but not as parent (bug#67463).
        call mode_setup()
        k = here()
        locals(k)%has_frameworks = .true.
        allocate (locals(k)%frameworks(0))
        ! Speed up `syntax-ppss': JSON files can be big but can't hold
        ! regexp matchers nor #! thingies (and `js-enabled-frameworks' is
        ! nil).
        call syntax_propertize_function%set_local(ignore_propertize)
        call major_modes%js_json_mode%finish(saved, status)
    end subroutine js_json_mode

    ! The `syntax_propertize_function' ignore.
    ! Not a port.
    recursive subroutine ignore_propertize(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        status = status_type()
        if (start > end) continue
    end subroutine ignore_propertize

    ! `js_indent_line' as an `indent_line_function'.
    ! Not a port.
    recursive subroutine indent_line_proc(status, noindent)
        type(status_type), intent(out) :: status
        logical, intent(out) :: noindent

        noindent = .false.
        call js_indent_line(status)
    end subroutine indent_line_proc

    ! The command `js_indent_line'.
    recursive subroutine cmd_js_indent_line(status)
        type(status_type), intent(out) :: status

        call js_indent_line(status)
    end subroutine cmd_js_indent_line

    !> Define JavaScript mode: its variables, tables and commands.
    !> Not a port.
    recursive subroutine syms_of_js()
        if (syntax_table_live_p(js_mode_syntax_table)) return
        call forward_setting(js_indent_level)
        call forward_setting(js_expr_indent_offset)
        call forward_setting(js_paren_indent_offset)
        call forward_setting(js_square_indent_offset)
        call forward_setting(js_curly_indent_offset)
        call forward_setting(js_switch_indent_offset)
        call forward_setting(js_flat_functions)
        call forward_setting(js_indent_align_list_continuation)
        call forward_setting(js_indent_first_init)
        call forward_setting(js_chain_indent)
        if (.not. associated(js_comment_lineup_func)) js_comment_lineup_func => c_lineup_c_comments
        if (.not. allocated(js_enabled_frameworks)) js_enabled_frameworks = AVAILABLE_FRAMEWORKS
        call init_syntax_table()
        call init_rules()
        call init_keywords()
        call add_local_variables_killer(kill_js_locals)
        call major_modes%js_base_mode%define("js_base_mode", "JavaScript", js_base_mode, js_base_mode_hook, &
                                             parent=major_modes%prog_mode, syntax_table=js_mode_syntax_table)
        js_base_mode_map = major_modes%js_base_mode%map
        call major_modes%js_mode%define("js_mode", "JavaScript", js_mode, js_mode_hook, parent=major_modes%js_base_mode)
        js_mode_map = major_modes%js_mode%map
        call major_modes%js_json_mode%define("js_json_mode", "JSON", js_json_mode, js_json_mode_hook, &
                                             parent=major_modes%prog_mode, syntax_table=js_mode_syntax_table)
        js_json_mode_map = major_modes%js_json_mode%map
        commands%js_base_mode%run => js_base_mode
        commands%js_mode%run => js_mode
        commands%javascript_mode%run => js_mode
        commands%js_json_mode%run => js_json_mode
        commands%js_indent_line%run => cmd_js_indent_line
        commands%js_syntactic_context%run => cmd_js_syntactic_context
    end subroutine syms_of_js
end module js_fm

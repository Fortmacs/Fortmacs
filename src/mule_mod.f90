! mule_mod.f90 -- commands and functions for coding systems.
! Ported from GNU Emacs 31.1 lisp/international/mule.el,
! lisp/international/mule-cmds.el and lisp/international/mule-diag.el.
!
! Copyright (C) 1997-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
!   2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
! Copyright (C) 2003
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H13PRO009
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

!> The coding-system side of Mule: finding the coding system a file asks
!> for (`set_auto_coding', the `coding:' tag, `auto_codings' and
!> `auto_coding_regexps'), setting `buffer_file_coding_system' after
!> a file is read, choosing a coding system that can encode the text
!> being written (`select_safe_coding_system', with its *Warning* buffer
!> and prompt), `prefer_coding_system', and the commands C-x RET f
!> (`set_buffer_file_coding_system'), C-x RET r
!> (`revert_buffer_with_coding_system') and C-h C (`describe_coding_system').
!>
!> C-x RET c (`universal_coding_system_argument', running the next command
!> through `this_command_wrapper') and C-x RET p
!> (`set_buffer_process_coding_system') are ported.
!>
!> C-x RET t (`set_terminal_coding_system') and C-x RET k
!> (`set_keyboard_coding_system') set the terminal's coding systems.
!>
!> Language environments: `language_environments', which the language
!> submodules of coding_mod fill, C-x RET l (`set_language_environment'),
!> `reset_language_environment' and C-h L (`describe_language_environment').
!>
!> The input method framework: `input_methods' (`register_input_method'),
!> C-x RET C-\ (`set_input_method'), C-\ (`toggle_input_method'),
!> C-x \ (`activate_transient_input_method') and C-h I (`describe_input_method');
!> the keyboard gives keys to the buffer's `input_method_function'.
!>
!> Deferred: the echo of C-x RET c's coding system while the next
!> command's keys are typed, the selection and file-name coding system
!> commands (C-x RET x, X, F), Quail and its input methods (step 41j),
!> `auto_coding_functions' (the xml, html
!> and sgml detection), `enable_character_translation' tags, the help-xref
!> buttons of the *Warning* and *Help* buffers, completion of only the
!> coding systems that encode the buffer first (completion-table-in-turn),
!> `list_coding_systems' and `describe_current_coding_system_briefly'.
module mule_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use callint_mod, only: PREFIX_NONE, command_of
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR, FM_CODING_SYSTEM_ERROR, format_message
    use handle_mod, only: handle_type
    use value_mod, only: value_type, xstring, make_list, vtag, V_STRING
    use character_mod, only: str_to_multibyte
    use data_mod, only: forward_setting, default_value, set_default
    use fns_mod, only: put
    use commands_mod, only: commands
    use callint_mod, only: current_prefix_arg
    use buffer_mod, only: buffers, current, buf_bytes, BEGINNING_BYTE, current_buffer, is_buffer_live, set_buffer, &
                          restore_buffer_modified
    use keymap_mod, only: define_key, key_parse, make_sparse_keymap
    use search_mod, only: string_match, match_beginning, match_end, case_fold_search
    use minibuf_mod, only: completion_list_table
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use coding_mod, only: init_coding, buffer_file_coding_system, last_coding_system_used, &
                          coding_system_for_read, inhibit_eol_conversion, default_process_coding_system, &
                          file_coding_systems, is_coding_system, check_coding_system, coding_system_base, &
                          coding_system_type, coding_system_eol_type, coding_system_subsidiaries, &
                          coding_system_change_eol_conversion, coding_system_change_text_conversion, &
                          coding_system_mnemonic, coding_system_doc_string, coding_system_aliases, coding_system_equal, &
                          coding_system_priority_list, set_coding_system_priority, coding_system_mime_charset, &
                          coding_system_charset_list, coding_system_list, is_coding_system_prefer_utf_8, &
                          is_coding_system_ascii_compatible, find_coding_systems_text, check_coding_systems_text, &
                          coding_system_list_entry_type, unencodable_type, EOL_UNIX, EOL_DOS, EOL_MAC, EOL_UNDECIDED, &
                          is_coding_system_iso_7_bit, is_coding_system_suitable_for_keyboard
    use fileio_mod, only: set_auto_coding_function, after_insert_file_set_coding, &
                          select_safe_coding_system_function, is_file_name_match
    use files_mod, only: revert_buffer
    use xdisp_mod, only: message
    use help_mod, only: with_help_window
    use simple_mod, only: prefix_command_preserve_state, prefix_command_preserve_state_hook
    use keyboard_mod, only: pre_command_hook, this_command_wrapper
    use callint_mod, only: this_command, execute_interactively
    use coding_mod, only: coding_system_for_write, coding_system_require_warning
    use process_mod, only: get_buffer_process, set_process_coding_system
    use coding_mod, only: set_terminal_coding_system_internal, terminal_coding_system, &
                          set_keyboard_coding_system_internal, keyboard_coding_system
    use keyboard_mod, only: current_input_meta_mode, set_input_meta_mode
    use dispnew_mod, only: redraw_frame
    use frame_mod, only: selected_frame
    use hooks_mod, only: hook_type
    use data_mod, only: string_setting_type
    use minibuf_mod, only: completion_ignore_case
    use help_mod, only: substitute_command_keys
    use charset_mod, only: set_charset_priority, charset_description, CHARSET_NAME_LEN, current_iso639_language, &
                           charsets, find_charset, charset_list, charset_chars, decode_char
    use help_mod, only: with_output_to_temp_buffer
    use buffer_mod, only: header_line_format_type, current_input_method_procedure
    use keyboard_mod, only: ring_bell
    implicit none(type, external)
    private

    public :: universal_coding_system_argument, set_buffer_process_coding_system, set_terminal_coding_system, &
              set_keyboard_coding_system
    public :: init_mule, find_auto_coding, set_auto_coding, auto_codings_lookup, after_insert_file_set_coding_1, &
              find_new_buffer_file_coding_system, merge_coding_systems, set_buffer_file_coding_system, &
              revert_buffer_with_coding_system, prefer_coding_system, set_default_coding_systems, &
              select_safe_coding_system, describe_coding_system, describe_current_coding_system, read_coding_system, &
              buffer_file_coding_system_explicit
    public :: define_language_environment, find_language_environment, set_language_environment, &
              reset_language_environment, describe_language_environment, read_language_name, &
              language_environment_procedure, language_environment_type, language_environments
    public :: set_locale_environment, find_locale_entry, locale_charset_to_coding_system, locale_translate, &
              get_locale_names, set_display_table_and_terminal_coding_system
    public :: list_character_sets, list_charset_chars, describe_character_set, read_charset
    public :: register_input_method, find_input_method, read_input_method_name, activate_input_method, &
              deactivate_input_method, set_input_method, toggle_input_method, activate_transient_input_method, &
              deactivate_transient_input_method, describe_input_method, describe_current_input_method, &
              input_method_activate_procedure, input_method_type, input_methods

    !> The keys of a language environment's information that
    !> `read_language_name' can ask for (get-language-info's KEY):
    !> `documentation', `charset', `sample-text', `coding-system',
    !> `coding-priority', `input-method' and `tutorial'.
    integer, parameter, public :: LANGUAGE_INFO_DOCUMENTATION = 1, LANGUAGE_INFO_CHARSET = 2, &
                                  LANGUAGE_INFO_SAMPLE_TEXT = 3, LANGUAGE_INFO_CODING_SYSTEM = 4, &
                                  LANGUAGE_INFO_CODING_PRIORITY = 5, LANGUAGE_INFO_INPUT_METHOD = 6, &
                                  LANGUAGE_INFO_TUTORIAL = 7

    abstract interface
        !> A `setup_function' or `exit_function' of a language environment.
        subroutine language_environment_procedure(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine language_environment_procedure
    end interface

    !> The definition of a language environment (an element of
    !> `language_environments'): its name and the information under each
    !> key of upstream's language-info-alist, as typed components
    !> (unallocated when the key is absent).
    type :: language_environment_type
        character(len=:), allocatable :: name
        !> `documentation': its text, unallocated where upstream's is t.
        character(len=:), allocatable :: documentation
        !> Whether it has a `documentation' key (a text, or none).
        logical :: has_documentation = .false.
        !> `charset': the charsets it supports.
        character(len=CHARSET_NAME_LEN), allocatable :: charsets(:)
        !> `sample-text': the string its expression gives.
        character(len=:), allocatable :: sample_text
        !> `coding-system': the coding systems it uses.
        character(len=40), allocatable :: coding_systems(:)
        !> `coding-priority': the coding systems to try first, in priority
        !> order.
        character(len=40), allocatable :: coding_system_order(:)
        !> `nonascii-translation', `input-method', `unibyte-display',
        !> `tutorial' and `iso639-language'.
        character(len=:), allocatable :: nonascii_translation, input_method, unibyte_display, tutorial
        character(len=:), allocatable :: iso639_language
        !> `setup-function' and `exit-function'.
        procedure(language_environment_procedure), pointer, nopass :: setup_function => null(), exit_function => null()
    end type language_environment_type

    !> The language environments (upstream's language-info-alist), in the
    !> order of definition (upstream's latest defined first); the first
    !> `language_environment_count' are in use.
    type(language_environment_type), allocatable, protected :: language_environments(:)
    integer :: language_environment_count = 0
    logical :: defining_language_environments = .false.

    !> The last language environment specified with `set_language_environment'.
    !> This variable should be set only with \[customize], which is equivalent
    !> to using the function `set_language_environment'.  Port of
    !> mule-cmds.el current-language-environment.
    type(string_setting_type), target, public :: current_language_environment = &
                                                 string_setting_type(name="current_language_environment")

    !> Default input method for multilingual text (a string).
    !> This is the input method activated automatically by the command
    !> `toggle_input_method' (\[toggle_input_method]).  Port of
    !> mule-cmds.el default-input-method ("" for none).
    type(string_setting_type), target, public :: default_input_method = string_setting_type(name="default_input_method")

    !> Default transient input method.
    !> This is the input method activated by the command
    !> `activate_transient_input_method' (\[activate_transient_input_method]).
    !> Port of mule-cmds.el default-transient-input-method ("" for none).
    type(string_setting_type), target, public :: default_transient_input_method = &
                                                 string_setting_type(name="default_transient_input_method")

    !> Normal hook run just after an input method is activated.
    !>
    !> The variable `current_input_method' keeps the input method name
    !> just activated.  Port of mule-cmds.el input-method-activate-hook.
    type(hook_type), target, public :: input_method_activate_hook = hook_type(name="input_method_activate_hook")

    !> Normal hook run just after an input method is deactivated.
    !>
    !> The variable `current_input_method' still keeps the input method name
    !> just deactivated.  Port of mule-cmds.el input-method-deactivate-hook.
    type(hook_type), target, public :: input_method_deactivate_hook = hook_type(name="input_method_deactivate_hook")

    !> Values of `input_method_verbose_flag'.
    integer, parameter, public :: INPUT_METHOD_VERBOSE_FLAG_NO = 0, INPUT_METHOD_VERBOSE_FLAG_YES = 1, &
                                  INPUT_METHOD_VERBOSE_FLAG_COMPLEX_ONLY = 2, INPUT_METHOD_VERBOSE_FLAG_DEFAULT = 3

    !> A flag to control extra guidance given by input methods.
    !> The value should be `INPUT_METHOD_VERBOSE_FLAG_NO',
    !> `INPUT_METHOD_VERBOSE_FLAG_YES', `INPUT_METHOD_VERBOSE_FLAG_COMPLEX_ONLY',
    !> or `INPUT_METHOD_VERBOSE_FLAG_DEFAULT'.
    !>
    !> The extra guidance is done by showing list of available keys in echo
    !> area.  When you use the input method in the minibuffer, the guidance
    !> is shown at the bottom short window (split from the existing window).
    !>
    !> If the value is `INPUT_METHOD_VERBOSE_FLAG_YES', extra guidance is
    !> always given, if the value is `INPUT_METHOD_VERBOSE_FLAG_NO', extra
    !> guidance is always suppressed.
    !>
    !> If the value is `INPUT_METHOD_VERBOSE_FLAG_COMPLEX_ONLY', only complex
    !> input methods such as `chinese-py' and `japanese' give extra
    !> guidance.
    !>
    !> If the value is `INPUT_METHOD_VERBOSE_FLAG_DEFAULT', complex input
    !> methods always give extra guidance, but simple input methods give it
    !> only when you are not in the minibuffer.
    !>
    !> See also the variable `input_method_highlight_flag'.  Port of
    !> mule-cmds.el input-method-verbose-flag (upstream's four values as
    !> named constants).
    integer, public :: input_method_verbose_flag = INPUT_METHOD_VERBOSE_FLAG_DEFAULT

    !> If this flag is true, input methods highlight partially-entered text.
    !> For instance, while you are in the middle of a Quail input method sequence,
    !> the text inserted so far is temporarily underlined.
    !> The underlining goes away when you finish or abort the input method sequence.
    !> See also the variable `input_method_verbose_flag'.  Port of
    !> mule-cmds.el input-method-highlight-flag.
    logical, public :: input_method_highlight_flag = .true.

    !> Normal hook run just after an input method insert some chunk of text.
    !> Port of mule-cmds.el input-method-after-insert-chunk-hook.
    type(hook_type), target, public :: input_method_after_insert_chunk_hook = &
                                       hook_type(name="input_method_after_insert_chunk_hook")

    !> This flag controls when an input method returns.
    !> Usually, the input method does not return while there's a possibility
    !> that it may find a different translation if a user types another key.
    !> But, if this flag is true, the input method returns as soon as the
    !> current key sequence gets long enough to have some valid translation.
    !> Port of mule-cmds.el input-method-exit-on-first-char.
    logical, public :: input_method_exit_on_first_char = .false.

    !> This flag controls how an input method shows an intermediate key sequence.
    !> Usually, the input method inserts the intermediate key sequence,
    !> or candidate translations corresponding to the sequence,
    !> at point in the current buffer.
    !> But, if this flag is true, it displays them in echo area instead.
    !> Port of mule-cmds.el input-method-use-echo-area.
    logical, public :: input_method_use_echo_area = .false.

    abstract interface
        !> The procedure that activates an input method, given its name (the
        !> ACTIVATE_FUNCTION of `register_input_method').  Not a port.
        recursive subroutine input_method_activate_procedure(input_method, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: input_method
            type(status_type), intent(out) :: status
        end subroutine input_method_activate_procedure
    end interface

    !> An input method (an element of `input_methods'): its name, its
    !> language environment, the procedure that activates it, its mode
    !> line title and its description.
    type :: input_method_type
        character(len=64) :: name = "", language_environment = "", title = ""
        procedure(input_method_activate_procedure), pointer, nopass :: activate_function => null()
        character(len=:), allocatable :: description
    end type input_method_type

    !> The input methods, the most recently registered first.  See the
    !> procedure `register_input_method' for the meanings of the
    !> components.  Port of mule-cmds.el input-method-alist.
    type(input_method_type), allocatable, protected :: input_methods(:)

    !> True inside `toggle_input_method'.
    !> Port of mule-cmds.el toggle-input-method-active.
    logical, public :: toggle_input_method_active = .false.

    !> Normal hook run after some language environment is set.
    !>
    !> When you set some hook function here, that effect usually should not
    !> be inherited to another language environment.  So, you had better set
    !> another function in `exit_language_environment_hook' (which see) to
    !> cancel the effect.
    type(hook_type), target, public :: set_language_environment_hook = &
                                       hook_type(name="set_language_environment_hook")

    !> Normal hook run after exiting from some language environment.
    !> When this hook is run, the variable `current_language_environment'
    !> still holds the language environment being exited.
    !>
    !> This hook is mainly used for canceling the effect of
    !> `set_language_environment_hook' (which see).
    type(hook_type), target, public :: exit_language_environment_hook = &
                                       hook_type(name="exit_language_environment_hook")

    !> An entry of `locale_language_names', `locale_charset_language_names'
    !> or `locale_preferred_coding_systems': a regexp matched against the
    !> start of a downcased locale name, and the language environment
    !> and coding system it names ("" when absent).
    type, public :: locale_entry_type
        character(len=40) :: regexp = ""
        character(len=24) :: language = ""
        character(len=24) :: coding_system = ""
    end type locale_entry_type

    !> Table of locale regexps and language names.  The first
    !> element whose locale regexp matches the start of a downcased locale
    !> specifies the language name and the coding system (when given)
    !> corresponding to that locale.  If the locale specifies only a
    !> language name, the coding system comes from the locale's codeset or
    !> from `locale_preferred_coding_systems'.  Port of mule-cmds.el
    !> locale-language-names.
    type(locale_entry_type), public :: locale_language_names(194) = [ &
                                       locale_entry_type(regexp="aa_DJ", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="aa", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="af", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="am", language="Ethiopic", coding_system="utf-8"), &
                                       locale_entry_type(regexp="an", language="Latin-9", coding_system=""), &
                                       locale_entry_type(regexp="arn", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="ar", language="Arabic", coding_system=""), &
                                       locale_entry_type(regexp="as", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="az", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="ba", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="be", language="Belarusian", coding_system="cp1251"), &
                                       locale_entry_type(regexp="bg", language="Bulgarian", coding_system="cp1251"), &
                                       locale_entry_type(regexp="bn", language="Bengali", coding_system="utf-8"), &
                                       locale_entry_type(regexp="bo", language="Tibetan", coding_system=""), &
                                       locale_entry_type(regexp="br", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="bs", language="Latin-2", coding_system=""), &
                                       locale_entry_type(regexp="byn", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="ca", language="Catalan", coding_system="iso-8859-1"), &
                                       locale_entry_type(regexp="co", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="cs", language="Czech", coding_system="iso-8859-2"), &
                                       locale_entry_type(regexp="cy", language="Welsh", coding_system="iso-8859-14"), &
                                       locale_entry_type(regexp="da", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="de", language="German", coding_system="iso-8859-1"), &
                                       locale_entry_type(regexp="dv", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="ee", language="Latin-4", coding_system=""), &
                                       locale_entry_type(regexp="el", language="Greek", coding_system="iso-8859-7"), &
                                       locale_entry_type(regexp="en_IN", language="English", coding_system="utf-8"), &
                                       locale_entry_type(regexp="en", language="English", coding_system="iso-8859-1"), &
                                       locale_entry_type(regexp="eo", language="Esperanto", coding_system=""), &
                                       locale_entry_type(regexp="es", language="Spanish", coding_system="iso-8859-1"), &
                                       locale_entry_type(regexp="et", language="Latin-9", coding_system=""), &
                                       locale_entry_type(regexp="eu", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="fa", language="Persian", coding_system="utf-8"), &
                                       locale_entry_type(regexp="fil", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="fpo", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="fi", language="Latin-9", coding_system=""), &
                                       locale_entry_type(regexp="fj", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="fo", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="fr", language="French", coding_system="iso-8859-1"), &
                                       locale_entry_type(regexp="fy", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="ga", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="gd", language="Latin-9", coding_system=""), &
                                       locale_entry_type(regexp="gez", language="Ethiopic", coding_system="utf-8"), &
                                       locale_entry_type(regexp="gla", language="Latin-9", coding_system=""), &
                                       locale_entry_type(regexp="gl", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="gu", language="Gujarati", coding_system="utf-8"), &
                                       locale_entry_type(regexp="gv", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="he", language="Hebrew", coding_system="iso-8859-8"), &
                                       locale_entry_type(regexp="hi", language="Devanagari", coding_system="utf-8"), &
                                       locale_entry_type(regexp="hni_IN", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="hr", language="Croatian", coding_system="iso-8859-2"), &
                                       locale_entry_type(regexp="hu", language="Latin-2", coding_system=""), &
                                       locale_entry_type(regexp="hy", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="id", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="ig", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="ibo", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="is", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="it", language="Italian", coding_system="iso-8859-1"), &
                                       locale_entry_type(regexp="iw", language="Hebrew", coding_system="iso-8859-8"), &
                                       locale_entry_type(regexp="ja", language="Japanese", coding_system="euc-jp"), &
                                       locale_entry_type(regexp="kal", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="ka", language="Georgian", coding_system="georgian-ps"), &
                                       locale_entry_type(regexp="kk", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="kl", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="km", language="Khmer", coding_system="utf-8"), &
                                       locale_entry_type(regexp="knk", language="Devanagari", coding_system="utf-8"), &
                                       locale_entry_type(regexp="kok", language="Devanagari", coding_system="utf-8"), &
                                       locale_entry_type(regexp="kn", language="Kannada", coding_system="utf-8"), &
                                       locale_entry_type(regexp="ko", language="Korean", coding_system="euc-kr"), &
                                       locale_entry_type(regexp="ks", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="kw", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="ky", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="lao", language="Lao", coding_system="utf-8"), &
                                       locale_entry_type(regexp="la", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="lb", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="lg", language="Latin-6", coding_system=""), &
                                       locale_entry_type(regexp="lo", language="Lao", coding_system="utf-8"), &
                                       locale_entry_type(regexp="lt", language="Lithuanian", coding_system="iso-8859-13"), &
                                       locale_entry_type(regexp="lv", language="Latvian", coding_system="iso-8859-13"), &
                                       locale_entry_type(regexp="mi", language="Latin-7", coding_system=""), &
                                       locale_entry_type(regexp="mk", language="Cyrillic-ISO", coding_system="iso-8859-5"), &
                                       locale_entry_type(regexp="mlt", language="Latin-3", coding_system=""), &
                                       locale_entry_type(regexp="ml", language="Malayalam", coding_system="utf-8"), &
                                       locale_entry_type(regexp="mn", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="mri", language="Latin-7", coding_system=""), &
                                       locale_entry_type(regexp="mr", language="Devanagari", coding_system="utf-8"), &
                                       locale_entry_type(regexp="ms", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="mt", language="Latin-3", coding_system=""), &
                                       locale_entry_type(regexp="mym", language="Malayalam", coding_system="utf-8"), &
                                       locale_entry_type(regexp="my", language="Burmese", coding_system="utf-8"), &
                                       locale_entry_type(regexp="nb", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="ne", language="Devanagari", coding_system="utf-8"), &
                                       locale_entry_type(regexp="nl", language="Dutch", coding_system="iso-8859-1"), &
                                       locale_entry_type(regexp="nn", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="non", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="no", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="nr_ZA", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="nso_ZA", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="oc", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="om_ET", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="om", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="or", language="Oriya", coding_system="utf-8"), &
                                       locale_entry_type(regexp="pa", language="Punjabi", coding_system="utf-8"), &
                                       locale_entry_type(regexp="pl", language="Polish", coding_system="iso-8859-2"), &
                                       locale_entry_type(regexp="ps", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="pas", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="pt_BR", language="Brazilian Portuguese", &
                                                         coding_system="iso-8859-1"), &
                                       locale_entry_type(regexp="pt", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="rm", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="ro", language="Romanian", coding_system="iso-8859-2"), &
                                       locale_entry_type(regexp="ru_RU.koi8r", language="Cyrillic-KOI8", coding_system="koi8-r"), &
                                       locale_entry_type(regexp="ru_RU", language="Russian", coding_system="iso-8859-5"), &
                                       locale_entry_type(regexp="ru_UA", language="Russian", coding_system="koi8-u"), &
                                       locale_entry_type(regexp="rw", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="sa", language="Devanagari", coding_system=""), &
                                       locale_entry_type(regexp="se", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="sh", language="Latin-2", coding_system=""), &
                                       locale_entry_type(regexp="si", language="Sinhala", coding_system="utf-8"), &
                                       locale_entry_type(regexp="sid", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="sk", language="Slovak", coding_system="iso-8859-2"), &
                                       locale_entry_type(regexp="sl", language="Slovenian", coding_system="iso-8859-2"), &
                                       locale_entry_type(regexp="so_ET", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="so", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="sq", language="Latin-2", coding_system=""), &
                                       locale_entry_type(regexp="sr", language="Latin-2", coding_system=""), &
                                       locale_entry_type(regexp="st", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="sv", language="Swedish", coding_system="iso-8859-1"), &
                                       locale_entry_type(regexp="sw", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="taj", language="Tajik", coding_system="koi8-t"), &
                                       locale_entry_type(regexp="ta", language="Tamil", coding_system="utf-8"), &
                                       locale_entry_type(regexp="te", language="Telugu", coding_system="utf-8"), &
                                       locale_entry_type(regexp="tg", language="Tajik", coding_system="koi8-t"), &
                                       locale_entry_type(regexp="th_TH.tis620", language="Thai", coding_system="tis-620"), &
                                       locale_entry_type(regexp="th_TH.TIS-620", language="Thai", coding_system="tis-620"), &
                                       locale_entry_type(regexp="th_TH", language="Thai", coding_system="iso-8859-11"), &
                                       locale_entry_type(regexp="th", language="Thai", coding_system="iso-8859-11"), &
                                       locale_entry_type(regexp="ti", language="Ethiopic", coding_system="utf-8"), &
                                       locale_entry_type(regexp="tig_ER", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="tk", language="Latin-5", coding_system=""), &
                                       locale_entry_type(regexp="tuk", language="Latin-5", coding_system=""), &
                                       locale_entry_type(regexp="tl", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="tn", language="Latin-9", coding_system=""), &
                                       locale_entry_type(regexp="tr", language="Turkish", coding_system="iso-8859-9"), &
                                       locale_entry_type(regexp="tsn", language="Latin-9", coding_system=""), &
                                       locale_entry_type(regexp="ts", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="tt", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="ug", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="uk", language="Ukrainian", coding_system="koi8-u"), &
                                       locale_entry_type(regexp="ur", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="uz_UZ@cyrillic", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="uz", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="ve", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="vi", language="Vietnamese", coding_system="utf-8"), &
                                       locale_entry_type(regexp="wa", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="wo", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="xh", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="yi", language="Windows-1255", coding_system=""), &
                                       locale_entry_type(regexp="yo", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="zh_HK", language="Chinese-Big5", coding_system=""), &
                                       locale_entry_type(regexp="zh_TW", language="Chinese-Big5", coding_system=""), &
                                       locale_entry_type(regexp="zh_CN.GB2312", language="Chinese-GB", coding_system=""), &
                                       locale_entry_type(regexp="zh_CN.GBK", language="Chinese-GBK", coding_system=""), &
                                       locale_entry_type(regexp="zh_CN.GB18030", language="Chinese-GB18030", coding_system=""), &
                                       locale_entry_type(regexp="zh_CN.UTF-8", language="Chinese-GBK", coding_system=""), &
                                       locale_entry_type(regexp="zh_CN", language="Chinese-GB", coding_system=""), &
                                       locale_entry_type(regexp="zhh", language="Chinese-Big5", coding_system=""), &
                                       locale_entry_type(regexp="zhi", language="Chinese-GBK", coding_system=""), &
                                       locale_entry_type(regexp="zhm", language="Chinese-Big5", coding_system=""), &
                                       locale_entry_type(regexp="zh", language="Chinese-GB", coding_system=""), &
                                       locale_entry_type(regexp="zu", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="c$", language="ASCII", coding_system=""), &
                                       locale_entry_type(regexp="posix$", language="ASCII", coding_system=""), &
                                       locale_entry_type(regexp="ipa$", language="IPA", coding_system=""), &
                                       locale_entry_type(regexp="cz", language="Czech", coding_system=""), &
                                       locale_entry_type(regexp="iw", language="Hebrew", coding_system=""), &
                                       locale_entry_type(regexp="sp", language="Cyrillic-ISO", coding_system=""), &
                                       locale_entry_type(regexp="su", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="jp", language="Japanese", coding_system=""), &
                                       locale_entry_type(regexp="chs", language="Chinese-GBK", coding_system=""), &
                                       locale_entry_type(regexp="cht", language="Chinese-BIG5", coding_system=""), &
                                       locale_entry_type(regexp="gbz", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="div", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="wee", language="Latin-2", coding_system=""), &
                                       locale_entry_type(regexp="wen", language="Latin-2", coding_system=""), &
                                       locale_entry_type(regexp="ind", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="sme", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="smf", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="smg", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="kdi", language="Kannada", coding_system="utf-8"), &
                                       locale_entry_type(regexp="mar", language="Devanagari", coding_system="utf-8"), &
                                       locale_entry_type(regexp="khm", language="Khmer", coding_system="utf-8"), &
                                       locale_entry_type(regexp="iri", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp="uig", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp="kin", language="UTF-8", coding_system="") &
                                       ]

    !> Table of locale regexps and charset language names.
    !> The first element whose locale regexp matches the start of a downcased
    !> locale specifies the language name whose charset corresponds to that
    !> locale.  This language name is used if the locale is not listed in
    !> `locale_language_names'.  Port of mule-cmds.el
    !> locale-charset-language-names.
    type(locale_entry_type), public :: locale_charset_language_names(9) = [ &
                                       locale_entry_type(regexp=".*8859[-_]?1\>", language="Latin-1", coding_system=""), &
                                       locale_entry_type(regexp=".*8859[-_]?2\>", language="Latin-2", coding_system=""), &
                                       locale_entry_type(regexp=".*8859[-_]?3\>", language="Latin-3", coding_system=""), &
                                       locale_entry_type(regexp=".*8859[-_]?4\>", language="Latin-4", coding_system=""), &
                                       locale_entry_type(regexp=".*8859[-_]?9\>", language="Latin-5", coding_system=""), &
                                       locale_entry_type(regexp=".*8859[-_]?14\>", language="Latin-8", coding_system=""), &
                                       locale_entry_type(regexp=".*8859[-_]?15\>", language="Latin-9", coding_system=""), &
                                       locale_entry_type(regexp=".*utf\(?:-?8\)?\>", language="UTF-8", coding_system=""), &
                                       locale_entry_type(regexp=".*@euro\>", language="Latin-9", coding_system="") &
                                       ]

    !> Table of locale regexps and preferred coding systems.
    !> The first element whose locale regexp matches the start of a downcased
    !> locale specifies the coding system to prefer when using that locale.
    !> This coding system is used if the locale specifies a specific charset.
    !> Port of mule-cmds.el locale-preferred-coding-systems.
    type(locale_entry_type), public :: locale_preferred_coding_systems(24) = [ &
                                       locale_entry_type(regexp=".*8859[-_]?1\>", language="", coding_system="iso-8859-1"), &
                                       locale_entry_type(regexp=".*8859[-_]?2\>", language="", coding_system="iso-8859-2"), &
                                       locale_entry_type(regexp=".*8859[-_]?3\>", language="", coding_system="iso-8859-3"), &
                                       locale_entry_type(regexp=".*8859[-_]?4\>", language="", coding_system="iso-8859-4"), &
                                       locale_entry_type(regexp=".*8859[-_]?9\>", language="", coding_system="iso-8859-9"), &
                                       locale_entry_type(regexp=".*8859[-_]?14\>", language="", coding_system="iso-8859-14"), &
                                       locale_entry_type(regexp=".*8859[-_]?15\>", language="", coding_system="iso-8859-15"), &
                                       locale_entry_type(regexp=".*utf\(?:-?8\)?", language="", coding_system="utf-8"), &
                                       locale_entry_type(regexp=".*@euro", language="", coding_system="iso-8859-15"), &
                                       locale_entry_type(regexp="koi8-?r", language="", coding_system="koi8-r"), &
                                       locale_entry_type(regexp="koi8-?u", language="", coding_system="koi8-u"), &
                                       locale_entry_type(regexp="tcvn", language="", coding_system="tcvn"), &
                                       locale_entry_type(regexp="big5[-_]?hkscs", language="", coding_system="big5-hkscs"), &
                                       locale_entry_type(regexp="big5", language="", coding_system="big5"), &
                                       locale_entry_type(regexp="euc-?tw", language="", coding_system="euc-tw"), &
                                       locale_entry_type(regexp="euc-?cn", language="", coding_system="euc-cn"), &
                                       locale_entry_type(regexp="gb2312", language="", coding_system="gb2312"), &
                                       locale_entry_type(regexp="gbk", language="", coding_system="gbk"), &
                                       locale_entry_type(regexp="gb18030", language="", coding_system="gb18030"), &
                                       locale_entry_type(regexp="ja.*[._]euc", language="", coding_system="japanese-iso-8bit"), &
                                       locale_entry_type(regexp="ja.*[._]jis7", language="", coding_system="iso-2022-jp"), &
                                       locale_entry_type(regexp="ja.*[._]pck", language="", coding_system="japanese-shift-jis"), &
                                       locale_entry_type(regexp="ja.*[._]sjis", language="", coding_system="japanese-shift-jis"), &
                                       locale_entry_type(regexp="jpn", language="", coding_system="japanese-shift-jis") &
                                       ]

    ! The charset list_charset_chars shows, for its body in the buffer.
    character(len=:), allocatable :: listed_charset

    !> File name for the system's file of locale-name aliases, or "" if none.
    !> Port of mule-cmds.el locale-translation-file-name.
    character(len=:), allocatable, public :: locale_translation_file_name

    !> The currently set locale environment.  Port of mule-cmds.el
    !> current-locale-environment.
    character(len=:), allocatable, public :: current_locale_environment

    !> System locale for messages.  Port of emacs.c system-messages-locale
    !> ("" for the environment's).
    type(string_setting_type), public :: system_messages_locale = string_setting_type(name="system_messages_locale")

    !> System locale for time.  Port of emacs.c system-time-locale ("" for
    !> the environment's).
    type(string_setting_type), public :: system_time_locale = string_setting_type(name="system_time_locale")

    !> An entry of `auto_codings' or `auto_coding_regexps'.
    type, public :: auto_coding_type
        character(len=:), allocatable :: pattern, coding
    end type auto_coding_type

    !> Table of filename patterns vs corresponding coding systems.
    !> Each element looks like (REGEXP . CODING-SYSTEM).
    !> A file whose name matches REGEXP is decoded by CODING-SYSTEM on reading.
    !>
    !> The settings in this table take priority over `coding:' tags
    !> in the file (see the function `set_auto_coding')
    !> and the contents of `file_coding_systems'.  Port of mule.el
    !> auto-coding-alist.
    type(auto_coding_type), allocatable, public :: auto_codings(:)

    !> Table of patterns vs corresponding coding systems.
    !> Each element looks like (REGEXP . CODING-SYSTEM).
    !> A file whose first bytes match REGEXP is decoded by CODING-SYSTEM on reading.
    !>
    !> The settings in this table take priority over `coding:' tags
    !> in the file (see the function `set_auto_coding')
    !> and the contents of `file_coding_systems'.  Port of mule.el
    !> auto-coding-regexp-alist.
    type(auto_coding_type), allocatable, public :: auto_coding_regexps(:)

    !> If true, a coding system selected automatically is accepted without
    !> asking (the default coding systems that encode the text are always
    !> accepted: ACCEPT-DEFAULT-P has no function here).  Port of
    !> mule-cmds.el select-safe-coding-system-accept-default-p (Deferred:
    !> its function value).
    logical, public :: select_safe_coding_system_accept_default = .false.

    !> The most recent coding system specified explicitly by the user when
    !> asked to choose one.  Port of mule-cmds.el
    !> last-coding-system-specified.
    character(len=:), allocatable, public :: last_coding_system_specified

    !> Keymap for Mule (Multilingual environment) specific commands.
    !> Port of mule-cmds.el mule-keymap (the ported commands: c, f, k, p, r and t).
    type(handle_type), public :: mule_keymap

    !> Per-buffer `buffer_file_coding_system_explicit': the coding systems
    !> the user gave for reading and for writing the buffer's file
    !> (mule.el's permanent-local two values, READ and WRITE), by buffer serial.
    type :: explicit_type
        integer(int64) :: serial = -1
        character(len=:), allocatable :: read, write
    end type explicit_type
    type(explicit_type), allocatable :: explicit(:)

    !> Default value for the terminal coding system ("" for none).  This is
    !> normally set according to the selected language environment.  See
    !> also the command `set_terminal_coding_system'.  Port of mule.el
    !> default-terminal-coding-system.
    character(len=:), allocatable, public :: default_terminal_coding_system

    !> Default value of the keyboard coding system ("" for none).  This is
    !> normally set according to the selected language environment.  See
    !> also the command `set_keyboard_coding_system'.  Port of mule.el
    !> default-keyboard-coding-system.
    character(len=:), allocatable, public :: default_keyboard_coding_system

    !> Specify coding system for keyboard input.  If you set this on a
    !> terminal which can't distinguish Meta keys from 8-bit characters,
    !> you will have to use ESC to type Meta characters.  See Info node
    !> `Terminal Coding' and Info node `Unibyte Mode'.
    !>
    !> This is set at startup based on the locale.
    !>
    !> Setting this variable directly does not take effect; use
    !> \[set_keyboard_coding_system].  Port of mule.el
    !> keyboard-coding-system (the variable; the function of that name is
    !> `keyboard_coding_system').
    character(len=:), allocatable, public :: keyboard_coding_system_variable

    ! The terminal parameter keyboard-coding-saved-meta-mode.
    integer :: saved_meta_mode = 2
    logical :: has_saved_meta_mode = .false.

    ! mule-cmds--prefixed-command-next-coding-system and
    ! mule-cmds--prefixed-command-last-coding-system ("" for nil), and the
    ! coding system of the command `prefixed_command' runs.
    character(len=:), allocatable :: prefixed_next_coding_system, prefixed_last_coding_system, prefixed_coding_system

    character(len=*), parameter :: LF = achar(10), TAB = achar(9), CR = achar(13)

contains

    !> Set up the coding systems and the Mule commands: C-x RET (C-x C-m)
    !> and C-x \ in CTL_X_MAP, C-h C, C-h L and C-h I in HELP_MAP, and
    !> C-\ in GLOBAL_MAP.  Port of the top-level forms of mule.el, mule-cmds.el
    !> and mule-diag.el (and of bindings.el's C-\ and C-x \).
    subroutine init_mule(global_map, ctl_x_map, help_map)
        type(handle_type), intent(in) :: global_map, ctl_x_map, help_map
        type(value_type), allocatable :: key(:)
        type(status_type) :: st

        call init_coding()
        ! buffer.c: buffer-file-coding-system is local when set, and
        ! permanently local.
        call forward_setting(buffer_file_coding_system, local_if_set=.true.)
        call forward_setting(current_language_environment)
        call forward_setting(default_input_method)
        call forward_setting(default_transient_input_method)
        if (.not. allocated(current_language_environment%value)) current_language_environment%value = "English"
        if (.not. allocated(default_input_method%value)) default_input_method%value = ""
        call put(value_type(name="buffer_file_coding_system"), value_type(name="permanent_local"), value_type(.true.), st)
        set_auto_coding_function => set_auto_coding
        after_insert_file_set_coding => after_insert_file_set_coding_1
        select_safe_coding_system_function => select_safe_coding_system

        if (.not. allocated(auto_codings)) then
            allocate (auto_codings(8))
            auto_codings(1) = auto_coding_type("\.\(arc\|zip\|lzh\|lha\|zoo\|[jew]ar\|xpi\|rar\|7z\|squashfs\|"// &
                                               "ARC\|ZIP\|LZH\|LHA\|ZOO\|[JEW]AR\|XPI\|RAR\|7Z\|SQUASHFS\)\'", &
                                               "no-conversion-multibyte")
            auto_codings(2) = auto_coding_type("\.\(exe\|EXE\)\'", "no-conversion")
            auto_codings(3) = auto_coding_type("\.\(sx[dmicw]\|odt\|tar\|t[bg]z\)\'", "no-conversion")
            auto_codings(4) = auto_coding_type("\.\(gz\|Z\|bz\|bz2\|xz\|gpg\)\'", "no-conversion")
            auto_codings(5) = auto_coding_type("\.\(jpe?g\|png\|gif\|tiff?\|p[bpgn]m\)\'", "no-conversion")
            auto_codings(6) = auto_coding_type("\.pdf\'", "no-conversion")
            auto_codings(7) = auto_coding_type("/\.editorconfig\'", "utf-8")
            auto_codings(8) = auto_coding_type("/#[^/]+#\'", "utf-8-emacs-unix")
        end if

        if (.not. allocated(auto_coding_regexps)) then
            allocate (auto_coding_regexps(5))
            auto_coding_regexps(1) = auto_coding_type("\`BABYL OPTIONS:[ "//TAB//"]*-\*-[ "//TAB// &
                                                      "]*rmail[ "//TAB//"]*-\*-", "no-conversion")
            auto_coding_regexps(2) = auto_coding_type("\`"//char(int(z'FE'))//char(int(z'FF')), &
                                                      "utf-16be-with-signature")
            auto_coding_regexps(3) = auto_coding_type("\`"//char(int(z'FF'))//char(int(z'FE')), &
                                                      "utf-16le-with-signature")
            auto_coding_regexps(4) = auto_coding_type("\`"//char(int(z'EF'))//char(int(z'BB'))//char(int(z'BF')), &
                                                      "utf-8-with-signature")
            ! Emacs 20-compiled.
            auto_coding_regexps(5) = auto_coding_type("\`;ELC"//achar(20)//achar(0)//achar(0)//achar(0), &
                                                      "emacs-mule")
        end if

        commands%set_buffer_file_coding_system%execute => cmd_set_buffer_file_coding_system
        commands%revert_buffer_with_coding_system%execute => cmd_revert_buffer_with_coding_system
        commands%prefer_coding_system%execute => cmd_prefer_coding_system
        commands%describe_coding_system%execute => cmd_describe_coding_system
        commands%describe_current_coding_system%execute => cmd_describe_current_coding_system
        commands%universal_coding_system_argument%execute => cmd_universal_coding_system_argument
        commands%set_buffer_process_coding_system%execute => cmd_set_buffer_process_coding_system
        prefixed_next_coding_system = ""
        prefixed_last_coding_system = ""
        prefixed_coding_system = ""
        if (.not. allocated(default_terminal_coding_system)) default_terminal_coding_system = ""
        if (.not. allocated(default_keyboard_coding_system)) default_keyboard_coding_system = ""
        if (.not. allocated(keyboard_coding_system_variable)) keyboard_coding_system_variable = "utf-8-unix"
        commands%set_terminal_coding_system%execute => cmd_set_terminal_coding_system
        commands%set_keyboard_coding_system%execute => cmd_set_keyboard_coding_system
        commands%set_language_environment%execute => cmd_set_language_environment
        commands%set_locale_environment%execute => cmd_set_locale_environment
        commands%reset_language_environment%execute => cmd_reset_language_environment
        commands%describe_language_environment%execute => cmd_describe_language_environment
        commands%list_character_sets%execute => cmd_list_character_sets
        commands%list_charset_chars%execute => cmd_list_charset_chars
        commands%describe_character_set%execute => cmd_describe_character_set
        commands%set_input_method%execute => cmd_set_input_method
        commands%toggle_input_method%execute => cmd_toggle_input_method
        commands%activate_transient_input_method%execute => cmd_activate_transient_input_method
        commands%describe_input_method%execute => cmd_describe_input_method
        if (.not. allocated(input_methods)) allocate (input_methods(0))
        call make_sparse_keymap(mule_keymap)
        call bind(mule_keymap, "f", "set_buffer_file_coding_system")
        call bind(mule_keymap, "c", "universal_coding_system_argument")
        call bind(mule_keymap, "r", "revert_buffer_with_coding_system")
        call bind(mule_keymap, "p", "set_buffer_process_coding_system")
        call bind(mule_keymap, "t", "set_terminal_coding_system")
        call bind(mule_keymap, "k", "set_keyboard_coding_system")
        call bind(mule_keymap, "l", "set_language_environment")
        call bind(mule_keymap, "C-\", "set_input_method")
        ! Keep "C-x C-m ..." for mule specific commands.
        call key_parse("C-m", key, st)
        call define_key(ctl_x_map, key, keymap_value(mule_keymap), st)
        call bind(help_map, "C", "describe_coding_system")
        call bind(help_map, "L", "describe_language_environment")
        call bind(help_map, "I", "describe_input_method")
        call bind(global_map, "C-\", "toggle_input_method")
        call bind(ctl_x_map, "\", "activate_transient_input_method")
    end subroutine init_mule

    !> MAP as a keymap value, for binding it as a prefix.  Not a port.
    function keymap_value(map) result(v)
        use value_mod, only: make_keymap_ref, value_type
        type(handle_type), intent(in) :: map
        type(value_type) :: v

        v = make_keymap_ref(map)
    end function keymap_value

    ! Bind KEYS (in `kbd' form) to the command named CMD in MAP.
    ! Not a port.
    subroutine bind(map, keys, cmd)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(keys, key, status)
        call define_key(map, key, value_type(name=cmd), status)
    end subroutine bind

    ! ------------------------------------------------------------------
    ! Coding systems a file specifies
    ! ------------------------------------------------------------------

    !> Return the coding system specified by `auto_codings' for FILENAME.
    !> Port of mule.el:auto-coding-alist-lookup (file names are case
    !> sensitive on Linux).
    function auto_codings_lookup(filename) result(coding)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: coding
        integer :: k

        coding = ""
        if (.not. allocated(auto_codings)) return

        do k = 1, size(auto_codings)
            if (is_file_name_match(auto_codings(k)%pattern, filename)) then
                coding = auto_codings(k)%coding
                return
            end if
        end do
    end function auto_codings_lookup

    !> Lookup `auto_coding_regexps' for TEXT (bytes, as a unibyte
    !> buffer holds them, unless MULTIBYTE).  Port of
    !> mule.el:auto-coding-regexp-alist-lookup.
    function auto_coding_regexps_lookup(text, multibyte) result(coding)
        character(len=*), intent(in) :: text
        logical, intent(in) :: multibyte
        character(len=:), allocatable :: coding, subject
        type(status_type) :: st
        logical :: saved
        integer :: k

        coding = ""
        if (.not. allocated(auto_coding_regexps)) return

        ! The regexps hold bytes: as raw-byte characters on both sides.
        if (multibyte) then
            subject = text
        else
            subject = str_to_multibyte(text)
        end if

        saved = case_fold_search%value
        case_fold_search%value = .true.

        do k = 1, size(auto_coding_regexps)
            if (string_match(str_to_multibyte(auto_coding_regexps(k)%pattern), subject, st, &
                             inhibit_modify=.true.) > 0) then
                coding = auto_coding_regexps(k)%coding
                exit
            end if
        end do

        case_fold_search%value = saved
    end function auto_coding_regexps_lookup

    !> Find a coding system for a file FILENAME of which TEXT follows point.
    !> These bytes should include at least the first 1k of the file
    !> and the last 3k of the file, but the middle may be omitted.
    !>
    !> FILENAME should be an absolute file name
    !> or "" (which means that there is no associated file).
    !> The function checks FILENAME against the variable `auto_codings'.
    !> If FILENAME doesn't match any entries in the variable, it checks the
    !> contents of the current buffer following point against
    !> `auto_coding_regexps'.  If no match is found, it checks for a
    !> `coding:' tag in the first one or two lines following point.  If no
    !> `coding:' tag is found, it checks any local variables list in the last
    !> 3K bytes out of the SIZE bytes.
    !>
    !> If a coding system is specified, CODING is it and SOURCE says by
    !> what it is specified: "auto-coding-alist", "auto-coding-regexp-alist"
    !> or ":coding".  Note that the validity of CODING is not checked;
    !> it's the caller's responsibility to check it.
    !>
    !> If nothing is specified, CODING is "".  Port of
    !> mule.el:find-auto-coding (no `auto_coding_functions', no
    !> `enable_character_translation' and no obsolete `unibyte:' tag).
    subroutine find_auto_coding(filename, text, coding, source, multibyte)
        character(len=*), intent(in) :: filename, text
        character(len=:), allocatable, intent(out) :: coding, source
        logical, intent(in), optional :: multibyte
        character(len=:), allocatable :: head, tail, spec
        integer :: size_, head_end, tail_start, k
        logical :: mb, head_found, tail_found

        mb = .false.
        if (present(multibyte)) mb = multibyte
        source = ""
        coding = auto_codings_lookup(filename)

        if (len(coding) > 0) then
            source = "auto-coding-alist"
            return
        end if

        ! Try using `auto-coding-regexp-alist'.
        coding = auto_coding_regexps_lookup(text, mb)

        if (len(coding) > 0) then
            source = "auto-coding-regexp-alist"
            return
        end if

        size_ = len(text)
        head_end = min(size_, 1024)
        tail_start = max(size_ - 3072, 0)
        head = text(1:head_end)
        tail = text(tail_start + 1:)
        ! Try a short cut by searching for the string "coding:" at the
        ! head and tail of SIZE bytes.
        head_found = index(lower(head), "coding:") > 0
        tail_found = index(lower(tail), "coding:") > 0

        ! At first check the head.
        if (head_found) then
            spec = auto_mode_region(head)
            if (len(spec) > 0) coding = coding_tag(spec)
        end if

        ! If no coding: tag in the head, check the tail.
        if (tail_found .and. len(coding) == 0) coding = local_variables_coding(tail)

        if (len(coding) > 0) then
            ! If the coding-system name ends with "!", remove it.
            k = len(coding)
            if (coding(k:k) == "!") coding = coding(1:k - 1)
            source = ":coding"
        end if
    end subroutine find_auto_coding

    !> The text of the -*- spec of TEXT, between the markers and without
    !> the spaces next to them ("" if none): looked for in the first line,
    !> or the first two when the text starts with "#!" or '\" (exec
    !> interpreter magic).  Port of files.el:set-auto-mode-1 (no
    !> `inhibit_local_variables_regexps').
    function auto_mode_region(text) result(spec)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: spec, line, rest
        integer :: nl, i, j, a, b
        logical :: skip

        spec = ""
        nl = index(text, LF)

        if (nl == 0) then
            line = text
        else
            line = text(1:nl - 1)
        end if

        skip = .false.
        if (len(text) >= 2) skip = text(1:2) == "#!"
        if (len(text) >= 3) skip = skip .or. text(1:3) == "'\"""
        i = index(line, "-*-")

        if (i == 0 .and. skip .and. nl > 0) then
            line = text(nl + 1:)
            nl = index(line, LF)
            if (nl > 0) line = line(1:nl - 1)
            i = index(line, "-*-")
        end if

        if (i == 0) return
        rest = line(i + 3:)
        j = index(rest, "-*-")
        if (j == 0) return
        rest = rest(1:j - 1)
        a = verify(rest, " "//TAB)
        if (a == 0) return
        b = verify(rest, " "//TAB, back=.true.)
        spec = rest(a:b)
    end function auto_mode_region

    !> The coding system the `coding:' tag of -*- spec SPEC names, or "".
    !> Port of mule.el:find-auto-coding, in part.
    function coding_tag(spec) result(coding)
        character(len=*), intent(in) :: spec
        character(len=:), allocatable :: coding
        type(status_type) :: st
        integer(int64) :: b, e
        logical :: saved

        coding = ""
        saved = case_fold_search%value
        case_fold_search%value = .true.

        if (string_match("\(.*;\)?[ "//TAB//"]*coding:[ "//TAB//"]*\([^ ;]+\)", spec, st) > 0) then
            b = match_beginning(2, st)
            e = match_end(2, st)
            if (st%ok() .and. b > 0 .and. e >= b) coding = spec(b:e - 1)
        end if

        case_fold_search%value = saved
    end function coding_tag

    !> The coding system the local variables list in TAIL (the last 3K
    !> bytes) names, or "".  Here we must pay attention to the case that
    !> the end-of-line is just "\r" and we can't use "^" nor "$" in regexp.
    !> Port of mule.el:find-auto-coding, in part.
    function local_variables_coding(tail) result(coding)
        character(len=*), intent(in) :: tail
        character(len=:), allocatable :: coding, prefix, suffix, re_coding, re_end, rest
        type(status_type) :: st
        integer(int64) :: k, start, pos, endpos
        logical :: saved

        coding = ""
        saved = case_fold_search%value
        case_fold_search%value = .true.
        start = 1
        ! (re-search-forward "[\r\n]\^L" tail-end t)
        k = string_match("["//CR//LF//"]"//achar(12), tail, st)
        if (k > 0) start = match_end(0, st)
        rest = tail(start:)
        k = string_match("["//CR//LF//"]\([^"//CR//LF//"]*\)[ "//TAB//"]*Local Variables:[ "//TAB//"]*\([^"//CR//LF// &
                         "]*\)["//CR//LF//"]", rest, st)

        if (k > 0) then
            ! The prefix is what comes before "local variables:" in its
            ! line.  The suffix is what comes after "local variables:"
            ! in its line.
            prefix = regexp_quote(rest(match_beginning(1, st):match_end(1, st) - 1))
            suffix = regexp_quote(rest(match_beginning(2, st):match_end(2, st) - 1))
            ! (forward-char -1): from the \r or \n ending that line.
            pos = match_end(0, st) - 1
            re_coding = "["//CR//LF//"]"//prefix//"[ "//TAB//"]*coding[ "//TAB//"]*:[ "//TAB//"]*\([^ "//TAB//CR//LF// &
                        "]+\)[ "//TAB//"]*"//suffix//"["//CR//LF//"]"
            re_end = "["//CR//LF//"]"//prefix//"[ "//TAB//"]*End *:[ "//TAB//"]*"//suffix//"["//CR//LF//"]?"
            rest = rest(pos:)
            endpos = len(rest, kind=int64) + 1
            k = string_match(re_end, rest, st)
            if (k > 0) endpos = match_end(0, st)
            rest = rest(1:endpos - 1)
            if (string_match(re_coding, rest, st) > 0) coding = rest(match_beginning(1, st):match_end(1, st) - 1)
        end if

        case_fold_search%value = saved
    end function local_variables_coding

    !> S with the regexp special characters quoted.  Port of
    !> search.c:Fregexp_quote.
    function regexp_quote(s) result(q)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: q
        integer :: i

        q = ""

        do i = 1, len(s)
            if (index("[*.\?+^$", s(i:i)) > 0) q = q//"\"
            q = q//s(i:i)
        end do
    end function regexp_quote

    !> S in lower case (ASCII).  Not a port.
    pure function lower(s) result(t)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: i

        t = s

        do i = 1, len(s)
            if (s(i:i) >= "A" .and. s(i:i) <= "Z") t(i:i) = achar(iachar(s(i:i)) + 32)
        end do
    end function lower

    !> Return coding system for a file FILENAME whose first bytes HEAD
    !> follow point.
    !> See `find_auto_coding' for how the coding system is found.
    !> Return "" if an invalid coding system is found.
    !>
    !> The variable `set_auto_coding_function' (which see) is set to this
    !> function by default.  Port of mule.el:set-auto-coding.
    subroutine set_auto_coding(filename, head, coding)
        character(len=*), intent(in) :: filename, head
        character(len=:), allocatable, intent(out) :: coding
        character(len=:), allocatable :: source

        call find_auto_coding(filename, head, coding, source)

        if (len(coding) > 0) then
            if (.not. is_coding_system(coding)) coding = ""
        end if
    end subroutine set_auto_coding

    !> The explicit coding systems of the current buffer (an entry is
    !> made if MAKE).  Not a port.
    integer function explicit_index(make) result(k)
        logical, intent(in) :: make
        type(explicit_type), allocatable :: grown(:)
        integer :: n

        k = 0
        if (current == 0) return
        if (.not. allocated(explicit)) allocate (explicit(0))

        do n = 1, size(explicit)
            if (explicit(n)%serial == buffers(current)%serial) then
                k = n
                return
            end if
        end do

        if (.not. make) return
        n = size(explicit)
        allocate (grown(n + 1))
        grown(1:n) = explicit
        grown(n + 1)%serial = buffers(current)%serial
        grown(n + 1)%read = ""
        grown(n + 1)%write = ""
        call move_alloc(grown, explicit)
        k = n + 1
    end function explicit_index

    !> The coding systems the user explicitly specified for reading
    !> (READ) and writing (WRITE) the current buffer's file, "" if none.
    !> Port of mule.el buffer-file-coding-system-explicit.
    subroutine buffer_file_coding_system_explicit(read, write)
        character(len=:), allocatable, intent(out) :: read, write
        integer :: k

        read = ""
        write = ""
        k = explicit_index(.false.)
        if (k == 0) return
        read = explicit(k)%read
        write = explicit(k)%write
    end subroutine buffer_file_coding_system_explicit

    !> Set `buffer_file_coding_system' of current buffer after text is inserted.
    !> INSERTED is the number of characters that were inserted, as figured
    !> in the situation before this function.
    !> VISIT true means that we are visiting a file.  Port of
    !> mule.el:after-insert-file-set-coding.
    subroutine after_insert_file_set_coding_1(inserted, visit)
        integer(int64), intent(in) :: inserted
        logical, intent(in) :: visit
        character(len=:), allocatable :: coding
        integer :: k

        if (inserted < 0) return

        if (visit .and. allocated(coding_system_for_read%value)) then
            k = explicit_index(.true.)
            explicit(k)%read = coding_system_for_read%value
            explicit(k)%write = ""
        end if

        if (.not. allocated(last_coding_system_used%value)) return
        if (len(last_coding_system_used%value) == 0) return
        coding = find_new_buffer_file_coding_system(last_coding_system_used%value)
        if (len(coding) > 0) call buffer_file_coding_system%set_local(coding)
    end subroutine after_insert_file_set_coding_1

    !> Return a coding system for a buffer when a file of CODING is inserted.
    !> The local variable `buffer_file_coding_system' of the current buffer
    !> is set to the returned value.
    !> Return "" if there's no need to set `buffer_file_coding_system'.
    !> Port of mule.el:find-new-buffer-file-coding-system.
    function find_new_buffer_file_coding_system(coding) result(new)
        character(len=*), intent(in) :: coding
        character(len=:), allocatable :: new
        character(len=:), allocatable :: bfcs, local_coding, found_coding, new_coding
        integer :: local_eol, found_eol, new_eol
        logical :: local

        new = ""
        if (len(coding) == 0) return
        bfcs = ""
        if (allocated(buffer_file_coding_system%value)) bfcs = buffer_file_coding_system%value
        local = buffer_file_coding_system%local_p()
        ! Get information of `buffer-file-coding-system' in LOCAL-EOL and
        ! LOCAL-CODING.
        local_eol = coding_system_eol_type(bfcs)
        local_coding = ""

        if (len(bfcs) > 0) then
            if (coding_system_type(bfcs) /= "undecided") local_coding = coding_system_base(bfcs)
        end if

        ! The current buffer has already set full coding-system, we had
        ! better not change it.
        if (local .and. local_eol /= EOL_UNDECIDED .and. len(local_coding) > 0) return
        found_eol = coding_system_eol_type(coding)
        ! If EOL conversions are inhibited, force unix eol-type.
        if (found_eol == EOL_UNDECIDED .and. inhibit_eol_conversion%value) found_eol = EOL_UNIX
        found_coding = coding_system_base(coding)
        ! No valid coding information found.
        if (found_eol == EOL_UNDECIDED .and. found_coding == "undecided") return

        ! The local setting takes precedence over the found one.
        if (local) then
            new_coding = local_coding
            if (len(new_coding) == 0) new_coding = found_coding
            new_eol = local_eol
            if (new_eol == EOL_UNDECIDED) new_eol = found_eol
        else
            new_coding = found_coding
            if (len(new_coding) == 0) new_coding = local_coding
            new_eol = found_eol
            if (new_eol == EOL_UNDECIDED) new_eol = local_eol
        end if

        new = new_coding

        if (new_eol /= EOL_UNDECIDED .and. coding_system_eol_type(new_coding) == EOL_UNDECIDED) then
            new = coding_system_change_eol_conversion(new_coding, new_eol)
        end if
    end function find_new_buffer_file_coding_system

    !> Fill in any unspecified aspects of coding system FIRST from SECOND.
    !> Return the resulting coding system.  Port of
    !> mule.el:merge-coding-systems.
    function merge_coding_systems(first, second) result(cs)
        character(len=*), intent(in) :: first, second
        character(len=:), allocatable :: cs
        integer :: eol

        cs = first
        ! If FIRST doesn't specify text conversion, merge with that of SECOND.
        if (coding_system_base(cs) == "undecided") cs = coding_system_change_text_conversion(cs, second)
        ! If FIRST doesn't specify eol conversion, merge with that of SECOND.
        eol = coding_system_eol_type(cs)

        if (eol == EOL_UNDECIDED .and. coding_system_eol_type(second) /= EOL_UNDECIDED) then
            cs = coding_system_change_eol_conversion(cs, coding_system_eol_type(second))
        end if
    end function merge_coding_systems

    ! ------------------------------------------------------------------
    ! Reading coding systems
    ! ------------------------------------------------------------------

    !> Every coding system name, as completion candidates
    !> (Vcoding_system_alist).  Not a port.
    function coding_system_names() result(list)
        type(value_type) :: list
        type(coding_system_list_entry_type), allocatable :: names(:), aliases(:)
        type(value_type), allocatable :: items(:)
        character(len=40) :: subs(0:2)
        integer :: k, j, i, n

        call coding_system_list(names)
        allocate (items(4*size(names) + 16))
        n = 0

        do k = 1, size(names)
            call coding_system_aliases(names(k)%name, aliases)

            do j = 1, size(aliases)
                if (any_name(items(1:n), aliases(j)%name)) cycle
                n = n + 1
                items(n) = value_type(text=aliases(j)%name)
            end do

            call coding_system_subsidiaries(names(k)%name, subs)

            do i = 0, 2
                if (len_trim(subs(i)) == 0) cycle
                if (any_name(items(1:n), trim(subs(i)))) cycle
                n = n + 1
                items(n) = value_type(text=trim(subs(i)))
            end do
        end do

        list = make_list(items(1:n))

    contains

        logical function any_name(vs, name)
            type(value_type), intent(in) :: vs(:)
            character(len=*), intent(in) :: name
            integer :: m

            any_name = .false.

            do m = 1, size(vs)
                if (xstring(vs(m)) == name) then
                    any_name = .true.
                    return
                end if
            end do
        end function any_name
    end function coding_system_names

    !> Read a coding system from the minibuffer, prompting with string PROMPT.
    !> If the user enters null input, return second argument DEFAULT_CODING_SYSTEM.
    !> Return the coding-system's name, or "" if both the user input and
    !> DEFAULT_CODING_SYSTEM are empty.
    !> Ignores case when completing coding systems (all Emacs coding systems
    !> are lower-case).  Port of coding.c:Fread_coding_system.
    subroutine read_coding_system(prompt, default_coding_system, coding, status)
        character(len=*), intent(in) :: prompt, default_coding_system
        character(len=:), allocatable, intent(out) :: coding
        type(status_type), intent(out) :: status

        call completing_read(prompt, completion_list_table(coding_system_names()), coding, status, &
                             require_match=REQUIRE_MATCH_YES, history="coding_system_history", &
                             default=default_coding_system)
        if (.not. status%ok()) coding = ""
    end subroutine read_coding_system

    !> The prompt "PROMPT (default DEFAULT): ", or "PROMPT: ".  Not a port.
    function format_prompt(prompt, default) result(full)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable :: full

        full = prompt
        if (len(default) > 0) full = full//" (default "//default//")"
        full = full//": "
    end function format_prompt

    !> The whole text of the current buffer.  Not a port.
    function buffer_text() result(text)
        character(len=:), allocatable :: text

        text = buf_bytes(buffers(current), BEGINNING_BYTE, buffers(current)%text%z_byte)
    end function buffer_text

    !> Whether NAME is in NAMES.  Not a port.
    logical function has_coding_system(name, names)
        character(len=*), intent(in) :: name
        type(coding_system_list_entry_type), intent(in) :: names(:)
        integer :: k

        has_coding_system = .false.

        do k = 1, size(names)
            if (names(k)%name == name) then
                has_coding_system = .true.
                return
            end if
        end do
    end function has_coding_system

    !> Read the coding system for saving the current buffer, offering as
    !> the default the coding system the buffer's text asks for, or the
    !> first preferred one that encodes it and differs from the current
    !> one.  Port of mule.el:read-buffer-file-coding-system (completion
    !> offers every coding system).
    subroutine read_buffer_file_coding_system(coding, status)
        character(len=:), allocatable, intent(out) :: coding
        type(status_type), intent(out) :: status
        type(coding_system_list_entry_type), allocatable :: bcss(:), prio(:)
        character(len=:), allocatable :: auto_cs, filename, bfcs_default, current_coding, default, cs
        type(value_type) :: v
        type(status_type) :: vst
        logical :: all, any_safe
        integer :: k

        call find_coding_systems_text(buffer_text(), bcss, all)
        filename = ""
        if (allocated(buffers(current)%filename)) filename = buffers(current)%filename
        call set_auto_coding(filename, buffer_text(), auto_cs)
        call default_value(value_type(name="buffer_file_coding_system"), v, vst)
        bfcs_default = ""
        if (vtag(v) == V_STRING) bfcs_default = xstring(v)
        current_coding = ""
        if (allocated(buffer_file_coding_system%value)) current_coding = coding_system_base(buffer_file_coding_system%value)
        ! Generally use as a default the first preferred coding-system
        ! different from the current coding-system, except for the case of
        ! auto-cs since choosing anything else is asking for trouble.
        default = auto_cs

        if (len(default) == 0) then
            any_safe = all .or. has_coding_system(coding_system_base(bfcs_default), bcss)

            if (len(bfcs_default) > 0 .and. any_safe .and. coding_system_base(bfcs_default) /= current_coding) then
                default = bfcs_default
            else
                call coding_system_priority_list(prio)

                do k = 1, size(prio)
                    cs = prio(k)%name
                    if (len(coding_system_mime_charset(cs)) == 0) cycle
                    if (.not. (all .or. has_coding_system(coding_system_base(cs), bcss))) cycle
                    if (coding_system_base(cs) == current_coding) cycle
                    default = cs
                    exit
                end do
            end if
        end if

        call completing_read(format_prompt("Coding system for saving file", default), &
                             completion_list_table(coding_system_names()), coding, status, &
                             require_match=REQUIRE_MATCH_YES, history="coding_system_history", default=default)
        if (.not. status%ok()) coding = ""
    end subroutine read_buffer_file_coding_system

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Set the file coding-system of the current buffer to CODING-SYSTEM.
    !> This means that when you save the buffer, it will be converted
    !> according to CODING-SYSTEM.  For a list of possible values of
    !> CODING-SYSTEM, use \[list_coding_systems].
    !>
    !> If CODING-SYSTEM leaves the text conversion unspecified, or if it leaves
    !> the end-of-line conversion unspecified, FORCE controls what to do.
    !> If FORCE is false, get the unspecified aspect (or aspects) from the buffer's
    !> previous `buffer_file_coding_system' value (if it is specified there).
    !> Otherwise, leave it unspecified.
    !>
    !> This marks the buffer modified so that the succeeding \[save_buffer]
    !> surely saves the buffer with CODING-SYSTEM.  From a program, if you
    !> don't want to mark the buffer modified, specify true for NOMODIFY.
    !> If you know exactly what coding system you want to use,
    !> just set the variable `buffer_file_coding_system' directly.  Port of
    !> mule.el:set-buffer-file-coding-system (INTERACTIVE: called
    !> interactively, which checks that saving would succeed).
    recursive subroutine set_buffer_file_coding_system(coding_system, force, nomodify, interactive, status)
        character(len=*), intent(in) :: coding_system
        logical, intent(in) :: force, nomodify, interactive
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs, chosen
        type(coding_system_list_entry_type), allocatable :: css(:), unsafe(:)
        logical :: all
        integer :: k

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return
        cs = coding_system

        if (len(cs) > 0 .and. allocated(buffer_file_coding_system%value) .and. .not. force) then
            if (len(buffer_file_coding_system%value) > 0) cs = merge_coding_systems(cs, buffer_file_coding_system%value)
        end if

        if (interactive .and. coding_system_charset_list(cs) /= "emacs") then
            ! Check whether save would succeed, and jump to the offending
            ! char(s) if not.
            call find_coding_systems_text(buffer_text(), css, all)

            if (.not. all .and. .not. has_coding_system(coding_system_base(cs), css)) then
                allocate (unsafe(1))
                unsafe(1)%name = cs
                call select_safe_coding_system_interactively(1_int64, buffers(current)%text%z, css, unsafe, "", chosen, &
                                                             status)
                if (.not. status%ok()) return
                cs = chosen
            end if
        end if

        call buffer_file_coding_system%set_local(cs)
        k = explicit_index(.true.)
        explicit(k)%write = cs
        if (.not. nomodify) call restore_buffer_modified(.true.)
    end subroutine set_buffer_file_coding_system

    recursive subroutine cmd_set_buffer_file_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs
        logical :: force

        force = current_prefix_arg%kind /= PREFIX_NONE
        call read_buffer_file_coding_system(cs, status)
        if (.not. status%ok()) return
        call set_buffer_file_coding_system(cs, force, .false., .true., status)
    end subroutine cmd_set_buffer_file_coding_system

    !> Execute an I/O command using the specified CODING_SYSTEM.  Port of
    !> mule-cmds.el:universal-coding-system-argument (the echo of the
    !> coding system while the command's keys are typed is Deferred).
    recursive subroutine universal_coding_system_argument(coding_system, status)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status

        status = status_type()
        call prefix_command_preserve_state()
        prefixed_next_coding_system = coding_system
        call pre_command_hook%append(prefixed_command_pch)
        call prefix_command_preserve_state_hook%append(prefixed_command_preserve)
    end subroutine universal_coding_system_argument

    recursive subroutine cmd_universal_coding_system_argument(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, coding

        default = ""

        if (allocated(buffer_file_coding_system%value)) then
            if (len(buffer_file_coding_system%value) > 0) then
                if (coding_system_type(buffer_file_coding_system%value) /= "undecided") then
                    default = buffer_file_coding_system%value
                end if
            end if
        end if

        call read_coding_system(format_prompt("Coding system for following command", default), default, coding, status)
        if (.not. status%ok()) return
        call universal_coding_system_argument(coding, status)
    end subroutine cmd_universal_coding_system_argument

    ! The function of `pre_command_hook' that runs the command with the
    ! coding system of `universal_coding_system_argument'.  Port of
    ! mule-cmds.el:mule-cmds--prefixed-command-pch.
    recursive subroutine prefixed_command_pch(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (len(prefixed_next_coding_system) == 0) then
            call pre_command_hook%remove(prefixed_command_pch)
            call prefix_command_preserve_state_hook%remove(prefixed_command_preserve)
        else
            prefixed_coding_system = prefixed_next_coding_system
            this_command_wrapper => prefixed_command
            prefixed_last_coding_system = prefixed_next_coding_system
            prefixed_next_coding_system = ""
        end if
    end subroutine prefixed_command_pch

    ! The command run in place of CMD: CMD with `coding_system_for_read'
    ! and `coding_system_for_write' bound to the coding system (the
    ! lambda of mule-cmds--prefixed-command-pch).  Port of
    ! international/mule-cmds.el:mule-cmds--prefixed-command-pch, in part.
    recursive subroutine prefixed_command(cmd, status)
        type(value_type), intent(in) :: cmd
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved_read, saved_write
        logical :: had_read, had_write, saved_warning

        this_command => command_of(cmd)
        had_read = allocated(coding_system_for_read%value)
        if (had_read) saved_read = coding_system_for_read%value
        had_write = allocated(coding_system_for_write%value)
        if (had_write) saved_write = coding_system_for_write%value
        saved_warning = coding_system_require_warning%value
        coding_system_for_read = prefixed_coding_system
        coding_system_for_write = prefixed_coding_system
        coding_system_require_warning = .true.
        call execute_interactively(cmd, status)

        if (had_read) then
            coding_system_for_read = saved_read
        else
            call coding_system_for_read%clear()
        end if

        if (had_write) then
            coding_system_for_write = saved_write
        else
            call coding_system_for_write%clear()
        end if

        coding_system_require_warning = saved_warning
    end subroutine prefixed_command

    ! The function of `prefix_command_preserve_state_hook' keeping the
    ! coding system for the next command.  Port of
    ! mule-cmds.el:mule-cmds--prefixed-command-preserve.
    recursive subroutine prefixed_command_preserve(status)
        type(status_type), intent(out) :: status

        status = status_type()
        prefixed_next_coding_system = prefixed_last_coding_system
    end subroutine prefixed_command_preserve

    !> Set coding systems for the process associated with the current
    !> buffer.  DECODING is the coding system to be used to decode input
    !> from the process, ENCODING is the coding system to be used to
    !> encode output to the process.
    !>
    !> For a list of possible coding systems, use
    !> \[list_coding_systems].  Port of
    !> mule.el:set-buffer-process-coding-system.
    recursive subroutine set_buffer_process_coding_system(decoding, encoding, status)
        character(len=*), intent(in) :: decoding, encoding
        type(status_type), intent(out) :: status
        type(handle_type) :: process

        process = get_buffer_process(current_buffer())

        if (process%index == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No process")
            return
        end if

        if (len(decoding) > 0) then
            call check_coding_system(decoding, status)
            if (.not. status%ok()) return
        end if

        if (len(encoding) > 0) then
            call check_coding_system(encoding, status)
            if (.not. status%ok()) return
        end if

        ! (The mode line is redisplayed after each command: nothing for
        ! force-mode-line-update to do.)
        call set_process_coding_system(process, decoding, encoding, status)
    end subroutine set_buffer_process_coding_system

    recursive subroutine cmd_set_buffer_process_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: decoding, encoding

        call read_coding_system("Coding-system for output from the process: ", "", decoding, status)
        if (.not. status%ok()) return
        call read_coding_system("Coding-system for input to the process: ", "", encoding, status)
        if (.not. status%ok()) return
        call set_buffer_process_coding_system(decoding, encoding, status)
    end subroutine cmd_set_buffer_process_coding_system

    !> Set coding system of terminal output to CODING_SYSTEM.
    !> All text output to the terminal will be encoded with the specified
    !> coding system.
    !>
    !> For a list of possible values of CODING_SYSTEM, use
    !> \[list_coding_systems].  The default is determined by the selected
    !> language environment or by the previous use of this command.
    !>
    !> By default, this function will redraw the current frame; optional
    !> argument INHIBIT_REFRESH, if true, prevents that.  Port of
    !> mule.el:set-terminal-coding-system (the one terminal).
    recursive subroutine set_terminal_coding_system(coding_system, status, inhibit_refresh)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: inhibit_refresh
        character(len=:), allocatable :: cs
        logical :: refresh

        cs = coding_system
        if (len(cs) == 0 .and. len(terminal_coding_system()) == 0) cs = default_terminal_coding_system
        if (len(cs) > 0) default_terminal_coding_system = cs
        call set_terminal_coding_system_internal(cs, status)
        if (.not. status%ok()) return
        refresh = .true.
        if (present(inhibit_refresh)) refresh = .not. inhibit_refresh
        if (refresh) call redraw_frame(selected_frame)
    end subroutine set_terminal_coding_system

    recursive subroutine cmd_set_terminal_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, coding

        default = ""
        if (len(terminal_coding_system()) == 0) default = default_terminal_coding_system
        call read_coding_system(format_prompt("Coding system for terminal display", default), default, coding, status)
        if (.not. status%ok()) return
        call set_terminal_coding_system(coding, status)
    end subroutine cmd_set_terminal_coding_system

    !> Set coding system for keyboard input on the terminal to
    !> CODING_SYSTEM.
    !>
    !> For a list of possible values of CODING_SYSTEM, use
    !> \[list_coding_systems].  The default is determined by the selected
    !> language environment or by the previous use of this command.
    !>
    !> If CODING_SYSTEM is "" or the coding-type of CODING_SYSTEM is
    !> `raw-text', the decoding of keyboard input is disabled.
    !>
    !> The setting has no effect on graphical terminals.  Port of
    !> mule.el:set-keyboard-coding-system (the one terminal).
    recursive subroutine set_keyboard_coding_system(coding_system, status)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs, coding_type, name
        logical :: accept_8_bit

        status = status_type()
        cs = coding_system
        name = cs
        if (len(name) == 0) name = "nil"

        if (len(cs) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Unsuitable coding system for keyboard: "//name)
            return
        end if

        if (.not. (is_coding_system_suitable_for_keyboard(cs) .or. is_coding_system_ascii_compatible(cs))) then
            call set_error(status, FM_GENERIC_ERROR, "Unsuitable coding system for keyboard: "//name)
            return
        end if

        coding_type = coding_system_type(cs)

        select case (coding_type)
        case ("raw-text", "charset", "utf-8", "shift-jis", "big5", "ccl")
            accept_8_bit = .true.
        case ("iso-2022")
            accept_8_bit = .not. is_coding_system_iso_7_bit(cs)
        case default
            call set_error(status, FM_GENERIC_ERROR, "Unsupported coding system for keyboard: "//name)
            return
        end select

        if (accept_8_bit) then
            if (.not. has_saved_meta_mode) then
                saved_meta_mode = current_input_meta_mode()
                has_saved_meta_mode = .true.
            end if

            call set_input_meta_mode(2)
        else if (has_saved_meta_mode) then
            call set_input_meta_mode(saved_meta_mode)
            has_saved_meta_mode = .false.
        end if

        ! Avoid end-of-line conversion.
        cs = coding_system_change_eol_conversion(cs, EOL_UNIX)
        call set_keyboard_coding_system_internal(cs, status)
        if (.not. status%ok()) return
        keyboard_coding_system_variable = cs
    end subroutine set_keyboard_coding_system

    recursive subroutine cmd_set_keyboard_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, coding

        default = ""
        if (coding_system_type(keyboard_coding_system()) == "raw-text") default = default_keyboard_coding_system
        call read_coding_system(format_prompt("Coding system for keyboard input", default), default, coding, status)
        if (.not. status%ok()) return
        call set_keyboard_coding_system(coding, status)
    end subroutine cmd_set_keyboard_coding_system

    !> Visit the current buffer's file again using coding system CODING-SYSTEM.
    !> For a list of possible values of CODING-SYSTEM, use \[list_coding_systems].
    !>
    !> If CODING-SYSTEM leaves the text conversion unspecified, or if it leaves
    !> the end-of-line conversion unspecified, FORCE controls what to do.
    !> If FORCE is false, get the unspecified aspect (or aspects) from the buffer's
    !> previous `buffer_file_coding_system' value (if it is specified there).
    !> Otherwise, determine it from the file contents as usual for visiting a file.
    !> Port of mule.el:revert-buffer-with-coding-system.
    recursive subroutine revert_buffer_with_coding_system(coding_system, force, status)
        character(len=*), intent(in) :: coding_system
        logical, intent(in) :: force
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs, saved
        logical :: had

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return
        cs = coding_system

        if (len(cs) > 0 .and. allocated(buffer_file_coding_system%value) .and. .not. force) then
            if (len(buffer_file_coding_system%value) > 0) cs = merge_coding_systems(cs, buffer_file_coding_system%value)
        end if

        ! (let ((coding-system-for-read coding-system)) (revert-buffer))
        had = allocated(coding_system_for_read%value)
        if (had) saved = coding_system_for_read%value

        if (len(cs) > 0) then
            coding_system_for_read = cs
        else
            call coding_system_for_read%clear()
        end if

        call revert_buffer(status)

        if (had) then
            coding_system_for_read = saved
        else
            call coding_system_for_read%clear()
        end if
    end subroutine revert_buffer_with_coding_system

    recursive subroutine cmd_revert_buffer_with_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs
        logical :: force

        force = current_prefix_arg%kind /= PREFIX_NONE
        call read_coding_system("Coding system for visited file (default nil): ", "", cs, status)
        if (.not. status%ok()) return
        call revert_buffer_with_coding_system(cs, force, status)
    end subroutine cmd_revert_buffer_with_coding_system

    !> Set default value of various coding systems to CODING-SYSTEM.
    !> This sets the following coding systems:
    !>   o coding system of a newly created buffer
    !>   o default coding system for subprocess I/O
    !> Port of mule-cmds.el:set-default-coding-systems (the file name,
    !> terminal and keyboard coding systems are Deferred).
    subroutine set_default_coding_systems(coding_system, status)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: output, input

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return
        call set_default(value_type(name="buffer_file_coding_system"), value_type(text=coding_system), status)
        if (.not. status%ok()) return
        ! Preserve eol-type from existing default-process-coding-systems.
        output = coding_system_change_text_conversion(trim(default_process_coding_system(1)), coding_system)
        input = coding_system_change_text_conversion(trim(default_process_coding_system(2)), coding_system)
        default_process_coding_system(1) = output
        default_process_coding_system(2) = input
    end subroutine set_default_coding_systems

    !> Add CODING-SYSTEM at the front of the priority list for automatic detection.
    !> This also sets the following coding systems:
    !>   o coding system of a newly created buffer
    !>   o default coding system for subprocess I/O
    !>
    !> If CODING-SYSTEM specifies a certain type of EOL conversion, the coding
    !> systems set by this function will use that type of EOL conversion.
    !>
    !> A coding system that requires automatic detection of text+encoding
    !> (e.g. undecided, unix) can't be preferred.
    !>
    !> To prefer, for instance, utf-8, say the following:
    !>
    !>   call prefer_coding_system("utf-8", status)
    !>
    !> Port of mule-cmds.el:prefer-coding-system.
    subroutine prefer_coding_system(coding_system, status, interactive)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status
        !> Called interactively: say when the base was preferred.
        logical, intent(in), optional :: interactive
        character(len=:), allocatable :: base, cs, t
        integer :: eol

        if (len(coding_system) == 0 .or. .not. is_coding_system(coding_system)) then
            call set_error(status, FM_GENERIC_ERROR, &
                           format_message("Invalid coding system `")//coding_system//format_message("'"))
            return
        end if

        t = coding_system_type(coding_system)

        if (t == "raw-text" .or. t == "undecided") then
            call set_error(status, FM_GENERIC_ERROR, &
                           format_message("Can't prefer the coding system `")//coding_system//format_message("'"))
            return
        end if

        base = coding_system_base(coding_system)
        eol = coding_system_eol_type(coding_system)
        call set_coding_system_priority([character(len=len(base)) :: base], status)
        if (.not. status%ok()) return

        if (present(interactive)) then
            if (interactive .and. base /= coding_system) then
                call message("Highest priority is set to "//base//" (base of "//coding_system//")")
            end if
        end if

        ! If they asked for specific EOL conversion, honor that.
        cs = base
        if (eol /= EOL_UNDECIDED) cs = coding_system_change_eol_conversion(base, eol)
        call set_default_coding_systems(cs, status)
    end subroutine prefer_coding_system

    recursive subroutine cmd_prefer_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs

        do
            call completing_read("Coding system: ", completion_list_table(coding_system_names()), cs, status, &
                                 require_match=REQUIRE_MATCH_YES, history="coding_system_history")
            if (.not. status%ok()) return
            if (len(cs) > 0) exit
        end do

        call prefer_coding_system(cs, status, interactive=.true.)
    end subroutine cmd_prefer_coding_system

    ! ------------------------------------------------------------------
    ! Choosing a coding system that can encode the text
    ! ------------------------------------------------------------------

    !> Ask a user to select a safe coding system from candidates.
    !> The candidates of coding systems which can safely encode a text
    !> between FROM and TO are shown in a popup window.  Among them, the most
    !> proper one is suggested as the default.
    !>
    !> The list of `buffer_file_coding_system' of the current buffer, the
    !> default `buffer_file_coding_system', and the most preferred coding
    !> system (if it corresponds to a MIME charset) is treated as the
    !> default coding system list.  Among them, the first one that safely
    !> encodes the text is normally selected silently and returned without
    !> any user interaction.  See also the command `prefer_coding_system'.
    !>
    !> However, the user is queried if the chosen coding system is
    !> inconsistent with what would be selected by `find_auto_coding' from
    !> coding cookies &c. if the contents of the region were read from a
    !> file.  (That could lead to data corruption in a file subsequently
    !> re-visited and edited.)
    !>
    !> DEFAULT_CODING_SYSTEM specifies a coding system to be prepended to
    !> the default coding system list.  ONLY_DEFAULT true is upstream's list
    !> whose first element is t: then only DEFAULT_CODING_SYSTEM is the
    !> default list.
    !>
    !> FILE is the file name to use for this purpose.
    !> That is different from `buffer_file_name' when handling `write_region'
    !> (for example).  Port of mule-cmds.el:select-safe-coding-system (no
    !> ACCEPT-DEFAULT-P function, `find_file_literally' or string FROM).
    recursive subroutine select_safe_coding_system(from, to, default_coding_system, only_default, file, coding, status)
        integer(int64), intent(in) :: from, to
        character(len=*), intent(in) :: default_coding_system, file
        logical, intent(in) :: only_default
        character(len=:), allocatable, intent(out) :: coding
        type(status_type), intent(out) :: status
        type(coding_system_list_entry_type), allocatable :: defaults(:), codings(:), safe(:), unsafe(:), rejected(:), &
                                                            prio(:)
        character(len=:), allocatable :: auto_cs, source, text, file_name, base, dflt, read, write, preferred, cs, by
        type(value_type) :: v
        type(status_type) :: vst
        logical :: no_other_defaults, all, ok
        integer :: k, nd, eol, n
        integer(int64) :: tick

        coding = ""
        status = status_type()
        no_other_defaults = only_default
        allocate (defaults(0))
        nd = 0
        if (len(default_coding_system) > 0) call append(defaults, nd, default_coding_system)
        ! Find an auto-coding that is specified for the current buffer and
        ! file from the region FROM and TO.
        text = region_text(from, to)
        file_name = file
        if (len(file_name) == 0 .and. allocated(buffers(current)%filename)) file_name = buffers(current)%filename
        call find_auto_coding(file_name, text, auto_cs, source, multibyte=buffers(current)%multibyte)

        if (len(auto_cs) > 0) then
            if (.not. is_coding_system(auto_cs)) then
                if (source == ":coding") then
                    by = ":coding tag"
                else
                    by = format_message("variable `")//source//format_message("'")
                end if

                call message(format_message("Warning (mule): Invalid coding system `")//auto_cs// &
                             format_message("' is specified")//LF//"for the current buffer/file by the "//by//"."//LF// &
                             "It is highly recommended to fix it before writing to a file.")
                call yes_or_no(ok, status)
                if (.not. status%ok()) return

                if (.not. ok) then
                    call set_error(status, FM_GENERIC_ERROR, "Save aborted")
                    return
                end if

                auto_cs = ""
            end if
        end if

        if (len(auto_cs) > 0 .and. .not. no_other_defaults) then
            ! If the file has a coding cookie, use it regardless of any
            ! other setting.
            base = coding_system_base(auto_cs)

            if (base /= "undecided") then
                ! For encoding, no-conversion-multibyte is the same as
                ! no-conversion.
                if (base == "no-conversion-multibyte") then
                    auto_cs = "no-conversion"
                    base = "no-conversion"
                end if

                deallocate (defaults)
                allocate (defaults(0))
                nd = 0
                call append(defaults, nd, auto_cs)
                no_other_defaults = .true.
            end if
        end if

        if (.not. no_other_defaults) then
            ! If buffer-file-coding-system is not nil nor undecided, append
            ! it to the defaults.
            if (allocated(buffer_file_coding_system%value)) then
                if (len(buffer_file_coding_system%value) > 0) then
                    base = coding_system_base(buffer_file_coding_system%value)

                    if (base /= "undecided" .and. .not. has_base(defaults(1:nd), base)) then
                        call append(defaults, nd, buffer_file_coding_system%value)
                    end if
                end if
            end if

            call buffer_file_coding_system_explicit(read, write)

            if (len(write) == 0) then
                ! If default buffer-file-coding-system is not nil nor
                ! undecided, append it to the defaults.
                call default_value(value_type(name="buffer_file_coding_system"), v, vst)
                dflt = ""
                if (vtag(v) == V_STRING) dflt = xstring(v)

                if (len(dflt) > 0) then
                    base = coding_system_base(dflt)
                    if (base /= "undecided" .and. .not. has_base(defaults(1:nd), base)) call append(defaults, nd, dflt)
                end if

                ! If the most preferred coding system has the property
                ! mime-charset, append it to the defaults.
                call coding_system_priority_list(prio, .true.)

                if (size(prio) > 0) then
                    preferred = prio(1)%name
                    base = coding_system_base(preferred)

                    if (len(coding_system_mime_charset(preferred)) > 0 .and. .not. has_base(defaults(1:nd), base)) then
                        call append(defaults, nd, preferred)
                    end if
                end if
            end if
        end if

        ! Decide the eol-type from the top of the default codings, current
        ! buffer-file-coding-system, or default buffer-file-coding-system.
        if (nd > 0) then
            eol = coding_system_eol_type(defaults(1)%name)

            if (eol == EOL_UNDECIDED .and. allocated(buffer_file_coding_system%value)) then
                eol = coding_system_eol_type(buffer_file_coding_system%value)
            end if

            if (eol == EOL_UNDECIDED) then
                call default_value(value_type(name="buffer_file_coding_system"), v, vst)
                if (vtag(v) == V_STRING) eol = coding_system_eol_type(xstring(v))
            end if

            if (eol /= EOL_UNDECIDED) then
                do k = 1, nd
                    defaults(k)%name = coding_system_change_eol_conversion(defaults(k)%name, eol)
                end do
            end if
        end if

        call find_coding_systems_text(text, codings, all)
        tick = buffers(current)%text%chars_modiff

        if (all) then
            ! Any coding system is ok.
            if (nd > 0) coding = defaults(1)%name
        else
            ! Classify the defaults into safe, rejected, and unsafe.
            allocate (safe(0), unsafe(0), rejected(0))
            n = 0

            do k = 1, nd
                if (has_coding_system(coding_system_base(defaults(k)%name), codings)) then
                    call append(safe, n, defaults(k)%name)
                end if
            end do

            n = 0

            do k = 1, nd
                if (.not. has_coding_system(coding_system_base(defaults(k)%name), codings)) call append(unsafe, n, defaults(k)%name)
            end do

            ! If there are safe ones, the first one is what we want.
            if (size(safe) > 0) coding = safe(1)%name

            ! If all the defaults failed, ask a user.
            if (len(coding) == 0) then
                ! If UTF-8 is in CODINGS, but is not its first member, make
                ! it the first one, so it is offered as the default.
                if (has_coding_system("utf-8", codings)) call to_front(codings, "utf-8")
                cs = ""
                if (size(codings) > 0) cs = codings(1)%name
                call select_safe_coding_system_interactively(from, to, codings, unsafe, cs, coding, status)
                if (.not. status%ok()) return
            end if
        end if

        ! Check we're not inconsistent with what `coding:' spec &c would
        ! give when file is re-read.
        if (len(auto_cs) > 0 .and. .not. (len(coding) > 0 .and. no_other_defaults .and. coding == auto_cs)) then
            call merge_auto_cs(coding, auto_cs)

            if (len(auto_cs) > 0 .and. len(auto_codings_lookup(file_name)) == 0 &
                .and. .not. coding_system_equal(coding, auto_cs)) then
                if (coding_system_type(auto_cs) == "charset" .or. coding_system_type(coding) == "charset" &
                    .or. coding_system_type(auto_cs) /= coding_system_type(coding)) then
                    call yes_or_no_prompt("Selected encoding "//coding//" disagrees with "//auto_cs// &
                                          " specified by file contents.  Really save (else edit coding cookies "// &
                                          "and try again)? ", ok, status)
                    if (.not. status%ok()) return

                    if (.not. ok) then
                        call set_error(status, FM_GENERIC_ERROR, "Save aborted")
                        return
                    end if
                end if
            end if
        end if

        if (tick /= buffers(current)%text%chars_modiff) then
            call set_error(status, FM_GENERIC_ERROR, "Canceled because the buffer was modified")
            return
        end if

        if (len(coding) > 0) then
            if (coding_system_type(coding) == "undecided" .and. is_coding_system_prefer_utf_8(coding) &
                .and. to - from < len(text, kind=int64)) then
                coding = coding_system_change_text_conversion(coding, "utf-8")
            end if
        end if

    contains

        !> Merge CODING and AUTO_CS as far as possible.
        subroutine merge_auto_cs(coding, auto_cs)
            character(len=:), allocatable, intent(inout) :: coding, auto_cs
            integer :: eol1, eol2

            if (len(coding) == 0) then
                coding = auto_cs
                return
            end if

            eol1 = coding_system_eol_type(coding)
            eol2 = coding_system_eol_type(auto_cs)

            if (coding_system_base(coding) == "undecided") then
                coding = coding_system_change_text_conversion(coding, auto_cs)
            else if (coding_system_base(auto_cs) == "undecided") then
                auto_cs = coding_system_change_text_conversion(auto_cs, coding)
            end if

            if (eol1 == EOL_UNDECIDED) then
                if (eol2 /= EOL_UNDECIDED) coding = coding_system_change_eol_conversion(coding, eol2)
            else if (eol2 == EOL_UNDECIDED) then
                auto_cs = coding_system_change_eol_conversion(auto_cs, eol1)
            end if
        end subroutine merge_auto_cs
    end subroutine select_safe_coding_system

    !> Ask "Really proceed with writing? ".  Not a port.
    subroutine yes_or_no(ok, status)
        logical, intent(out) :: ok
        type(status_type), intent(out) :: status

        call yes_or_no_prompt("Really proceed with writing? ", ok, status)
    end subroutine yes_or_no

    ! Ask PROMPT, to be answered yes or no.  Not a port.
    subroutine yes_or_no_prompt(prompt, ok, status)
        use fns_mod, only: read_yes_or_no
        character(len=*), intent(in) :: prompt
        logical, intent(out) :: ok
        type(status_type), intent(out) :: status

        call read_yes_or_no(prompt, ok, status)
    end subroutine yes_or_no_prompt

    !> The text between FROM and TO of the current buffer.  Not a port.
    function region_text(from, to) result(text)
        use marker_mod, only: char_to_byte
        integer(int64), intent(in) :: from, to
        character(len=:), allocatable :: text

        text = buf_bytes(buffers(current), char_to_byte(from), char_to_byte(to))
    end function region_text

    ! Append the coding system NAME to LIST (N used).  Not a port.
    subroutine append(list, n, name)
        type(coding_system_list_entry_type), allocatable, intent(inout) :: list(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: name
        type(coding_system_list_entry_type), allocatable :: grown(:)

        allocate (grown(n + 1))
        grown(1:n) = list(1:n)
        grown(n + 1)%name = name
        call move_alloc(grown, list)
        n = n + 1
    end subroutine append

    !> Whether a coding system in LIST has base BASE (rassq).  Not a port.
    logical function has_base(list, base)
        type(coding_system_list_entry_type), intent(in) :: list(:)
        character(len=*), intent(in) :: base
        integer :: k

        has_base = .false.

        do k = 1, size(list)
            if (coding_system_base(list(k)%name) == base) has_base = .true.
        end do
    end function has_base

    !> Move NAME to the front of LIST.  Not a port.
    subroutine to_front(list, name)
        type(coding_system_list_entry_type), intent(inout) :: list(:)
        character(len=*), intent(in) :: name
        integer :: k, j

        do k = 1, size(list)
            if (list(k)%name == name) exit
        end do

        if (k > size(list) .or. k == 1) return

        do j = k, 2, -1
            list(j)%name = list(j - 1)%name
        end do

        list(1)%name = name
    end subroutine to_front

    !> Select interactively a coding system for the region FROM ... TO.
    !> CODINGS is the list of base coding systems known to be safe for this region,
    !>   typically obtained with `find_coding_systems_region'.
    !> UNSAFE is a list of coding systems known to be unsafe for this region.
    !> DEFAULT is the coding system to use by default in the query.
    !> Port of mule-cmds.el:select-safe-coding-system-interactively (no
    !> REJECTED coding systems; the problematic characters are not
    !> buttons).
    recursive subroutine select_safe_coding_system_interactively(from, to, codings, unsafe, default, coding, status)
        integer(int64), intent(in) :: from, to
        type(coding_system_list_entry_type), intent(in) :: codings(:), unsafe(:)
        character(len=*), intent(in) :: default
        character(len=:), allocatable, intent(out) :: coding
        type(status_type), intent(out) :: status
        type(unencodable_type), allocatable :: bad(:)
        character(len=40) :: names(1)
        character(len=:), allocatable :: text, bufname, list_line, line
        integer(int64) :: p, firstpos
        integer :: k, j
        type(status_type) :: st

        coding = ""
        bufname = buffers(current)%name
        text = "These default coding systems were tried to safely encode the following"//LF// &
               format_message("problematic characters in the buffer `")//bufname//format_message("'")//":"//LF
        firstpos = to

        do k = 1, size(unsafe)
            names(1) = unsafe(k)%name
            call check_coding_systems_text(region_text(from, to), from, names, bad, st)

            if (size(bad) > 0) then
                line = "  "//unsafe(k)%name//":"

                do j = 1, min(11, size(bad(1)%positions))
                    p = bad(1)%positions(j)
                    line = line//" ("//itoa(p)//" . "//char_at(p)//")"
                    firstpos = min(firstpos, p)
                end do

                text = text//line//LF
            end if
        end do

        text = text//"However, each of them encountered characters it couldn"//format_message("'")//"t encode safely:"//LF

        do k = 1, size(unsafe)
            names(1) = unsafe(k)%name
            call check_coding_systems_text(region_text(from, to), from, names, bad, st)
            if (size(bad) == 0) cycle
            line = "  "//unsafe(k)%name//" cannot safely encode these:"

            do j = 1, size(bad(1)%positions)
                if (j <= 10) then
                    line = line//" "//char_at(bad(1)%positions(j))
                else
                    line = line//" ..."
                    exit
                end if
            end do

            text = text//line//LF
        end do

        text = text//LF//"Click on a character (or switch to this window by "//format_message("`C-x o'")//LF// &
               "and select the characters by RET) to jump to the place it appears,"//LF// &
               "where "//format_message("`C-u C-x ='")//" will give information about it."//LF
        text = text//LF//"Select one of the safe coding systems listed below,"//LF// &
               "or cancel the writing with C-g and edit the buffer"//LF// &
               "   to remove or modify the problematic characters,"//LF// &
               "or specify any other coding system (and risk losing"//LF// &
               "   the problematic characters)."//LF//LF
        list_line = ""

        do k = 1, size(codings)
            list_line = list_line//"  "//codings(k)%name
        end do

        text = text//fill_list(list_line)//LF
        call show_warning(text, status)
        if (.not. status%ok()) return
        ! Read a coding system.
        call read_coding_system(format_prompt("Select coding system", default), default, coding, status)
        if (.not. status%ok()) return
        last_coding_system_specified = coding
        call close_warning()

    contains

        function char_at(pos) result(s)
            use marker_mod, only: char_to_byte
            use character_mod, only: multibyte_length
            integer(int64), intent(in) :: pos
            character(len=:), allocatable :: s
            integer(int64) :: b
            integer :: n

            b = char_to_byte(pos)
            s = buf_bytes(buffers(current), b, b + 1)
            n = multibyte_length(buf_bytes(buffers(current), b, min(b + 5, buffers(current)%text%z_byte)), 1_int64, .false.)
            if (n > 1) s = buf_bytes(buffers(current), b, b + n)
        end function char_at
    end subroutine select_safe_coding_system_interactively

    !> The coding system names of LINE, filled as fill-region-as-paragraph
    !> fills them with a fill prefix of two spaces (at `fill_column' 70).
    !> Not a port.
    function fill_list(line) result(filled)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: filled, word, current
        integer :: i, j

        filled = ""
        current = " "
        i = 1

        do while (i <= len(line))
            if (line(i:i) == " ") then
                i = i + 1
                cycle
            end if

            j = index(line(i:), " ")

            if (j == 0) then
                word = line(i:)
                i = len(line) + 1
            else
                word = line(i:i + j - 2)
                i = i + j
            end if

            if (len(current) + 1 + len(word) > 70 .and. len(current) > 2) then
                filled = filled//current//LF
                current = " "
            end if

            current = current//" "//word
        end do

        filled = filled//current
    end function fill_list

    !> Show TEXT in the "*Warning*" buffer (with-output-to-temp-buffer).
    !> Not a port.
    subroutine show_warning(text, status)
        use buffer_mod, only: get_or_make_buffer
        use insdel_mod, only: insert
        use buffer_mod, only: erase_buffer
        use window_mod, only: temp_buffer_window_show
        character(len=*), intent(in) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, previous, window
        type(status_type) :: st

        previous = current_buffer()
        call get_or_make_buffer("*Warning*", buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call erase_buffer(status)
        call insert(text, status)
        call temp_buffer_window_show(buffer, window, status)
        if (is_buffer_live(previous)) call set_buffer(previous, st)
    end subroutine show_warning

    ! Kill the "*Warning*" buffer, if any.  Not a port.
    subroutine close_warning()
        use buffer_mod, only: get_buffer, close_buffer
        type(handle_type) :: buffer
        type(status_type) :: st

        buffer = get_buffer("*Warning*")
        if (is_buffer_live(buffer)) call close_buffer(buffer, st)
    end subroutine close_warning

    ! N as a decimal string.  Not a port.
    function itoa(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buffer

        write (buffer, "(i0)") n
        s = trim(buffer)
    end function itoa

    ! ------------------------------------------------------------------
    ! Describing coding systems
    ! ------------------------------------------------------------------

    !> Print name and mnemonic letter of CODING-SYSTEM, and with DOC its
    !> docstring.  Port of mule-diag.el:print-coding-system-briefly.
    function print_coding_system_briefly(coding_system, doc) result(s)
        character(len=*), intent(in) :: coding_system
        logical, intent(in) :: doc
        character(len=:), allocatable :: s, alias_list
        type(coding_system_list_entry_type), allocatable :: aliases(:)
        character(len=40) :: subs(0:2)
        integer :: k, eol

        if (len(coding_system) == 0) then
            s = "nil"//LF
            return
        end if

        s = achar(coding_system_mnemonic(coding_system))//" -- "//coding_system
        call coding_system_aliases(coding_system, aliases)

        if (size(aliases) > 0) then
            if (aliases(1)%name == coding_system) then
                if (size(aliases) > 1) then
                    alias_list = ""

                    do k = 2, size(aliases)
                        alias_list = alias_list//" "//aliases(k)%name
                    end do

                    s = s//" (alias:"//alias_list//")"
                end if
            else if (has_coding_system(coding_system, aliases)) then
                s = s//" (alias of "//aliases(1)%name//")"
            else
                eol = coding_system_eol_type(coding_system)
                call coding_system_subsidiaries(aliases(1)%name, subs)

                if (eol /= EOL_UNDECIDED .and. len_trim(subs(0)) > 0) then
                    if (trim(subs(eol)) /= coding_system) s = s//" (alias of "//trim(subs(eol))//")"
                end if
            end if
        end if

        s = s//LF
        if (doc) s = s//LF//coding_system_doc_string(coding_system)//LF
    end function print_coding_system_briefly

    !> Display information about CODING-SYSTEM.  Port of
    !> mule-diag.el:describe-coding-system (no xref buttons for charsets).
    recursive subroutine describe_coding_system(coding_system, status)
        character(len=*), intent(in) :: coding_system
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s, t, charsets
        character(len=40) :: subs(0:2)

        if (len(coding_system) == 0) then
            call describe_current_coding_system(status)
            return
        end if

        call check_coding_system(coding_system, status)
        if (.not. status%ok()) return
        s = print_coding_system_briefly(coding_system, .true.)
        t = coding_system_type(coding_system)
        s = s//"Type: "//t

        select case (t)
        case ("undecided")
            s = s//" (do automatic conversion)"
        case ("utf-8")
            s = s//" (UTF-8: Emacs internal multibyte form)"
        case ("charset")
            s = s//" (charset)"
        case ("raw-text")
            s = s//" (text with random binary characters)"
        case default
            ! (UTF-16: nothing more.)
            continue
        end select

        s = s//LF//"EOL type: "

        select case (coding_system_eol_type(coding_system))
        case (EOL_UNDECIDED)
            call coding_system_subsidiaries(coding_system, subs)
            s = s//"Automatic selection from:"//LF//TAB//"["//trim(subs(0))//" "//trim(subs(1))//" "// &
                trim(subs(2))//"]"//LF
        case (EOL_UNIX)
            s = s//"LF"//LF
        case (EOL_DOS)
            s = s//"CRLF"//LF
        case default
            s = s//"CR"//LF
        end select

        charsets = coding_system_charset_list(coding_system)

        if (coding_system_base(coding_system) /= "raw-text" .and. len(charsets) > 0) then
            s = s//"This coding system encodes the following charsets:"//LF//"  "//charsets
        end if

        call with_help_window(s, status)
    end subroutine describe_coding_system

    recursive subroutine cmd_describe_coding_system(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: cs

        call read_coding_system("Describe coding system (default current choices): ", "", cs, status)
        if (.not. status%ok()) return
        call describe_coding_system(cs, status)
    end subroutine cmd_describe_coding_system

    !> Display coding systems currently used, in detail.  Port of
    !> mule-diag.el:describe-current-coding-system (the keyboard and
    !> terminal coding systems are those of a UTF-8 terminal; no process
    !> coding systems of the buffer's process; the patterns are shown as
    !> Fortran strings).
    recursive subroutine describe_current_coding_system(status)
        use print_mod, only: fortran_string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: s, dflt, aliases_text, pair
        type(coding_system_list_entry_type), allocatable :: prio(:), aliases(:)
        type(value_type) :: v
        type(status_type) :: vst
        integer :: k, j

        s = "Coding system for saving this buffer:"//LF//"  "

        if (buffer_file_coding_system%local_p() .and. allocated(buffer_file_coding_system%value)) then
            s = s//print_coding_system_briefly(buffer_file_coding_system%value, .false.)
        else
            s = s//"Not set locally, use the default."//LF
        end if

        call default_value(value_type(name="buffer_file_coding_system"), v, vst)
        dflt = ""
        if (vtag(v) == V_STRING) dflt = xstring(v)
        s = s//"Default coding system (for new files):"//LF//"  "//print_coding_system_briefly(dflt, .false.)
        s = s//"Coding system for keyboard input:"//LF//"  "//print_coding_system_briefly("utf-8-unix", .false.)
        s = s//"Coding system for terminal output:"//LF//"  "//print_coding_system_briefly("utf-8-unix", .false.)
        s = s//"Defaults for subprocess I/O:"//LF
        s = s//"  decoding: "//print_coding_system_briefly(trim(default_process_coding_system(1)), .false.)
        s = s//"  encoding: "//print_coding_system_briefly(trim(default_process_coding_system(2)), .false.)
        s = s//LF//"Priority order for recognizing coding systems when reading files:"//LF
        call coding_system_priority_list(prio)

        do k = 1, size(prio)
            s = s//"  "//itoa(int(k, int64))//". "//prio(k)%name//" "
            call coding_system_aliases(prio(k)%name, aliases)

            if (size(aliases) > 0) then
                if (aliases(1)%name == prio(k)%name) then
                    if (size(aliases) > 1) then
                        aliases_text = "(alias:"

                        do j = 2, size(aliases)
                            aliases_text = aliases_text//" "//aliases(j)%name
                        end do

                        s = s//aliases_text//")"
                    end if
                else
                    s = s//"(alias of "//aliases(1)%name//")"
                end if
            end if

            s = s//LF
        end do

        s = s//LF//"  Other coding systems cannot be distinguished automatically"//LF// &
            "  from these, and therefore cannot be recognized automatically"//LF// &
            "  with the present coding system priorities."//LF//LF
        s = s//"Particular coding systems specified for certain file names:"//LF//LF
        s = s//"  OPERATION"//TAB//"TARGET PATTERN"//TAB//TAB//"CODING SYSTEM(s)"//LF
        s = s//"  ---------"//TAB//"--------------"//TAB//TAB//"----------------"//LF
        s = s//"  File I/O"

        do k = 1, size(auto_codings)
            s = s//table_row(fortran_string(auto_codings(k)%pattern), auto_codings(k)%coding)
        end do

        do k = 1, size(file_coding_systems)
            if (file_coding_systems(k)%decoding == file_coding_systems(k)%encoding) then
                pair = file_coding_systems(k)%decoding
            else if (len(file_coding_systems(k)%encoding) == 0) then
                pair = "("//file_coding_systems(k)%decoding//" . nil)"
            else
                pair = "("//file_coding_systems(k)%decoding//" . "//file_coding_systems(k)%encoding//")"
            end if

            s = s//table_row(fortran_string(file_coding_systems(k)%pattern), pair)
        end do

        s = s//"  Process I/O"//TAB//"nothing specified"//LF
        s = s//"  Network I/O"//TAB//"nothing specified"//LF
        call with_help_window(s, status)

    contains

        !> A row: the pattern from column 16 and the coding systems from
        !> column 40 (a pattern reaching column 40 gets a line of its own).
        function table_row(pattern, codings) result(row)
            character(len=*), intent(in) :: pattern, codings
            character(len=:), allocatable :: row
            integer :: col

            col = current_col()
            row = indent_to(col, 16)//pattern
            col = max(col, 16) + len(pattern)

            if (col >= 40) then
                row = row//LF
                col = 0
            end if

            row = row//indent_to(col, 40)//codings//LF
        end function table_row

        !> The column at the end of S.
        integer function current_col()
            integer :: nl, i

            nl = index(s, LF, back=.true.)
            current_col = 0

            do i = nl + 1, len(s)
                if (s(i:i) == TAB) then
                    current_col = (current_col/8 + 1)*8
                else
                    current_col = current_col + 1
                end if
            end do
        end function current_col

        !> Tabs and spaces from column FROM to column TO (indent-to with
        !> `indent_tabs_mode' true), or "" if FROM is beyond TO.
        function indent_to(from, to) result(ws)
            integer, intent(in) :: from, to
            character(len=:), allocatable :: ws
            integer :: col

            ws = ""
            col = from

            do while ((col/8 + 1)*8 <= to)
                ws = ws//TAB
                col = (col/8 + 1)*8
            end do

            if (col < to) ws = ws//repeat(" ", to - col)
        end function indent_to
    end subroutine describe_current_coding_system

    recursive subroutine cmd_describe_current_coding_system(status)
        type(status_type), intent(out) :: status

        call describe_current_coding_system(status)
    end subroutine cmd_describe_current_coding_system

    ! ------------------------------------------------------------------
    ! Language environments
    ! ------------------------------------------------------------------

    ! Define the language environments of the language files once.
    ! Not a port.
    subroutine init_language_environments()
        use coding_mod, only: define_language_environments

        if (allocated(language_environments)) return
        allocate (language_environments(128))
        if (.not. allocated(current_language_environment%value)) current_language_environment%value = "English"
        if (.not. allocated(default_input_method%value)) default_input_method%value = ""
        defining_language_environments = .true.
        call define_language_environments()
        defining_language_environments = .false.
    end subroutine init_language_environments

    !> The index in `language_environments' of the language environment
    !> NAME (case-insensitive, as assoc-string with CASE-FOLD), 0 if there
    !> is none; its definition is `language_environments(id)', and the
    !> information under each key its components.
    !> Port of mule-cmds.el:get-language-info.
    integer function find_language_environment(name) result(id)
        character(len=*), intent(in) :: name

        call init_language_environments()

        do id = 1, language_environment_count
            if (lower(language_environments(id)%name) == lower(name)) return
        end do

        id = 0
    end function find_language_environment

    !> Store the given information as the definition of language
    !> environment NAME (the keys given replace those it had); see
    !> `language_environment_type' for the meanings of the keys.
    !> HAS_DOCUMENTATION without DOCUMENTATION gives it a documentation
    !> key with no text.  Port of mule-cmds.el:set-language-info-alist (the menus are Not applicable).
    subroutine define_language_environment(name, documentation, has_documentation, charsets, sample_text, &
                                           coding_systems, coding_system_order, nonascii_translation, input_method, &
                                           unibyte_display, tutorial, iso639_language, setup_function, exit_function)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: documentation, sample_text, nonascii_translation, input_method
        character(len=*), intent(in), optional :: unibyte_display, tutorial, iso639_language
        logical, intent(in), optional :: has_documentation
        character(len=*), intent(in), optional :: charsets(:), coding_systems(:), coding_system_order(:)
        procedure(language_environment_procedure), optional :: setup_function, exit_function
        type(language_environment_type), allocatable :: grown(:)
        type(status_type) :: status
        integer :: id, i

        if (.not. defining_language_environments) call init_language_environments()
        id = 0

        do i = 1, language_environment_count
            if (language_environments(i)%name == name) id = i
        end do

        if (id == 0) then
            if (language_environment_count == size(language_environments)) then
                allocate (grown(2*language_environment_count))

                do i = 1, language_environment_count
                    grown(i) = language_environments(i)
                end do

                call move_alloc(grown, language_environments)
            end if

            language_environment_count = language_environment_count + 1
            id = language_environment_count
            language_environments(id)%name = name
        end if

        associate (environment => language_environments(id))
            if (present(documentation)) environment%documentation = documentation
            if (present(documentation)) environment%has_documentation = .true.
            if (present(has_documentation)) environment%has_documentation = has_documentation
            if (present(charsets)) environment%charsets = charsets
            if (present(sample_text)) environment%sample_text = sample_text
            if (present(coding_systems)) environment%coding_systems = coding_systems
            if (present(coding_system_order)) environment%coding_system_order = coding_system_order
            if (present(nonascii_translation)) environment%nonascii_translation = nonascii_translation
            if (present(input_method)) environment%input_method = input_method
            if (present(unibyte_display)) environment%unibyte_display = unibyte_display
            if (present(tutorial)) environment%tutorial = tutorial
            if (present(iso639_language)) environment%iso639_language = iso639_language
            if (present(setup_function)) environment%setup_function => setup_function
            if (present(exit_function)) environment%exit_function => exit_function
        end associate

        ! The language files are loaded with the current language
        ! environment already set up (its state is GNU Emacs's at
        ! startup).
        if (.not. defining_language_environments) then
            if (name == current_language_environment%value) call set_language_environment(name, status)
        end if
    end subroutine define_language_environment

    ! The names of the language environments, the latest defined first;
    ! with KEY (a LANGUAGE_INFO_* constant) only those having information
    ! for it.  Not a port.
    function language_names(key) result(names)
        integer, intent(in), optional :: key
        character(len=40), allocatable :: names(:)
        integer :: id, n

        call init_language_environments()
        allocate (names(language_environment_count))
        n = 0

        do id = language_environment_count, 1, -1
            if (present(key)) then
                if (.not. has_key(language_environments(id), key)) cycle
            end if

            n = n + 1
            names(n) = language_environments(id)%name
        end do

        names = names(1:n)
    end function language_names

    ! Whether ENVIRONMENT has information for KEY (a LANGUAGE_INFO_*
    ! constant).  Port of mule-cmds.el:get-language-info (a key's
    ! presence).
    logical function has_key(environment, key)
        type(language_environment_type), intent(in) :: environment
        integer, intent(in) :: key

        select case (key)
        case (LANGUAGE_INFO_DOCUMENTATION)
            has_key = environment%has_documentation
        case (LANGUAGE_INFO_CHARSET)
            has_key = allocated(environment%charsets)
        case (LANGUAGE_INFO_SAMPLE_TEXT)
            has_key = allocated(environment%sample_text)
        case (LANGUAGE_INFO_CODING_SYSTEM)
            has_key = allocated(environment%coding_systems)
        case (LANGUAGE_INFO_CODING_PRIORITY)
            has_key = allocated(environment%coding_system_order)
        case (LANGUAGE_INFO_INPUT_METHOD)
            has_key = allocated(environment%input_method)
        case (LANGUAGE_INFO_TUTORIAL)
            has_key = allocated(environment%tutorial)
        case default
            has_key = .false.
        end select
    end function has_key

    !> Read a language environment name which has information for KEY (a
    !> `LANGUAGE_INFO_*' constant; any language environment when KEY is
    !> absent).  Prompt with PROMPT.  DEFAULT is the default choice of
    !> language environment.  Return "" for an empty answer.
    !> Port of mule-cmds.el:read-language-name.
    recursive subroutine read_language_name(prompt, default, name, status, key)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: key
        character(len=40), allocatable :: names(:)
        type(value_type), allocatable :: items(:)
        logical :: saved_ignore_case
        integer :: i, id

        names = language_names(key)
        allocate (items(size(names)))

        do i = 1, size(names)
            items(i) = value_type(text=trim(names(i)))
        end do

        saved_ignore_case = completion_ignore_case%value
        completion_ignore_case%value = .true.
        call completing_read(prompt, completion_list_table(make_list(items)), name, status, &
                             require_match=REQUIRE_MATCH_YES, default=default)
        completion_ignore_case%value = saved_ignore_case

        if (.not. status%ok()) then
            name = ""
            return
        end if

        if (present(key)) then
            if (len(name) > 0) then
                id = find_language_environment(name)

                if (id == 0) then
                    name = ""
                else if (.not. has_key(language_environments(id), key)) then
                    name = ""
                end if
            end if
        end if
    end subroutine read_language_name

    !> Reset multilingual environment of Emacs to the default status.
    !>
    !> The default status is as follows:
    !>
    !>   The default value of `buffer_file_coding_system' is "".
    !>   The default coding system for process I/O is "".
    !>   The default value for the command `set_terminal_coding_system' is "".
    !>   The default value for the command `set_keyboard_coding_system' is "".
    !>
    !>   The order of priorities of coding systems are as follows:
    !>         utf-8
    !>         iso-2022-7bit
    !>         iso-latin-1
    !>         iso-2022-7bit-lock
    !>         iso-2022-8bit-ss2
    !>         emacs-mule
    !>         raw-text
    !>
    !> Port of mule-cmds.el:reset-language-environment (`default_sendmail_coding_system' and
    !> `default_file_name_coding_system' wait for mail and file name
    !> encoding).
    subroutine reset_language_environment(status)
        type(status_type), intent(out) :: status

        call set_coding_system_priority([character(len=24) :: "utf-8", "iso-2022-7bit", "iso-latin-1", &
                                         "iso-2022-7bit-lock", "iso-2022-8bit-ss2", "emacs-mule", "raw-text"], status)
        if (.not. status%ok()) return
        call set_default_coding_systems("", status)
        if (.not. status%ok()) return
        ! Preserve eol-type from existing default-process-coding-systems.
        default_process_coding_system(1) = coding_system_change_text_conversion(trim(default_process_coding_system(1)), &
                                                                                "undecided")
        default_process_coding_system(2) = coding_system_change_text_conversion(trim(default_process_coding_system(2)), &
                                                                                "utf-8")
        ! Put the highest priority to the charset iso-8859-1 to prefer the
        ! registry iso8859-1 over iso8859-2 in font selection.
        call set_charset_priority([character(len=CHARSET_NAME_LEN) :: "iso-8859-1"], status)
    end subroutine reset_language_environment

    recursive subroutine cmd_reset_language_environment(status)
        type(status_type), intent(out) :: status

        call reset_language_environment(status)
    end subroutine cmd_reset_language_environment

    !> Set up multilingual environment for using LANGUAGE_NAME.
    !> This sets the coding system priority and the default input method
    !> and sometimes other things.  LANGUAGE_NAME should be a string
    !> which is the name of a language environment.  For example, "Latin-1"
    !> specifies the character set for the major languages of Western Europe.
    !>
    !> If there is a prior value for `current_language_environment', this
    !> executes the hook `exit_language_environment_hook'.  After setting up
    !> the new language environment, it executes `set_language_environment_hook'.
    !> Port of mule-cmds.el:set-language-environment (its `features' are Not
    !> applicable: there are no Lisp libraries to require).
    recursive subroutine set_language_environment(language_name, status)
        character(len=*), intent(in) :: language_name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        type(status_type) :: hook_status
        integer :: id, old_id

        name = language_name
        if (len(name) == 0) name = "English"
        id = find_language_environment(name)

        if (id == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Language environment not defined: """//name//"""")
            return
        end if

        name = language_environments(id)%name

        if (len(current_language_environment%value) > 0) then
            old_id = find_language_environment(current_language_environment%value)
            call exit_language_environment_hook%execute(hook_status)

            if (old_id /= 0) then
                if (associated(language_environments(old_id)%exit_function)) then
                    call language_environments(old_id)%exit_function(hook_status)
                end if
            end if
        end if

        call reset_language_environment(status)
        if (.not. status%ok()) return
        current_language_environment%value = name
        call set_language_environment_coding_systems(id, status)
        if (.not. status%ok()) return

        ! set-language-environment-input-method.
        if (allocated(language_environments(id)%input_method)) then
            default_input_method%value = language_environments(id)%input_method
        end if

        ! set-language-environment-nonascii-translation has no effect now.
        call set_language_environment_charset(id, status)
        if (.not. status%ok()) return

        if (associated(language_environments(id)%setup_function)) then
            call language_environments(id)%setup_function(hook_status)
        end if

        if (allocated(language_environments(id)%iso639_language)) then
            current_iso639_language = language_environments(id)%iso639_language
        end if

        call set_language_environment_hook%execute(hook_status)
    end subroutine set_language_environment

    ! Do various coding system setups for language environment ID.
    ! Port of mule-cmds.el:set-language-environment-coding-systems.
    subroutine set_language_environment_coding_systems(id, status)
        integer, intent(in) :: id
        type(status_type), intent(out) :: status
        type(value_type) :: v
        character(len=:), allocatable :: default_coding, buffer_default
        type(status_type) :: value_status
        integer :: eol

        status = status_type()
        if (.not. allocated(language_environments(id)%coding_system_order)) return
        if (size(language_environments(id)%coding_system_order) == 0) return
        default_coding = trim(language_environments(id)%coding_system_order(1))
        call default_value(value_type(name="buffer_file_coding_system"), v, value_status)
        buffer_default = ""
        if (vtag(v) == V_STRING) buffer_default = xstring(v)
        if (len(buffer_default) == 0) buffer_default = "unix"
        eol = coding_system_eol_type(buffer_default)

        if (any(eol == [EOL_UNIX, EOL_DOS, EOL_MAC])) then
            default_coding = coding_system_change_eol_conversion(default_coding, eol)
        end if

        call set_default_coding_systems(default_coding, status)
        if (.not. status%ok()) return
        call set_coding_system_priority(language_environments(id)%coding_system_order, status)
    end subroutine set_language_environment_coding_systems

    ! Do various charset setups for language environment ID: put higher
    ! priorities to such charsets that are supported by the coding
    ! systems of higher priorities in this environment.
    ! Port of mule-cmds.el:set-language-environment-charset.
    subroutine set_language_environment_charset(id, status)
        integer, intent(in) :: id
        type(status_type), intent(out) :: status
        character(len=CHARSET_NAME_LEN), allocatable :: priority_charsets(:)
        character(len=:), allocatable :: charset_list
        integer :: i, first, last

        status = status_type()
        allocate (priority_charsets(0))

        if (allocated(language_environments(id)%charsets)) then
            priority_charsets = language_environments(id)%charsets
        end if

        if (allocated(language_environments(id)%coding_system_order)) then
            do i = 1, size(language_environments(id)%coding_system_order)
                charset_list = coding_system_charset_list(trim(language_environments(id)%coding_system_order(i)))
                ! A list of charsets (not `iso-2022' or `emacs-mule').
                if (any(charset_list == [character(len=10) :: "iso-2022", "emacs-mule", ""])) cycle
                first = 1

                do while (first <= len(charset_list))
                    last = index(charset_list(first:), " ")

                    if (last == 0) then
                        last = len(charset_list) + 1
                    else
                        last = first + last - 1
                    end if

                    priority_charsets = [character(len=CHARSET_NAME_LEN) :: priority_charsets, charset_list(first:last - 1)]
                    first = last + 1
                end do
            end do
        end if

        if (size(priority_charsets) > 0) call set_charset_priority(priority_charsets, status)
    end subroutine set_language_environment_charset

    recursive subroutine cmd_set_language_environment(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_language_name(format_prompt("Set language environment", "English"), "", name, status)
        if (.not. status%ok()) return
        call set_language_environment(name, status)
    end subroutine cmd_set_language_environment

    !> Describe how Emacs supports language environment LANGUAGE_NAME.
    !> Port of mule-cmds.el:describe-language-environment (the input
    !> methods wait for them; the help buttons wait for buttons).
    recursive subroutine describe_language_environment(language_name, status)
        character(len=*), intent(in) :: language_name
        type(status_type), intent(out) :: status
        type(coding_system_list_entry_type), allocatable :: aliases(:)
        character(len=:), allocatable :: name, text, sample
        integer :: id, i, j

        call init_language_environments()
        name = language_name
        if (len(name) == 0) name = current_language_environment%value
        id = 0
        if (len(name) > 0) id = find_language_environment(name)

        if (id /= 0) then
            if (.not. language_environments(id)%has_documentation) id = 0
        end if

        if (id == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No documentation for the specified language")
            return
        end if

        associate (environment => language_environments(id))
            text = name//" language environment"//LF//LF

            if (allocated(environment%documentation)) then
                text = text//substitute_command_keys(environment%documentation)//LF//LF
            end if

            if (allocated(environment%sample_text)) then
                ! (string-replace "\n" "\n  " string)
                sample = ""

                do i = 1, len(environment%sample_text)
                    sample = sample//environment%sample_text(i:i)
                    if (environment%sample_text(i:i) == LF) sample = sample//"  "
                end do

                text = text//"Sample text:"//LF//"  "//sample//LF//LF
            end if

            text = text//"Character sets:"//LF

            if (.not. allocated(environment%charsets)) then
                text = text//"  nothing specific to "//name//LF
            else if (size(environment%charsets) == 0) then
                text = text//"  nothing specific to "//name//LF
            else
                do i = 1, size(environment%charsets)
                    text = text//"  "//trim(environment%charsets(i))//": "// &
                           charset_description(trim(environment%charsets(i)))//LF
                end do
            end if

            text = text//LF//"Coding systems:"//LF

            if (.not. allocated(environment%coding_systems)) then
                text = text//"  nothing specific to "//name//LF
            else if (size(environment%coding_systems) == 0) then
                text = text//"  nothing specific to "//name//LF
            else
                do i = 1, size(environment%coding_systems)
                    text = text//"  "//trim(environment%coding_systems(i))//substitute_command_keys(" (`")// &
                           achar(coding_system_mnemonic(trim(environment%coding_systems(i))))// &
                           substitute_command_keys("' in mode line):"//LF//TAB)// &
                           substitute_command_keys(coding_system_doc_string(trim(environment%coding_systems(i))))//LF
                    call coding_system_aliases(trim(environment%coding_systems(i)), aliases)

                    if (size(aliases) > 0) then
                        text = text//TAB//"(alias:"

                        do j = 1, size(aliases)
                            text = text//" "//aliases(j)%name
                        end do

                        text = text//")"//LF
                    end if
                end do
            end if
        end associate

        call with_help_window(text, status)
    end subroutine describe_language_environment

    recursive subroutine cmd_describe_language_environment(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call init_language_environments()
        call read_language_name(format_prompt("Describe language environment", current_language_environment%value), &
                                current_language_environment%value, name, status, key=LANGUAGE_INFO_DOCUMENTATION)
        if (.not. status%ok()) return
        call describe_language_environment(name, status)
    end subroutine cmd_describe_language_environment

    ! ------------------------------------------------------------------
    ! Locale environments
    ! ------------------------------------------------------------------

    !> The index in ENTRIES of the first entry whose regexp matches the
    !> start of LOCALE, 0 if there is none.  Port of
    !> mule-cmds.el:locale-name-match.
    integer function find_locale_entry(locale, entries) result(id)
        character(len=*), intent(in) :: locale
        type(locale_entry_type), intent(in) :: entries(:)
        type(status_type) :: status

        do id = 1, size(entries)
            if (string_match("\`\(?:"//trim(entries(id)%regexp)//"\)", locale, status, inhibit_modify=.true.) > 0) return
        end do

        id = 0
    end function find_locale_entry

    !> Whether charset names (strings) CHARSET1 and CHARSET2 are equivalent.
    !> Matching is done ignoring case and any hyphens and underscores in the
    !> names.  E.g. `ISO_8859-1' and `iso88591' both match `iso-8859-1'.
    !> Port of mule-cmds.el:locale-charset-match-p.
    logical function is_locale_charset_match(charset1, charset2)
        character(len=*), intent(in) :: charset1, charset2

        is_locale_charset_match = lower(without_separators(charset1)) == lower(without_separators(charset2))
    end function is_locale_charset_match

    ! TEXT without its `-' and `_' characters.  Not a port.
    pure function without_separators(text) result(kept)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: kept
        integer :: i

        kept = ""

        do i = 1, len(text)
            if (scan(text(i:i), "-_") == 0) kept = kept//text(i:i)
        end do
    end function without_separators

    !> Find coding system corresponding to CHARSET.
    !> CHARSET is any sort of non-Emacs charset name, such as might be used
    !> in a locale codeset, or elsewhere.  It is matched to a coding system
    !> in the coding system list, treating case and the characters `-' and
    !> `_' as insignificant.  The coding system base is returned ("" if
    !> none).  Thus, for instance, if charset "ISO8859-2", `iso-latin-2' is
    !> returned.
    !> Port of mule-cmds.el:locale-charset-to-coding-system
    !> (`locale-charset-alist' is empty in Emacs, so it is not kept).
    function locale_charset_to_coding_system(charset) result(coding_system)
        character(len=*), intent(in) :: charset
        character(len=:), allocatable :: coding_system
        type(coding_system_list_entry_type), allocatable :: names(:)
        integer :: k

        coding_system = ""
        call coding_system_list(names)

        ! `coding-system-alist' has the latest defined first.
        do k = size(names), 1, -1
            if (is_locale_charset_match(charset, names(k)%name)) then
                coding_system = coding_system_base(names(k)%name)
                return
            end if
        end do
    end function locale_charset_to_coding_system

    !> Expand LOCALE according to `locale_translation_file_name', if possible.
    !> For example, translate "swedish" into "sv_SE.ISO8859-1".  Port of
    !> mule-cmds.el:locale-translate.
    function locale_translate(locale) result(translated)
        character(len=*), intent(in) :: locale
        character(len=:), allocatable :: translated, line
        character(len=1024) :: record
        integer :: unit, io_status, locale_length, i, first_kept
        logical :: exists

        translated = locale
        if (.not. allocated(locale_translation_file_name)) return
        if (len(locale_translation_file_name) == 0) return
        inquire (file=locale_translation_file_name, exist=exists)
        if (.not. exists) return
        open (newunit=unit, file=locale_translation_file_name, status="old", action="read", iostat=io_status)
        if (io_status /= 0) return
        locale_length = len(locale)

        do
            read (unit, "(a)", iostat=io_status) record
            if (io_status /= 0) exit
            line = trim(record)
            if (len(line) <= locale_length) cycle
            if (line(1:locale_length) /= locale) cycle
            ! ^LOCALE:?[ \t]+
            i = locale_length + 1
            if (line(i:i) == ":") i = i + 1
            if (i > len(line)) cycle
            if (scan(line(i:i), " "//TAB) == 0) cycle
            first_kept = verify(line(i:), " "//TAB)

            if (first_kept == 0) then
                translated = ""
            else
                translated = line(i + first_kept - 1:)
            end if

            exit
        end do

        close (unit)
    end function locale_translate

    !> Return a list of locale names (the output of `locale -a').
    !> Port of mule-cmds.el:get-locale-names (the locale.alias fallback
    !> for systems without `locale' is not needed on Linux).
    subroutine get_locale_names(names)
        use buffer_mod, only: get_or_make_buffer, close_buffer
        use callproc_mod, only: execute_process
        use editfns_mod, only: buffer_string
        character(len=80), allocatable, intent(out) :: names(:)
        character(len=*), parameter :: WHITESPACE = " "//TAB//LF
        character(len=:), allocatable :: text
        type(handle_type) :: buffer, old_buffer
        type(value_type) :: exit_status, arguments(1)
        type(status_type) :: status
        integer :: first, last

        allocate (names(0))
        old_buffer = current_buffer()
        call get_or_make_buffer(" *locale-names*", buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        arguments(1) = value_type(text="-a")
        call execute_process("locale", exit_status, status, destination=value_type(.true.), args=make_list(arguments))
        text = buffer_string()
        call set_buffer(old_buffer, status)
        call close_buffer(buffer, status)
        first = 1

        do while (first <= len(text))
            if (scan(text(first:first), WHITESPACE) > 0) then
                first = first + 1
                cycle
            end if

            last = scan(text(first:), WHITESPACE)

            if (last == 0) then
                last = len(text)
            else
                last = first + last - 2
            end if

            names = [character(len=80) :: names, text(first:last)]
            first = last + 1
        end do
    end subroutine get_locale_names

    !> Set up the display table and terminal coding system for
    !> LANGUAGE_NAME.  If INHIBIT_REFRESH, don't redraw the current frame.
    !> Port of mule-cmds.el:set-display-table-and-terminal-coding-system
    !> (the 8-bit display of `standard-display-european-internal' waits
    !> for display tables).
    recursive subroutine set_display_table_and_terminal_coding_system(language_name, coding_system, status, &
                                                                      inhibit_refresh)
        character(len=*), intent(in) :: language_name, coding_system
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: inhibit_refresh
        character(len=:), allocatable :: unibyte_display
        integer :: id

        unibyte_display = ""
        id = find_language_environment(language_name)

        if (id /= 0) then
            if (allocated(language_environments(id)%unibyte_display)) then
                unibyte_display = language_environments(id)%unibyte_display
            end if
        end if

        if (len(coding_system) > 0) then
            call set_terminal_coding_system(coding_system, status, inhibit_refresh)
        else
            call set_terminal_coding_system(unibyte_display, status, inhibit_refresh)
        end if
    end subroutine set_display_table_and_terminal_coding_system

    !> Set up multilingual environment for using LOCALE_NAME.
    !> This sets the language environment, the coding system priority,
    !> the default input method and sometimes other things.
    !>
    !> LOCALE_NAME should be a string which is the name of a locale supported
    !> by the system.  Often it is of the form xx_XX.CODE, where xx is a
    !> language, XX is a country, and CODE specifies a character set and
    !> coding system.  For example, the locale name "ja_JP.EUC" might name
    !> a locale for Japanese in Japan using the `japanese-iso-8bit'
    !> coding-system.  The name may also have a modifier suffix, e.g. `@euro'
    !> or `@cyrillic'.
    !>
    !> If LOCALE_NAME is "", its value is taken from the environment
    !> variables LC_ALL, LC_CTYPE and LANG (the first one that is set).
    !>
    !> The locale names supported by your system can typically be found in a
    !> directory named `/usr/share/locale' or `/usr/lib/locale'.  LOCALE_NAME
    !> will be translated according to the table specified by
    !> `locale_translation_file_name'.
    !>
    !> This function sets the `current_locale_environment' variable.
    !>
    !> By default, this function will redraw the current frame.  If
    !> INHIBIT_REFRESH is true, this isn't done.
    !>
    !> See also `locale_charset_language_names', `locale_language_names',
    !> `locale_preferred_coding_systems' and `locale_coding_system'.
    !>
    !> Port of mule-cmds.el:set-locale-environment (FRAME waits for more
    !> than one terminal; `woman-locale' and `ps-paper-type' wait for
    !> woman and ps-print; `default-sendmail-coding-system' waits for
    !> mail).
    recursive subroutine set_locale_environment(locale_name, status, inhibit_refresh)
        use env_mod, only: getenv
        use coding_mod, only: locale_coding_system
        character(len=*), intent(in) :: locale_name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: inhibit_refresh
        character(len=*), parameter :: ALIAS_FILES(5) = [character(len=40) :: "/usr/share/X11/locale/locale.alias", &
                                                                                               "/usr/lib/X11/locale/locale.alias", &
                                                                                         "/usr/X11R6/lib/X11/locale/locale.alias", &
                                                                                           "/usr/openwin/lib/locale/locale.alias", &
                                                                                                   "/usr/share/locale/locale.alias"]
        character(len=*), parameter :: VARIABLES(3) = [character(len=8) :: "LC_ALL", "LC_CTYPE", "LANG"]
        character(len=*), parameter :: LOWER_LETTERS = "abcdefghijklmnopqrstuvwxyz"
        character(len=:), allocatable :: locale, language_name, coding_system, keyboard_coding, default_coding
        type(value_type) :: default_setting
        type(status_type) :: default_status
        logical :: exists, found
        integer :: i, k, language_entry, charset_entry, preferred_entry, eol_type, id

        status = status_type()
        call init_language_environments()
        locale_translation_file_name = ""

        do i = 1, size(ALIAS_FILES)
            inquire (file=trim(ALIAS_FILES(i)), exist=exists)

            if (exists) then
                locale_translation_file_name = trim(ALIAS_FILES(i))
                exit
            end if
        end do

        locale = locale_name

        ! Use the first of these three environment variables that has a
        ! nonempty value.
        do i = 1, size(VARIABLES)
            if (len(locale) > 0) exit
            call getenv(trim(VARIABLES(i)), locale, found)
            if (.not. found) locale = ""
        end do

        if (len(locale) == 0) return
        locale = locale_translate(locale)
        current_locale_environment = locale

        ! Leave the system locales alone if the caller did not specify an
        ! explicit locale name.
        if (len(locale_name) > 0) then
            system_messages_locale = locale
            system_time_locale = locale
        end if

        if (len(locale) >= 2) then
            if (verify(locale(1:2), LOWER_LETTERS) == 0) current_iso639_language = locale(1:2)
        end if

        locale = lower(locale)
        language_entry = find_locale_entry(locale, locale_language_names)
        charset_entry = find_locale_entry(locale, locale_charset_language_names)
        call default_value(value_type(name="buffer_file_coding_system"), default_setting, default_status)
        default_coding = ""
        if (vtag(default_setting) == V_STRING) default_coding = xstring(default_setting)
        eol_type = EOL_UNDECIDED
        if (len(default_coding) > 0) eol_type = coding_system_eol_type(default_coding)
        preferred_entry = find_locale_entry(locale, locale_preferred_coding_systems)
        coding_system = ""

        if (preferred_entry /= 0) then
            coding_system = trim(locale_preferred_coding_systems(preferred_entry)%coding_system)
        else
            i = index(locale, ".")

            if (i > 0 .and. i < len(locale)) then
                k = index(locale(i + 1:), "@")

                if (k == 0) then
                    coding_system = locale_charset_to_coding_system(locale(i + 1:))
                else if (k > 1) then
                    coding_system = locale_charset_to_coding_system(locale(i + 1:i + k - 1))
                end if
            end if
        end if

        language_name = ""

        if (language_entry /= 0) then
            language_name = trim(locale_language_names(language_entry)%language)
            ! What locale_preferred_coding_systems specifies has higher
            ! priority.
            if (len(coding_system) == 0) coding_system = trim(locale_language_names(language_entry)%coding_system)
        else if (charset_entry /= 0) then
            ! Otherwise use what locale_charset_language_names lists.
            language_name = trim(locale_charset_language_names(charset_entry)%language)
        end if

        ! If a specific EOL conversion was specified in the default
        ! buffer-file-coding-system, preserve it in the coding system we
        ! will be using from now on.
        if (any(eol_type == [EOL_UNIX, EOL_DOS, EOL_MAC]) .and. len(coding_system) > 0) then
            if (is_coding_system(coding_system)) coding_system = coding_system_change_eol_conversion(coding_system, eol_type)
        end if

        if (len(language_name) > 0) then
            ! Set up for this character set.
            call set_language_environment(language_name, status)
            if (.not. status%ok()) return
            call set_display_table_and_terminal_coding_system(language_name, coding_system, status, inhibit_refresh)
            if (.not. status%ok()) return
            ! Set the keyboard coding system if appropriate.
            keyboard_coding = coding_system
            id = find_language_environment(language_name)

            if (len(keyboard_coding) == 0) then
                if (allocated(language_environments(id)%coding_systems)) then
                    if (size(language_environments(id)%coding_systems) > 0) then
                        keyboard_coding = trim(language_environments(id)%coding_systems(1))
                    end if
                end if
            end if

            if (len(keyboard_coding) > 0) then
                call set_keyboard_coding_system(keyboard_coding, status)
                if (.not. status%ok()) return
            end if

            locale_coding_system = ""

            if (allocated(language_environments(id)%coding_system_order)) then
                if (size(language_environments(id)%coding_system_order) > 0) then
                    locale_coding_system = trim(language_environments(id)%coding_system_order(1))
                end if
            end if
        end if

        if (len(coding_system) > 0) then
            found = .false.
            if (len(locale_coding_system%value) > 0) found = coding_system_equal(coding_system, locale_coding_system%value)

            if (.not. found) then
                call prefer_coding_system(coding_system, status)
                if (.not. status%ok()) return
                locale_coding_system = coding_system
            end if
        end if
    end subroutine set_locale_environment

    recursive subroutine cmd_set_locale_environment(status)
        type(status_type), intent(out) :: status
        character(len=80), allocatable :: names(:)
        type(value_type), allocatable :: items(:)
        character(len=:), allocatable :: name
        integer :: i

        call get_locale_names(names)
        allocate (items(size(names)))

        do i = 1, size(names)
            items(i) = value_type(text=trim(names(i)))
        end do

        call completing_read("Set environment for locale: ", completion_list_table(make_list(items)), name, status)
        if (.not. status%ok()) return
        call set_locale_environment(name, status)
    end subroutine cmd_set_locale_environment

    ! ------------------------------------------------------------------
    ! Charsets (mule-diag.el)
    ! ------------------------------------------------------------------

    !> Display a list of all character sets.
    !>
    !> The D column contains the dimension of this character set.  The CH
    !> column contains the number of characters in a block of this character
    !> set.  The FINAL-BYTE column contains an ISO-2022 <final-byte> to use
    !> in the designation escape sequence for this character set in
    !> ISO-2022-based coding systems.
    !>
    !> With prefix ARG, the output format gets more cryptic,
    !> but still shows the full information.
    !>
    !> Port of mule-diag.el:list-character-sets (its column titles and
    !> charset names are not buttons: buttons wait for button.el).
    subroutine list_character_sets(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text

        if (arg) then
            text = list_character_sets_2()
        else
            text = "Supplementary character sets are shown below."//LF// &
                   substitute_command_keys("Use \[help_follow]:"//LF)// &
                   "  on a column title to sort by that title,"//TAB//"+----DIMENSION"//LF// &
                   "  on a charset name to list characters."//TAB//TAB//"| +--CHARS"//LF// &
                   "CHARSET-NAME"//repeat(TAB, 5)//"D CH  FINAL-BYTE"//LF// &
                   "------------"//repeat(TAB, 5)//"- --- ----------"//LF// &
                   list_character_sets_1("name")
        end if

        call with_output_to_temp_buffer("*Character Set List*", text, status)
    end subroutine list_character_sets

    recursive subroutine cmd_list_character_sets(status)
        type(status_type), intent(out) :: status

        call list_character_sets(current_prefix_arg%kind /= PREFIX_NONE, status)
    end subroutine cmd_list_character_sets

    !> The list of character sets sorted by SORT_KEY ("name" or
    !> "iso-spec"), the supplementary ones after the others.
    !> Port of mule-diag.el:list-character-sets-1.
    function list_character_sets_1(sort_key) result(text)
        character(len=*), intent(in) :: sort_key
        character(len=:), allocatable :: text
        character(len=CHARSET_NAME_LEN), allocatable :: names(:)
        integer, allocatable :: main_entries(:), supplementary_entries(:), dimensions(:), chars(:), final_chars(:)
        type(status_type) :: st
        integer :: i, id

        names = charset_list()
        allocate (dimensions(size(names)), chars(size(names)), final_chars(size(names)))
        allocate (main_entries(0), supplementary_entries(0))

        ! Pushed onto the lists: the earliest defined first.
        do i = size(names), 1, -1
            id = find_charset(trim(names(i)))
            dimensions(i) = charsets(id)%dimension
            chars(i) = charset_chars(trim(names(i)), st)
            final_chars(i) = charsets(id)%iso_final

            if (charsets(id)%supplementary .and. .not. charsets(id)%internal) then
                supplementary_entries = [supplementary_entries, i]
            else
                main_entries = [main_entries, i]
            end if
        end do

        call sort_entries(main_entries)
        call sort_entries(supplementary_entries)
        text = ""

        do i = 1, size(main_entries)
            text = text//entry_line(main_entries(i))
        end do

        text = text//LF//"-------------- Supplementary Character Sets --------------"//LF// &
               "Character sets for defining other charsets, or for backward compatibility"//LF//LF

        do i = 1, size(supplementary_entries)
            text = text//entry_line(supplementary_entries(i))
        end do

    contains

        ! A stable insertion sort of ENTRIES (indexes of `names') by the
        ! sort key.
        subroutine sort_entries(entries)
            integer, intent(inout) :: entries(:)
            integer :: j, k, entry

            do j = 2, size(entries)
                entry = entries(j)
                k = j - 1

                do while (k >= 1)
                    if (.not. is_before(entry, entries(k))) exit
                    entries(k + 1) = entries(k)
                    k = k - 1
                end do

                entries(k + 1) = entry
            end do
        end subroutine sort_entries

        ! Whether entry X sorts before entry Y.
        logical function is_before(x, y)
            integer, intent(in) :: x, y

            if (sort_key == "iso-spec") then
                ! By DIMENSION, CHARS and FINAL-CHAR.
                if (dimensions(x) /= dimensions(y)) then
                    is_before = dimensions(x) < dimensions(y)
                else if (chars(x) /= chars(y)) then
                    is_before = chars(x) < chars(y)
                else
                    is_before = final_chars(x) < final_chars(y)
                end if
            else
                is_before = llt(trim(names(x)), trim(names(y)))
            end if
        end function is_before

        ! The name of entry K, a tab and tabs to column 48, the dimension,
        ! the chars and the final char.
        function entry_line(k) result(line)
            integer, intent(in) :: k
            character(len=:), allocatable :: line
            character(len=16) :: buffer
            integer :: column

            column = (len_trim(names(k))/8 + 1)*8
            line = trim(names(k))//TAB//repeat(TAB, max(0, (48 - column + 7)/8))
            write (buffer, "(i0, 1x, i3, 1x)") dimensions(k), chars(k)
            line = line//trim(buffer)//" "

            if (final_chars(k) < 0) then
                line = line//"none"
            else
                line = line//achar(final_chars(k))
            end if

            line = line//LF
        end function entry_line
    end function list_character_sets_1

    ! All character sets in a form that a program can easily parse.
    ! Port of mule-diag.el:list-character-sets-2.
    function list_character_sets_2() result(text)
        character(len=:), allocatable :: text
        character(len=CHARSET_NAME_LEN), allocatable :: names(:)
        type(status_type) :: st
        character(len=32) :: buffer
        integer :: i, id

        text = "#########################"//LF//"## LIST OF CHARSETS"//LF// &
               "## Each line corresponds to one charset."//LF// &
               "## The following attributes are listed in this order"//LF// &
               "## separated by a colon `:' in one line."//LF// &
               "##"//TAB//"CHARSET-NAME,"//LF// &
               "##"//TAB//"DIMENSION (1-4)"//LF// &
               "##"//TAB//"CHARS (number of characters in first dimension of charset)"//LF// &
               "##"//TAB//"ISO-FINAL-CHAR (character code of ISO-2022's final character)"//LF// &
               "##"//TAB//TAB//"-1 means that no final character is assigned."//LF// &
               "##"//TAB//"DESCRIPTION (describing string of the charset)"//LF
        names = charset_list()

        do i = 1, size(names)
            id = find_charset(trim(names(i)))
            write (buffer, "(i0, ':', i0, ':', i0)") charsets(id)%dimension, charset_chars(trim(names(i)), st), &
                charsets(id)%iso_final
            text = text//trim(names(i))//":"//trim(buffer)//":"//charsets(id)%docstring//LF
        end do
    end function list_character_sets_2

    !> Read a character set from the minibuffer, prompting with string PROMPT.
    !> It must be an Emacs character set listed in `charset_list'.
    !> Return the charset name ("" for empty input).  DEFAULT_VALUE is the
    !> default value.
    !> Port of mule-diag.el:read-charset.
    recursive subroutine read_charset(prompt, charset, status, default_value, initial_input)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: charset
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: default_value, initial_input
        character(len=CHARSET_NAME_LEN), allocatable :: names(:)
        type(value_type), allocatable :: items(:)
        integer :: i

        names = charset_list()
        allocate (items(size(names)))

        do i = 1, size(names)
            items(i) = value_type(text=trim(names(i)))
        end do

        call completing_read(prompt, completion_list_table(make_list(items)), charset, status, &
                             require_match=REQUIRE_MATCH_YES, initial=initial_input, history="charset_history", &
                             default=default_value)
    end subroutine read_charset

    ! The characters of CHARSET with code points ROW*256+MIN_CODE to
    ! ROW*256+MAX_CODE (ROW is the first byte of a 2-dimensional charset, 0
    ! for a 1-dimensional one).
    ! Port of mule-diag.el:list-block-of-chars.
    function list_block_of_chars(charset, row, min_code, max_code) result(text)
        use keymap_mod, only: single_key_description
        use character_mod, only: char_string, MAX_MULTIBYTE_LENGTH
        character(len=*), intent(in) :: charset
        integer, intent(in) :: row, min_code, max_code
        character(len=:), allocatable :: text
        character(len=MAX_MULTIBYTE_LENGTH) :: buffer
        character(len=8) :: hex
        type(status_type) :: st
        integer :: i, c, nbytes

        text = repeat("-", 7 + 4*16)//LF//"     "

        do i = 0, 15
            write (hex, "(z4)") i
            text = text//hex(1:4)
        end do

        i = (min_code/16)*16

        do while (i <= max_code)
            if (mod(i, 16) == 0) then
                write (hex, "(z6)") (row*256 + i)/16
                text = text//LF//hex(1:6)//"x"
            end if

            if (i < min_code) then
                c = 32
            else
                c = decode_char(charset, int(row*256 + i, int64), st)
                ! A gap in the mapping.
                if (c < 0) c = 32
            end if

            text = text//TAB

            ! Don't insert control codes, non-Unicode characters.
            if (c < 32 .or. c == 127) then
                text = text//single_key_description(value_type(int(c, int64)))
            else if (c >= 128 .and. c < 160) then
                write (hex, "(z2.2)") c
                text = text//hex(1:2)//"h"
            else
                if (c > int(z'10FFFF')) c = 32
                call char_string(c, buffer, nbytes, st)
                text = text//buffer(1:nbytes)
            end if

            i = i + 1
        end do

        text = text//LF
    end function list_block_of_chars

    !> Display a list of characters in character set CHARSET.
    !> Port of mule-diag.el:list-charset-chars.
    recursive subroutine list_charset_chars(charset, status)
        character(len=*), intent(in) :: charset
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        integer :: i, id

        id = find_charset(charset)

        if (id == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid character set: "//charset)
            return
        end if

        associate (cs => charsets(id))
            if (cs%dimension > 2) then
                call set_error(status, FM_GENERIC_ERROR, "Can only list 1- and 2-dimensional charsets")
                return
            end if

            text = "Characters in the coded character set "//charset//"."//LF

            if (cs%dimension == 1) then
                text = text//list_block_of_chars(charset, 0, cs%given_code_space(1), cs%given_code_space(2))
            else
                do i = cs%given_code_space(3), cs%given_code_space(4)
                    text = text//list_block_of_chars(charset, i, cs%given_code_space(1), cs%given_code_space(2))
                end do
            end if
        end associate

        listed_charset = charset
        call with_output_to_temp_buffer("*Character List*", text, status, list_charset_chars_body)
    end subroutine list_charset_chars

    ! The rest of list-charset-chars's body, in *Character List*: its coding
    ! system (when the charset is also a coding system), the charset in
    ! the mode line, a tab width of 4 and the `charset' text property.
    ! Not a port: the body that list_charset_chars gives
    ! with_output_to_temp_buffer (the rest of upstream's body).
    recursive subroutine list_charset_chars_body(status)
        use xdisp_mod, only: tab_width
        use textprop_mod, only: put_text_property
        use editfns_mod, only: point_min, point_max
        type(status_type), intent(out) :: status
        type(header_line_format_type) :: id

        status = status_type()

        if (is_coding_system(listed_charset)) then
            call set_buffer_file_coding_system(listed_charset, .false., .false., .false., status)
            if (.not. status%ok()) return
        end if

        call id%append("%12b", "mode_line_buffer_id")
        call id%append(" ("//listed_charset//")")
        buffers(current)%mode_line_buffer_identification = id
        call tab_width%set_local(4_int64)
        call put_text_property(point_min(), point_max(), value_type(name="charset"), value_type(name=listed_charset), &
                               status)
    end subroutine list_charset_chars_body

    recursive subroutine cmd_list_charset_chars(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: charset

        call read_charset("Character set: ", charset, status)
        if (.not. status%ok()) return
        call list_charset_chars(charset, status)
    end subroutine cmd_list_charset_chars

    !> Display information about built-in character set CHARSET.
    !> Port of mule-diag.el:describe-character-set (`:parents' is never set).
    recursive subroutine describe_character_set(charset, status)
        use character_mod, only: char_string, MAX_MULTIBYTE_LENGTH
        character(len=*), intent(in) :: charset
        type(status_type), intent(out) :: status
        character(len=CHARSET_NAME_LEN), allocatable :: names(:)
        character(len=MAX_MULTIBYTE_LENGTH) :: buffer
        type(status_type) :: st
        character(len=:), allocatable :: text, aliases
        character(len=24) :: number
        integer :: i, id, nbytes

        id = find_charset(charset)

        if (id == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid charset: "//charset)
            return
        end if

        associate (cs => charsets(id))
            text = "Character set: "//charset
            if (cs%name /= charset) text = text//" (alias of "//cs%name//")"
            text = text//LF//LF//cs%docstring//LF//LF//"Number of contained characters: "

            do i = 1, cs%dimension
                if (i > 1) text = text//"x"
                write (number, "(i0)") charset_chars(charset, st, i)
                text = text//trim(number)
            end do

            text = text//LF

            if (cs%iso_final > 0) then
                text = text//"Final char of ISO2022 designation sequence: "// &
                       format_message("`"//achar(cs%iso_final)//"'")//LF
            end if

            aliases = ""
            names = charset_list()

            ! Pushed onto the list: the earliest defined first.
            do i = size(names), 1, -1
                if (trim(names(i)) == charset) cycle

                if (charsets(find_charset(trim(names(i))))%name == charset) then
                    if (len(aliases) > 0) aliases = aliases//", "
                    aliases = aliases//trim(names(i))
                end if
            end do

            if (len(aliases) > 0) text = text//"Aliases: "//aliases//LF
            if (cs%ascii_compatible) text = text//"ASCII compatible."//LF
            if (len(cs%map) > 0) text = text//"Map file: "//cs%map//LF
            if (len(cs%unify_map) > 0) text = text//"Unification map file: "//cs%unify_map//LF

            if (cs%has_invalid_code) then
                call char_string(int(cs%invalid_code, int32), buffer, nbytes, st)
                write (number, "(i0)") cs%invalid_code
                text = text//"Invalid character: "//buffer(1:nbytes)//" (code "//trim(number)//")"//LF
            end if

            if (cs%emacs_mule_id >= 0) then
                write (number, "(i0)") cs%emacs_mule_id
                text = text//"Id in emacs-mule coding system: "//trim(number)//LF
            end if

            text = text//"Code space: ["

            do i = 1, cs%code_space_count
                write (number, "(i0)") cs%given_code_space(i)
                if (i > 1) text = text//" "
                text = text//trim(number)
            end do

            text = text//"]"//LF

            if (cs%has_code_offset) then
                write (number, "(i0)") cs%code_offset
                text = text//"Code offset: "//trim(number)//LF
            end if

            if (cs%iso_revision >= 0 .and. .not. cs%internal) then
                write (number, "(i0)") cs%iso_revision
                text = text//"ISO revision number: "//trim(number)//LF
            end if

            if (cs%supplementary .and. .not. cs%internal) then
                text = text//"Used only as a parent or a subset of some other charset,"//LF// &
                       "or provided just for backward compatibility."//LF
            end if
        end associate

        call with_help_window(text, status)
    end subroutine describe_character_set

    recursive subroutine cmd_describe_character_set(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: charset

        call read_charset("Charset: ", charset, status)
        if (.not. status%ok()) return
        call describe_character_set(charset, status)
    end subroutine cmd_describe_character_set

    ! ------------------------------------------------------------------
    ! Input methods
    ! ------------------------------------------------------------------

    !> Register INPUT_METHOD as an input method for language environment
    !> LANGUAGE_ENVIRONMENT.
    !>
    !> ACTIVATE_FUNCTION is a procedure to call to activate this method.
    !> TITLE is a string to show in the mode line when this method is active.
    !> DESCRIPTION is a string describing this method and what it is good for.
    !> The argument to ACTIVATE_FUNCTION is INPUT_METHOD.
    !>
    !> In case you want to register a new Quail input method by yourself, be
    !> careful to use the same input method title as given in the third
    !> parameter of `quail_define_package'.  (If the values are different, the
    !> string specified in this procedure takes precedence.)
    !>
    !> The commands `describe_input_method' and `list_input_methods' need
    !> these duplicated values to show some information about input methods
    !> without loading the relevant Quail packages.
    !> Port of mule-cmds.el:register-input-method (no ARGS: a Quail package is built in, not loaded from a file).
    subroutine register_input_method(input_method, language_environment, activate_function, title, description)
        character(len=*), intent(in) :: input_method, language_environment, title, description
        procedure(input_method_activate_procedure) :: activate_function
        type(input_method_type), allocatable :: grown(:)
        integer :: id, i

        if (.not. allocated(input_methods)) allocate (input_methods(0))
        id = find_input_method(input_method)

        if (id == 0) then
            ! A new input method goes first.
            allocate (grown(size(input_methods) + 1))

            do i = 1, size(input_methods)
                grown(i + 1) = input_methods(i)
            end do

            call move_alloc(grown, input_methods)
            id = 1
            input_methods(id)%name = input_method
        end if

        input_methods(id)%language_environment = language_environment
        input_methods(id)%activate_function => activate_function
        input_methods(id)%title = title
        input_methods(id)%description = description
    end subroutine register_input_method

    !> The index of input method INPUT_METHOD in `input_methods', 0 if it
    !> is not registered.
    !> Port of mule-cmds.el:activate-input-method (its lookup of input-method-alist).
    integer function find_input_method(input_method) result(id)
        character(len=*), intent(in) :: input_method

        id = 0
        if (allocated(input_methods)) id = findloc(input_methods%name, input_method, dim=1)
    end function find_input_method

    !> Read a name of input method from a minibuffer prompting with PROMPT.
    !> If DEFAULT is not empty, use that as the default.
    !> If INHIBIT_NULL is true, null input is an error.
    !>
    !> The result is a string ("" for null input).
    !> Port of mule-cmds.el:read-input-method-name (PROMPT comes with DEFAULT in it; not `input_method_history').
    recursive subroutine read_input_method_name(prompt, default, inhibit_null, input_method, status)
        character(len=*), intent(in) :: prompt, default
        logical, intent(in) :: inhibit_null
        character(len=:), allocatable, intent(out) :: input_method
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: items(:)
        logical :: saved_ignore_case
        integer :: i

        if (.not. allocated(input_methods)) allocate (input_methods(0))
        allocate (items(size(input_methods)))

        do i = 1, size(input_methods)
            items(i) = value_type(text=trim(input_methods(i)%name))
        end do

        saved_ignore_case = completion_ignore_case%value
        completion_ignore_case%value = .true.
        call completing_read(prompt, completion_list_table(make_list(items)), input_method, status, &
                             require_match=REQUIRE_MATCH_YES, default=default)
        completion_ignore_case%value = saved_ignore_case

        if (.not. status%ok()) then
            input_method = ""
            return
        end if

        if (len(input_method) == 0 .and. inhibit_null) then
            call set_error(status, FM_GENERIC_ERROR, "No valid input method is specified")
        end if
    end subroutine read_input_method_name

    !> Switch to input method INPUT_METHOD for the current buffer.
    !> If some other input method is already active, turn it off first.
    !> If INPUT_METHOD is empty, deactivate any current input method.
    !> Port of mule-cmds.el:activate-input-method.
    recursive subroutine activate_input_method(input_method, status)
        character(len=*), intent(in) :: input_method
        type(status_type), intent(out) :: status
        type(status_type) :: hook_status
        integer :: id

        if (allocated(buffers(current)%current_input_method)) then
            if (buffers(current)%current_input_method /= input_method) then
                call deactivate_input_method(status)
                if (.not. status%ok()) return
            end if
        end if

        if (allocated(buffers(current)%current_input_method) .or. len(input_method) == 0) return
        id = find_input_method(input_method)

        if (id == 0) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Can't activate input method `"//input_method//"'"))
            return
        end if

        if (.not. associated(input_methods(id)%activate_function)) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Can't activate input method `"//input_method//"'"))
            return
        end if

        if (allocated(buffers(current)%current_input_method_title)) deallocate (buffers(current)%current_input_method_title)
        call input_methods(id)%activate_function(input_method, status)
        if (.not. status%ok()) return
        buffers(current)%current_input_method = input_method

        if (.not. allocated(buffers(current)%current_input_method_title)) then
            buffers(current)%current_input_method_title = trim(input_methods(id)%title)
        end if

        call input_method_activate_hook%execute(hook_status)
    end subroutine activate_input_method

    !> Turn off the current input method.
    !> Port of mule-cmds.el:deactivate-input-method.
    recursive subroutine deactivate_input_method(status)
        type(status_type), intent(out) :: status
        procedure(current_input_method_procedure), pointer :: deactivate
        type(status_type) :: hook_status

        if (.not. allocated(buffers(current)%current_input_method)) return

        if (.not. allocated(buffers(current)%current_transient_input_method)) then
            call buffers(current)%input_method_history%append(buffers(current)%current_input_method)
        end if

        buffers(current)%input_method_function => null()
        if (allocated(buffers(current)%current_input_method_title)) deallocate (buffers(current)%current_input_method_title)
        deactivate => buffers(current)%deactivate_current_input_method_function
        if (associated(deactivate)) call deactivate(status)
        call input_method_deactivate_hook%execute(hook_status)
        deallocate (buffers(current)%current_input_method)
    end subroutine deactivate_input_method

    !> The input method a prompt offers by default: the most recent one in
    !> the current buffer's `input_method_history', else
    !> `default_input_method' ("" for none).
    !> Port of mule-cmds.el:set-input-method (its default).
    function input_method_prompt_default() result(default)
        character(len=:), allocatable :: default

        default = ""

        if (allocated(buffers(current)%input_method_history%items)) then
            if (size(buffers(current)%input_method_history%items) > 0) then
                default = buffers(current)%input_method_history%items(1)%text
                return
            end if
        end if

        if (allocated(default_input_method%value)) default = default_input_method%value
    end function input_method_prompt_default

    !> Select and activate input method INPUT_METHOD for the current buffer.
    !> This also sets the default input method to the one you specify.
    !> If INPUT_METHOD is empty, this procedure turns off the input method,
    !> and also causes you to be prompted for a name of an input method the
    !> next time you invoke \[toggle_input_method].
    !>
    !> To deactivate the input method interactively, use
    !> \[toggle_input_method].  To deactivate it programmatically, use
    !> `deactivate_input_method'.
    !> Port of mule-cmds.el:set-input-method (Custom's marking of `default_input_method' as set is Deferred).
    recursive subroutine set_input_method(input_method, status)
        character(len=*), intent(in) :: input_method
        type(status_type), intent(out) :: status

        call activate_input_method(input_method, status)
        if (.not. status%ok()) return
        default_input_method%value = input_method
    end subroutine set_input_method

    !> C-x RET C-\: read the input method and set it.
    !> Port of mule-cmds.el:set-input-method (the command reading its argument).
    recursive subroutine cmd_set_input_method(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, input_method

        default = input_method_prompt_default()
        call read_input_method_name(format_prompt("Select input method", default), default, .true., &
                                    input_method, status)
        if (.not. status%ok()) return
        call set_input_method(input_method, status)
    end subroutine cmd_set_input_method

    !> Enable or disable multilingual text input method for the current buffer.
    !> Only one input method can be enabled at any time in a given buffer.
    !>
    !> The normal action is to enable an input method if none was enabled,
    !> and disable the current one otherwise.  Which input method to enable
    !> can be determined in various ways--either the one most recently used,
    !> or the one specified by `default_input_method', or as a last resort
    !> by reading the name of an input method in the minibuffer.
    !>
    !> With a prefix argument ARG, read an input method name with the minibuffer
    !> and enable that one.  The default is the most recent input method specified
    !> (not including the currently active input method, if any).
    !> Port of mule-cmds.el:toggle-input-method (Custom's marking of `default_input_method' as set is Deferred).
    recursive subroutine toggle_input_method(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, input_method

        if (toggle_input_method_active) then
            call set_error(status, FM_GENERIC_ERROR, format_message("Recursive use of `toggle_input_method'"))
            return
        end if

        ! No associate around the calls: hooks may make buffers.
        if (allocated(buffers(current)%current_input_method) .and. .not. arg) then
            if (allocated(buffers(current)%current_transient_input_method)) then
                call deactivate_transient_input_method(status)
            else
                call deactivate_input_method(status)
            end if

            return
        end if

        default = input_method_prompt_default()

        associate (buffer => buffers(current))
            if (arg .and. len(default) > 0 .and. allocated(buffer%current_input_method)) then
                if (buffer%current_input_method == default) then
                    if (allocated(buffer%input_method_history%items)) then
                        if (size(buffer%input_method_history%items) > 1) then
                            default = buffer%input_method_history%items(2)%text
                        end if
                    end if
                end if
            end if
        end associate

        toggle_input_method_active = .true.

        if (arg .or. len(default) == 0) then
            call read_input_method_name(format_prompt("Input method", default), default, .true., input_method, status)
        else
            input_method = default
        end if

        if (status%ok()) call activate_input_method(input_method, status)
        toggle_input_method_active = .false.
        if (.not. status%ok()) return
        if (.not. allocated(default_input_method%value)) default_input_method%value = ""

        if (len(default_input_method%value) == 0 .and. allocated(buffers(current)%current_input_method)) then
            default_input_method%value = buffers(current)%current_input_method
        end if
    end subroutine toggle_input_method

    !> C-\: toggle the input method, reading it with a prefix argument.
    !> Port of mule-cmds.el:toggle-input-method (the command reading its argument).
    recursive subroutine cmd_toggle_input_method(status)
        type(status_type), intent(out) :: status

        call toggle_input_method(current_prefix_arg%kind /= PREFIX_NONE, status)
    end subroutine cmd_toggle_input_method

    !> Select and enable a transient input method for the current buffer.
    !> If `default_transient_input_method' was not yet defined, prompt for it.
    !> Port of mule-cmds.el:activate-transient-input-method (its clearfun is `clear_transient_input_method').
    recursive subroutine activate_transient_input_method(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, input_method

        if (.not. allocated(default_transient_input_method%value)) default_transient_input_method%value = ""

        if (arg .or. len(default_transient_input_method%value) == 0) then
            default = input_method_prompt_default()
            call read_input_method_name(format_prompt("Transient input method", default), default, .true., &
                                        input_method, status)
            if (.not. status%ok()) return
            default_transient_input_method%value = input_method
        end if

        call input_method_after_insert_chunk_hook%append(clear_transient_input_method)

        if (allocated(buffers(current)%current_input_method)) then
            buffers(current)%previous_transient_input_method = buffers(current)%current_input_method
            call deactivate_input_method(status)
            if (.not. status%ok()) return
        else if (allocated(buffers(current)%previous_transient_input_method)) then
            deallocate (buffers(current)%previous_transient_input_method)
        end if

        call activate_input_method(default_transient_input_method%value, status)
        if (.not. status%ok()) return
        buffers(current)%current_transient_input_method = default_transient_input_method%value
    end subroutine activate_transient_input_method

    !> Turn off the transient input method once its input method has
    !> inserted a chunk of text, and remove this procedure from
    !> `input_method_after_insert_chunk_hook'.
    !> Port of mule-cmds.el:activate-transient-input-method (its clearfun and exitfun).
    recursive subroutine clear_transient_input_method(status)
        type(status_type), intent(out) :: status

        call deactivate_transient_input_method(status)
        call input_method_after_insert_chunk_hook%remove(clear_transient_input_method)
    end subroutine clear_transient_input_method

    !> C-x \: enable the transient input method, reading it with a prefix
    !> argument.
    !> Port of mule-cmds.el:activate-transient-input-method (the command reading its argument).
    recursive subroutine cmd_activate_transient_input_method(status)
        type(status_type), intent(out) :: status

        call activate_transient_input_method(current_prefix_arg%kind /= PREFIX_NONE, status)
    end subroutine cmd_activate_transient_input_method

    !> Disable currently active transient input method for the current buffer.
    !> Port of mule-cmds.el:deactivate-transient-input-method.
    recursive subroutine deactivate_transient_input_method(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: previous

        if (.not. allocated(buffers(current)%current_transient_input_method)) return
        call deactivate_input_method(status)
        if (.not. status%ok()) return

        if (allocated(buffers(current)%previous_transient_input_method)) then
            previous = buffers(current)%previous_transient_input_method
            call activate_input_method(previous, status)
            if (.not. status%ok()) return
            deallocate (buffers(current)%previous_transient_input_method)
        end if

        deallocate (buffers(current)%current_transient_input_method)
    end subroutine deactivate_transient_input_method

    !> Describe input method INPUT_METHOD.
    !> Port of mule-cmds.el:describe-input-method (no help cross-references).
    recursive subroutine describe_input_method(input_method, status)
        character(len=*), intent(in) :: input_method
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: previous
        type(status_type) :: restore_status
        integer :: id

        if (len(input_method) == 0) then
            call describe_current_input_method(status)
            return
        end if

        previous = ""
        if (allocated(buffers(current)%current_input_method)) previous = buffers(current)%current_input_method
        call activate_input_method(input_method, status)
        if (status%ok()) call describe_current_input_method(status)

        if (status%ok()) then
            call activate_input_method(previous, status)
            return
        end if

        call activate_input_method(previous, restore_status)
        id = find_input_method(input_method)

        if (id == 0) then
            ! Not registered: what upstream shows for its missing entry.
            call with_output_to_temp_buffer("*Help*", "Input method: "//input_method// &
                                            format_message(" (`' in mode line) for ")//LF//"  "//LF, status)
        else
            associate (method => input_methods(id))
                call with_output_to_temp_buffer("*Help*", "Input method: "//input_method//format_message(" (`")// &
                                                trim(method%title)//format_message("' in mode line) for ")// &
                                                trim(method%language_environment)//LF//"  "//method%description//LF, &
                                                status)
            end associate
        end if
    end subroutine describe_input_method

    !> C-h I: read the input method and describe it.
    !> Port of mule-cmds.el:describe-input-method (the command reading its argument).
    recursive subroutine cmd_describe_input_method(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, input_method

        default = ""
        if (allocated(buffers(current)%current_input_method)) default = buffers(current)%current_input_method
        call read_input_method_name(format_prompt("Describe input method", default), default, .false., &
                                    input_method, status)
        if (.not. status%ok()) return
        call describe_input_method(input_method, status)
    end subroutine cmd_describe_input_method

    !> Describe the input method currently in use.
    !> This is a subroutine for `describe_input_method'.
    !> Port of mule-cmds.el:describe-current-input-method.
    recursive subroutine describe_current_input_method(status)
        type(status_type), intent(out) :: status
        procedure(current_input_method_procedure), pointer :: describe

        if (.not. allocated(buffers(current)%current_input_method)) then
            call set_error(status, FM_GENERIC_ERROR, "No input method is activated now")
            return
        end if

        describe => buffers(current)%describe_current_input_method_function

        if (associated(describe)) then
            call describe(status)
        else
            call message(format_message("No way to describe the current input method `"// &
                                        buffers(current)%current_input_method//"'"))
            call ring_bell()
        end if
    end subroutine describe_current_input_method
end module mule_mod

# multilingual_environment_mod

- Module: multilingual_environment_mod
- Upstream: mule.el, mule-cmds.el, mule-diag.el (coding systems, charsets)
- State: partial

## Ported

`find-auto-coding` and `set-auto-coding` (`auto-coding-alist`, `auto-coding-regexp-alist`, the -*- `coding:` tag, the local variables list, a trailing `!`),
`after-insert-file-set-coding`,
`find-new-buffer-file-coding-system`,
`merge-coding-systems`,
`buffer-file-coding-system-explicit`,
C-x RET f `set-buffer-file-coding-system` (read-buffer-file-coding-system's default),
C-x RET r `revert-buffer-with-coding-system`,
`prefer-coding-system`,
`set-default-coding-systems`,
`select-safe-coding-system` (defaults, the coding cookie check, the *Warning* buffer and prompt of select-safe-coding-system-interactively),
`read-coding-system`,
C-h C `describe-coding-system`,
`describe-current-coding-system`;
the mode line's mule info (display_engine_mod);
C-x RET c `universal-coding-system-argument` (mule-cmds--prefixed-command-pch through `this_command_wrapper`),
C-x RET p `set-buffer-process-coding-system`,
C-x RET t `set-terminal-coding-system` and C-x RET k `set-keyboard-coding-system` (`default-terminal-coding-system`, `default-keyboard-coding-system`, `keyboard-coding-system` as `keyboard_coding_system_variable`; the ISO-2022 coding systems, with `:suitable-for-keyboard`);
language environments: `language-info-alist` as `language_environments` (records of `language_environment_type`, filled by the language submodules of coding_mod through `define_language_environment`),
`get-language-info` as `find_language_environment` and the components,
`read-language-name` (KEY as a `LANGUAGE_INFO_*` constant, absent for any),
C-x RET l `set-language-environment` (coding priorities, charset priority, the default input method, setup and exit functions, `set-language-environment-hook`, `exit-language-environment-hook`, `current-language-environment`, `default-input-method`),
`reset-language-environment`,
C-h L `describe-language-environment`;
`set-locale-environment` (called by startup.el:command-line too, after the GNU/Linux `eol-mnemonic-dos` and `eol-mnemonic-mac`),
`locale-language-names`,
`locale-charset-language-names`,
`locale-preferred-coding-systems` (as arrays of `locale_entry_type`),
`locale-name-match` as `find_locale_entry`,
`locale-charset-match-p` as `is_locale_charset_match`,
`locale-charset-to-coding-system`,
`locale-translate` (`locale-translation-file-name`),
`get-locale-names`,
`current-locale-environment`,
`system-messages-locale`,
`system-time-locale` (emacs.c),
`set-display-table-and-terminal-coding-system`;
mule-diag.el's charset commands: `list-character-sets` (with C-u, `list-character-sets-1` and -2; the C-u listing's header names its first field CHARSET-NAME, upstream's CHARSET-SYMBOL-NAME),
`read-charset`,
`list-charset-chars` (`list-block-of-chars`, the charset in the mode line through `%12b`, the `charset` text property; its body as the BODY procedure of help_mod's `with_output_to_temp_buffer`),
`describe-character-set`;
the input method framework: `input-method-alist` (as the registry `input_methods` of `input_method_type`, found with `find_input_method`),
`register-input-method`,
`read-input-method-name`,
`activate-input-method`,
`deactivate-input-method`,
`set-input-method` (C-x RET C-\\),
`toggle-input-method` (C-\\),
`activate-transient-input-method` (C-x \\),
`deactivate-transient-input-method`,
`describe-input-method` (C-h I),
`describe-current-input-method`,
`default-transient-input-method`,
`input-method-activate-hook`,
`input-method-deactivate-hook`,
the per-buffer `current-input-method`,
`current-input-method-title`,
`current-transient-input-method`,
`previous-transient-input-method`,
`input-method-history`,
`deactivate-current-input-method-function` and `describe-current-input-method-function` (buffer_mod),
the title in the mode line (bindings.el)

## Deferred

the buttons of `list-character-sets` (sorting by column, a charset name listing its characters) and of the help buffers (wait for button.el),
the echo of C-x RET c's coding system while the next keys are typed,
C-x RET x/X/F (selection and file-name coding systems),
the input methods of describe-language-environment,
Quail (step 41j),
`input-method-history` as the minibuffer history,
`read-multilingual-string`,
read-event's and read-char's INHERIT-INPUT-METHOD,
`input-method-use-echo-area`,
`input-method-exit-on-first-char`,
`input-method-verbose-flag` and the other variables input methods read,
the transient input method turned off after a chunk (Quail's `input-method-after-insert-chunk-hook`),
Custom marking `default-input-method` as set,
`set-locale-environment`'s FRAME (one terminal),
`with-locale-environment`,
`woman-locale`,
`ps-paper-type` and `default-sendmail-coding-system` (wait for woman, ps-print and mail),
the 8-bit display of `standard-display-european-internal` (waits for display tables),
the setup functions other than Catalan's (CJK width tables, Turkish case tables, Ethiopic, Korean, Thai: Deferred),
the `features` key (Not applicable),
`auto-coding-functions`,
`enable-character-translation`,
help-xref buttons,
completion-table-in-turn,
`list-coding-systems`,
the `charset` text property list-charset-chars puts (with the other `charset` properties);
C-x RET r fails until reverting file buffers is ported

## Tests

test_coding_mod (ports find-auto-coding--bug27391; own: tags, visiting and saving Latin-1 and DOS files, C-x RET f, mode line, prefer-coding-system, C-h C, language environments with C-x RET l and C-h L, set-locale-environment, the charset commands, input methods)

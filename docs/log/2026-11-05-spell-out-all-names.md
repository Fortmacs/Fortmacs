# 2026-11-05: spell-out-all-names

- Owner: the no-abbreviations rule covers every name, Emacs's own included: commands, variables, functions, modes, modules and files (`comint_send_input` → `send_command_interpreter_input`, `isearch_forward` → `incremental_search_forward`). Keys and mode-line text are unchanged. CLAUDE.md's two naming lines say so.
- Next: a survey of the abbreviated words in all names with their counts, an owner questionnaire per word, then the renames through tools/fortran-names.d (the module names are in PR spell-out-module-names).

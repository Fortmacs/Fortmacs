# python_mod

- Module: python_mod
- Upstream: progmodes/python.el
- State: partial

## Ported

`python-mode` and `python-base-mode` (the syntax tables and the dotty one, `python-mode-map` with its remaps and `python-indent-repeat-map`, three Font Lock levels with their matchers: f-strings, assignments, escape sequences in strings and bytes; `python-font-lock-syntactic-face-function`, `python-syntax-stringify` and `python-syntax-propertize-function`),
`python-syntax-context` and the other `python-syntax-*` and `python-info-*` functions (`python-info-current-defun`, Python mode's `add-log-current-defun-function`, `python-info-encoding`, statement, block, line and docstring tests),
`python-util-*`,
navigation (`python-nav-beginning-of-defun`, `-end-of-defun`, statements, blocks, `python-nav-forward-sexp` and the safe variants, `python-nav-up-list`, `python-nav-backward-up-list`, `python-nav-if-name-main`),
indentation (`python-indent-context` with the PYTHON_INDENT_* contexts, `python-indent-calculate-indentation` and levels, `python-indent-line`, `python-indent-region`, `python-indent-guess-indent-offset`, `python-indent-dedent-line`, `python-indent-dedent-line-backspace`, `python-indent-shift-left`/`-right`, `python-indent-post-self-insert-function`, `python-electric-pair-string-delimiter`),
`python-mark-defun`,
filling (`python-fill-paragraph` and the comment, string, decorator and paren functions, `python-fill-docstring-style` as PYTHON_DOCSTRING_* constants),
the user options;
auto-mode-alist and interpreter-mode-alist entries;
electric_mod's buffer-local `electric-indent-chars`;
Imenu: `python-imenu-create-index` (`python-imenu--build-tree`, `python-imenu--put-parent`, `python-imenu--get-defun-type-name`; the tree in a pool of nodes),
`python-imenu-format-item-label`,
`-parent-item-label`,
`-parent-item-jump-label` and their variables,
C-c C-j;
`python-info-current-defun` in `which-func-functions` (step 49c)

## Deferred

the inferior Python (`run-python`, `python-shell-*`, sending code, `python-pdbtrack`),
completion,
ElDoc,
`python-imenu-create-flat-index` and the tree-sitter index,
`python-check`,
the Flymake backend,
ffap,
the skeletons (skeleton.el),
hideshow,
`python-describe-at-point`,
`python-ts-mode`,
the menu,
`python-fix-imports` and the other import commands

## Tests

test_python_mod (ports python-tests.el's indentation, navigation, info, util and syntax tests: 126);
test_index_menu_mod (the index),
test_which_function_mod

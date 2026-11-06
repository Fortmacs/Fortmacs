# javascript_mod

- Module: javascript_mod
- Upstream: progmodes/js.el
- State: partial

## Ported

`js-mode` with its alias `javascript-mode` (one command, `javascript_mode`: the alias's name is the mode's spelled-out name),
`js-base-mode` and `js-json-mode` (the syntax table, `js-syntax-propertize` for regexp literals and `#!`, `js-font-lock-syntactic-face-function` with doc comments, the three Font Lock levels with `cpp-font-lock-keywords`, the class declarations of `js-enabled-frameworks` through `js--class-decl-matcher` (and the parent class trick), the Dojo matchers, `js--variable-decl-matcher` with destructuring, formal parameters),
the parse cache (`js--ensure-cache` with its pitems in `js--pstate`/`js--pend` text properties, `js--flush-caches` in `before-change-functions`, `js--update-quick-match-re`),
`js-syntactic-context`,
`js-beginning-of-defun` and `js-end-of-defun` (nested and `js-flat-functions`),
`js--re-search-forward`/`-backward`,
indentation (`js-indent-line`, `js--proper-indentation` with array comprehensions, chained expressions, braceless control statements, multi-line declarations, `js-indent-first-init`, the offset options and `js-comment-lineup-func`),
`electric-indent-chars`;
auto-mode-alist and interpreter-mode-alist entries

## Deferred

JSX (`js-jsx-syntax`, its detection, `js-jsx-mode`, its keywords and indentation),
filling (`js-fill-paragraph`, `js-do-auto-fill`, the CC Mode paragraph variables, `c-indent-new-comment-line`),
Imenu,
`which-func`,
`prettify-symbols-alist`,
`electric-layout-rules`,
the hideshow variables,
`js-use-syntactic-mode-name`,
the obsolete MozRepl variables,
`js-ts-mode`

## Tests

test_javascript_mod (ports js-tests: the regexp syntax, indentation-error, doc comment face, propertize and subprogram tests and the indentation tests of the five non-JSX resource files, embedded; own: the syntactic context, Font Lock)

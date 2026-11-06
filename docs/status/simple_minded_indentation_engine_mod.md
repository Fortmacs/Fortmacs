# simple_minded_indentation_engine_mod

- Module: simple_minded_indentation_engine_mod
- Upstream: emacs-lisp/smie.el
- State: partial

## Ported

the grammar conversions (`smie-bnf->prec2` with precs and (T1 REL T2) resolvers and the adjacent-nonterminal errors, `smie-precs->prec2`, `smie-merge-prec2s`, `smie-prec2->grammar` with its cycle error, `smie-set-prec2tab` with overrides, `smie-bnf--classify`, their warnings ("Conflict: ...", "token ... is both ...", "Total: N warnings", of the type simple_minded_indentation_engine), `smie-bnf--closer-alist`; insertion-ordered prec2 tables, so the levels are upstream's),
the BNF,
precs,
prec2 and grammar as types,
`smie-grammar`,
the token functions (`smie-default-forward-token`, `smie-default-backward-token`),
`smie-next-sexp`,
`smie-backward-sexp`,
`smie-forward-sexp`,
`smie-forward-sexp-command` and `smie-backward-sexp-command` (the `forward-sexp-function`),
`smie-close-block`,
`smie-down-list`,
blinking (`smie-blink-matching-check`, `smie-blink-matching-open` on `post-self-insert-hook`, `smie-blink-matching-triggers`, `smie-blink-matching-inners`),
`smie--matching-block-data` and `smie--opener/closer-at-point` around `show-paren-data-function`,
the indentation engine (`smie-indent-basic`, `smie-rules-function` with :after, :before, :elem, :list-intro, :close-all, the rule helpers `smie-rule-hanging-p`, `-is_beginning_of_line`, `-parent-p`, `-next-p`, `-prev-p`, `-sibling-p`, `-parent`, `-separator` with `smie-rule-separator-outdent`, `smie-indent-functions` with all twelve functions, `smie-indent-calculate`, `smie-indent-line` with its demoted errors, `smie-indent-virtual`, `smie-indent-keyword`),
`smie-auto-fill` around `normal-auto-fill-function`,
`smie-setup` (:forward-token, :backward-token),
`smie--next-indent-change`,
smie-config (`smie-config` as an array of `simple_minded_indentation_engine_configuration_rule_type` records with the mode's rules installed from `after_change_major_mode_hook` for the modes the major mode derives from, `smie-config-local`, the advice in the one place `simple_minded_indentation_engine_rules_function` is called, `smie-config--get-trace`, `smie-config-show-indent`, `smie-config-set-indent`, `smie-config--guess-value`, `smie-config-guess` with `smie-config--guess` and `-guess-1`, `smie-config-save`; rules and answers shown as Fortran: `SIMPLE_MINDED_INDENTATION_ENGINE_AFTER "then" -> 4`),
`smie--hanging-eolp-function` as a buffer-local procedure setting (`simple_minded_indentation_engine_hanging_line_test_function`: `%set_local` advises it in a buffer)

## Deferred

marking `simple_minded_indentation_engine_configuration` as set for Customize in `smie-config-save` (customize-mark-as-set),
`smie-edebug`,
`debug-on-error` in `smie-rule-separator`

## Tests

test_simple_minded_indentation_engine_mod (upstream has no SMIE tests; own: levels from precs and a BNF, the closer table, expression motion and `forward-sexp`, indentation, `smie-close-block`; test_shell_script_mod's smie-config test (show, guess and the local rules, as GNU Emacs gives them for sh-mode))

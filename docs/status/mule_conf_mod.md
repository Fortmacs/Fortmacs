# mule_conf_mod

- Module: mule_conf_mod, mule_conf_charsets_mod (submodule of charset_mod)
- Upstream: international/mule-conf.el
- State: partial

## Ported

`password-word-equivalents`,
`password-colon-equivalents`;
all the charsets in their order (`define-charset`, `define-charset-alias`, the attributes set on charset.c's own charsets, the `unify-charset` calls)

## Deferred

the coding systems not in coding_mod (step 41)

## Tests

test_charset_mod;
through test_comint_mod (the password prompt regexp)

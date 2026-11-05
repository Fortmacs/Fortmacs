# multilingual_environment_configuration_mod

- Module: multilingual_environment_configuration_mod, multilingual_environment_configuration_character_sets_mod (submodule of character_set_mod)
- Upstream: international/mule-conf.el
- State: partial

## Ported

`password-word-equivalents`,
`password-colon-equivalents`;
all the charsets in their order (`define-charset`, `define-charset-alias`, the attributes set on charset.c's own charsets, the `unify-charset` calls)

## Deferred

the coding systems not in coding_mod (step 41)

## Tests

test_character_set_mod;
through test_command_interpreter_mod (the password prompt regexp)

# env_mod

- Module: env_mod
- Upstream: env.el
- State: partial

## Ported

`getenv` (`process_environment`, through `getenv_internal`; DISPLAY falls back to `initial_environment`),
`setenv` (VALUE absent removes the variable; a name with `=` is an error; through `setenv-internal` with KEEP-EMPTY; also set in the editor's own environment, so the ported code that still reads it with get_environment_variable, upstream's egetenv, sees it),
`substitute-env-vars` (WHEN-UNDEFINED nil or t)

## Deferred

the commands M-x getenv and M-x setenv (`read-envvar-name`, `setenv-history`, the prefix argument) and SUBSTITUTE-ENV-VARS,
which only they use;
FRAME (frames' environments);
encoding with `locale-coding-system`;
`set-time-zone-rule` when TZ is set;
`substitute-env-in-file-name`;
`with-environment-variables`

## Tests

test_env_mod (ports env-tests: test-getenv-setenv; own: removal, `=` in a name, `substitute_env_vars` after `setenv`),
test_fileio_mod

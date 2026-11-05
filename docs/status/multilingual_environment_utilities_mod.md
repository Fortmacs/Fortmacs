# multilingual_environment_utilities_mod

- Module: multilingual_environment_utilities_mod
- Upstream: international/mule-util.el
- State: partial

## Ported

`truncate-string-to-width` (with ELLIPSIS-TEXT-PROPERTY as `truncate_string_to_width_propertized`),
`truncate-string-ellipsis` (the function; its variable is `truncate_string_ellipsis_variable`)

## Deferred

`store-substring`,
nested tables,
the coding-system helpers,
`with-coding-priority`,
`detect-coding-with-language-environment`,
`filepos-to-bufferpos`,
`bufferpos-to-filepos`

## Tests

test_multilingual_environment_utilities_mod (ports the mule-util-test-truncate tests)

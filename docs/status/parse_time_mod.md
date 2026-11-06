# parse_time_mod

- Module: parse_time_mod
- Upstream: calendar/parse-time.el (+ cl-extra.el `cl-parse-integer` for its tokens)
- State: done

## Ported

`parse-time-string-chars`,
`parse-time-tokenize`,
`parse-time-months`,
`parse-time-weekdays`,
`parse-time-zoneinfo` (records of names and numbers),
`parse-time-rules` (the code of `parse_time_string`: its predicates and extractors are procedures, not data),
`parse-time-string` (ISO 8601 first, then the rules; FORM t is KEEP_FRACTION),
`parse-iso8601-time-string`

## Deferred

numbers of more than 18 digits in the text (upstream's bignums: an `overflow_error` error condition)

## Tests

test_parse_time_mod (ports parse-time-tests.el; own tests of the tokens)

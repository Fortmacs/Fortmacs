# iso8601_mod

- Module: iso8601_mod
- Upstream: calendar/iso8601.el
- State: done

## Ported

`iso8601-parse` (`parse_iso8601`; FORM t is KEEP_FRACTION),
`iso8601-parse-date` (years, calendar dates, dates without day or year, week dates, ordinal dates, the obsolete --MM and ---DD),
`iso8601-parse-time` (fractions of a second, a minute and an hour with KEEP_FRACTION),
`iso8601-parse-zone`,
`iso8601-valid-p` (`is_iso8601_valid`),
`iso8601-parse-duration`,
`iso8601-parse-interval` (START, END and DURATION),
the regular expressions (`iso8601--concat-regexps`, `iso8601--match`),
`iso8601--zone-dst`,
`iso8601--decimalize` (ties to even),
`iso8601--value` and `iso8601--decoded-time` (time_date_mod's `partial_decoded_time_type`),
`iso8601--encode-time`;
text that is not ISO 8601 is a `wrong_type_argument` error condition

## Deferred

fractions of a second with more than 18 digits (upstream's bignums)

## Tests

test_iso8601_mod (ports iso8601-tests.el: all 21)

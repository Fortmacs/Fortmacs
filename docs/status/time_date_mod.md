# time_date_mod

- Module: time_date_mod (+ submodule time_date_parse_mod)
- Upstream: calendar/time-date.el (+ timefns.c `encode-time` of a decoded time with nil components, `check_tm_member`)
- State: done

## Ported

decoded times whose components may be unknown (`partial_decoded_time_type`; its structure constructor is `make-decoded-time`),
`date-to-time`,
`time-since` (a time value or a date string),
`date-to-day`,
`days-between`,
`date-leap-year-p` (`is_date_leap_year`),
`time-date--day-in-year`,
`time-to-day-in-year`,
`time-to-days`,
`time-to-number-of-days` (real(time)/86400, Intrinsics first),
`days-to-time` (a real number of days; an integer one is time_type(seconds=86400*days), the constructor written where it is used, R6-45),
`safe-date-to-time`,
`format-seconds`,
`seconds-to-string` (READABLE, `expanded`, ABBREV, PRECISION),
`date-days-in-month`,
`date-ordinal-to-time`,
`decoded-time-add` (with `decoded-time--alter-month`, `decoded-time--alter-day`, `decoded-time--alter-second`),
`decoded-time-set-defaults` (`set_decoded_time_defaults`),
`decoded-time-period`,
`encode_partial_decoded_time` (encode-time of such a decoded time: an unknown component is a `wrong_type_argument` error condition);
`time-to-seconds` and `subtract-time` are aliases of `float-time` (`real`) and `time-subtract`

## Deferred

Not applicable: upstream's list forms of a time value: the obsolete `encode-time-value` and `with-decoded-time-value`,
`seconds-to-time` (`time_convert` of a real number),
the two-element list `days-to-time` gives for an integer

## Tests

test_time_date_mod (ports time-date-tests.el but test-obsolete-with-decoded-time-value and test-obsolete-encode-time-value; own tests of the readable forms, unknown components in `decoded_time_add` and the errors of `format_seconds`)

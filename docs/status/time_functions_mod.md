# time_functions_mod

- Module: time_functions_mod
- Upstream: timefns.c (+ Gnulib lib/strftime.c, the C locale)
- State: partial

## Ported

time values as one `time_type` (R6-3: `seconds`, `count`/`count_rate`; `TIMESPEC_COUNT_RATE` for file and clock times; exact for any int64 rate, with 128-bit products),
`current-time` (clock_gettime through system_dependent_mod),
`current-cpu-time` (CLOCK_PROCESS_CPUTIME_ID, as clock(3); count_rate CLOCKS_PER_SEC),
`time-add`,
`time-subtract` (`time_arith`: the shared rate, else the least common multiple reduced but not below the smaller rate),
`time-less-p` (`is_time_less`),
`time-equal-p` (`is_time_equal`) (`time_cmp`),
`float-time` (the intrinsic generic `real` extended with a specific for `time_type`, R6-14: `real(t)`, `real(current_time())` for the current time; `frac_to_double`: correctly rounded, ties to even),
`time-convert` (generic: a time value to another count rate, truncating; a real to a time value, `decode_float_time`; FORM `integer` is the `%seconds` component, a real result `real(t)`),
`decode-time` (FORM t: `%second` keeps TIME's precision; `decoded_time_type`, simple.el's decoded-time, with DECODED_TIME_DST_YES/_NO/_UNKNOWN),
`encode-time` (a `decoded_time_type`: DST, an integer `zone` applied without daylight saving time through `tzlookup`'s rule "<+HHMM>-HH:MM:SS", or a TZ string; `check_tm_member`),
`format-time-string` (strftime.c's `__strftime_internal` in Fortran: every conversion, the flags `-` `_` `0` `+` `^` `#`, widths, `%N` digits, `%:z` to `%:::z`, `E`/`O` in the C locale; `emacs_nmemftime`'s NUL bytes),
`current-time-string`,
`current-time-zone` (`time_zone_type`: offset and name, the numeric name when there is none),
`set-time-zone-rule` (absent or `TIME_ZONE_SYSTEM`: wall clock time; `emacs_setenv_TZ` sets TZ for the C library only, leaving the environment subprocesses get alone),
ZONE as a TZ string ("UTC0" for upstream's t; R6-12: an integer ZONE and (OFFSET ABBR) as the rule `time_zone_rule(offset, abbreviation)` builds, tzlookup's "<-08>8:00:00" and "<PST>8:00:00"; R6-13: `wall` as the named constant `TIME_ZONE_SYSTEM`, an impossible TZ value, for which TZ is unset during the call and then restored) applied through the C library (localtime_r, mktime; `emacs_localtime_rz`, `xtzfree`),
the errors "Specified time is not representable",
"Invalid time specification",
"Invalid time frequency";
editing_functions_mod's `current_time_string`,
compile.el's start and end times,
dired-aux.el's touch default,
add-log.el's ISO 8601 date,
the progress reporter's `float-time`,
`buffer-display-time` (ibuffer recency) and the generation list now use it

## Deferred

the locale's names and formats (the C locale's English names; upstream asks the C library's strftime for `%a`, `%b`, `%c`, `%p`, `%x`, `%X`, `%E`/`%O` alternatives, and converts with `locale-coding-system`),
rates whose least common multiple exceeds huge(0_int64) in `time-add`/`time-subtract` (the smaller-rate operand is truncated to the larger rate) and reals needing a rate above 2**62 (truncated),
times whose seconds exceed int64 (upstream's bignums);
the times that other modules still keep in their own form: file_stat_type's `mtime_sec`/`mtime_nsec` (system_dependent_mod, below time_functions_mod; `file-attributes` times),
info_mod's `file_time_type`,
save_history_mod's `modtime_type`,
bookmark_mod's timestamps,
vc's checkout times,
timer_mod's and compile_mod's `system_clock` counts (timer times are time values upstream);
Not applicable: Lisp's other shapes of a time value and the transition between them (`current-time-list`, FORM `list`, `hi_time`, `lo_time`, `ticks_hz_list4`, `decode_time_components`, `decode_lisp_time`, `decode_ticks_hz`, `lisp_time_argument`, `lisp_seconds_argument`, `make_lisp_time`, `timespec_to_lisp`, `timespec_ticks`, `timespec_mpz`, `mpz_set_time`, `mpz_time`, `ticks_hz_to_timespec`, `ticks_hz_seconds`, `s_ns_to_timespec`, `list4_to_timespec`, `current_time_in_cform`, `lispint_arith`, `trillion_factor`),
encode-time's obsolescent six-or-more-argument convention (the structure constructor `decoded_time_type(second=..., ...)`),
`tm_diff` and `tm_gmtoff` (glibc has tm_gmtoff),
`init_timefns`,
`emacs_getenv_TZ` and `invalid_time_zone_specification` (the C library reads TZ and takes any string),
`syms_of_timefns` and `syms_of_timefns_for_pdumper`;
cited elsewhere: `monotonic_coarse_timespec` (timer_mod's `system_clock`),
`make_timeval` (process_mod's waits)

## Tests

test_time_functions_mod (ports timefns-tests.el: all 16, with the zones (-28800 "PST") and -28800 of format-time-string-with-zone as `time_zone_rule(-28800, "PST")` and `time_zone_rule(-28800)`, and the values int64 holds; own: current-time-string, the other directives, set-time-zone-rule, `time_zone_rule`, `TIME_ZONE_SYSTEM`, the clocks),
test_file_input_output_mod (modtime)

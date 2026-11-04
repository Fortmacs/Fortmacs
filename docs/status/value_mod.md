# value_mod

- Module: value_mod, value_names_mod (submodule), names_mod, data_mod, data_buffer_mod (submodule), fns_mod, fns_hash_mod (submodule)
- Upstream: lisp.h, alloc.c, lread.c (obarray), data.c, fns.c (+ buffer.c local variables)
- State: partial

## Ported

`value_type` (nil, fixnums, symbols, strings, proper lists, buffer/marker/keymap refs),
constructors (the generic `value_type(...)`: `value_type()`, `value_type(.true.)`, `value_type(42_int64)`, `value_type(name="x")`, `value_type(text="abc")`) and tests,
obarray (`intern`, `intern-soft`),
`eq`,
`car`/`cdr`,
symbol values,
`equal`,
`length`,
`nth`,
`memq`,
`member`,
`string-lessp`,
`compare-strings`,
`substring` and COUNT_CHARACTERS on strings,
plists,
`get`/`put`,
fns.c `yes-or-no-p` (submodule fns_ask_mod; ASCII downcase);
buffer-local variables in data_buffer_mod: `make-local-variable`,
`make-variable-buffer-local`,
`kill-local-variable`,
`kill-all-local-variables` (running `change-major-mode-hook`, keeping `permanent-local` ones),
`local-variable-p`,
`local-variable-if-set-p`,
`default-value`,
`set-default`,
`default-boundp`,
`buffer-local-value`,
`buffer-local-variables` (two-element lists),
`setq-local`,
choice settings (`choice_setting_type`: `.true.`/`.false.` and named constants, R6-38, with a `%procedure` in an extension, R6-37; buffer-local when forwarded),
the history lists (`history_setting_type` with `%items`, `%length` and `%append`, found by name with `find_history` in the settings registry),
and variables forwarded to Fortran settings (`fill-column`, `tab-width`, `case-fold-search`, `fill-prefix`, `auto-fill-function`, `major-mode`, `mode-name`, `buffer-read-only` and others);
lread.c string literal syntax (`read_string_literal`: the escapes of the reader, \N{U+X}) for `ls --quoting-style=escape` file names,
window values (`make_window_ref`, `is_window`),
strings' text property intervals (`string_intervals`, `set_string_intervals`; `equal` ignores them);
Not applicable: run-time variables (a variable that exists only because a name was set at run time: data.c `set`, `boundp`, `makunbound`, `symbol-value` on names no Fortran object declares, and the data-tests cases on such variables);
unknown names in set_variable,
file-local and directory-local variables are ignored ;
step 42f chunk 4 in fns_mod (and submodule fns_buffer_mod): `string-search`,
`string-distance` (BYTECOMPARE),
`string_char_to_byte`/`string_byte_to_char`,
`substring-no-properties`,
`string-equal` (`is_string_equal`, names allowed),
`string-collate-lessp`/`-equalp` (`is_string_collate_less`/`_equal`, the C locale: character-code order, A–Z folded for IGNORE-CASE; bookmark_mod's private copy is gone),
`clear-string`,
`equal-including-properties` (`value_equal_including_properties`, intervals.c `compare_string_intervals`),
the base64 functions (`base64-encode-string`, `base64url-encode-string`, `base64-decode-string`, and the region commands `base64-encode-region`, `base64url-encode-region`, `base64-decode-region` with M-x; MULTIBYTE for raw bytes),
`buffer-line-statistics`,
`random` (a generic: no argument, a positive LIMIT, a string seed, `.true.` to reseed from the system's entropy; the intrinsic `random_number` behind sysdep.c `get_random`),
`load-average` (integers times 100 or reals; Gnulib getloadavg's /proc/loadavg reading in Fortran, sysdep_mod `get_load_averages`),
`locale-info` (the C locale's codeset, days, months and paper, items `LOCALE_INFO_CODESET`/`_DAYS`/`_MONTHS`/`_PAPER`; a result of another kind stays unallocated, upstream's nil);
data.c `string-to-number` (int64 or real64 result, BASE 2–16, +INF/NaN);
`string-bytes` is the intrinsic `len`,
`memq`/`member` the intrinsics `any`/`findloc` (no procedure);
`Flength`,
`Fnth`,
`Fcar`,
`set_internal`,
`default_value`,
`find_symbol_value`,
`set_default_internal`,
`swap_in_global_binding`,
`do_symval_forwarding` cited where implemented;
fns.c `md5`,
`secure-hash` (strings and buffers; START/END as BEGINNING/END, CODING-SYSTEM, NOERROR, BINARY; the algorithms the named constants `SECURE_HASH_MD5` … `SECURE_HASH_SHA3_512`; the input encoded as extract_data_from_object does: a string by the most preferred coding system or `raw-text` if unibyte, a buffer by the coding system `write-region` would choose, with `select_safe_coding_system_function`; the `iv-auto` OBJECT as `hash_random_bytes(algorithm, length, hash, status, binary)` (R6-31), LENGTH bytes from getrandom through sysdep_mod `fortmacs_getrandom`),
`secure-hash-algorithms`,
`buffer-hash` (SHA-1 of both sides of the gap),
`secure_hash`,
`hexbuf_digest`/`make_digest_string` (`digest_to_hex`),
in submodule fns_hash_mod over md5_mod … sha3_mod

## Deferred

dotted pairs,
floats,
bignums,
vectors,
hash tables,
function cells,
`let` (also buffer-local),
variable watchers and aliases,
`default-toplevel-value` ;
other locales than C for collation and `locale-info`,
unibyte strings (base64 results are byte strings in `character`),
the string-to-byte cache,
bignum results of `string-to-number` (nearest int64);
subr.el's `sha1` (it is `secure_hash` with SECURE_HASH_SHA1)

## Tests

test_values_mod (ports fns-tests length and plist cases, binding-test-set-constant),
test_fns_mod (ports fns-tests-compare-strings; own `substring` tests),
test_data_buffer_mod (ports binding-test-manual, -setq-default and -makunbound without `let`) ;
test_fns_mod ports string-search,
test-string-distance,
substring-no-properties,
string-equal,
clear-string,
fns-tests-collate-strings, -collate-sort (C locale cases), -equal-including-properties (and /string-prop-vals), -base64-encode-region, -base64-encode-string,
fns-test-base64url-encode-region, -base64url-encode-string,
fns-tests-base64-decode-string,
fns-tests-random,
test-buffer-line-stats-nogap, -gap,
and own string-indexes,
base64-decode-region,
load-average and locale-info tests;
test_values_mod ports lread-string-to-number-trailing-dot and own string-to-number tests;
test_fns_mod ports test-secure-hash (`iv-auto` as `hash_random_bytes`) and fns-tests-hash-buffer,
and own md5/secure-hash range,
buffer,
BINARY,
coding-system and error tests and hash-random-bytes (digest lengths, BINARY, two calls differ, errors)

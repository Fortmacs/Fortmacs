# character_set_mod

- Module: character_set_mod
- Upstream: charset.c (+ mule.el `define-charset`, `charset-dimension`, `charset-chars`, `charset-iso-final-char`, `charset-description`, `charset-short-name`, `charset-long-name`, `charset-list`, put-charset-property for the docstring, the names and `preferred-coding-system`; mule-cmds.el reset-language-environment's and the English language environment's `set-charset-priority`)
- State: partial

## Ported

charsets named by strings with aliases;
the four methods (`CHARSET_METHOD_OFFSET`, `_MAP`, `_SUBSET`, `_SUPERSET`) with code spaces that are not linear,
`:min-code`,
`:max-code`,
`:invalid-code` and fast maps;
`charsetp` as `is_charset`,
`charset-list`,
`charset_attributes` (`charset-plist` as `charset_attributes_type`, typed components),
`decode-char`,
`encode-char`,
`make-char`,
`split-char`,
`char-charset` (RESTRICTION as an optional array of charsets),
`charset-after`,
`iso-charset`,
`get-unused-iso-final-char`,
`declare-equiv-charset`,
`charset-priority-list` (HIGHESTP as `charset_priority_highest`),
`set-charset-priority` (with the non-preferred head, the ISO-2022 and emacs-mule orders and the unibyte charset),
`sort-charsets`,
`charset-id-internal`,
`unify-charset` (decoding through the unification table, encoding through the deunifier),
`map-charset-chars` (an ACTION procedure),
`find-charset-region`,
`find-charset-string`;
all the maps built in (character_set_maps_mod) and loaded on first use (a map that is not, for a charset defined later, is an error on use);
for coding_mod,
the charsets by id (`charsets`, protected),
`find_charset`,
DECODE_CHARACTER as `decode_character_id` and character_charset with a charset list as `character_charset_id`;
`current-iso639-language` (set by `set_language_environment`);
which attributes a charset was given,
for `describe-character-set` (`code_space_count`, the number of elements of `:code-space`; `has_code_offset`, `has_invalid_code`; `internal` for charset.c's own charsets)

## Deferred

translation tables (TABLE of find-charset-*),
a coding system as RESTRICTION of `char-charset`,
`get-charset-property` and `put-charset-property` beyond the attributes above,
`charset-info` (obsolete),
`charset-map-path`,
`clear-charset-maps` and `inhibit-load-charset-map` (dumping only)

## Tests

test_character_set_mod (ports charset-tests.el; own: values as an interactive GNU Emacs gives them)

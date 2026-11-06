# coding_mod

- Module: coding_mod
- Upstream: coding.c (+ mule-conf.el's coding systems and `file-coding-system-alist`, mule-cmds.el reset-language-environment's priorities, mule.el `coding-system-change-eol-conversion`, `coding-system-change-text-conversion`, `coding-system-eol-type-mnemonic`, `coding-system-type`, `coding-system-mnemonic`, `coding-system-equal`, `coding-system-list`; buffer.c `buffer-file-coding-system`)
- State: partial

## Ported

`terminal-coding-system`,
`set-terminal-coding-system-internal` (terminal_output_mod encodes terminal output with it, "?" for what it cannot encode),
`keyboard-coding-system`,
`set-keyboard-coding-system-internal` (keyboard_mod decodes terminal input with it);
coding systems named by strings (upstream's symbol names, "" for nil) with subsidiaries and aliases: `undecided`,
`prefer-utf-8`,
`no-conversion`/`binary`,
`raw-text`,
`no-conversion-multibyte`,
`iso-latin-1`,
`us-ascii`,
`utf-8`,
`utf-8-with-signature`,
`utf-8-auto`,
`utf-8-emacs`,
the five UTF-16 ones;
categories and priorities (`set-coding-system-priority`, `coding-system-priority-list`);
detection as detect_coding and detect_coding_system (null bytes, `prefer-utf-8`, BOMs, the UTF-16 spread test, latin-extra-code-table, line ends; the candidate list's quirk of one unmasked category);
decoding and encoding (UTF-8 with raw bytes and overlong/surrogate rejection, UTF-16 with surrogates, the charsets of a coding system of type `character_set` (its valid first bytes, code points of several bytes, raw bytes for invalid or incomplete ones, `default-char`; `define_character_set_coding_system` as mule.el's `define-coding-system` for that type), Shift-JIS and Big5 (detection, decoding with SJIS_TO_JIS and SJIS_TO_JIS2, encoding with their inverses; `define_character_set_coding_system` with CTYPE `CODING_SJIS` or `CODING_BIG5`), ISO-2022 (`define_iso_2022_coding_system` as mule.el turns `:designation` and `:flags` into the attributes; the detector of the six ISO categories, also run from the head scan at the first ESC, SI or SO; decoding with designations, locking and single shifts, direction sequences, CTEXT extended segments and embedded UTF-8; encoding with designation and invocation, resets at line ends and control characters, designations at the beginning of lines; the safe charsets: register 0 for each charset of a `:charset-list`, requests and register usage for `iso-2022`) with the 7 of mule-conf.el, emacs-mule (detection, decoding of leading codes with the extended ones of the private charsets, encoding with Vemacs_mule_charset_list), raw text, line ends with stray CRs);
`decode-coding-string`/`-region`,
`encode-coding-string`/`-region`,
`detect-coding-string`/`-region`,
`check-coding-systems-region`,
find-coding-systems-region-internal,
`is_character_encodable`,
`find-operation-coding-system` (files),
`coding-system-p`,
`check-coding-system` (`coding-system-error`),
`coding-system-eol-type`,
`coding-system-base`,
`coding-system-aliases`,
`define-coding-system-alias`,
`coding_inherit_end_of_line_type`,
`raw_text_coding_system`;
`coding-system-for-read`/`-write`,
`last-coding-system-used`,
`inhibit-eol-conversion`,
`inhibit-null-byte-detection`,
`eol-mnemonic-*`,
`default-process-coding-system`,
`locale-coding-system`,
`coding-system-require-warning`

## Deferred

CCL and UTF-7 (their categories reject everything),
compositions in ISO-2022 and emacs-mule (0x80 decodes as a raw byte) (their escape sequences decode as raw bytes, as without CODING_ANNOTATE_COMPOSITION_MASK),
the preferred charsets charset annotations give the ISO-2022 encoder,
compound-text-with-extensions,
in-is13194-devanagari (post-read and pre-write conversions) and eucjp-ms (translation tables),
the charset annotations decoding puts (the `character_set` text property),
language environments (English with a UTF-8 locale),
translation tables,
compositions and annotations,
post-read/pre-write conversions,
`define-coding-system`,
`coding-system-put`/`-plist`,
NOCOPY,
the process and network tables and the table functions (xml, po, latexenc);
coding systems whose charset map is Deferred are left out of `find_coding_systems_text` (select-safe-coding-system never offers them),
and `is_character_encodable` reports them through its optional STATUS (terminal_output_mod shows such a glyph as unencodable)

## Tests

test_coding_mod (ports coding-tests.el: bogus-coding-systems, coding-ascii, coding-latin, coding-binary, coding-prefer-utf-8 (no sjis), nocopy-ascii, check-coding-systems-region; the charset coding systems as GNU Emacs decodes, encodes and detects with them; own: detection as GNU Emacs 29 detects these bytes, the functions)

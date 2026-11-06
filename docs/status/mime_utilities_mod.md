# mime_utilities_mod

- Module: mime_utilities_mod
- Upstream: gnus/mm-util.el (in part: the MIME charsets and coding systems rfc2047.el needs)
- State: partial

## Ported

`mm-coding-system-p` (`is_coding_system` of a name that is not ""),
`mm-charset-synonym-alist` (`mime_character_set_synonyms`, computed on first use as upstream computes it at load time),
`mm-charset-to-coding-system` (`mime_character_set_to_coding_system`: the overrides with ALLOW-OVERRIDE, "ascii" for US-ASCII, coding-system-from-name, the synonyms, the coding systems' MIME charsets, the "Unknown charset" message),
`mm-charset-override-alist` (`mime_character_set_overrides`),
`mm-coding-system-priorities` (`mime_coding_system_priorities`, Japanese language environments' default set on first use),
`mm-use-find-coding-systems-region` (`mime_use_find_coding_systems_region`),
`mm-sort-coding-systems-predicate` (a stable sort by the priorities),
`mm-find-mime-charset-region` (`find_mime_character_set_region`: the most preferred coding system's MIME charset; ASCII and raw bytes need none)

## Deferred

the rest of mm-util.el (Gnus's MIME library),
the LBT and SILENT arguments of `mm-charset-to-coding-system`,
the HACK-CHARSETS argument of `mm-find-mime-charset-region` (iso-8859-15 and iso-2022-jp-2) and its fallback for text no coding system with a MIME charset encodes (`mm-find-charset-region`, `mm-mime-charset`: a `void_function` error condition);
Not applicable: `mm-charset-eval-alist` (Lisp forms evaluated to load a charset)

## Tests

test_rfc2047_mod (encoding through `find_mime_character_set_region` and `mime_character_set_to_coding_system`)

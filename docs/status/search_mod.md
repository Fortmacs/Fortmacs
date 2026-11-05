# search_mod

- Module: search_mod, search_regexp_mod (submodule)
- Upstream: search.c
- State: done

## Ported

`find_newline` (both directions, across the gap; as `find_newline1` without the cache),
`scan_newline`,
`scan_newline_from_point`,
`find_newline_no_quit`,
`find_before_next_newline`;
`search-forward`,
`search-backward`,
`re-search-forward`,
`re-search-backward`,
`posix-search-forward`,
`posix-search-backward` (`search_command`, `search_buffer` with `is_trivial_regexp`, `search_buffer_regexp`, `search_buffer_non_regexp`, `simple_search`, through the current buffer's case canon table with `case-fold-search`; the commands read the string in the minibuffer),
`looking-at`,
`posix-looking-at`,
`string-match`,
`posix-string-match` (1-based string indices),
the cache of compiled patterns (`compile_pattern`, `compile_pattern_1`),
`freeze_pattern`/`unfreeze_pattern` (entries in use marked busy so a nested search from syntax-propertize compiles elsewhere: "Too much matching reentrancy"),
`fast_string_match_internal` and `fast_c_string_match_internal` (`fast_string_match`),
`fast_looking_at` (in strings and the buffer, up to a limit),
`search-spaces-regexp`,
the match data (`match-beginning`, `match-end`, `match-data` as integers, `set-match-data`, `match-data--translate`),
saving it while a process filter or sentinel runs (`running_asynch_code`, `save_search_regs`, `restore_search_regs`, `record_unwind_save_match_data`, `unwind_set_match_data`; process_mod calls them through `begin_asynch_code`/`end_asynch_code`),
`regexp-quote`,
`replace-match` (in a string or the buffer, case conversion of the replacement, `\&`, `\N` and `\\`, SUBEXP; `update_search_regs`),
`syms_of_search` (its commands)

## Deferred

the newline cache (region-cache.c; `newline_cache_on_off`),
`boyer_moore` (a speed-up only: `simple_search` finds the same matches),
`match-data` markers and REUSE,
`translation-table-for-input`,
`inhibit-changing-match-data`;
Not applicable: debugging helpers (`newline-cache-check`, `re--describe-compiled`, whose description needs a checking build),
`freeze_buffer_relocation` (ralloc.c),
`shrink_regexp_cache` (called by GC),
`syms_of_search_for_pdumper` (the dumper);
waits for the GUI: nothing

## Tests

test_editing_commands_mod,
test_search_mod (ports search-test--replace-match-update-data; own, including the match data during process filters and the fast matchers),
test_regular_expression_mod (own)

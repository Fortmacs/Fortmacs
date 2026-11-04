# bidi_mod

- Module: bidi_mod
- Upstream: bidi.c (+ dispextern.h struct bidi_it, xdisp.c `bidi-inhibit-bpa`)
- State: done

## Ported

every function: the bidi types and categories (`bidi_get_type`, `bidi_get_category`, `bidi_check_type`, `bidi_mirror_char`, `bidi_paired_bracket_type`, `bidi_explicit_dir_char`, `bidi_isolate_fmt_char`; tables in uni_bidi_mod, so `bidi_initialize` has nothing to load),
the directional status stack (`bidi_push_embedding_level`, `bidi_pop_embedding_level`, `bidi_set_sos_type`, `bidi_remember_char`, `bidi_copy_it`),
the cache of iterator states (`bidi_cache_reset(_to)`, `bidi_cache_shrink`, `bidi_cache_fetch_state`, `bidi_cache_search`, `bidi_cache_find_level_change`, `bidi_level_start`, `bidi_cache_ensure_space`, `bidi_cache_iterator_state`, `bidi_cache_find`, `bidi_peek_at_next_level`, `bidi_push_it`, `bidi_pop_it`, `bidi_shelve_cache`, `bidi_unshelve_cache`),
initialization (`bidi_init_it`, `bidi_set_paragraph_end`, `bidi_line_init`),
fetching characters of the buffer or a string (`bidi_fetch_char`, `bidi_fetch_char_skip_isolates`, `bidi_char_at_pos`, `bidi_count_bytes`),
paragraphs (`bidi_at_paragraph_end`, `bidi_find_paragraph_start`, `find_first_strong_char`, `bidi_paragraph_init`),
the UBA (`bidi_resolve_explicit`, `bidi_resolve_weak`, `bidi_resolve_neutral_1`, `bidi_find_bracket_pairs` with a bracket stack as deep as upstream's on a 64-bit system, `bidi_record_type_for_neutral`, `bidi_resolve_brackets`, `bidi_resolve_neutral`, `bidi_type_of_next_char`, `bidi_level_of_next_char`),
visual order (`bidi_find_other_level_edge`, `bidi_move_to_visually_next`),
`bidi_find_first_overridden`

## Deferred

display properties during the iteration (`compute_display_string_pos`/`_end` of xdisp.c: text a display string replaces counts as its characters),
the paragraph region cache (`bidi_paragraph_cache_on_off`, `cache-long-scans`),
the buffer-local `bidi-paragraph-start-re` and `bidi-paragraph-separate-re` (their defaults are used),
the redisplay ticks;
redisplay does not reorder yet (xdisp_mod row);
Not applicable: the debugging dump `bidi_dump_cached_states`,
the scanner's artifacts `Copyright`,
`static_assert`;
waits for the GUI: nothing

## Tests

test_bidi_mod (ports xdisp-tests--find-directional-overrides-case-1 to -3; own: bidi-classes, bidi-paragraph-direction, bidi-visual-order, bidi-numbers-in-rtl, a string OBJECT)

# bidirectional_mod

- Module: bidirectional_mod
- Upstream: bidi.c (+ dispextern.h struct bidi_it, xdisp.c `bidi-inhibit-bpa`)
- State: done

## Ported

every function: the bidi types and categories (`bidirectional_get_type`, `bidirectional_get_category`, `bidi_check_type`, `bidirectional_mirror_character`, `bidirectional_paired_bracket_type`, `bidirectional_explicit_directory_character`, `bidirectional_isolate_fmt_character`; tables in unicode_bidirectional_mod, so `bidi_initialize` has nothing to load),
the directional status stack (`bidirectional_push_embedding_level`, `bidirectional_pop_embedding_level`, `bidirectional_set_sos_type`, `bidirectional_remember_character`, `bidirectional_copy_it`),
the cache of iterator states (`bidirectional_cache_reset(_to)`, `bidirectional_cache_shrink`, `bidirectional_cache_fetch_state`, `bidirectional_cache_search`, `bidirectional_cache_find_level_change`, `bidirectional_level_start`, `bidirectional_cache_ensure_space`, `bidirectional_cache_iterator_state`, `bidirectional_cache_find`, `bidirectional_peek_at_next_level`, `bidirectional_push_it`, `bidirectional_pop_it`, `bidirectional_shelve_cache`, `bidirectional_unshelve_cache`),
initialization (`bidirectional_initialize_iterator`, `bidirectional_set_paragraph_end`, `bidirectional_line_initialize`),
fetching characters of the buffer or a string (`bidirectional_fetch_character`, `bidirectional_fetch_character_skip_isolates`, `bidirectional_character_at_position`, `bidi_count_bytes`),
paragraphs (`bidirectional_at_paragraph_end`, `bidirectional_find_paragraph_start`, `find_first_strong_character`, `bidirectional_paragraph_initialize`),
the UBA (`bidirectional_resolve_explicit`, `bidirectional_resolve_weak`, `bidirectional_resolve_neutral_1`, `bidirectional_find_bracket_pairs` with a bracket stack as deep as upstream's on a 64-bit system, `bidirectional_record_type_for_neutral`, `bidirectional_resolve_brackets`, `bidirectional_resolve_neutral`, `bidirectional_type_of_next_character`, `bidirectional_level_of_next_character`),
visual order (`bidirectional_find_other_level_edge`, `bidirectional_move_to_visually_next`),
`bidirectional_find_first_overridden`

## Deferred

display properties during the iteration (`compute_display_string_pos`/`_end` of xdisp.c: text a display string replaces counts as its characters),
the paragraph region cache (`bidi_paragraph_cache_on_off`, `cache-long-scans`),
the buffer-local `bidi-paragraph-start-re` and `bidi-paragraph-separate-re` (their defaults are used),
the redisplay ticks;
redisplay does not reorder yet (display_engine_mod row);
Not applicable: the debugging dump `bidi_dump_cached_states`,
the scanner's artifacts `Copyright`,
`static_assert`;
waits for the GUI: nothing

## Tests

test_bidirectional_mod (ports xdisp-tests--find-directional-overrides-case-1 to -3; own: bidi-classes, bidi-paragraph-direction, bidi-visual-order, bidi-numbers-in-rtl, a string OBJECT)

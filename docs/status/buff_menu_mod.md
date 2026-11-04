# buff_menu_mod

- Module: buff_menu_mod
- Upstream: buff-menu.el
- State: partial

## Ported

`list-buffers` (C-x C-b),
`buffer-menu`,
`buffer-menu-other-window`,
`list-buffers-noselect` (FILES-ONLY, BUFFER-LIST as buffers or a function, FILTER-PREDICATE),
`Buffer-menu-mode` (derived from Tabulated List mode; `buffer-stale-function` fast, refreshed by `tabulated-list-revert-hook`) with `Buffer-menu-mode-map` and all its commands: `Buffer-menu-mark`,
`-unmark`,
`-unmark-all-buffers`,
`-unmark-all`,
`-backup-unmark`,
`-delete`,
`-delete-backwards`,
`-save`,
`-not-modified`,
`-execute`,
`-select`,
`-multi-occur`,
`-1-window`,
`-2-window`,
`-this-window`,
`-other-window` (as `buffer_menu_other_window_command`),
`-switch-other-window`,
`-toggle-read-only`,
`-bury`,
`-view`,
`-view-other-window`,
`-toggle-files-only`,
`-toggle-internal`;
`Buffer-menu-buffer`,
`Buffer-menu-marked-buffers`,
`tabulated-list-entry-size->`,
`Buffer-menu--dynamic-name-width`,
the options (`Buffer-menu-name-width` as a number or a function, `-size-width`, `-mode-width`, `-use-frame-buffer-list`, `-human-readable-sizes`, `-use-header-line`, `-marker-char`, `-del-char`),
the `buffer-menu-buffer` face

## Deferred

groups (`Buffer-menu-group-by`, `-group-sort-by` and their functions; Outline minor mode),
the fake header line for `Buffer-menu-use-header-line` nil,
`Buffer-menu-isearch-buffers` and `-regexp` (misearch.el),
`Buffer-menu-visit-tags-table` (etags.el),
the menu and `Buffer-menu-mouse-select`,
the face alias `Buffer-menu-buffer`,
the warning of a failed save in `Buffer-menu-execute`,
the :advertised-binding properties,
the obsolete `Buffer-menu-sort`

## Tests

test_buff_menu_mod (ports buff-menu-24962; own tests of the listing, marks, execute, internal buffers and the size test)

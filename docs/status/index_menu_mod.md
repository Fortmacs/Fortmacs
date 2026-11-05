# index_menu_mod

- Module: index_menu_mod
- Upstream: imenu.el
- State: partial

## Ported

the index as `imenu_index_type` (a flat array of `imenu_entry_type`: name, position or marker, the `parent` submenu, `submenu`),
`imenu-generic-expression` as an array of `imenu_pattern_type` (menu title, regexp or a matcher procedure, subexpression) given to a buffer with `%set_local`,
`imenu-create-index-function`,
`imenu-prev-index-position-function`,
`imenu-extract-index-name-function` (buffer-local procedure settings),
`imenu-default-create-index-function` (the prev/extract loop with `imenu-max-index-time` and its infinite-loop error, or the generic expression),
`imenu--generic-function` (`make_imenu_generic_index`: submenus in reverse order of creation, then the top level; sorting by position, duplicates and comments and strings skipped (`imenu-generic-skip-comments-and-strings`), invisible matches, empty matches, `imenu-case-fold-search` or font-lock's, `imenu-syntax-alist` as `imenu_syntax_modifications`),
`imenu--make-index-alist` (`make_imenu_index`: the cache imenu--index-alist per buffer with `has_imenu_index`/`get_imenu_index`/`set_imenu_index`, `imenu-auto-rescan`, `imenu-auto-rescan-maxout`, the *Rescan* entry, NOERROR, `imenu-unavailable`),
`imenu-flush-cache`,
`imenu--cleanup` (`destroy_imenu_markers`),
`imenu--truncate-items` (`imenu-max-item-length`),
`imenu-use-markers`,
`imenu--sort-by-name`,
`imenu--sort-by-position`,
`imenu--in-alist`,
`imenu-name-lookup-function`,
`imenu-find-default`,
`imenu--completion-buffer` (completion with `imenu-space-replacement`, the default at point, the `imenu` category, submenus read in turn, `imenu--history-list` as `imenu_history`),
`imenu--flatten-index-alist` with `imenu-level-separator`,
`imenu-flatten` (`prefix`),
`imenu-choose-buffer-index` (rescanning),
`imenu-default-goto-function` (the procedure `goto_imenu_position`; `imenu_default_goto_function` for the variable),
`imenu` (M-g i; by name or entry, `push-mark`, `imenu-after-jump-hook`),
`imenu-menubar-modified-tick`

## Deferred

the mouse menu and the menu bar (`imenu--mouse-menu`, `imenu--create-keymap`, `imenu--split-menu`, `imenu--split`, `imenu--split-submenus`, `imenu-add-to-menubar`, `imenu-add-menubar-index`, `imenu-update-menubar`, `imenu--menubar-select`, `imenu-use-popup-menu`, `imenu-sort-function`, `imenu-max-items`, `imenu-submenus-on-top`, `imenu-allow-duplicate-menu-items`),
`imenu-eager-completion-buffer` (eager display is not ported in completion),
`imenu-flatten`'s `annotation` and `group` (text properties on candidates),
the FUNCTION and ARGUMENTS of special elements and generic expressions,
`imenu-name-lookup-function` and `imenu-default-goto-function` as buffer-local variables,
Eglot's `imenu-region` (`imenu--parentify-index-alist`),
the obsolete `imenu-progress-message`;
indexes of ported modes waiting on unported code: js-mode's `js--imenu-create-index` (the parse cache's items),
the tree-sitter indexes (`treesit-simple-imenu-settings`)

## Tests

test_index_menu_mod (ports imenu-tests.el: all 3 tests; own: the generic index, comments and duplicates, the cache and markers, `imenu-unavailable`, `imenu` by name and through M-g i, the defaults, flattening, the indexes of Conf, Makefile, F90, Diff, SGML, HTML, CSS and Python modes)

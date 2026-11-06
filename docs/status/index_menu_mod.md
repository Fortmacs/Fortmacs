# index_menu_mod

- Module: index_menu_mod
- Upstream: imenu.el
- State: partial

## Ported

the index as `index_menu_index_type` (a flat array of `index_menu_entry_type`: name, position or marker, the `parent` submenu, `submenu`),
`imenu-generic-expression` as an array of `index_menu_pattern_type` (menu title, regexp or a matcher procedure, subexpression) given to a buffer with `%set_local`,
`imenu-create-index-function`,
`imenu-prev-index-position-function`,
`imenu-extract-index-name-function` (buffer-local procedure settings),
`imenu-default-create-index-function` (the prev/extract loop with `imenu-max-index-time` and its infinite-loop error, or the generic expression),
`index_menu--generic-function` (`make_index_menu_generic_index`: submenus in reverse order of creation, then the top level; sorting by position, duplicates and comments and strings skipped (`imenu-generic-skip-comments-and-strings`), invisible matches, empty matches, `imenu-case-fold-search` or font-lock's, `imenu-syntax-alist` as `index_menu_syntax_modifications`),
`index_menu--make-index-alist` (`make_index_menu_index`: the cache imenu--index-alist per buffer with `has_index_menu_index`/`get_index_menu_index`/`set_index_menu_index`, `imenu-auto-rescan`, `imenu-auto-rescan-maxout`, the *Rescan* entry, NOERROR, `imenu-unavailable`),
`imenu-flush-cache`,
`index_menu--cleanup` (`destroy_index_menu_markers`),
`index_menu--truncate-items` (`imenu-max-item-length`),
`imenu-use-markers`,
`index_menu--sort-by-name`,
`index_menu--sort-by-position`,
`index_menu--in-alist`,
`imenu-name-lookup-function`,
`imenu-find-default`,
`index_menu--completion-buffer` (completion with `imenu-space-replacement`, the default at point, the `index_menu` category, submenus read in turn, `index_menu--history-list` as `index_menu_history`),
`index_menu--flatten-index-alist` with `imenu-level-separator`,
`imenu-flatten` (`prefix`),
`imenu-choose-buffer-index` (rescanning),
`imenu-default-goto-function` (the procedure `goto_index_menu_position`; `index_menu_default_goto_function` for the variable),
`index_menu` (M-g i; by name or entry, `push-mark`, `imenu-after-jump-hook`),
`imenu-menubar-modified-tick`

## Deferred

the mouse menu and the menu bar (`index_menu--mouse-menu`, `index_menu--create-keymap`, `index_menu--split-menu`, `index_menu--split`, `index_menu--split-submenus`, `imenu-add-to-menubar`, `imenu-add-menubar-index`, `imenu-update-menubar`, `index_menu--menubar-select`, `imenu-use-popup-menu`, `imenu-sort-function`, `imenu-max-items`, `imenu-submenus-on-top`, `imenu-allow-duplicate-menu-items`),
`imenu-eager-completion-buffer` (eager display is not ported in completion),
`imenu-flatten`'s `annotation` and `group` (text properties on candidates),
the FUNCTION and ARGUMENTS of special elements and generic expressions,
`imenu-name-lookup-function` and `imenu-default-goto-function` as buffer-local variables,
Eglot's `imenu-region` (`index_menu--parentify-index-alist`),
the obsolete `imenu-progress-message`;
indexes of ported modes waiting on unported code: js-mode's `js--imenu-create-index` (the parse cache's items),
the tree-sitter indexes (`treesit-simple-imenu-settings`)

## Tests

test_index_menu_mod (ports imenu-tests.el: all 3 tests; own: the generic index, comments and duplicates, the cache and markers, `imenu-unavailable`, `index_menu` by name and through M-g i, the defaults, flattening, the indexes of Conf, Makefile, F90, Diff, SGML, HTML, CSS and Python modes)

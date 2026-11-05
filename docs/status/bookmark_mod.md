# bookmark_mod

- Module: bookmark_mod
- Upstream: bookmark.el
- State: partial

## Ported

`bookmark-alist` as an array of `bookmark_type` (name, filename, position, front and rear context strings, annotation, last-modified time),
saved in `bookmark-default-file` in namelist syntax;
`bookmark-set` (C-x r m),
`bookmark-set-no-overwrite` (C-x r M),
`bookmark-set-internal`,
`bookmark-make-record` (`bookmark-make-record-function`, `bookmark-make-record-default`, `bookmark-inhibit-context-functions`, `bookmark-search-size`, the record's defaults),
`bookmark-store`,
`bookmark-minibuffer-read-name-map` with `bookmark-yank-word`,
`bookmark-jump` (C-x r b),
`bookmark-jump-other-window`,
`bookmark--jump-via` (`bookmark-after-jump-hook`),
`bookmark-handle-bookmark` (relocating a bookmark whose file is gone),
`bookmark-default-handler`,
`bookmark-relocate`,
`bookmark-insert-location` (`bookmark-locate`),
`bookmark-location`,
`bookmark-rename`,
`bookmark-insert`,
`bookmark-delete`,
`bookmark-delete-all`,
`bookmark-save` (C-u C-u makes the default),
`bookmark-write`,
`bookmark-write-file`,
`bookmark-load` (`bookmark-import-new-list`, `bookmark-maybe-rename`, `bookmark-after-load-file-hook`),
`bookmark-maybe-load-default-file` with `bookmark-watch-bookmark-file`,
`bookmark-save-flag` and `bookmark-time-to-save-p`,
`bookmark-exit-hook-internal` on `kill-emacs-hook`,
annotations (`bookmark-use-annotations`, `bookmark-edit-annotation` with Edit-Bookmark-Annotation mode, `bookmark-show-annotation`, `bookmark-show-all-annotations`, `bookmark-automatically-show-annotations`),
`bookmark-current-bookmark` (buffer-local),
`bookmark-maybe-sort-alist`,
`bookmark-completing-read` (category `bookmark`, `bookmark-history`),
the fringe mark overlay (`bookmark-fringe-mark`, `bookmark-face`),
`bookmark-map`;
error_mod's `bookmark-errors` and `bookmark-error-no-filename`;
system_dependent_mod's `fortmacs_clock_realtime` (clock_gettime) ,
the bookmark list (`bookmark-bmenu-list` and its aliases `list-bookmarks` and `edit-bookmarks`, C-x r l, Bookmark Menu mode with all its commands, `bookmark-bmenu-search`, `bookmark-bmenu-filter-alist-by-regexp`, the options of the list, `bookmark-menu-bookmark`, the list rebuilt when bookmarks change, annotations edited from it)

## Deferred

its menu and mouse command,
collation order (names sort case-insensitively by character code, as in the POSIX locale),
`truncate-lines`;
handlers (Help, VC-Dir and Shell bookmarks; the `buffer`, `location`, `handler` properties, bookmark types and `bookmark-inhibit`),
a buffer-local `bookmark-make-record-function`;
`bookmark-jump-other-frame` (frames);
menus (the popup for a mouse event, `menu-bar-bookmark-map`, `bookmark-menu-length`);
fringe bitmaps;
numbered backups (`bookmark-version-control`);
`select-safe-coding-system` when saving;
obsolete variables,
format upgrades,
`bookmark-load-hook`

## Tests

test_bookmark_mod (ports bookmark-tests.el; its resource files in namelist syntax)

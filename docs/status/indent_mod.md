# indent_mod

- Module: indent_mod, indent_cmds_mod (submodule)
- Upstream: indent.c, indent.el
- State: partial

## Ported

`current-column`,
`move-to-column` (FORCE t: splitting a tab, padding the line; a FORCE neither nil nor t such as rect.el's `coerce`: splitting only),
`indent-to`,
`current-indentation`,
`indent-tabs-mode`,
`vertical-motion` (LINES or (COLS . LINES), in the selected window, continuing lines as redisplay does),
point's screen column (as posn-at-point gives it);
indent.el: `indent-for-tab-command` (TAB),
`indent-according-to-mode`,
`indent-line-function` (default `indent-relative`),
`insert-tab`,
`indent-rigidly` (C-x TAB with its transient map: arrows, S-arrows, TAB),
`indent-rigidly-left`/`-right`/`-to-tab-stop`,
`indent-line-to`,
`current-left-margin`,
`move-to-left-margin`,
`indent-to-left-margin`,
`delete-to-left-margin`,
`beginning-of-line-text`,
`indent-region` (C-M-\\; to a column, with the fill prefix, `indent-region-function` (buffer-local), line by line),
`indent-relative`,
`indent-relative-first-indent-point`,
`tab-stop-list`,
`indent-next-tab-stop`,
`indent-accumulate-tab-stops`,
`tab-to-tab-stop` (M-i),
`move-to-tab-stop`,
`standard-indent`;
columns skip invisible text (not text shown as an ellipsis) and count display strings and spaces (scan_for_column with skip_invisible and check_display_width);
`vertical-motion` and point's screen column through xdisp_mod's display_row

## Deferred

images in columns,
selective display,
truncated and horizontally scrolled lines,
`compute_motion`;
indent.el's `tab-always-indent` `complete` and `tab-first-completion`,
TAB with a prefix argument (expressions),
`edit-tab-stops`,
the margin text-property commands,
right-to-left paragraphs,
`indent--default-inside-comment` (needs `syntax-ppss`)

## Tests

test_indent_mod (ports indent-tests.el; own tests)

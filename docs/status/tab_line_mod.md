# tab_line_mod

- Module: tab_line_mod
- Upstream: tab-line.el (step 44c; with xdisp.c's display_mode_lines for the tab line in display_engine_mod, window.c's window_wants_tab_line in window_mod, buffer.c's tab-line-format in buffer_mod, bindings.el's mode-line-window-selected-p in display_engine_mod)
- State: partial

## Ported

`tab-line-mode` (`tab-line-mode-map`, `tab-line-mode-hook`; a `tab_line_format` set outside of the mode stays, with upstream's message),
`global-tab-line-mode` (define-globalized-minor-mode: `global-tab-line-mode-hook`, `global-tab-line-mode-enable-in-buffer`, `tab-line-mode-set-explicitly`, with `tab-line--define-keys` and `tab-line--undefine-keys`),
`tab-line-mode--turn-on`,
the tab line of a window, drawn by redisplay on the window's top line above the header line from what the buffer's `tab_line_format` gives (its procedure `format_tab_line`, upstream `(:eval (tab-line-format))`), in `tab_line_active` or `tab_line_inactive`,
`tab-line-format` with `tab-line-cache-key-default` (the cache of each window),
`tab-line-format-template`,
`tab-line-force-update`,
`tab-line-auto-hscroll` (the function, measuring the strings on a line as wide as the window's text area, as vertical-motion does, with the text properties' stickiness),
`tab-line-hscroll` (`scroll_tab_line`),
`tab-line-hscroll-right`,
`tab-line-hscroll-left` (from the keyboard, in the selected window),
`tab-line-tab-name-format-default`,
`tab-line-tab-name-buffer`,
`tab-line-tab-name-truncated-buffer`,
`tab-line-tabs-window-buffers`,
`tab-line-tabs-fixed-window-buffers` (the persistent window parameter `tab-line-buffers`, kept in window configurations and window states),
`tab-line-tabs-mode-buffers`,
`tab-line-tabs-buffer-groups` (the function: the group tab, the groups, and the buffers of a group, which close their buffer; the window parameters `tab-line-group` and `tab-line-groups`),
`tab-line-tabs-buffer-list`,
`tab-line-tab-modified-p`,
`tab-line-tabs-buffer-group-by-mode`,
`tab-line-tabs-buffer-group-by-project`,
`tab-line-tabs-buffer-group-name`,
`tab-line-tabs-buffer-group-sort-by-name`,
`tab-line-tabs-non-excluded`,
`tab-line-tab-face-inactive-alternating`,
`tab-line-tab-face-special`,
`tab-line-tab-face-modified`,
`tab-line-tab-face-group`,
`tab-line-select-tab-buffer`,
`tab-line-switch-to-prev-tab`,
`tab-line-switch-to-next-tab`,
`tab-line-move-tab-forward`,
`tab-line-move-tab-backward`,
`tab-line--current-tab`,
`tab-line-close-tab`,
`tab-line-close-other-tabs` (from the keyboard: the current tab of the selected window),
the faces `tab-line-tab`, `tab-line-tab-inactive`, `tab-line-tab-inactive-alternate`, `tab-line-tab-special`, `tab-line-tab-modified`, `tab-line-tab-group`, `tab-line-tab-current`, `tab-line-highlight`, `tab-line-close-highlight`,
the options `tab-line-tab-face-functions` (a typed list of procedures),
`tab-line-new-button-show`,
`tab-line-close-button-show`,
`tab-line-close-modified-button-show`,
`tab-line-tab-name-function`,
`tab-line-tab-name-truncated-max`,
`tab-line-tabs-function`,
`tab-line-tabs-buffer-group-function`,
`tab-line-tabs-buffer-group-sort-function`,
`tab-line-tabs-buffer-groups-sort-function`,
`tab-line-tabs-window-buffers-filter-function`,
`tab-line-tab-name-format-function`,
`tab-line-auto-hscroll`,
`tab-line-switch-cycling`,
`tab-line-close-tab-function` (a choice setting: TAB_LINE_CLOSE_TAB_FUNCTION_BURY_BUFFER, _CLOSE_BUFFER, `%procedure`),
`tab-line-define-keys`,
`tab-line-exclude-modes` (major mode objects, `%append`),
the variables `tab-line-new-button`, `tab-line-new-button-functions`, `tab-line-close-button`, `tab-line-close-modified-button`, `tab-line-left-button`, `tab-line-right-button` (the icons' text: the bullet of the close button of a modified buffer where the terminal's coding system can encode it), `tab-line-separator`, `tab-line-tabs-buffer-list-function`, `tab-line-tabs-buffer-groups` (records of a regular expression and a group name, mouse.el's mouse-buffer-menu-mode-groups by default), `tab-line-exclude` (buffer-local when set) and the `tab-line-exclude` property of a major mode (`major_mode_type%tab_line_exclude`);
`tab-line-switch-repeat-map`;
C-x <left>,
C-x C-<left>,
C-x M-<left>,
C-x <right>,
C-x C-<right>,
C-x M-<right> while Global Tab-Line mode is on

## Deferred

the mouse and touch screen on the tab line (`tab-line-tab-map`, `tab-line-add-map`, `tab-line-tab-close-map`, `tab-line-left-map`, `tab-line-right-map`, `tab-line-select-tab`, `tab-line-mouse-move-tab`, `tab-line-tab-context-menu`, `tab-line-context-menu`, `tab-line-track-tap`, `tab-line-event-start` for touch events, the global `[tab-line ...]` bindings of the mouse buttons and wheel, the `mouse-face` and `help-echo` of the tab line's text),
`tab-line-new-tab` and `tab-line-new-tab-choice` (its default, a buffer menu, needs `mouse-buffer-menu` or tmm, neither ported),
the icons' images on window-system frames (`define-icon` with images; the text is shown there too),
`tab-line-exclude-buffers` (`buffer-match-p` is not ported: no buffer matches, as with its default),
`completion-list-mode` as the default of `tab-line-exclude-modes` (the mode is not ported, so the *Completions* buffer is not excluded),
the window parameter `tab-line-format` (`none` and a window's own format),
the function variable `tab-line-cache-key-function` (its default is used),
`tab-line-tab-name-ellipsis` (its default, the default ellipsis, is used),
the Linux console's font table for the bullet of `tab-line-close-modified-button` (terminal.c's terminal_glyph_code: the coding system decides),
dired.el's buffer-local `tab-line-exclude` of nil in the *Marked Files* window (dired_mode_mod cannot reach tab_line_mod's variable without a cycle of modules),
the :set functions of the options (Customize)

## Tests

test_tab_line_mod (own tests: Tab Line mode and its format, the fixed window buffers, switching and moving tabs, closing tabs, the automatic and manual scrolling, the groups, Global Tab-Line mode and the keys of C-x; there is no tab-line-tests.el upstream);
test_display_engine_mod's tab-line (the tab line above the header line)

# follow_mod

- Module: follow_mod
- Upstream: follow.el
- State: partial

## Ported

`follow-mode` (buffer-local, its lighter `follow-mode-line-text`, `follow-mode-hook`, `turn-on-follow-mode`, `turn-off-follow-mode`; Follow mode's function before `pre-redisplay-function`, add-function :before, removed when no buffer is in Follow mode),
`follow-mode-map` with `follow-mode-submap` on `follow-mode-prefix-key` (C-c . C-v, M-v, v, 1, b, C-b, C-l, <, >, n, p) and the remap of `end-of-buffer`;
the window chain (`follow-all-followers` with `follow--window-sorter`, `follow-split-followers`),
`follow-calc-win-end`,
`follow-calc-win-start`,
`follow-windows-start-end` with `follow-windows-start-end-cache` and `follow-cache-valid-p`,
`follow-pos-visible`,
`follow-windows-aligned-p`,
`follow-point-visible-all-windows-p`,
`follow-update-window-start`,
`follow-select-if-visible`,
`follow-select-if-end-visible`,
`follow-select-if-visible-from-first`,
`follow-redisplay`,
`follow-estimate-first-window-start`,
`follow-calculate-first-window-start-from-above` and `-from-below`,
`follow-avoid-tail-recenter` (also on `window-scroll-functions`),
`follow-pre-redisplay-function`,
`follow-post-command-hook` (the `follow-mode-use-cache` property as `command_type`'s `follow_mode_use_cache`, set for `follow-cache-command-list`),
`follow-adjust-window`,
`follow-maximize-region`,
`follow-get-scrolled-point`;
the commands `follow-scroll-up`,
`follow-scroll-down`,
`follow-scroll-up-window`,
`follow-scroll-down-window` (with `follow-scroll-up-arg`, `follow-scroll-down-arg`),
`follow-delete-other-windows-and-split`,
`follow-switch-to-buffer`,
`follow-switch-to-buffer-all`,
`follow-switch-to-current-buffer-all`,
`follow-next-window`,
`follow-previous-window`,
`follow-first-window`,
`follow-last-window`,
`follow-recenter`,
`follow-redraw`,
`follow-end-of-buffer`;
`follow-hide-ghost-cursors`,
`follow-debug` (`follow-debug-message`),
`follow-internal-force-redisplay`,
`follow-inside-post-command-hook-call`,
`follow-fixed-window`

## Deferred

the window group functions and what Follow mode sets them to,
for isearch and ispell (`window-group-start-function` and its relatives in window.el; `follow-window-start`, `follow-window-end`, `follow-set-window-start`, `follow-pos-visible-in-window-p`, `follow-move-to-window-line`, `follow-start-end-invalid`, `follow-after-change` on `after-change-functions`);
`window-size-change-functions` and `follow-window-size-change` (with the window change hooks);
the isearch,
query-replace and ispell update hooks and `isearch-lazy-highlight` `all-windows`;
`cursor-in-non-selected-windows` and `follow-prev-buffer` (the variable is not ported: a text terminal shows only the selected window's cursor);
the permanent-local property of `follow-mode` (a typed attribute of the mode's state is not ported: changing the major mode turns it off);
`follow-auto` and `follow-find-file-hook` (its :set function adds the hook: Customize),
the :set function of `follow-mode-prefix-key` and the obsolete `follow-mode-prefix`,
changes of `follow-mode-line-text` after startup (the lighter is a string);
compilation and comint output (`follow-align-compilation-windows` with `waiting-for-user-input-p`, `follow-comint-scroll-to-bottom`);
the mouse and scroll bars (`follow-scroll-bar-toolkit-scroll`, `follow-scroll-bar-drag`, `follow-scroll-bar-scroll-up`, `follow-scroll-bar-scroll-down`, `follow-mwheel-scroll`, `follow-redraw-after-event`);
the Tools menu (`follow-menu-filter`);
`scroll-preserve-screen-position` (not ported: the `scroll-command` property is not looked at)

## Tests

test_follow_mod (ports follow-tests-do-nothing-test, upstream's only test; own: the split, point moving between windows, M->, scrolling, the window commands, the mode turned off)

# info_mod

- Module: info_mod
- Upstream: info.el (+ mule-util.el `filepos-to-bufferpos` and `bufferpos-to-filepos` for UTF-8 and single-byte files, files.el `prune-directory-list`)
- State: partial

## Ported

`info` (C-h i; a non-numeric prefix argument reads a file name, a numeric one selects *info*<N>),
`info-other-window` (C-h 4 i),
`info-pop-to-buffer`,
`info-emacs-manual` (C-h r) and `info-emacs-bug` (`info_fortmacs_manual` and `info_fortmacs_bug`: the Info file `fortmacs`, R6-32),
`info-initialize` (INFOPATH, `Info-default-directory-list`, the standard directories and `configure_info_directory`),
`Info-find-file` (the suffixes and decoders of `Info-suffix-list`, the decoder reading the file as standard input),
`info-insert-file-contents`,
`Info-find-node`,
`Info-find-node-2` (tag tables, indirect files and their subfiles, anchors, the slop of makeinfo 5, the `NODE.PART` fallback, going back after an error),
`Info-on-current-buffer`,
`Info-revert-buffer-function`,
`Info-insert-dir` (merging the `directory` files, cached until they change) with `Info-dir-remove-duplicates` and `Info-streamline-headings`,
virtual files (`Info-virtual-files`: the directory),
`Info-select-node`,
`Info-set-mode-line` (buffer identification segments),
images as their text,
`Info-hide-cookies-node`,
`Info-goto-node` (g, with completion of file and node names),
`Info-url-for-node` and `Info-url-alist`,
`Info-next` (n),
`Info-prev` (p),
`Info-up` (u, ^),
`Info-history-back` (l),
`Info-history-forward` (r),
`Info-history-list`,
`Info-directory` (d),
`Info-follow-reference` (f),
`Info-menu` (m, completing the menu items),
`Info-nth-menu-item` (1–9),
`Info-top-node` (t, <),
`Info-final-node` (>),
`Info-forward-node` (]),
`Info-backward-node` ([), `Info-next-menu-item`, `Info-last-menu-item`, `Info-next-preorder`, `Info-last-preorder`, `Info-scroll-up` (SPC), `Info-scroll-down` (DEL, S-SPC), `Info-next-reference` (TAB), `Info-prev-reference` (M-TAB, <backtab>), `Info-index-nodes` and `Info-index-node` (index cookies or "Index" in the node name), `Info-follow-nearest-node` (RET: references, footnotes, menu items, the header's pointers), `Info-copy-current-node-name` (w, c), `Info-help` (h), `Info-summary` (?), `Info-mode` (derived from Special mode; `Info-mode-syntax-table`, `Info-mode-hook`, `Info-selection-hook`, `Info-kill-buffer`), `Info-history-skip-intermediate-nodes`, `Info-scroll-prefer-subnodes`, `Info-additional-directory-list`; the display of nodes (`Info-fontify-node`: the header's pointers, the header line (`Info-use-header-line`, the header of the node with its faces), breadcrumbs (`Info-breadcrumbs`, `Info-breadcrumbs-depth`), titles, references shown as "see" or hidden (`Info-hide-note-references`, `Info--dont-hide-references`), visited references (`Info-fontify-visited-nodes`, `Info-fontify-maximum-menu-size`), menus with every third star highlighted and their references hidden, menu headers, index line numbers hidden, URLs, footnotes, the empty lines at the end), the faces (`info-node`, `info-title-1` to `-4`, `info-menu-header`, `info-menu-star`, `info-xref`, `info-xref-visited`, `info-header-xref`, `info-header-node`, `info-index-match`, `Info-quoted`), `Info-mode-font-lock-keywords`, `Info-toc` (T, `Info-toc-find-node`, `Info-toc-insert`, `Info-toc-build`, `Info-toc-nodes`), `Info-history` (L, the virtual file `*History*`), `Info-virtual-nodes`; `Info-search` (s; the subfiles of split files, `Info-search-whitespace-regexp`, `Info-isearch-filter` skipping invisible text, headers and tag tables), `Info-search-case-sensitively` (S), `Info-search-next`, `Info-search-backward`, `Info-index` (i; completing the entries of all index nodes in a temporary Info buffer, `clone-buffer` not being ported), `Info-index-next` (,; `Info-warn-on-index-alternatives-wrap`), `Info-find-index-name`, `Info-virtual-index` (I; the virtual node `*Index for ‘TOPIC’*`), `info-apropos` (the virtual file `*Apropos*`), `Info-goto-emacs-command-node` (C-h F, `info_goto_fortmacs_command_node`; a command's `info_file`, "fortmacs" by default, `Info-file-list-for-emacs` (`info_files_for_fortmacs`) with the prefixes followed by `_`), `Info-goto-emacs-key-command-node` (C-h K, `info_goto_fortmacs_key_command_node`); Isearch across nodes (`Info-isearch-search`, `Info-isearch-wrap`, `Info-isearch-push-state`, `Info-isearch-pop-state`, `Info-isearch-start`, `Info-isearch-end`, the end of node and end of manual failures), `info-display-manual` (C-h R, with `info--manual-names` and `info--filter-manual-names`; one frame)

## Deferred

`Info-refill-paragraphs` (needs `fill-individual-paragraphs`),
the keymaps of the links (mouse);
`isearch-fold-quotes-mode` (char folding);
`info-lookup-symbol` (C-h S) and `info-display-manual` (C-h R) the manuals of Fortmacs (34d);
`Info-goto-node-web` (G) and following URLs (browse-url),
`clone-buffer` (M-n) and FORK,
`info-standalone`,
`info-finder`,
`goto-line-relative`,
images,
the mouse,
menus and tool bar,
bookmarks,
desktop,
speedbar,
active nodes,
the display table for terminals without UTF-8,
the Info files of the source tree

## Tests

test_info_mod (ports test-info-urls; own tests on test/info-resources: nodes, history, menus, references, anchors, split files, node motion, index nodes, the directory node, compressed files, fontification, breadcrumbs, the table of contents and history nodes, searching, the indexes, Isearch across nodes)

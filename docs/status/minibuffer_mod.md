# minibuffer_mod

- Module: minibuffer_mod
- Upstream: minibuffer.el
- State: partial

## Ported

`completing-read` (completing-read-default),
suffix annotations in *Completions* (the `:annotation-function` of `completion-extra-properties` as `completion_annotation_function`, in the face `completions-annotations`),
`completion-boundaries`,
`completion-try-completion` and `completion-all-completions` (in the submodule minibuffer_styles_mod: `completion-styles` with the styles emacs21, emacs22, basic, partial-completion, substring, flex (Gotoh scoring, *Completions* sorted by cost) and initials; `completion-category-defaults`, `completion-category-overrides`; categories buffer, file, symbol-help, command, dabbrev; `completion-pcm-word-delimiters`, `completion-pcm-leading-wildcard`, `completion-flex-nospace`),
`completion-at-point` (C-M-i through `complete-symbol`, `completion-at-point-functions` with `:test` and `:exclusive`),
`completion-help-at-point`,
`completion-in-region` (`completion-in-region-function`, `completion--in-region`, `completion--in-region-1` with backtab scrolling),
`completion-in-region-mode` (its map on `minor-mode-overriding-map-alist`, `completion-in-region-mode-predicate`, the post-command-hook check),
`completion--do-completion`,
`minibuffer-complete` (TAB through `completion-in-region`; a repeated TAB scrolls the list),
`minibuffer-complete-word` (SPC, with completion--try-word-completion),
`minibuffer-complete-and-exit` (RET when a match is required; confirm and confirm-after-completion),
`minibuffer-completion-exit`,
`minibuffer-completion-help` (*Completions* through `display-buffer-at-bottom` in a softly dedicated window fitted to the list, `completions-max-height`; horizontal format, alphabetical),
`minibuffer-hide-completions` (bury-buffer in its window),
`minibuffer-message`,
the completion keymaps (incl. the file-name variants),
`read-file-name-internal` (completion--file-name-table: substitute-in-file-name, with completion-file-name-table),
`exit-minibuffer` (innermost checks),
`minibuffer-recenter-top-bottom`,
`minibuffer-scroll-up-command`,
`minibuffer-scroll-down-command`,
`minibuffer-message-timeout`,
`delete-minibuffer-contents`,
`read-file-name` (read-file-name-default: abbreviated directory as input, default file, "" for empty input, the history replaced with the substituted name, the minibuffer's directory, `read-file-name-completion-ignore-case`),
`insert-default-directory`,
minibuffer-local-map's C-g/C-j/RET,
`read-buffer` (`read-buffer-completion-ignore-case`, `internal-complete-buffer`, internal-complete-buffer-except),
`completion--insert-strings` (for Dired's *Deletions* list),
files.el's `read-directory-name` (here: files_mod comes first),
`completing-read` with a list of defaults,
the `:exit-function` of completion data (completion--done with exact, sole, finished),
capfs that return a function performing completion,
identity display sorting (`completion-table-with-metadata` of an unsorted list)

## Deferred

the shorthand style (no symbol shorthands),
cycling,
metadata other than categories and identity sorting (groups, other sorting functions, the rest of `completion-extra-properties`),
affixation functions,
faces on completions (`completions-common-part`, `completions-first-difference`),
capfs that complete by themselves when called,
the completion-in-region-mode RET/M-<up>/M-<down>/M-RET keys,
choosing from *Completions* (M-<up>, M-<down>, M-RET, mouse),
`completion-show-help`,
tags-completion-at-point-function,
environment variables and ~USER in file name completion,
function MUSTMATCH of `read-file-name`,
minibuffer-default-add-function,
minibuffer-local-filename-syntax,
file-name-quote,
file name shadowing

## Tests

test_minibuf_mod (ports completion-auto-help-test for nil and t; own tests),
test_minibuffer_mod (ports completion-test--pcm-bug38458, completion-pcm-test-1 to 4 and 6 to 8, -anydelim, completion-pcm-bug4219, completion-substring-test-1 to 3 and 5, completion-flex-test-1 and 3; own: non-ASCII flex, default styles, the buffer category, completion-at-point)

# fill_mod

- Module: fill_mod, fill_comment_mod (submodule)
- Upstream: textmodes/fill.el
- State: partial

## Ported

`fill-paragraph` (M-q, the region when active),
`fill-region`,
`fill-region-as-paragraph` (fill-region-as-paragraph-default),
`unfill-paragraph`,
`set-fill-prefix` (C-x .),
`current-fill-column` (with `right-margin`),
`canonically-space-region`,
`colon-double-space`,
adaptive fill (`adaptive-fill-mode`, `-regexp`, `-first-line-regexp`, `fill-context-prefix`, `fill-common-string-prefix`, `fill-match-adaptive-prefix`, `adaptive-fill-function` (buffer-local)),
`fill-nobreak-p` with `fill-nobreak-predicate` and the single-word,
French,
Polish and single-char tests,
`fill-delete-prefix`,
`fill-delete-newlines` (CJK words joined without a space, `fill-nospace-between-words-table` by script, `fill-separate-heterogeneous-words-with-space`, the `fill-space` property),
`fill-move-to-break-point`,
`fill-newline`,
`fill-indent-to-left-margin`,
`fill-indent-according-to-mode`,
`default-justification`,
`current-justification`,
`justify-current-line` (left, right, center, full),
`unjustify-current-line`,
`unjustify-region`,
`fill-paragraph-function` (buffer-local; `fill-minibuffer-function` in the minibuffer),
`fill-paragraph-handle-comment`,
`fill-comment-paragraph` (fill_comment_mod) and the comment steps of `fill-paragraph` (comment lines kept apart from code, bug#80449),
`fill-paragraph-function` and `adaptive-fill-function` as buffer-local procedure settings (`%procedure`, `%set_local`, `%get_current`, `%clear_local`)

## Deferred

comments in `fill-region` and auto-fill,
hard newlines,
`fill-region-as-paragraph-function`,
the semantic-linefeed variants,
kinsoku (`fill-find-break-point`),
invisible text,
`fill-individual-paragraphs`,
`fill-nonuniform-paragraphs`,
`set-justification`

## Tests

test_fill_mod (ports fill-test-no-fill-polish-nobreak-p, fill-test-breakable-paragraph, fill-test-end-period, and the erts files of fill-test-fill-paragraph, -fill-region-as-paragraph-default, -fill-region-as-paragraph and -fill-region, run by a small erts reader; own tests)

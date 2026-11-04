# paragraphs_mod

- Module: paragraphs_mod
- Upstream: textmodes/paragraphs.el
- State: partial

## Ported

`forward-paragraph` (M-}, C-<down>),
`backward-paragraph` (M-{, C-<up>),
`mark-paragraph` (M-h, extending),
`kill-paragraph`,
`backward-kill-paragraph`,
`start-of-paragraph-text`,
`end-of-paragraph-text`,
`paragraph-start` and `paragraph-separate` (regexps),
sentences: `sentence-end` (and its variables),
`forward-sentence` (M-e),
`backward-sentence` (M-a),
`kill-sentence` (M-k),
`backward-kill-sentence` (C-x DEL),
`mark-end-of-sentence`,
`transpose-paragraphs`,
`transpose-sentences`,
`count-sentences`

## Deferred

`fill-prefix`,
`paragraph-ignore-fill-prefix`,
`use-hard-newlines`,
`left-margin`,
`repunctuate-sentences`

## Tests

test_paragraphs_mod (ports paragraphs-tests-forward-backward-paragraph, -mark-paragraph, -kill-paragraph, -backward-kill-paragraph, -start-of-paragraph-text, -end-of-paragraph-text, test-mark-paragraphs, paragraphs-tests-sentence-end, -forward-sentence, -backward-sentence, -kill-sentence, -backward-kill-sentence, -mark-end-of-sentence, -transpose-paragraphs, -transpose-sentences; own tests)

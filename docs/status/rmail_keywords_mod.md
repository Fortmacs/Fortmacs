# rmail_keywords_mod

- Module: rmail_keywords_mod
- Upstream: mail/rmailkwd.el
- State: done

## Ported

`rmail-label-obarray` (`rmail_labels`, an array of names, starting with the attribute names),
`rmail-make-label` (`append_rmail_label`),
`rmail-add-label`,
`rmail-kill-label` (`remove_rmail_label`),
`rmail-read-label` (`read_rmail_label`),
`rmail-set-label`,
`rmail-previous-labeled-message`,
`rmail-next-labeled-message`

## Deferred

nothing

## Tests

test_rmail_keywords_mod (own; upstream has no rmailkwd-tests.el: adding and removing labels, a label naming an attribute, moving to labeled messages)

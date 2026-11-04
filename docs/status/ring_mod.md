# ring_mod

- Module: ring_mod
- Upstream: emacs-lisp/ring.el
- State: partial

## Ported

rings of strings (`ring_type`): `make-ring`,
`ring-length`,
`ring-size`,
`ring-empty-p`,
`ring-insert`,
`ring-insert-at-beginning`,
`ring-remove`,
`ring-ref`,
`ring-member`,
`ring-extend`,
`ring-resize`,
`ring-index`

## Deferred

rings of arbitrary objects,
`ring-p`,
`ring-copy` (assignment copies),
`ring-next`,
`ring-previous`,
`ring-elements`,
`ring-insert+extend`,
`ring-remove+insert+extend`,
`ring-convert-sequence-to-ring`

## Tests

test_ring_mod (ports ring-tests.el for the ported functions)

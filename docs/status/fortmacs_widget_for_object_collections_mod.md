# fortmacs_widget_for_object_collections_mod

- Module: fortmacs_widget_for_object_collections_mod
- Upstream: emacs-lisp/ewoc.el
- State: done

## Ported

`ewoc-create` (header, footer, NOSEP),
`ewoc-location`,
`ewoc-data`,
`ewoc-set-data`,
`ewoc-enter-first`,
`ewoc-enter-last`,
`ewoc-enter-after`,
`ewoc-enter-before`,
`ewoc-next`,
`ewoc-prev`,
`ewoc-nth`,
`ewoc-map`,
`ewoc-delete`,
`ewoc-filter`,
`ewoc-locate`,
`ewoc-invalidate`,
`ewoc-goto-prev`,
`ewoc-goto-next`,
`ewoc-goto-node`,
`ewoc-refresh`,
`ewoc-collect`,
`ewoc-buffer`,
`ewoc-get-hf`,
`ewoc-set-hf` (headers and footers with faces as parts);
ewocs are handles freed with `destroy_ewoc`,
their data integers the caller gives meaning to

## Deferred

—

## Tests

test_fortmacs_widget_for_object_collections_mod (own: entering, locating, walking, refreshing, filtering, collecting, headers and footers; upstream has no tests)

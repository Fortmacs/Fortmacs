# ansi_color_mod

- Module: ansi_color_mod
- Upstream: ansi-color.el
- State: partial

## Ported

`ansi-color-apply`,
`ansi-color-filter-apply`,
`ansi-color-apply-on-region` (PRESERVE-SEQUENCES),
`ansi-color-filter-region`,
the contexts (`ansi-color-context`, `ansi-color-context-region`, partial sequences kept),
`ansi-color--update-face-vec` (basic faces, 8, 16, 256 and 24-bit colors),
`ansi-color--face-vec-face`,
`ansi-color--code-as-hex`,
`ansi-color-apply-face-function` (`ansi-color-apply-overlay-face`, `ansi-color-apply-text-property-face`),
`ansi-color-make-extent`,
`ansi-color-freeze-overlay` (the overlay's modification hooks),
`ansi-color-process-output`,
`ansi-color-compilation-filter`,
`ansi-color-for-comint-mode` (and its on, off and filter commands),
`ansi-color-for-compilation-mode`,
`ansi-color-bold-is-bright`,
the basic,
normal and bright face vectors,
the faces `ansi-color-bold` to `ansi-color-bright-white`

## Deferred

the obsolete variables and functions (`ansi-color-faces-vector`, `ansi-color-names-vector`, `ansi-color-map`, `ansi-color-make-color-map`, `ansi-color-get-face-1`, `ansi-color-apply-sequence`, `ansi-color-parse-sequence`, `ansi-color-set-extent-face`, `ansi-color-make-face`)

## Tests

test_ansi_color_mod (ports ansi-color-tests.el, the incomplete-sequences test comparing texts)

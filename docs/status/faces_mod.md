# faces_mod

- Module: faces_mod
- Upstream: faces.el (the deffaces, `face-spec-recalc`, `face-spec-choose`, `face-spec-set-match-display`), font.c (weight and slant tables)
- State: partial

## Ported

`color-values`,
`color-dark-p` and `readable-foreground-color` (with `color-luminance-dark-limit`);
every face of faces.el as a named object (`named_face_type`, typed attributes assignable from the configuration through `faces%`: `faces%region%background = "blue"`; `face_underline` and `face_box` build the property-list values of `:underline` (:color, :style, :position) and `:box` (:line-width, :color, :style), and a box takes a color as well),
its spec alternatives with display conditions (class, min-colors, background, type, `supports`),
`face_spec_recalc` (the alternative matching the frame, then the configuration's attributes),
`face-list` order,
`faces_equal` (lface_equal_p)

## Deferred

themes and Customize,
frame-local `set-face-attribute`,
X resources,
face aliases,
`face-override-spec`,
stipples,
fonts and fontsets,
`face-no-inherit`

## Tests

test_faces_mod (own)

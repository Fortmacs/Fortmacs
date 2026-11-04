# color_mod

- Module: color_mod
- Upstream: color.el
- State: partial

## Ported

`color-name-to-rgb`,
`color-rgb-to-hex`,
`color-hue-to-rgb`,
`color-hsl-to-rgb`

## Deferred

the rest of color.el (HSV, CIE XYZ and L\*a\*b\* conversions, `color-complement`, `color-saturate-name`, `color-lighten-name`, `color-blend`, `color-gradient` and the others)

## Tests

test_css_mode_mod (color.el's tests use the conversions not ported yet)

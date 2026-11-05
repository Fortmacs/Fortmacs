# css_mode_mod

- Module: css_mode_mod
- Upstream: textmodes/css-mode.el
- State: partial

## Ported

`css-mode`,
`css-base-mode` and `scss-mode` (the syntax tables, `css-mode-map` with C-c C-f and the `complete-symbol` remap, `css-syntax-propertize-function` for url(...), the Font Lock keywords of CSS and SCSS with the faces `css-selector`, `css-property` and `css-proprietary-property`, `css--fontify-region` with `css-fontify-colors`, the SMIE grammar, tokens and rules with `css-indent-offset`, `css-electric-keys`),
the identifier tables,
`css-property-alist` and `css-value-class-alist` (`css--property-values` with its cache, `css--value-class-lookup`),
completion at point (properties, values, pseudo-classes and -elements, at-rules, bang-rules, selectors with HTML tags, `css-class-list-function` and `css-id-list-function` through `css--foreign-completions`, the merged table and its `:exit-function`),
the color parsers (`css--rgb-color`, `css--hsl-color`, `css--hex-color`, `css--hex-alpha`, `css--named-color`, `css--compute-color`),
`css-cycle-color-format` and its conversions,
`css-fill-paragraph`,
`css-adaptive-fill`,
`css-current-defun-name` (with `css--prev-index-position`, `css--extract-index-name`, `css--join-nested-selectors`; CSS mode's `add-log-current-defun-function`),
`css--mdn-find-symbol`,
`css-lookup-url-format`;
auto-mode-alist entries;
Imenu (`css--prev-index-position` and `css--extract-index-name` as the buffer's functions, `imenu-space-replacement` " ") (step 49c)

## Deferred

`css-lookup-symbol` (eww),
the menu,
`css-ts-mode`,
`less-css-mode` (less-css-mode.el),
the `:company-kind` properties

## Tests

test_css_mode_mod (ports css-mode-tests: all 38, the resource files embedded and the random selector combinations made fixed; own: color fontification, filling);
test_index_menu_mod (the index)

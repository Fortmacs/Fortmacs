# font_lock_mod

- Module: font_lock_mod
- Upstream: font-lock.el
- State: partial

## Ported

the faces (font-lock-comment-face … font-lock-misc-punctuation-face),
`font-lock-maximum-decoration` (levels and t),
`font-lock-verbose`,
`font-lock-dont-widen`,
`font-lock-support-mode` (jit-lock or none),
`font-lock-extra-managed-props`,
comment delimiters in `font-lock-comment-delimiter-face` (`font-lock-comment-start-skip`, `font-lock-comment-end-skip`),
`font-lock-specified-p`,
`font-lock-initial-fontify`,
`font-lock-mode-internal`,
`font-lock-add-keywords` (mode or buffer; with `font-lock-keywords-alist`),
`font-lock-turn-on/off-thing-lock`,
`font-lock-fontify-buffer`,
`font-lock-unfontify-buffer`,
`font-lock-fontify-region`,
`font-lock-unfontify-region`,
`font-lock-flush`,
`font-lock-ensure`,
`font-lock-update` (C-x x f),
`font-lock-default-fontify-buffer`/`-unfontify-buffer`,
`font-lock-extend-region-wholelines`/`-multiline`,
`font-lock-default-fontify-region`/`-unfontify-region`,
`font-lock-after-change-function`,
`font-lock-extend-jit-lock-region-after-change`,
`font-lock-fontify-block`,
the prepend/append/fillin text property functions,
`font-lock-default-fontify-syntactically` (strings and comments via `syntax-ppss` and `parse-partial-sexp`),
`font-lock-syntactic-face-function` (a subroutine with the face as an `intent(out)` argument: gfortran 14 cannot re-export a procedure pointer to a function with a deferred-length result),
`font-lock-apply-highlight` (overrides nil, t, keep, prepend, append; LAXMATCH),
anchored keywords,
`font-lock-fontify-keywords-region`,
`font-lock-choose-keywords`,
`font-lock-set-defaults` (SYNTAX-ALIST),
`font-lock-refresh-defaults`,
`font-lock-debug-fontify`;
FACENAME (face FACE PROP t ...) as `true_properties`;
a highlight's face function is a subroutine (gfortran frees a procedure pointer component to a function with an allocatable result);
`syntax-propertize` before syntactic fontification,
`syntax-ppss-table` there,
`font-lock-extend-region-wholelines` through `syntax-propertize-wholelines`;
keyword levels given as functions (font-lock-eval-keywords) and a buffer's `font-lock-syntactic-face-function` from OTHER-VARS,
highlights of the form (face FACE PROP VAL ...) with nil properties,
a `display` string or properties that depend on the match,
`cpp-font-lock-keywords` (with `cpp-font-lock-keywords-source-directives` and `-depth`),
`font-lock-fontify-region-function` as a buffer-local procedure setting (`%procedure`, default `font_lock_default_fontify_region`; `%set_local`, `%get_current`, `%clear_local`)

## Deferred

font-lock-syntactic-keywords,
font-lock-multiline,
OTHER-VARS other than `font-lock-syntactic-face-function`,
`font-lock-remove-keywords`,
`font-lock-ignore`,
keyword forms to evaluate,
(face FACE PROP VAL…) values other than t,
the table forms of `font-lock-maximum-decoration`/`-verbose`,
the toplevel-defun keyword,
the obsolete face variables (not applicable)

## Tests

test_font_lock_mod (own)

# sgml_mode_mod

- Module: sgml_mode_mod
- Upstream: textmodes/sgml-mode.el
- State: partial

## Ported

`sgml-mode` and `html-mode` (the syntax table and `sgml-tag-syntax-table` with the paired brackets of BidiBrackets.txt, `sgml-syntax-propertize` with its rules and `sgml--syntax-propertize-ppss`, the Font Lock keywords with the `sgml-namespace` face and `html-tag-face-alist`, `sgml-font-lock-syntactic-face`, `syntax-ppss-table`, the comment and paragraph variables, `sgml-comment-indent`, `sgml-xml-guess`),
indentation (`sgml-indent-line`, `sgml-calculate-indent`, `sgml-lexical-context`, `sgml-parse-tag-backward`, `sgml-get-context`, `sgml-basic-offset`, `sgml-attribute-offset`, the empty, unclosed and whitespace-sensitive tags),
`sgml-beginning-of-tag`,
`sgml-skip-tag-backward`/`-forward` (C-c C-b, C-c C-f),
`sgml-delete-tag` (C-c C-d),
`sgml-close-tag` (C-c /),
`sgml-quote`,
`sgml-pretty-print`,
`sgml-slash` and `sgml-slash-matching`,
the character names (`sgml-char-names`, `sgml-name-char` C-c C-n, `sgml-namify-char`, `sgml-name-self`, `sgml-maybe-name-self` on the Latin-1 keys, `sgml-name-8bit-mode` C-c 8),
`sgml-maybe-end-tag`,
`sgml-validate` (C-c C-v),
`sgml-guess-indent`,
`sgml-parse-dtd`,
`html-current-defun-name` (HTML mode's `add-log-current-defun-function`),
the tag tables (`sgml-tag-alist` and `html-tag-alist` as `sgml_tags_type` tables of `sgml_tag_rule_type` and `sgml_attribute_rule_type`, `sgml-tag-help` as `sgml_tag_help_variable` and `html-tag-help`),
`sgml-tag` (C-c C-t, C-c C-o, `sgml-tag-last`, `sgml-tag-history`),
`sgml-attributes` (C-c C-a),
`sgml-auto-attributes`,
`sgml-value`,
`sgml-tag-help` (C-c ?),
the 23 `html-*` skeleton commands and their keys in `html-mode-map`,
`sgml-quick-keys` and `html-quick-keys` (their key bindings made when SGML mode first runs) with `sgml-slash`'s electric cases,
`sgml-transformation-function`,
the skeleton settings of `sgml-mode` (`skeleton-further-elements`, `skeleton-end-newline`, `skeleton-end-hook`, `skeleton-transformation-function`);
auto-mode-alist entries;
Imenu: SGML mode's `imenu-generic-expression`,
`html-imenu-index` with `html-imenu-regexp` (step 49c)

## Deferred

`html-mode--complete-at-point` (completion of tags, attributes and values at point),
the :set functions of `sgml-transformation-function` and the quick keys after SGML mode first ran (Customize),
`sgml-tags-invisible`,
`sgml-electric-tag-pair-mode`,
`sgml-show-context`,
facemenu,
tildify,
the outline variables,
`sgml-comment-indent-new-line`,
the buffer-local `adaptive-fill-regexp`,
`fill-nobreak-predicate` and `sentence-end-base`,
HTML classes and IDs (libxml),
yank-media,
`html-autoview-mode`,
`mhtml-mode`

## Tests

test_sgml_mode_mod (ports sgml-mode-tests: all 10; own: indentation, `sgml-close-tag`, `sgml-tag` and the HTML skeletons as GNU Emacs inserts them);
test_index_menu_mod (the indexes)

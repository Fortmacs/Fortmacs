# skeleton_mod

- Module: skeleton_mod
- Upstream: skeleton.el
- State: done

## Ported

skeletons as typed data (`skeleton_element_type` records: `skeleton_text`, `skeleton_literal`, `skeleton_delete`, the interactors `skeleton_prompt`, `skeleton_interactor`, `skeleton_for_each` and SKELETON_NO_INTERACTOR, the named elements SKELETON_NEWLINE, SKELETON_POINT, SKELETON_POINT_OVERRIDE, SKELETON_INDENT, SKELETON_POSITION, SKELETON_IF_MOVED, SKELETON_IF_NOT_MOVED, SKELETON_RESUME, SKELETON_STRING, SKELETON_V1, SKELETON_V2, SKELETON_END, procedures as `skeleton_action` and `skeleton_value`; see Design decisions),
`skeleton-insert` (regions of words and of the mark ring, interregions, the end newline, `skeleton-end-hook`, recentering),
`skeleton-internal-list` (string per list, the first `\n` as `>`, quitting to `resume:`, removing a sub-skeleton as far as shown),
`skeleton-internal-1` (all elements, `> \n`, `skeleton-untabify`, `skeleton-newline-indent-rigidly`, sub-skeletons repeated or once per string),
`skeleton-read` (`%s` and `skeleton-subprompt`, initial input with a position, the quit on unchanged input),
`skeleton-proxy-new` (`skeleton-autowrap`, the prefix argument, `skeleton-filter-function` over `skeleton_variants_type`),
the command of define-skeleton (`skeleton_command`, with subr.el's change groups for atomic-change-group),
`skeleton-pair-insert-maybe` (`skeleton-pair`, `skeleton-pair-on-word`, `skeleton-pair-filter-function`, `skeleton-pair-alist` as `skeleton_pairs`, `skeleton-pair-default-alist` as `skeleton_default_pairs`),
`skeleton-transformation-function`,
`skeleton-end-newline`,
`skeleton-further-elements` (a procedure binding the mode's variables),
`skeleton-positions`,
buffer-local values, as setting objects: `skeleton-transformation-function`, `skeleton-filter-function`, `skeleton-pair-filter-function` and `skeleton-further-elements` (`%procedure`), `skeleton-pair-alist` and `skeleton-pair-default-alist` (`%pairs`, `%append`); `%set_local`, `%get_current`, `%clear_local`

## Deferred

the help text while reading (`help` and minibuffer-help-form, not ported),
the no-self-insert property of skeleton commands,
`mouse-drag-region` and overwrite mode in `skeleton-pair-insert-maybe` (no mouse; overwrite mode is not ported);
not applicable: `skeleton-debug` (keeps the skeleton in a Lisp variable) and the obsolete aliases;
`skeleton-markers` is not in 31.1

## Tests

test_skeleton_mod (upstream has no tests of skeleton.el; own: the elements, the interactor, sub-skeleton loops, quitting to `resume:`, `&` and `\|`, `skeleton-positions`, the transformation, `skeleton_for_each`, regions, the change group, values and v1, paired insertion)

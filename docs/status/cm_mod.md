# cm_mod

- Module: cm_mod
- Upstream: cm.c, cm.h macros
- State: done

## Ported

`cmcostinit`,
`calccost` (incl. tabs),
`cmgoto`,
`cmcheckmagic`,
`Wcm_clear`,
`Wcm_init`,
`cmat`,
`cmplus`,
`losecursor`

## Deferred

— (where upstream aborts in `cmcheckmagic`, the cursor is forgotten)

## Tests

test_term_mod

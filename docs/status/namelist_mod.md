# namelist_mod

- Module: namelist_mod
- Upstream: (none: saved state)
- State: done

## Ported

the namelist syntax of saved state: groups,
objects with subscripts and components,
character constants,
integers and logicals,
comments;
reading (`read_namelist`, invalid-read-syntax errors with the line) and writing (`namelist_text`)

## Deferred

the `nml=` specifier (not usable: see the design decision),
repeat counts and null values

## Tests

test_namelist_mod (own: writing and reading back, the syntax, errors)

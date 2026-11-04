# config_mod

- Module: config_mod
- Upstream: (none: building the configuration)
- State: partial

## Ported

XDG directories for init.f90 (`~/.config/fortmacs`),
generations (`~/.local/state/fortmacs/generations/N`, `current`) and scratch (`~/.cache/fortmacs`);
`install_prefix` (PREFIX/bin/fortmacs next to PREFIX/lib/libfortmacs.a),
`configure_info_directory` (PREFIX/share/info/, for Info),
`driver_source` (the user build's main program),
`recompile` (gfortran via `execute_process`, errors shown, the last good build kept) into a new generation;
`generation_numbers`,
`current_generation`,
`generation_of`,
`switch_generation` (a new link renamed over `current`),
`rollback`,
`list_generations` (as Guix lists them);
fortmacs_mod's `execute_editor` handles `--recompile`,
runs the user's build from an installed fortmacs (unless the installation is newer) and reminds when init.f90 changed;
`--list-generations`,
`--rollback`,
`--switch-generation N` ("switched from generation 3 to 2")

## Deferred

`--delete-generations`;
a generation for Fortmacs itself

## Tests

test_config_mod;
tools/test-recompile.sh (installs into a scratch prefix, builds docs/init.f90 twice, lists, rolls back and switches, and checks a failed build makes no generation; run by tools/check.sh)

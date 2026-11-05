# indian_mod

- Module: indian_mod, sinhala_mod, ethiopic_mod, czech_mod, slovak_mod, tai_viet_mod, utf_8_language_mod, khmer_mod, burmese_mod, cham_mod, philippine_mod, indonesian_mod (submodules of coding_mod), and `define_X_language_environments` in the other language submodules
- Upstream: language/*.el
- State: partial

## Ported

the 109 language environments (`set-language-info-alist` as `define_language_environment`),
defined in loadup.el's order (`define_language_environments`);
Catalan's setup and exit functions

## Deferred

the other setup and exit functions (see multilingual_environment_mod),
the `features` key (Not applicable),
input methods (41i)

## Tests

test_coding_mod

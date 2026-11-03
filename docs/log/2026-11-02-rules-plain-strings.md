# 2026-11-02: rules-plain-strings

CLAUDE.md gains the string rule PORTING.md already carries since #214
(R6-5, the owner's decision): strings are plain quoted literals at call
sites, never wrapped in `string_type`; several strings go through optional
arguments, as `max`/`min` take them; a stored list of strings keeps its one
shared `string_type` internal behind procedures taking literals.

# characters_mod

- Module: characters_mod
- Upstream: international/characters.el
- State: partial

## Ported

the standard syntax table's Unicode entries in characters.el's order (punctuation, symbols, spaces, brackets, Latin-1, Ethiopic, Hebrew, Lao, Thai, Tibetan, Tai Viet, fullwidth forms; cased letters and some numerals as words),
the categories it defines that are ported,
as definitions and ranges for the standard category table: ^ (combining, incl. every Mn),
C,
H,
K,
l (Latin), | (line breakable: the CJK ranges and full-width forms) and space (for indentation, given to no character)

## Deferred

entries made through legacy charsets (`map-charset-chars` over GB2312, Big5, JIS, KS C 5601: need their mapping tables),
the other categories,
the rest of characters.el

## Tests

test_syntax_mod,
test_category_mod

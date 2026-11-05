# mail_extract_mod

- Module: mail_extract_mod
- Upstream: mail/mail-extr.el
- State: done

## Ported

`mail-extract-address-components` (`extract_mail_address_components`, a generic of a string and a buffer address, with ALL; the work buffers " *extract address components*" and " *canonical address*"; groups, ROUTE-ADDRs with routes, UUCP paths with `mail_extr_mangle_uucp`, % routing truncated at a top-level domain, the name from a quoted phrase, a phrase, a comment or the mailbox, X.400 fields, the middle-initial guess, dropping a name equal to the mailbox or of one word; the `wrong_type_argument` upstream signals for a `>` before a `,@`),
`mail-extr-voodoo` (titles, suffixes, SCA names, a last name given first, comments and nicknames, quoted initials, VMS date stamps, HZ text, ham call signs, initials, LISTSERV list names, `&` substitution, trailing comments, telephone numbers and acronyms, PARC and ADOC),
`what-domain` (the command, completing with case ignored; `what_domain` also gives the text shown),
the syntax tables, the patterns and the table of top-level domains,
the user options (`mail-extr-@-binds-tighter-than-!` is `mail_extr_at_binds_tighter_than_exclamation_mark`, pending the panel; `mail_extr_disable_voodoo` is a choice setting whose regular expression is `%regexp`)

## Deferred

names with characters of category `j` (the default `mail_extr_disable_voodoo%regexp`, "\cj", leaves them alone upstream): the standard category table has no `j` yet (characters_mod: the JIS charsets need their mapping tables), so such names (accented Latin letters, Greek, Cyrillic, kana, kanji) are prettified as other names are; the canonical address is not affected;
`mail-header-parse-address`, which the docstring points to, is not ported (mail-parse.el)

## Tests

test_mail_extract_mod (mail-extr-tests.el's `mail-extract-address-components` and `what-domain`; own tests whose expected values come from Emacs 31.1: about 150 addresses with and without ALL, the options, a buffer as the address, folded lines, the error, an unknown domain)

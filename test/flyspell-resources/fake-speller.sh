#!/bin/sh
# fake-speller.sh -- a speller for test_spellcheck_mode_mod that needs no
# dictionary.  It says it is Aspell 0.60 and speaks the Ispell pipe
# protocol (-a) and Aspell's `list' command.  It accepts every word but
# these, until one is accepted with `@' or `*':
#   helo       misspelled, corrections hello, help
#   wrld       misspelled, correction world
#   badworddd  misspelled, no corrections
#
# Copyright (C) 2026 The Fortmacs authors
#
# This file is part of Fortmacs, under the GNU GPL version 3 or later.

accepted=" "

version() {
    printf '%s\n' "@(#) International Ispell Version 3.1.20 (but really Aspell 0.60.0)"
}

# The answer for word $1, or nothing if it is correct.
answer() {
    case "$accepted" in
    *" $1 "*) return ;;
    esac
    case "$1" in
    helo) printf '& helo 2 1: hello, help\n' ;;
    wrld) printf '& wrld 1 1: world\n' ;;
    badworddd) printf '# badworddd 1\n' ;;
    esac
}

pipe() {
    version
    while IFS= read -r line; do
        case "$line" in
        '' | '+' | '-' | '!' | '%' | '#' | '~'*) ;;
        '@'* | '*'*) accepted="$accepted${line#?} " ;;
        *)
            for word in $(printf '%s\n' "${line#^}" | tr -cs 'A-Za-z' ' '); do
                a=$(answer "$word")
                if [ -n "$a" ]; then printf '%s\n' "$a"; else printf '*\n'; fi
            done
            printf '\n'
            ;;
        esac
    done
}

list() {
    tr -cs 'A-Za-z' '\n' | while IFS= read -r word; do
        [ -n "$(answer "$word")" ] && printf '%s\n' "$word"
    done
    return 0
}

mode=
for arg in "$@"; do
    case "$arg" in
    -v | -vv) version; exit 0 ;;
    -a) mode=pipe ;;
    list | -l) mode=list ;;
    esac
done

case "$mode" in
pipe) pipe ;;
list) list ;;
*) printf 'Usage: aspell [options] <command>\n'; exit 4 ;;
esac

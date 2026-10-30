#!/bin/sh
# gen-charsets.sh -- generate src/charset_maps_fm.f90 from the charset
# map sources of GNU Emacs (admin/charsets).
#
# Copyright (C) 2026 The Fortmacs authors
#
# This file is part of Fortmacs.
#
# Fortmacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Fortmacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

# Usage: tools/gen-charsets.sh [UPSTREAM-DIR]
#
# Builds the map files (etc/charsets/*.map) the charsets of
# mule-conf.el use the way
# admin/charsets/Makefile.in does, running upstream's own
# mapconv and compact.awk on the glibc charmaps and map files, and
# writes each map as Fortran ranges (from code, to code, first
# character) in file order.

set -eu

cd "$(dirname "$0")/.."
root=$(pwd)
upstream=${1:-upstream/emacs}
out=src/charset_maps_fm.f90
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

maps="8859-2 8859-3 8859-4 8859-5 8859-6 8859-7 8859-8 8859-9 8859-10 8859-11
8859-13 8859-14 8859-15 8859-16 ALTERNATIVNYJ CP1125 CP1250 CP1251 CP1252 CP1253
CP1254 CP1255 CP1256 CP1257 CP1258 CP720 CP737 CP775 CP858 EBCDICUK EBCDICUS
HP-ROMAN8 IBM038 IBM1047 IBM256 IBM273 IBM274 IBM275 IBM277 IBM278 IBM280 IBM281
IBM284 IBM285 IBM290 IBM297 IBM437 IBM850 IBM851 IBM852 IBM855 IBM857 IBM860
IBM861 IBM862 IBM863 IBM864 IBM865 IBM866 IBM869 IBM874 JISX0201 KA-ACADEMY KA-PS
KOI8-R KOI8-T KOI8-U MACINTOSH MIK NEXTSTEP PTCP154 VISCII VSCII VSCII-2 stdenc
symbol MULE-ethiopic MULE-ipa MULE-is13194 MULE-lviscii MULE-sisheng MULE-tibetan
MULE-uviscii GB2312 GBK GB180302 GB180304 BIG5 BIG5-1 BIG5-2 BIG5-HKSCS CNS-1 CNS-2
CNS-3 CNS-4 CNS-5 CNS-6 CNS-7 CNS-F JISX0208 JISX0212 JISX213A JISX2131 JISX2132
JISC6226 CP932-2BYTE KSC5601 CP949-2BYTE"

(
    cd "$upstream"
    conv() { AWK=awk admin/charsets/mapconv "$@"; }
    glibc=admin/charsets/glibc
    mapfiles=admin/charsets/mapfiles
    compact=admin/charsets/compact.awk
    tab=$(printf '\t')

    for m in $maps; do
        case $m in
        8859-*) conv "$glibc/ISO-$m.gz" "/^<.*[ $tab]\/x/" GLIBC-1 $compact ;;
        KA-*) conv "$glibc/GEORGIAN-${m#KA-}.gz" "/^<.*[ $tab]\/x/" GLIBC-1 $compact ;;
        EBCDIC*) conv "$glibc/EBCDIC-${m#EBCDIC}.gz" "/^<.*[ $tab]\/x/" GLIBC-1 $compact ;;
        VSCII) conv $glibc/TCVN5712-1.gz "/^<.*[ $tab]\/x[0-9a-f].[ $tab]/" GLIBC-1 $compact ;;
        VSCII-2)
            conv $glibc/TCVN5712-1.gz "/^<.*[ $tab]\/x[2-7a-f].[ $tab]/" GLIBC-1 $compact |
                sed 's/0x20-0x7F.*/0x00-0x7F 0x0000/'
            ;;
        ALTERNATIVNYJ | GB180304 | BIG5-1 | BIG5-2 | JISX2131) continue ;;
        CP949-2BYTE) conv $glibc/CP949.gz "/^<.*[ $tab]\/x[89a-f]/" GLIBC-2 $compact ;;
        GB2312) conv $glibc/GB2312.gz "/^<.*[ $tab]\/x[a-f]/" GLIBC-2-7 $compact ;;
        GBK) conv $glibc/GBK.gz "/^<.*[ $tab]\/x[89a-f]/" GLIBC-2 $compact ;;
        GB180302) conv $glibc/GB18030.gz "/^<.*[ $tab]\/x..\/x..[ $tab]/" GLIBC-2 admin/charsets/gb180302.awk ;;
        JISX0208) conv $glibc/EUC-JP.gz "/^<.*[ $tab]\/x[a-f]/" GLIBC-2-7 | sed 's/0x2015/0x2014/' ;;
        JISX0212) conv $glibc/EUC-JP.gz "/^<.*[ $tab]\/x8f/ s,\/x8f,," GLIBC-2-7 $compact ;;
        JISX2132) conv $glibc/EUC-JISX0213.gz "/^<.*[ $tab]\/x8f/ s,\/x8f,," GLIBC-2-7 ;;
        JISX213A) cat $mapfiles/JISX213A.map ;;
        CP932-2BYTE) conv $mapfiles/CP932.TXT '/^0x[89A-F][0-9A-F][0-9A-F]/' UNICODE2 admin/charsets/cp932.awk ;;
        JISC6226)
            # As Uni2JIS doesn't contain mappings of characters added to
            # Unicode recently, upstream adds them by hand.
            conv $mapfiles/Uni2JIS '/^[^#].*0-/' YASUOKA admin/charsets/kuten.awk | sed -e '/0x2140/s/005C/FF3C/'
            echo '0x3442 0x3D4E'
            echo '0x374E 0x25874'
            echo '0x3764 0x28EF6'
            echo '0x513D 0x2F80F'
            echo '0x7045 0x9724'
            ;;
        KSC5601) conv $glibc/EUC-KR.gz "/^<.*[ $tab]\/x[a-f]/" GLIBC-2-7 $compact ;;
        BIG5) conv $glibc/BIG5.gz "/^<.*[ $tab]\/x[a-f]/" GLIBC-2 ;;
        BIG5-HKSCS) conv $glibc/BIG5-HKSCS.gz "/^<.*[ $tab]\/x[89a-f].\//" GLIBC-2 $compact ;;
        CNS-1) conv $glibc/EUC-TW.gz "/^<.*[ $tab]\/x[a-f]/" GLIBC-2-7 $compact ;;
        CNS-[2-7]) conv $mapfiles/cns2ucsdkw.txt "/^C${m#CNS-}/" KANJI-DATABASE $compact ;;
        CNS-F) conv $glibc/EUC-TW.gz "/^<.*\/x8e\/xaf/ s,\/x8e\/xaf,," GLIBC-2-7 $compact ;;
        MIK) conv $mapfiles/bulgarian-mik.txt '1,$' CZYBORRA $compact ;;
        PTCP154) conv $mapfiles/PTCP154 '/^0x/' IANA $compact ;;
        stdenc) conv $mapfiles/stdenc.txt '/^[0-9A-Fa-f]/' UNICODE $compact ;;
        symbol) conv $mapfiles/symbol.txt '/^[0-9A-Fa-f]/' UNICODE $compact ;;
        CP720 | CP858 | MULE-*) cat $mapfiles/$m.map ;;
        JISX0201)
            conv $glibc/JIS_X0201.gz "/^<.*[ $tab]\/x[0-9]/" GLIBC-1 $compact
            echo "# Generated by hand"
            echo "0xA1-0xDF 0xFF61"
            ;;
        *) conv "$glibc/$m.gz" "/^<.*[ $tab]\/x/" GLIBC-1 $compact ;;
        esac >"$work/$m.map"
    done

    # ALTERNATIVNYJ.map is IBM866.map with some glyphs changed.
    sed -e '1 d' \
        -e '/0xF2/ s/ .*/ 0x2019/' \
        -e '/0xF3/ s/ .*/ 0x2018/' \
        -e '/0xF4/ s/ .*/ 0x0301/' \
        -e '/0xF5/ s/ .*/ 0x0300/' \
        -e '/0xF6/ s/ .*/ 0x203A/' \
        -e '/0xF7/ s/ .*/ 0x2039/' \
        -e '/0xF8/ s/ .*/ 0x2191/' \
        -e '/0xF9/ s/ .*/ 0x2193/' \
        -e '/0xFA/ s/ .*/ 0x00B1/' \
        -e '/0xFB/ s/ .*/ 0x00F7/' <"$work/IBM866.map" >"$work/ALTERNATIVNYJ.map"

    # The maps made from other maps.
    awk -f admin/charsets/gb180304.awk <"$work/GB180302.map" >"$work/GB180304.map"
    sed -n -e '/0xa140/,/0xc8fe/p' <"$work/BIG5.map" | awk -f admin/charsets/big5.awk >"$work/BIG5-1.map"
    sed -n -e '/0xc940/,$ p' <"$work/BIG5.map" | awk -f admin/charsets/big5.awk >"$work/BIG5-2.map"
    sed -n -e '/^#/d' -e 's,.*0x\([0-9A-Z]*\)$,/0x0*\1$/d,p' <$mapfiles/JISX213A.map >"$work/jisx2131-filter"
    conv $glibc/EUC-JISX0213.gz "/^<.*[ $tab]\/x[a-f]/" GLIBC-2-7 |
        sed -f "$work/jisx2131-filter" |
        sed -e 's/0x2015/0x2014/' -e 's/0x2299/0x29BF/' >"$work/JISX2131.map"
)

{
    cat <<'EOF'
! charset_maps_fm.f90 -- the charset maps.
! Generated by tools/gen-charsets.sh from admin/charsets of GNU Emacs 31.1
! (the glibc charmaps and map files, through upstream's mapconv and
! compact.awk); do not edit.
!
! Copyright (C) 2003-2026 Free Software Foundation, Inc.
! Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H13PRO009
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Fortmacs is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

!> The map files of the charsets (etc/charsets/*.map upstream) as ranges of code points with the character of their first code, in
!> the order of the file.
module charset_maps_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none (type, external)
    private

    public :: charset_map_entries

EOF
    for m in $maps; do
        awk -v name="$m" '
        function hex(s,    i, v) {
            s = toupper(s)
            sub(/^0X/, "", s)
            v = 0
            for (i = 1; i <= length(s); i++) v = v * 16 + index("0123456789ABCDEF", substr(s, i, 1)) - 1
            return v
        }
        # Arrays of at most CHUNK entries, each within the 255
        # continuation lines of a statement.
        function array(suffix, a, kind,    i, k, first, last, m, line) {
            for (k = 1; (k - 1) * CHUNK < n; k++) {
                first = (k - 1) * CHUNK + 1
                last = (k * CHUNK < n) ? k * CHUNK : n
                m = last - first + 1
                printf "    integer(%s), parameter :: %s_%s_%d(%d) = [integer(%s) :: &\n", kind, id, suffix, k, m, kind
                line = ""
                for (i = first; i <= last; i++) {
                    line = line sprintf("%.0f", a[i]) (a[i] > 2147483647 ? "_int64" : "")
                    if (i < last) line = line ", "
                    if (length(line) > 100 || i == last) {
                        sub(/ $/, "", line)
                        printf "        %s &\n", line
                        line = ""
                    }
                }
                printf "        ]\n"
            }
            nchunks = k - 1
        }
        BEGIN {
            CHUNK = 1200
            id = "map_" tolower(name)
            gsub(/-/, "_", id)
            if (id ~ /^map_[0-9]/) sub(/^map_/, "map_iso", id)
        }
        /^#/ || NF < 2 { next }
        {
            split($1, r, "-")
            n++
            from[n] = hex(r[1])
            to[n] = (2 in r) ? hex(r[2]) : from[n]
            ch[n] = hex($2)
            delete r
        }
        END {
            printf "    ! %s.map\n", name
            array("from", from, "int64")
            array("to", to, "int64")
            array("char", ch, "int32")
            printf "\n"
            print nchunks >chunkfile
        }' chunkfile="$work/$m.chunks" "$work/$m.map"
    done
    cat <<'EOF'
contains

    !> The entries of map file NAME (without ".map"): FROM(i)..TO(i) are
    !> code points mapped to CHARS(i) and the characters after it.  FOUND
    !> is false for a map that is not built in.
    subroutine charset_map_entries(name, from, to, chars, found)
        character(len=*), intent(in) :: name
        integer(int64), allocatable, intent(out) :: from(:), to(:)
        integer(int32), allocatable, intent(out) :: chars(:)
        logical, intent(out) :: found

        found = .true.

        select case (name)
EOF
    for m in $maps; do
        id=$(printf '%s' "map_$m" | tr 'A-Z-' 'a-z_' | sed 's/^map_\([0-9]\)/map_iso\1/')
        printf '        case ("%s")\n' "$m"
        for part in from to char; do
            var=$part
            [ $part = char ] && var=chars
            awk -v id="$id" -v part=$part -v var=$var -v n="$(cat "$work/$m.chunks")" 'BEGIN {
                line = "            " var " = ["
                for (k = 1; k <= n; k++) {
                    item = id "_" part "_" k
                    if (length(line) + length(item) > 110) {
                        print line "&"
                        line = "                "
                    }
                    line = line item (k < n ? ", " : "]")
                }
                print line
            }'
        done
    done
    cat <<'EOF'
        case default
            found = .false.
            allocate (from(0), to(0), chars(0))
        end select
    end subroutine charset_map_entries
end module charset_maps_fm
EOF
} >"$root/$out"

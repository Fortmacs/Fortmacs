#!/bin/sh
# fetch-upstream.sh -- fetch the GNU Emacs 31.1 reference source into upstream/.
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

# The reference tree is read-only porting material and is never committed
# (upstream/ is git-ignored).  It is a shallow checkout of tag emacs-31.1,
# verified against the pinned commit below.
#
# Usage: tools/fetch-upstream.sh
#   UPSTREAM_URL  git URL to fetch from.  Defaults to the GitHub mirror;
#                 https://git.savannah.gnu.org/git/emacs.git works too.

set -eu

TAG=emacs-31.1
# Commit that tag emacs-31.1 points to (git rev-parse emacs-31.1^{commit}).
COMMIT=a360712c9d272d950d8d8255ef74570f7e90b7d9
URL=${UPSTREAM_URL:-https://github.com/emacs-mirror/emacs}

root=$(cd "$(dirname "$0")/.." && pwd)
dest="$root/upstream/emacs"

if [ -d "$dest/.git" ]; then
    have=$(git -C "$dest" rev-parse HEAD)
    if [ "$have" = "$COMMIT" ]; then
        echo "upstream/emacs already at $TAG ($COMMIT)"
        exit 0
    fi
    echo "upstream/emacs is at $have, expected $COMMIT; remove it and rerun" >&2
    exit 1
fi

mkdir -p "$root/upstream"
git -c advice.detachedHead=false clone --quiet --depth 1 --branch "$TAG" "$URL" "$dest"

have=$(git -C "$dest" rev-parse HEAD)
if [ "$have" != "$COMMIT" ]; then
    echo "checksum mismatch: $TAG resolved to $have, expected $COMMIT" >&2
    exit 1
fi
echo "fetched $TAG ($COMMIT) into upstream/emacs"

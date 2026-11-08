#!/usr/bin/env bash
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at https://mozilla.org/MPL/2.0/.
#
# A fake test command for test/soak/check.sh: run k (counted in the file
# COUNT) prints "run k" and writes "output k" to TESTS/group/out. Run k fails
# when k is even.

set -u
count=$1 tests=$2
k=$(($(cat "$count" 2>/dev/null || echo 0) + 1))
echo "$k" >"$count"
mkdir -p "$tests/group"
echo "output $k" >"$tests/group/out"
echo "run $k"
[ $((k % 2)) = 1 ]

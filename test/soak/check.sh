#!/usr/bin/env bash
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at https://mozilla.org/MPL/2.0/.
#
# The check of tools/soak.sh (phase 5c): 4 runs of a fake command that fails
# in runs 2 and 4. Each failed run must keep its own output and its own copy
# of the test outputs that it wrote (not an older output) after the next run,
# and the soak must fail with the count of the failures.
#
#   test/soak/check.sh SOAK FAKE

set -u
soak=$1 fake=$2
fails=0
bad() {
  echo "soak check: $1"
  fails=1
}

# An output of an earlier suite run, older than the soak: no copy of it.
mkdir -p tests/other
echo stale >tests/other/out
touch -t 200001010000 tests/other/out

out=$(bash "$soak" logs 4 tests bash "$fake" count tests)
status=$?
[ "$status" = 1 ] || bad "the exit status is $status, not 1"
case $out in
  *"soak: 4 runs, 2 failed."*) ;;
  *) bad "no summary of 4 runs and 2 failures: $out" ;;
esac
run=$(ls -d logs/soak.* 2>/dev/null)
[ -n "$run" ] || bad "no directory logs/soak.*"
for k in 2 4; do
  [ "$(cat "$run/soak-$k.log" 2>/dev/null)" = "run $k" ] ||
    bad "soak-$k.log does not have the text of run $k"
  [ "$(cat "$run/soak-$k.tests/group/out" 2>/dev/null)" = "output $k" ] ||
    bad "soak-$k.tests does not have the output of run $k, with its path"
  [ ! -e "$run/soak-$k.tests/other" ] ||
    bad "soak-$k.tests has an output that the run did not write"
done
for k in 1 3; do
  [ ! -e "$run/soak-$k.log" ] || bad "a run that passed has a log soak-$k.log"
done
# An empty DIR and a bad N are refused.
bash "$soak" "" 1 tests true >/dev/null && bad "an empty DIR is accepted"
bash "$soak" logs x tests true >/dev/null && bad "N=x is accepted"
bash "$soak" logs 0 tests true >/dev/null && bad "N=0 is accepted"
exit $fails

#!/usr/bin/env bash
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at https://mozilla.org/MPL/2.0/.
#
# The soak of a flaky test (phase 5c; the plan of phase 5, F7).
#
#   tools/soak.sh DIR N TESTS COMMAND...
#
# It runs COMMAND N times, in a new directory DIR/soak.XXXXXX for each soak
# (no file of an earlier soak is removed or written over). The output of each
# run goes to run.log in that directory. A run that fails keeps its output:
# before the next run starts, run.log moves to soak-<i>.log, and the files of
# the directory TESTS (the outputs of the tests of windtrap, which the next run
# writes over) that the run wrote are copied to soak-<i>.tests, with their
# paths. A file is "of the run" if it is newer than a mark that the script
# writes before the run. At the end, it prints the count of the failures
# and the logs, and it exits with 1 if a run failed. cistern#39 lost the
# message of its one failure, because a later run wrote over the outputs.

set -u

[ $# -ge 4 ] || { echo "usage: tools/soak.sh DIR N TESTS COMMAND..."; exit 2; }
dir=$1 n=$2 tests=$3
shift 3
case $n in '' | *[!0-9]*) echo "soak: N must be a number, not '$n'"; exit 2 ;; esac
[ "$n" -ge 1 ] || { echo "soak: N must be 1 or more, not $n"; exit 2; }
[ -n "$dir" ] || { echo "soak: DIR is empty"; exit 2; }
mkdir -p "$dir" || exit 2
run=$(mktemp -d "$dir/soak.XXXXXX") || exit 2
run=$(cd "$run" && pwd) || exit 2
echo "soak: the logs go to $run"

failed=0
logs=""
i=1
while [ "$i" -le "$n" ]; do
  # The mark of the start of the run. A file system with a time step of one
  # second can lose a file of the first second, thus the run starts after a
  # new second.
  : >"$run/mark"
  sleep 1
  if "$@" >"$run/run.log" 2>&1; then
    :
  else
    failed=$((failed + 1))
    mv "$run/run.log" "$run/soak-$i.log"
    if [ -d "$tests" ]; then
      mkdir -p "$run/soak-$i.tests"
      (cd "$tests" && find . -type f -newer "$run/mark") |
        while IFS= read -r f; do
          mkdir -p "$run/soak-$i.tests/$(dirname "$f")"
          cp "$tests/$f" "$run/soak-$i.tests/$f"
        done
    fi
    logs="$logs $run/soak-$i.log"
    echo "soak: run $i of $n failed: $run/soak-$i.log"
  fi
  i=$((i + 1))
done
echo "soak: $n runs, $failed failed.${logs:+ The logs:$logs}"
[ "$failed" = 0 ]

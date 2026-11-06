#!/usr/bin/env bash
# The report of the mutation gate (make mutate, mutate-all, mutate-report).
#
#   tools/mutate-report.sh [DIFF]
#
# DIFF is the output of git diff that the run gave to the suites, or nothing
# for all the lines. The environment gives DUNE and MUTATE (lists of words),
# MIN_SCORE, and after a run of the suites, RUN_LOG (the output of the run)
# and NSUITES (the number of suites).
#
# The rules:
#
# - After a run, each suite writes a verdict file, or prints the windtrap
#   message "so the suite runs without mutation" (no mutant in its scope).
#   A suite that does neither stopped before its mutation run, for example
#   with an exit at the top level of a test file. This is a failure.
# - No verdict file at all: with a diff, a pass (the rule of weft for a
#   change with no mutant). Without a diff, a failure: the suites reached no
#   mutant.
# - A survivor is a failure. With MIN_SCORE, only a score below it is.
# - With a diff, a mutant that no test reaches, or that no test evaluated
#   ("not_evaluated"), is a failure, also with MIN_SCORE. This rule is
#   stricter than weft: a new function with no test must not pass. Add a
#   test, or [@mutate off "reason"] if no suite can reach the code.

# DUNE and MUTATE are lists of words, thus the script splits them on purpose.
# shellcheck disable=SC2086

set -u

diff=${1:-}
verdicts=$(find _build/_mutants -name '*.mutants' 2>/dev/null | wc -l)

if [ -n "${RUN_LOG:-}" ]; then
  skipped=$(grep -c 'so the suite runs without mutation' "$RUN_LOG")
  if [ $((verdicts + skipped)) -ne "$NSUITES" ]; then
    echo "mutate: of $NSUITES suites, $verdicts wrote a verdict file and $skipped had no mutant." \
      "A suite stopped before its mutation run."
    exit 1
  fi
fi

if [ "$verdicts" -eq 0 ]; then
  if [ -n "$diff" ]; then
    echo "No suite tested a mutant."
    exit 0
  fi
  echo "No suite tested a mutant. Examine the backend and SUITES."
  exit 1
fi

args=()
[ -n "$diff" ] && args+=(--diff "$diff")
[ -n "${MIN_SCORE:-}" ] && args+=(--min "$MIN_SCORE")
json=$($DUNE exec $MUTATE windtrap -- mutants "${args[@]}" --json 2>/dev/null)
status=$?

num() { printf '%s\n' "$json" | sed -n "s/.*\"$1\": *\([0-9][0-9.]*\).*/\1/p" | head -n 1; }
reached=$(num reached)
killed=$(num killed)
survived=$(num survived)
unreached=$(num unreached)
not_evaluated=$(num not_evaluated)
score=$(num score)
if [ -z "$reached" ] || [ -z "$unreached" ] || [ -z "$not_evaluated" ]; then
  echo "Cannot read the report of windtrap mutants."
  exit 1
fi

echo "mutants: $reached reached, $killed killed, $survived survived, $not_evaluated not evaluated," \
  "$unreached never reached, score $score%"
printf '%s\n' "$json" | grep -oE '"(id|verdict)": "[^"]*"' | sed 's/^"[a-z]*": "//; s/"$//' |
  paste -d ' ' - - |
  awk '$2 == "survived" { print "  SURVIVED       " $1 } $2 == "unreached" { print "  NEVER REACHED  " $1 }'

[ "$status" -eq 0 ] || exit 1
if [ -n "$diff" ] && [ "$not_evaluated" != 0 ]; then
  echo "$not_evaluated mutants on the changed lines are not evaluated. Examine the report of windtrap."
  exit 1
fi
if [ -n "$diff" ] && [ "$unreached" != 0 ]; then
  echo "$unreached mutants on the changed lines are never reached." \
    "Add a test, or [@mutate off \"reason\"] if no suite can reach the code."
  exit 1
fi

#!/usr/bin/env bash
# The scope of the mutation gate (make check-scope, DESIGN.md "Tests").
#
#   tools/check-scope.sh SUITE...
#
# Rule 1: each suite has exactly one stanza (test (name X) ...), which links
# with -linkall and has no action field. Then the catalogue of the suite has
# each module of each library that it links, also a module that no test
# references. windtrap writes one verdict file for each test executable, and
# the report (tools/mutate-report.sh) counts one executable for each suite.
# An action field could print the windtrap message of a suite with no
# mutant, thus the check refuses it.
#
# Rule 2: each dune file outside test/ has one library stanza, with a
# public_name and the mutation backend. If the directory has a .ml file, a
# test stanza of a suite has the library in its libraries.
#
# The check fails closed: a dune file that it cannot read gives a failure,
# not a pass. It refuses enabled_if, build_if and include_subdirs in these
# dune files, and block comments (#| |#) and datum comments (#;), which the
# reader below does not know. It fails if git cannot list the files. Since
# phase 3a, ppx/ follows rule 2 too: the suite test/ppx links ppx_cistern.

set -u

status=0
fail() {
  echo "check-scope: $1"
  status=1
}

# The top-level stanzas of a dune file, one on each line, with no comments,
# with each run of white space as one space.
stanzas() {
  awk '
    {
      line = $0
      out = ""
      for (i = 1; i <= length(line); i++) {
        c = substr(line, i, 1)
        if (instr) {
          out = out c
          if (esc) esc = 0
          else if (c == "\\") esc = 1
          else if (c == "\"") instr = 0
          continue
        }
        if (c == ";") break
        if (c == "\"") { instr = 1; out = out c; continue }
        if (c == "(") depth++
        if (c == ")") depth--
        out = out c
        if (depth == 0 && c == ")") { buf = buf out; print buf; buf = ""; out = "" }
      }
      buf = buf out " "
    }
  ' "$1" | tr -s ' \t' '  ' | sed 's/^ //; s/( /(/g; s/ )/)/g'
}

# The values of a field of a stanza, for example "(libraries a b)".
field() {
  printf '%s\n' "$1" | sed -n "s/.*($2 \\([^()]*\\)).*/\\1/p"
}

top=$(git rev-parse --show-toplevel) || { echo "check-scope: git cannot find the repository"; exit 1; }
cd "$top" || exit 1
files=$(git ls-files --cached --others --exclude-standard -- '*dune') ||
  { echo "check-scope: git cannot list the files"; exit 1; }

# A stanza that dune can skip or extend hides a library or a suite from this
# check. Thus the check refuses it.
unsupported() {
  printf '%s\n' "$1" | grep -Eq '\((enabled_if|build_if) |^\(include_subdirs '
}

# A comment form that the reader of stanzas does not know.
unknown_comment() {
  grep -Fq -e '#|' -e '#;' "$1"
}

linked=""
for s in "$@"; do
  d=$s/dune
  [ -f "$d" ] || { fail "cannot read $d"; continue; }
  if unknown_comment "$d"; then fail "cannot read $d: a #| or #; comment"; continue; fi
  all=$(stanzas "$d")
  if unsupported "$all"; then fail "cannot read $d: enabled_if, build_if or include_subdirs"; continue; fi
  if printf '%s\n' "$all" | grep -q '^(tests '; then fail "cannot read $d: a tests stanza"; continue; fi
  n=$(printf '%s\n' "$all" | grep -c '^(test ')
  [ "$n" = 1 ] || { fail "cannot read $d: $n test stanzas, not 1"; continue; }
  t=$(printf '%s\n' "$all" | grep '^(test ')
  [ -n "$(field "$t" name)" ] || fail "cannot read $d: the test stanza has no name"
  if printf '%s\n' "$t" | grep -q '(action '; then fail "cannot read $d: the test stanza has an action"; fi
  printf '%s\n' "$t" | grep -q '(link_flags ([^()]*-linkall[^()]*))' ||
    fail "the test stanza of $d does not link with -linkall"
  linked="$linked $(field "$t" libraries)"
done

seen=""
for d in $files; do
  case $d in test/*) continue ;; esac
  seen=1
  dir=$(dirname "$d")
  if unknown_comment "$d"; then fail "cannot read $d: a #| or #; comment"; continue; fi
  all=$(stanzas "$d")
  if unsupported "$all"; then fail "cannot read $d: enabled_if, build_if or include_subdirs"; continue; fi
  n=$(printf '%s\n' "$all" | grep -c '^(library ')
  [ "$n" = 1 ] || { fail "cannot read $d: $n library stanzas, not 1"; continue; }
  lib=$(printf '%s\n' "$all" | grep '^(library ')
  pub=$(field "$lib" public_name)
  [ -n "$pub" ] || { fail "cannot read $d: no public_name"; continue; }
  printf '%s\n' "$lib" | grep -q '(instrumentation (backend ppx_windtrap.mutate))' ||
    fail "$d: $pub has no mutation backend"
  [ -n "$(find "$dir" -maxdepth 1 \( -name '*.ml' -o -name '*.mll' -o -name '*.mly' \))" ] || continue
  found=""
  for l in $linked; do [ "$l" = "$pub" ] && found=1; done
  [ -n "$found" ] || fail "no suite of SUITES links $pub"
done
[ -n "$seen" ] || fail "no dune file outside test/"

exit $status

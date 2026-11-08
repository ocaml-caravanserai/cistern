#!/usr/bin/env bash
# This Source Code Form is subject to the terms of the Mozilla Public License,
# v. 2.0. If a copy of the MPL was not distributed with this file, You can
# obtain one at https://mozilla.org/MPL/2.0/.
#
# The check of the pins of cistern.opam.template (phase 5d; the plan of
# phase 5, F8). The template must have exactly one pin-depends line for each
# of the packages weft, weft-tls, windtrap and ppx_windtrap, and no other pin:
#   ["<package>.dev" "git+https://github.com/<owner>/<repo>#<commit>"]
# The repo of weft and weft-tls is the flake input weft, and the repo of
# windtrap and ppx_windtrap is the flake input windtrap. The owner, the repo
# and the commit must be those of that input in flake.lock. Thus an opam user
# builds against the weft and windtrap of the flake. It reads only the lines
# of the block pin-depends: [ ... ], with no comment lines. The check reads
# flake.lock with nix (no jq). It fails closed: a line with a git URL that it
# cannot read gives a failure.
#
#   tools/check-pins.sh [TEMPLATE] [LOCK]

set -u
template=${1:-cistern.opam.template}
lock=${2:-flake.lock}
[ -f "$template" ] || { echo "check-pins: no $template"; exit 1; }
[ -f "$lock" ] || { echo "check-pins: no $lock"; exit 1; }
lockpath=$(cd "$(dirname "$lock")" && pwd)/$(basename "$lock")

# The flake input of each package.
input_of() {
  case $1 in
    weft | weft-tls) echo weft ;;
    windtrap | ppx_windtrap) echo windtrap ;;
    *) echo "" ;;
  esac
}

# "<owner>/<repo>#<commit>" of the flake input [input] in flake.lock.
locked() {
  nix eval --raw --impure --expr "
    let l = builtins.fromJSON (builtins.readFile $lockpath);
        i = l.nodes.root.inputs;
        n = l.nodes.\${i.\"$1\"}.locked;
    in if i ? \"$1\" then n.owner + \"/\" + n.repo + \"#\" + n.rev else \"none\""
}

status=0
seen=" "
block=0
while IFS= read -r line; do
  # Only the lines of the block pin-depends: [ ... ] count, and a comment
  # (a # after the blanks) does not count.
  trimmed=$(printf '%s' "$line" | sed 's/^[[:space:]]*//; s/[[:space:]]*$//')
  case $trimmed in
    '#'*) continue ;;
    pin-depends:*) block=1; continue ;;
    ']') block=0; continue ;;
  esac
  [ "$block" = 1 ] || continue
  case $line in
    *git+*) ;;
    *) continue ;;
  esac
  # The package and the URL of the line.
  rest=${line#*\[\"}
  pkg=${rest%%\"*}
  case $pkg in
    *.dev) pkg=${pkg%.dev} ;;
    *) echo "check-pins: not a <package>.dev pin: $line"; status=1; continue ;;
  esac
  url=${line#*\"git+https://github.com/}
  [ "$url" != "$line" ] || { echo "check-pins: not a GitHub git URL: $line"; status=1; continue; }
  url=${url%%\"*}
  commit=${url#*#}
  if [ ${#commit} != 40 ] || [ -n "$(printf '%s' "$commit" | tr -d '0-9a-f')" ]; then
    echo "check-pins: no commit of 40 hex digits: $line"
    status=1
    continue
  fi
  input=$(input_of "$pkg")
  [ -n "$input" ] || { echo "check-pins: a pin of another package: $pkg"; status=1; continue; }
  case $seen in
    *" $pkg "*) echo "check-pins: two pins of $pkg"; status=1; continue ;;
  esac
  seen="$seen$pkg "
  want=$(locked "$input") || { echo "check-pins: nix cannot read $lock"; exit 1; }
  if [ "$want" != "$url" ]; then
    echo "check-pins: $pkg is pinned at $url, but the input $input of $lock is $want"
    status=1
  fi
done <"$template"
for p in weft weft-tls windtrap ppx_windtrap; do
  case $seen in
    *" $p "*) ;;
    *) echo "check-pins: no pin of $p"; status=1 ;;
  esac
done
[ "$status" = 0 ] && echo "check-pins: the 4 pins agree with $lock"
exit $status

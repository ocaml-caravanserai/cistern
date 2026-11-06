# Mutation testing with windtrap (DESIGN.md, "Tests").
#
#   make mutate         Test the mutants on the lines that changed since BASE.
#   make mutate-all     Test all the mutants that the suites reach.
#   make mutate-report  Merge the verdicts of the last run of mutate or
#                       mutate-all, and list the survivors on the lines that
#                       changed since BASE. Give BASE= for all the lines.
#   make check-ocaml55  The OCaml 5.5 check of test/ocaml55/dune.
#
# The variables:
#
#   BASE       The git revision of the changed lines. The default is the
#              merge base with origin/main. make mutate refuses an empty BASE.
#   MIN_SCORE  If it is set, the report gives exit status 1 only when the
#              score is less than MIN_SCORE, and not for each survivor.
#   SUITES     The suites of the run (default: all the suites of the gate).
#
# Outside the dev shell, each command runs in `nix develop`.

BASE ?= $(shell git merge-base origin/main HEAD 2>/dev/null)

DUNE := $(if $(IN_NIX_SHELL),dune,nix develop -c dune)
MUTATE := --instrument-with ppx_windtrap.mutate

# The suites of the gate. They use no database server.
SUITES := test/ocaml55

# The changed lines since BASE, as the output of `git diff -U0`. windtrap
# needs an absolute path, because each suite runs in its own directory of
# _build. The options of GIT_DIFF fix the output of git, because some git
# configurations and attributes hide lines from windtrap: diff.mnemonicPrefix
# (other prefixes), color.diff (color codes), diff.renames (a moved file gives
# no lines), and the attribute -diff ("Binary files differ").
#
# The limits of the diff:
#
# - A hunk that only deletes lines names no line, thus it has no mutant to
#   test. A deleted line can remove the test of a condition. Only
#   make mutate-all finds such a survivor.
# - A change of a test file only has no mutant. A test that becomes weaker
#   is thus not found. make mutate-all finds the survivor.
# - A file that git does not track is not in the diff. make mutate gives a
#   warning for each such .ml file.
# - A changed library .ml that no suite of SUITES links passes. PR 0b-1 adds
#   the scope check.
DIFF := $(CURDIR)/.mutate.diff
GIT_DIFF := git diff --text --no-renames --no-color --no-ext-diff --no-textconv \
  --no-relative --src-prefix=a/ --dst-prefix=b/ -U0

# The report of `windtrap mutants`. $(1) is the diff file, or empty for all
# the lines. A suite that tests no mutant writes no verdict file, and then
# `windtrap mutants` gives an error. With a diff, no verdict file is a pass,
# as the rule of weft for a change with no mutant. Without a diff
# (mutate-all), it is a failure, because the suites reached no mutant.
report = if find _build/_mutants -name '*.mutants' 2>/dev/null | grep -q .; then \
  $(DUNE) exec $(MUTATE) windtrap -- mutants \
    $(if $(1),--diff $(1)) $(if $(MIN_SCORE),--min $(MIN_SCORE)); \
  else $(if $(1),echo "No suite tested a mutant.", \
    echo "No suite tested a mutant. Examine the backend and SUITES."; exit 1); fi

# The run of the suites. $(1) is the diff file, or empty for all the lines.
# A suite with a survivor gives exit status 1, thus the run ignores the
# status, and the report gives the result.
define run_suites
-WINDTRAP_MUTATE=1 $(if $(1),WINDTRAP_DIFF=$(1)) \
  $(DUNE) runtest --force $(MUTATE) $(SUITES)
endef

# The lines of describe, count and sum_lengths in test/ocaml55/ocaml55.ml,
# and the ids of the mutants that the backend puts on them.
OCAML55_LINES := test/ocaml55/ocaml55.ml:19-28
OCAML55_MUTANTS := \
  test/ocaml55/ocaml55.ml:22:5:neq \
  test/ocaml55/ocaml55.ml:26:32:not \
  test/ocaml55/ocaml55.ml:26:49:sub \
  test/ocaml55/ocaml55.ml:26:63:int \
  test/ocaml55/ocaml55.ml:28:49:sub

.PHONY: help check-windtrap mutate mutate-all mutate-report check-ocaml55

help:
	@sed -n '1,/^$$/p' Makefile | sed 's/^# \{0,1\}//'

# A dev shell that started before a change of flake.nix keeps its old
# windtrap. The windtrap of nixpkgs has no --diff.
check-windtrap:
	@$(DUNE) exec $(MUTATE) windtrap -- mutants --help 2>/dev/null | grep -q -- --diff || { \
	  echo "The windtrap of this shell is not the windtrap of flake.nix."; \
	  echo "Start nix develop again, or run direnv reload."; exit 2; }

# The build comes first, thus a compile error stops the target. The run
# removes the old verdict files. CI runs this target on each pull request.
mutate: check-windtrap
	@test -n "$(BASE)" || { \
	  echo "BASE is empty. Give BASE=<revision>, for example BASE=origin/main."; exit 2; }
	@untracked=$$(git ls-files --others --exclude-standard -- '*.ml'); \
	  if [ -n "$$untracked" ]; then \
	    echo "Warning: git does not track these files, thus the gate does not test them:"; \
	    echo "$$untracked"; fi
	$(DUNE) build $(MUTATE) $(SUITES:%=@%/all)
	rm -rf _build/_mutants
	$(GIT_DIFF) $(BASE) > $(DIFF)
	$(call run_suites,$(DIFF))
	$(call report,$(DIFF))

mutate-all: check-windtrap
	$(DUNE) build $(MUTATE) $(SUITES:%=@%/all)
	rm -rf _build/_mutants
	$(call run_suites,)
	$(call report,)

mutate-report: check-windtrap
	$(if $(BASE),$(GIT_DIFF) $(BASE) > $(DIFF))
	$(call report,$(if $(BASE),$(DIFF)))

check-ocaml55: check-windtrap
	$(DUNE) build $(MUTATE) @test/ocaml55/all
	rm -rf _build/_mutants
	-WINDTRAP_MUTATE=1 WINDTRAP_LINES=$(OCAML55_LINES) \
	  $(DUNE) runtest --force $(MUTATE) test/ocaml55
	@got=$$($(DUNE) exec $(MUTATE) windtrap -- mutants --lines $(OCAML55_LINES) --json 2>&1 \
	    | grep -oE '"(id|verdict)": "[^"]*"' | sed 's/^"[a-z]*": "//; s/"$$//' \
	    | paste -d ' ' - - | sort); \
	  want=$$(printf '%s killed\n' $(OCAML55_MUTANTS) | sort); \
	  if [ "$$got" = "$$want" ]; then echo "check-ocaml55: the expected mutants, all killed."; \
	  else echo "check-ocaml55: expected these mutants, all killed:"; echo "$$want"; \
	    echo "The report has:"; echo "$${got:-(no mutant)}"; exit 1; fi

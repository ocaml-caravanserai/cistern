# Mutation testing with windtrap (DESIGN.md, "Tests"), and the test servers
# (DESIGN.md, "Database servers for the tests").
#
#   make mutate         Test the mutants on the lines that changed since BASE.
#   make mutate-all     Test all the mutants that the suites reach.
#   make mutate-report  Merge the verdicts of the last run of mutate or
#                       mutate-all, and list the survivors on the lines that
#                       changed since BASE. Give BASE= for all the lines.
#   make mutate-arm MUTANT=core/decimal.ml:21:43:int
#                       Run SUITE with one mutant armed.
#   make check-ocaml55  The OCaml 5.5 check of test/ocaml55/dune.
#   make check-scope    The scope of the gate (tools/check-scope.sh).
#   make doc            The odoc of the packages. Each warning of odoc is
#                       an error (the file dune at the root). CI runs it.
#   make sim-slow       The slow tests: the storm of 1000 fibers of the sim
#                       suite, and random seeds in the stateful suite
#                       (CISTERN_SLOW). Not in the gate.
#   make db-up SERVER=mysql84|mariadb114|mariadb118
#                       Start a test server with a new data directory in
#                       DB_DIR (default .db). DB_PORT changes the port,
#                       DB_SERVER_ARGS adds server options.
#   make db-check SERVER=...
#                       The smoke check of a test server that runs.
#   make db-down [SERVER=...]
#                       Stop SERVER, or each test server that runs.
#   make integration SERVER=...
#                       The integration suite against a test server that
#                       runs (make db-up first), with the deterministic
#                       claims.
#   make integration-timing SERVER=...
#                       The timing claims (C8a, C8b, C9).
#   make integration-shutdown SERVER=...
#                       The claim C8c. It stops the server.
#   make db-errors      Compare the names of test/db/errors.tsv with perror
#                       of the server packages (no server runs).
#
# The variables:
#
#   BASE       The git revision of the changed lines. The default is the
#              merge base with origin/main. make mutate refuses an empty BASE.
#   MIN_SCORE  If it is set, the report gives exit status 1 only when the
#              score is less than MIN_SCORE, and not for each survivor. It
#              does not change the rule for unreached mutants.
#   SUITES     The suites of the run (default: all the suites of the gate).
#   SUITE      The suite of mutate-arm (default test/unit/test_unit.exe).
#
# Outside the dev shell, each command runs in `nix develop`.

BASE ?= $(shell git merge-base origin/main HEAD 2>/dev/null)
SUITE ?= test/unit/test_unit.exe

DUNE := $(if $(IN_NIX_SHELL),dune,nix develop -c dune)
MUTATE := --instrument-with ppx_windtrap.mutate

# The suites of the gate. They use no database server.
SUITES := test/unit test/prop test/expect test/ppx test/sim test/stateful test/ocaml55

# The changed lines since BASE, as the output of `git diff -U0`. windtrap
# needs an absolute path, because each suite runs in its own directory of
# _build. The output of git diff can hide lines from windtrap: with
# diff.mnemonicPrefix (other prefixes), color.diff (color codes),
# diff.renames (a moved file gives no lines) or the attribute -diff ("Binary
# files differ"). With the options of GIT_DIFF, these settings do not change
# the output.
#
# The limits of the diff:
#
# - A hunk that only deletes lines names no line, thus it has no mutant to
#   test. A deleted line can remove the test of a condition. Only
#   make mutate-all finds such a survivor.
# - A change of a test file only has no mutant. A test that becomes weaker
#   is thus not found. make mutate-all finds the survivor.
# - A moved file (git mv) gives all its lines as added lines, because of
#   --no-renames. Thus the gate tests each mutant of the moved file.
# - A file that git does not track is not in the diff. make mutate gives a
#   warning for each such .ml file.
# - A mutant on a site that only the initialization of a module evaluates
#   (for example a top-level constant) is "evaluated outside tests". windtrap
#   tests no such mutant, and the gate does not fail for it, as in weft: each
#   test depends on the value of the site, but no test can run with and
#   without the mutant. Thus keep each limit constant inside the function
#   that uses it, and call library code only inside a test, never at the top
#   level of a test file.
DIFF := $(CURDIR)/.mutate.diff
GIT_DIFF := git diff --text --no-renames --no-color --no-ext-diff --no-textconv \
  --no-relative --src-prefix=a/ --dst-prefix=b/ -U0

# The output of the last mutation run, for the report (tools/mutate-report.sh).
LOG := $(CURDIR)/.mutate.log

# The run of the suites. $(1) is the diff file, or empty for all the lines.
# First the instrumented suites run with no mutant armed: a test that fails,
# or a suite that crashes, stops the target. Then each suite runs with its
# mutants. A suite with a survivor gives exit status 1, thus this run
# ignores the status, and the report gives the result.
define run_suites
$(DUNE) runtest --force $(MUTATE) $(SUITES)
-WINDTRAP_MUTATE=1 $(if $(1),WINDTRAP_DIFF=$(1)) \
  $(DUNE) runtest --force $(MUTATE) $(SUITES) 2>&1 | tee $(LOG)
endef

# The report. $(1) is the diff file, or empty for all the lines. $(2) is
# empty, or the variables of a fresh run (the rules are in the script).
report = DUNE="$(DUNE)" MUTATE="$(MUTATE)" MIN_SCORE="$(MIN_SCORE)" $(2) \
  tools/mutate-report.sh $(1)
run_vars = RUN_LOG=$(LOG) NSUITES=$(words $(SUITES))

# The lines of describe, count and sum_lengths in test/ocaml55/ocaml55.ml,
# and the ids of the mutants that the backend puts on them.
OCAML55_LINES := test/ocaml55/ocaml55.ml:19-28
OCAML55_MUTANTS := \
  test/ocaml55/ocaml55.ml:22:5:neq \
  test/ocaml55/ocaml55.ml:26:32:not \
  test/ocaml55/ocaml55.ml:26:49:sub \
  test/ocaml55/ocaml55.ml:26:63:int \
  test/ocaml55/ocaml55.ml:28:49:sub

.PHONY: help check-windtrap check-scope mutate mutate-all mutate-report \
  mutate-arm check-ocaml55 doc sim-slow db-up db-check db-down db-errors integration \
  integration-timing integration-shutdown

help:
	@sed -n '1,/^$$/p' Makefile | sed 's/^# \{0,1\}//'

# A dev shell that started before a change of flake.nix keeps its old
# windtrap. The windtrap of nixpkgs has no --diff.
check-windtrap:
	@$(DUNE) exec $(MUTATE) windtrap -- mutants --help 2>/dev/null | grep -q -- --diff || { \
	  echo "The windtrap of this shell is not the windtrap of flake.nix."; \
	  echo "Start nix develop again, or run direnv reload."; exit 2; }

check-scope:
	@tools/check-scope.sh $(SUITES)

# The odoc of the packages (phase 5a). The env stanza of the file dune at the
# root makes each warning of odoc an error.
doc:
	$(DUNE) build @doc

# The build comes first, thus a compile error stops the target. The run
# removes the old verdict files. CI runs this target on each pull request.
mutate: check-windtrap check-scope
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
	@$(call report,$(DIFF),$(run_vars))

mutate-all: check-windtrap check-scope
	$(DUNE) build $(MUTATE) $(SUITES:%=@%/all)
	rm -rf _build/_mutants
	$(call run_suites,)
	@$(call report,,$(run_vars))

mutate-report: check-windtrap
	$(if $(BASE),$(GIT_DIFF) $(BASE) > $(DIFF))
	@$(call report,$(if $(BASE),$(DIFF)),)

mutate-arm:
	@test -n "$(MUTANT)" || { echo "Give MUTANT, for example MUTANT=core/decimal.ml:21:43:int"; exit 2; }
	$(DUNE) exec $(MUTATE) $(SUITE) -- --arm $(MUTANT)

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

# The slow tests. CISTERN_SLOW is a dependency of the two test stanzas, thus
# dune runs them again when it changes. --force runs them each time, so that
# the stateful suite gets a new random seed. ci.yml and db.yml run it.
sim-slow:
	CISTERN_SLOW=1 $(DUNE) build --force @test/sim/runtest @test/stateful/runtest

# The test servers (test/db/db.sh). They run outside the dev shell too.
export DB_DIR DB_PORT DB_SERVER_ARGS

db-up:
	@test -n "$(SERVER)" || { echo "Give SERVER=mysql84, mariadb114 or mariadb118."; exit 2; }
	DUNE="$(DUNE)" sh test/db/db.sh up $(SERVER)

db-check:
	@test -n "$(SERVER)" || { echo "Give SERVER=mysql84, mariadb114 or mariadb118."; exit 2; }
	sh test/db/db.sh check $(SERVER)

db-down:
	sh test/db/db.sh down $(SERVER)

db-errors:
	sh test/db/db.sh errors

# The integration suite (test/integration). The executable gets the absolute
# path of the env file of the server, and runs each time: it is not a dune
# rule that a cache can skip. The shell makes the path absolute, so that a
# DB_DIR with a space works.
#
# integration runs the deterministic group (with the deterministic claims).
# integration-timing runs the timing claims, and integration-shutdown the
# claim that stops the server (make db-up after it). The nightly db.yml runs
# the two, never the job of a pull request.
integration: GROUP = deterministic
integration-timing: GROUP = timing
integration-shutdown: GROUP = shutdown
integration integration-timing integration-shutdown:
	@test -n "$(SERVER)" || { echo "Give SERVER=mysql84, mariadb114 or mariadb118."; exit 2; }
	$(DUNE) build test/integration/integration.exe
	@dir="$(or $(DB_DIR),.db)"; \
	case "$$dir" in /*) ;; *) dir="$$(pwd)/$$dir" ;; esac; \
	echo "CISTERN_DB_ENV=$$dir/$(SERVER)/env CISTERN_GROUP=$(GROUP)"; \
	CISTERN_DB_ENV="$$dir/$(SERVER)/env" CISTERN_GROUP=$(GROUP) \
	  ./_build/default/test/integration/integration.exe

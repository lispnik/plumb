# plumb -- build, test, run.
#
# The binary is dumped by ASDF's PROGRAM-OP (see build.lisp and the "plumb/cli"
# system in plumb.asd), and includes every optional system -- plumb/json and
# plumb/crypto -- so a `plumb` on your PATH has all the built-ins.
#
# The CORE has no external dependencies, which is why `make test` needs nothing
# outside SBCL while the binary needs the vendored tree.  Every target runs with
# --no-userinit for a reproducible environment.

SBCL    ?= sbcl
ROOT    := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
BIN     := bin/plumb
SOURCES := plumb.asd build.lisp $(wildcard src/*.lisp)

# Single quotes cannot appear inside the --eval arguments below, hence (quote ...).
#
# ocicl/ is jzon, cl-csv, Ironclad and their dependencies, vendored here and
# pinned by ocicl.csv.  Listed BEFORE :inherit-configuration so it wins.
#
# That ordering is not enough on its own: the tree must also be COMPLETE.  Twice
# now a build has worked here and nowhere else, because ASDF quietly satisfied a
# missing transitive dependency out of a neighbouring project under the user's
# own (:tree "~/Projects/common-lisp/") -- Ironclad the first time, cl-ppcre the
# second.  `make check-vendored` proves the tree stands alone.
# ARP-SCAN is a sibling checkout, not a vendored library: it is under active
# development, so a pinned copy here would mean maintaining two.  Named
# explicitly rather than left to the user's own source registry, so the one
# unvendored dependency is visible -- CHECK-VENDORED excludes plumb/arp for
# exactly this reason.
ARPSCAN  ?= $(HOME)/Projects/common-lisp/arp-scan/
REGISTRY := (asdf:initialize-source-registry (quote (:source-registry (:directory "$(ROOT)") (:tree "$(ROOT)ocicl/") (:directory "$(ARPSCAN)") (:tree "$(ARPSCAN)ocicl/") :inherit-configuration)))
LISP     := $(SBCL) --noinform --non-interactive --no-userinit --eval "(require :asdf)" --eval '$(REGISTRY)'

.PHONY: all build test test-crypto test-json test-csv test-sql test-arp check-vendored demo repl clean help deps

# Stated rather than left to ordering.  DEPS is defined below but before ALL,
# and make takes the FIRST target in the file as its default goal -- so a bare
# `make' checked for ocicl/ and exited 0 having built nothing, while `make demo'
# and `make test' kept working because they name their target.  A build command
# that silently does nothing is worse than one that fails.
.DEFAULT_GOAL := all

# The optional systems need the vendored tree.  ocicl.csv is committed and
# ocicl/ is not, so a fresh clone has to restore it -- and should be told so
# plainly rather than meeting an ASDF "component not found" backtrace.
deps:
	@if ! ls ocicl 2>/dev/null | grep -q .; then \
	  echo "plumb needs its vendored dependencies."; \
	  echo "Run:  ocicl install"; \
	  echo "(ocicl.csv pins the exact versions; the tree itself is gitignored.)"; \
	  exit 1; \
	fi

all: build

build: $(BIN)

# One binary, with every optional system in it.  There used to be two flavours
# writing the same path, which needed marker files under bin/ so that `make`,
# `make demo` and `make crypto` could not silently hand you the wrong one.  With
# a single flavour that whole problem is gone.
#
# $(BIN) is removed first: PROGRAM-OP compares its output against its inputs
# like any other ASDF operation, so a binary newer than the sources makes
# ASDF:MAKE a no-op and the build would report success without rebuilding.
$(BIN): $(SOURCES) | deps
	@mkdir -p $(dir $(BIN))
	@rm -f $(BIN)
	@PLUMB_ARPSCAN=$(ARPSCAN) $(SBCL) --script build.lisp
	@echo "built $(BIN) ($$(du -h $(BIN) | cut -f1)) -- $$($(BIN) --version)"

test: | deps
	@$(LISP) --eval '(asdf:test-system "plumb")'

test-crypto: | deps
	@$(LISP) --eval '(asdf:test-system "plumb/crypto")'

test-json: | deps
	@$(LISP) --eval '(asdf:test-system "plumb/json")'

test-csv: | deps
	@$(LISP) --eval '(asdf:test-system "plumb/csv")'

test-sql: | deps
	@$(LISP) --eval '(asdf:test-system "plumb/sql")'

test-arp: | deps
	@$(LISP) --eval '(asdf:test-system "plumb/arp")'

# Loads every optional system with the user's own registry switched OFF, so a
# dependency that is only satisfied by some other checkout on this machine
# fails here rather than on someone else's.
check-vendored: | deps
	@$(SBCL) --noinform --non-interactive --no-userinit --eval "(require :asdf)" \
	  --eval '(asdf:initialize-source-registry (quote (:source-registry (:directory "$(ROOT)") (:tree "$(ROOT)ocicl/") :ignore-inherited-configuration)))' \
	  --eval '(handler-bind ((warning (function muffle-warning))) (dolist (s (list "plumb/json" "plumb/csv" "plumb/sql" "plumb/crypto")) (asdf:load-system s)))' \
	  --eval '(format t "~&vendored tree is self-contained~%")'

# The last section runs bin/plumb as a subprocess, so there has to be one.
demo:
	@test -x $(BIN) || $(MAKE) build
	@$(SBCL) --script demo.lisp

# An interactive plumb prompt without dumping a binary first.
repl:
	@$(SBCL) --noinform --no-userinit --eval "(require :asdf)" --eval '$(REGISTRY)' \
		--eval '(handler-bind ((warning (function muffle-warning))) (asdf:load-system "plumb/cli"))' \
		--eval "(plumb.cli:repl)" --quit

clean:
	rm -rf bin

help:
	@echo "make build   dump $(BIN) (default)"
	@echo "make test    run the test suite"
	@echo "make test-crypto  run the digest tests"
	@echo "make test-json    run the JSON tests"
	@echo "make test-csv     run the CSV tests"
	@echo "make test-sql     run the SQL tests"
	@echo "make test-arp     run the ARP tests (needs the arp-scan checkout)"
	@echo "make check-vendored  prove ocicl/ stands alone, with no inherited registry"
	@echo "make demo    sbcl --script demo.lisp"
	@echo "make repl    interactive plumb prompt, no binary needed"
	@echo "make clean   remove bin/"

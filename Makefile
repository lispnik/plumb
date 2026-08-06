# plumb -- build, test, run.
#
# The binary is dumped by ASDF's PROGRAM-OP (see build.lisp and the "plumb/cli"
# system in plumb.asd).  SBCL only; there are no external dependencies, so
# every target runs with --no-userinit for a reproducible environment.

SBCL    ?= sbcl
ROOT    := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
BIN     := bin/plumb
SOURCES := plumb.asd build.lisp $(wildcard src/*.lisp)

# Single quotes cannot appear inside the --eval arguments below, hence (quote ...).
#
# ocicl/ is Ironclad and its dependencies, vendored in this repository and
# pinned by ocicl.csv.  It is listed BEFORE :inherit-configuration so it wins:
# `make crypto` used to resolve Ironclad out of whatever neighbouring project
# the user's own source-registry happened to point at, which is not a build.
REGISTRY := (asdf:initialize-source-registry (quote (:source-registry (:directory "$(ROOT)") (:tree "$(ROOT)ocicl/") :inherit-configuration)))
LISP     := $(SBCL) --noinform --non-interactive --no-userinit --eval "(require :asdf)" --eval '$(REGISTRY)'

.PHONY: all build crypto test test-crypto demo repl clean help deps

# The core now uses com.inuoe.jzon, vendored under ocicl/ and pinned by the
# committed ocicl.csv.  The tree itself is NOT committed, so a fresh clone has
# to restore it -- and should be told so plainly rather than meeting an ASDF
# "component not found" backtrace.
deps:
	@if ! ls ocicl 2>/dev/null | grep -q .; then \
	  echo "plumb needs its vendored dependencies."; \
	  echo "Run:  ocicl install"; \
	  echo "(ocicl.csv pins the exact versions; the tree itself is gitignored.)"; \
	  exit 1; \
	fi

all: build

# Two flavours of the same binary, so each target needs to know which one is
# sitting in bin/.  A timestamp on $(BIN) cannot say -- both write the same
# file -- so each build drops a marker and deletes the other's.  `make build`
# after `make crypto` then rebuilds, instead of silently leaving a binary with
# Ironclad in it.
#
# Each recipe also removes $(BIN) first.  PROGRAM-OP compares its output file
# against its inputs like any other ASDF operation, so a binary newer than the
# sources makes ASDF:MAKE a no-op -- and `make crypto` would then report
# success over the plain binary it had just been handed.
build: bin/.plain

# The same binary with the digest stages baked in.  A separate target, not a
# flag on `build`, because it is the one build that can fail for a reason that
# has nothing to do with this repository.
crypto: bin/.crypto

bin/.plain: $(SOURCES) | deps
	@mkdir -p $(dir $(BIN))
	@rm -f bin/.crypto $(BIN)
	@$(SBCL) --script build.lisp
	@touch $@
	@echo "built $(BIN) ($$(du -h $(BIN) | cut -f1))"

bin/.crypto: $(SOURCES) | deps
	@mkdir -p $(dir $(BIN))
	@rm -f bin/.plain $(BIN)
	@PLUMB_CRYPTO=1 $(SBCL) --script build.lisp
	@touch $@
	@echo "built $(BIN) with digests ($$(du -h $(BIN) | cut -f1))"

test: | deps
	@$(LISP) --eval '(asdf:test-system "plumb")'

test-crypto: | deps
	@$(LISP) --eval '(asdf:test-system "plumb/crypto")'

# The last section runs bin/plumb as a subprocess, so there has to be one --
# but deliberately NOT `demo: build`, which rebuilt the plain flavour and threw
# away a crypto binary without saying so.  Build only if there is nothing there.
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
	@echo "make crypto  dump $(BIN) with the digest stages (Ironclad, vendored in ocicl/)"
	@echo "make test    run the test suite"
	@echo "make test-crypto  run the digest tests"
	@echo "make demo    sbcl --script demo.lisp"
	@echo "make repl    interactive plumb prompt, no binary needed"
	@echo "make clean   remove bin/"

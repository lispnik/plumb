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
# ocicl/ is jzon, Ironclad and their dependencies, vendored here and pinned by
# ocicl.csv.  It is listed BEFORE :inherit-configuration so it wins: `make
# crypto` once resolved Ironclad out of whatever neighbouring project the user's
# own source-registry happened to point at, which is not a build.
REGISTRY := (asdf:initialize-source-registry (quote (:source-registry (:directory "$(ROOT)") (:tree "$(ROOT)ocicl/") :inherit-configuration)))
LISP     := $(SBCL) --noinform --non-interactive --no-userinit --eval "(require :asdf)" --eval '$(REGISTRY)'

.PHONY: all build test test-crypto test-json demo repl clean help deps

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
	@$(SBCL) --script build.lisp
	@echo "built $(BIN) ($$(du -h $(BIN) | cut -f1)) -- $$($(BIN) --version)"

test: | deps
	@$(LISP) --eval '(asdf:test-system "plumb")'

test-crypto: | deps
	@$(LISP) --eval '(asdf:test-system "plumb/crypto")'

test-json: | deps
	@$(LISP) --eval '(asdf:test-system "plumb/json")'

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
	@echo "make demo    sbcl --script demo.lisp"
	@echo "make repl    interactive plumb prompt, no binary needed"
	@echo "make clean   remove bin/"

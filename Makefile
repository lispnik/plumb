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
REGISTRY := (asdf:initialize-source-registry (quote (:source-registry (:directory "$(ROOT)") :inherit-configuration)))
LISP     := $(SBCL) --noinform --non-interactive --no-userinit --eval "(require :asdf)" --eval '$(REGISTRY)'

.PHONY: all build test demo repl clean help

all: build

build: $(BIN)

$(BIN): $(SOURCES)
	@mkdir -p $(dir $@)
	@$(SBCL) --script build.lisp
	@echo "built $@ ($$(du -h $@ | cut -f1))"

test:
	@$(LISP) --eval '(asdf:test-system "plumb")'

# The last section runs bin/plumb as a subprocess, so build it first.
demo: build
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
	@echo "make demo    sbcl --script demo.lisp"
	@echo "make repl    interactive plumb prompt, no binary needed"
	@echo "make clean   remove bin/"

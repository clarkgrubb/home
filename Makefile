MAKEFLAGS += --warn-undefined-variables
SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:

scripts_dir := ~/Library/Scripts
applescripts := $(wildcard darwin/applescript/*)

$(scripts_dir):
	if [ $$OS_TYPE == Darwin ]; then \
	mkdir -p $@; \
	fi

.PHONY: install-applescript
install-applescript: | $(scripts_dir)
	if [ $$OS_TYPE == Darwin ]; then \
	for script in $(applescripts); do \
	cp $$script $(scripts_dir); \
	done; \
	fi

.PHONY: install
install: install-applescript
	@echo copying dot files to HOME directory
	./install.sh ~

.PHONY: all
all: install

MAKEFLAGS += --warn-undefined-variables
SHELL := /bin/bash
.SHELLFLAGS := -o pipefail
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:

SCRIPTS_DIR := ~/Library/Scripts
APPLESCRIPTS := $(wildcard mac/applescript/*)

.PHONY: scripts_dir install-applescript install all

scripts_dir:
	if [ $$OS_TYPE == Darwin ]; then \
	mkdir -p $(SCRIPTS_DIR); \
	fi

install-applescript: | scripts_dir
	if [ $$OS_TYPE == Darwin ]; then \
	for script in $(APPLESCRIPTS); do \
	cp $$script $(SCRIPTS_DIR); \
	done; \
	fi

install: install-applescript
	@echo copying dot files to HOME directory
	./install.sh ~

all: install

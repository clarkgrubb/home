MAKEFLAGS += --warn-undefined-variables
SHELL := /bin/bash
.SHELLFLAGS := -o pipefail
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:

SCRIPTS_DIR := ~/Library/Scripts
APPLESCRIPTS := $(wildcard applescript/*)

.PHONY: install all

scripts_dir:
	if [ $$OS_TYPE == Darwin ]; then \
	mkdir -p $(SCRIPTS_DIR); \
	fi

install: | scripts_dir
	@echo copying dot files to HOME directory
	./bin/install.sh ~
	if [ $$OS_TYPE == Darwin ]; then \
	for as in $(APPLESCRIPTS); do \
	cp $$as $(SCRIPTS_DIR); \
	done; \
	fi

all: install

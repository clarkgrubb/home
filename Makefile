MAKEFLAGS += --warn-undefined-variables
SHELL := /bin/bash
.SHELLFLAGS := -o pipefail
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:

SCRIPTS_DIR := ~/Library/Scripts
APPLESCRIPTS := ToggleCharacterViewer.scpt ToggleKeyboardViewer.scpt OpenSystemPreferences.scpt

.PHONY: install all

scripts_dir:
	mkdir -p $(SCRIPTS_DIR)

install: | scripts_dir
	@echo copying dot files to HOME directory
	./bin/install.sh ~
	if [ -d ~/Library ]; then \
	for applescript in $(APPLESCRIPTS); do \
	cp .applescript/$$applescript $(SCRIPTS_DIR); \
	done; \
	fi

all: install

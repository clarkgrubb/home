MAKEFLAGS += --warn-undefined-variables
SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:

user := clark
services_dir := /Library/LaunchDaemons
ipython_plist := $(services_dir)/$(user).ipython.plist
notebook_dir := \/Users\/$(user)\/Documents\/Notebooks

setup:
	mkdir -p $(notebook_dir)

install: ipython.plist
	sed 's/USER/$(user)/; s/NOTEBOOK_DIR/$(notebook_dir)/' \
	< $< > $(ipython_plist)
	launchctl load -w $(ipython_plist)

all:
	@echo 'run "make setup"'
	@echo 'run "sudo make install"'

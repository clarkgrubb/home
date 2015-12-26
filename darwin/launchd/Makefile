MAKEFLAGS += --warn-undefined-variables
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:

user := clark
services_dir := /Library/LaunchDaemons
jupyter_notebook_plist := $(services_dir)/com.$(user).jupyter-notebook.plist
notebook_dir := \/Users\/$(user)\/Documents\/Notebooks

setup:
	mkdir -p $(notebook_dir)

install: jupyter-notebook.plist
	sed 's/USER/$(user)/; s/NOTEBOOK_DIR/$(notebook_dir)/' \
	< $< > $(jupyter_notebook_plist)
	launchctl load -w $(jupyter_notebook_plist)

all:
	@echo 'run "make setup"'
	@echo 'run "sudo make install"'

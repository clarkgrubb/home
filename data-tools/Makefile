MAN_SOURCE = $(wildcard doc/*.md)

MAN_TARGETS = $(patsubst %.md,%,$(MAN_SOURCE))

.PHONY: all TAGS check clean test tags man

all: build

TAGS:
	find . -name '*.py' | xargs etags

check:
	find . -name 'test*.py' | xargs python

clean:
	find . -name '*.pyc' | xargs rm
	find . -name '*.[0-9]' | xargs rm
	find . -name '*.html' | xargs rm
	rm TAGS

install:
	@echo implement install

build:
	@echo nothing to be done.

cov:
	@echo implement cov

pep8:
	pep8 *.py

pylint:
	@echo implement pylint

# An uninstalled man page can be viewed with the man command:
#
#   man doc/foo.1
#
man: $(MAN_TARGETS)

doc/%.1: doc/%.1.md
	pandoc -s -s -w man $< -o $@

test: check

tags: TAGS

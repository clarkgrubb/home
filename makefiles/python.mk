HTML_SOURCE = $(wildcard *.md)
HTML_TARGETS = $(patsubst %.md,%.html,$(HTML_SOURCE))

.PHONY: all TAGS check clean test tags

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

html: $(HTML_TARGETS)

%.html: %.md
	markdown $< > $@

install:
	@echo implement install

build:
	@echo nothing to be done.

cov:
	@echo implement cov

pep8:
	@echo implement pep8

pylint:
	@echo implement pylint

# asciidoc must be installed to make man pages.  The source must
# be in a file of the format foo.1.txt
#
# The asciidoc format for man pages is described poorly here:
#
#   http://www.methods.co.nz/asciidoc/userguide.html#X1
#
# The uninstalled man page can be viewed with the man command:
#
#   man ./foo.1
#
man:
	@echo this takes time...
	find . -name '*.[0-9].txt' \
	| xargs a2x --no-xmllint --doctype manpage --format manpage

test: check

tags: TAGS

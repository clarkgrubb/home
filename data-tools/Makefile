MAN1_SOURCE = $(wildcard doc/*.1.md)

MAN1_TARGETS = $(patsubst %.md,%,$(MAN1_SOURCE))

LOCAL_INSTALL_DIR = /usr/local/bin

LOCAL_MAN_DIR = /usr/local/share/man


.PHONY: all TAGS check clean test man install install-man

all: man

TAGS:
	find . -name '*.py' | xargs etags

check_python:
	find . -name 'test*.py' | xargs python

check_ruby:
	find . -name 'test*.rb' | xargs ruby

check: check_python check_ruby

clean:
	-find . -name '*.pyc' | xargs rm
	-find doc -name '*.[0-9]' | xargs rm
	-find . -name '*.html' | xargs rm
	-rm TAGS

sudo.install: sudo.install-man
	ln -sf $(PWD)/csv_to_tsv.py $(LOCAL_INSTALL_DIR)/csv-to-tsv
	ln -sf $(PWD)/tsv_to_csv.py $(LOCAL_INSTALL_DIR)/tsv-to-csv
	ln -sf $(PWD)/json-awk.rb $(LOCAL_INSTALL_DIR)/json-awk
	ln -sf $(PWD)/dom-awk.rb $(LOCAL_INSTALL_DIR)/dom-awk

sudo.install-man: man
	if [ ! -d $(LOCAL_MAN_DIR)/man1 ]; then \
	echo directory does not exist: $(LOCAL_MAN_DIR)/man1; \
	return 1; \
	fi
	for target in $(MAN1_TARGETS); \
	do \
	sudo cp $$target $(LOCAL_MAN_DIR)/man1; \
	done

pep8:
	pep8 *.py

# An uninstalled man page can be viewed with the man command:
#
#   man doc/foo.1
#
man: $(MAN1_TARGETS)

doc/%.1: doc/%.1.md
	pandoc -s -s -w man $< -o $@

test: check

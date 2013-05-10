SHELL := /bin/bash
.SHELLFLAGS :=  -o pipefail

.DELETE_ON_ERROR:
.SUFFIXES:

HEXEDIT_URL := http://rigaux.org/hexedit-1.2.13.src.tgz

PATCH_URL := http://www.volkerschatz.com/unix/patches/hexedit-search2a.diff.gz

all: build

hexedit-1.2.13.src.tgz:
	curl $(HEXEDIT_URL) > $@

hexedit: hexedit-1.2.13.src.tgz
	tar xf $<

hexedit-search2a.diff:
	curl $(PATCH_URL) > $@.gz
	gunzip $@.gz

patch: hexedit-search2a.diff hexedit
	patch -p0 < $<

cygwin.patch: patch
	patch hexedit/hexedit.h < hexedit.h.diff

build: patch
	(cd hexedit; \
	./configure; \
	make)

cygwin.build: cygwin.patch
	(cd hexedit; \
	./configure; \
	make)

clobber:
	-rm -rf hexedit hexedit-1.2.13.src.tgz hexedit-search2a.diff

install:
	(cd hexedit; make install)

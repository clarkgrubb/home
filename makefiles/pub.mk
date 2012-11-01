SOURCE := $(wildcard *.txt)

DOCX_TARGETS := $(patsubst %.txt,%.docx,$(SOURCE))
EPUB_TARGETS := $(patsubst %.txt,%.epub,$(SOURCE))
MOBI_TARGETS := $(patsubst %.txt,%.mobi,$(SOURCE))
PDF_TARGETS := $(patsubst %.txt,%.pdf,$(SOURCE))
HTML_TARGETS := $(patsubst %.txt,%.html,$(SOURCE))

ALL_TARGETS := $(DOCX_TARGETS) $(EPUB_TARGETS) $(MOBI_TARGETS) \
	$(HTML_TARGETS) $(PDF_TARGETS)

BACKUP_DIR := /Users/clark/Dropbox/Backup/Pub

COVER_IMAGE := /Users/clark/Dropbox/Pictures/black.png

.PHONY: all

%.docx: %.txt
	pandoc --toc $< -o $@

%.epub: %.txt
	pandoc --toc --epub-cover-image=$(COVER_IMAGE) $< -o $@

%.pdf: %.txt
	pandoc $< -o $@

%.html: %.txt
	pandoc --toc $< -o $@

%.mobi: %.epub
	kindlegen $< -o $@

all: $(ALL_TARGETS)

clean:
	-rm $(ALL_TARGETS)

backup: clean
	rsync -r . $(BACKUP_DIR)

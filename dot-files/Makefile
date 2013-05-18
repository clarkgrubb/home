SCRIPTS_DIR := ~/Library/Scripts

APPLESCRIPTS := ToggleCharacterViewer.scpt ToggleKeyboardViewer.scpt

install: | scripts_dir
	@echo copying dot files to HOME directory
	./install.sh ~
	if [ -d ~/Library ]; then \
	for applescript in $(APPLESCRIPTS); do \
	cp .applescript/$$applescript $(SCRIPTS_DIR); \
	done; \
	fi

scripts_dir:
	mkdir -p $(SCRIPTS_DIR)

MAKEFLAGS += --warn-undefined-variables
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:

# TODO: clean task, because wget won't overwrite a file
# TODO: what if no command line emacs for compiling?
# TODO: separate task for each download?
# TODO: occasional remove compilation.log?

libs := git-modes magit scala-mode2
uncompiled_lisp := $(wildcard *.el $(patsubst %,%/*.el,$(libs)))
compiled_lisp := $(patsubst %.el,%.elc,$(uncompiled_lisp))
includes := $(patsubst %,-L %,$(libs))

%.elc: %.el
	emacs -batch -Q -L . $(includes) -f batch-byte-compile $< 2>> compilation.log

..PHONY: lisp
lisp: $(compiled_lisp)

.PHONY: download.scala-mode2
download.scala-mode2:
	git clone https://github.com/hvesalai/scala-mode2
	rm -rf scala-mode2/.git

.PHONY: download.haskell.mode
download.haskell.mode:
	wget http://projects.haskell.org/haskellmode-emacs/haskell-mode-2.8.0.tar.gz
	tar xf haskell-mode-2.8.0.tar.gz
	mv haskell-mode-2.8.0 haskell-mode

.PHONY: download.tuareg.caml.mode
download.tuareg.caml.mode:
	wget https://forge.ocamlcore.org/frs/download.php/514/tuareg-2.0.4.tgz
	tar xf tuareg-2.0.4.tgz
	wget https://forge.ocamlcore.org/frs/download.php/516/tuareg-caml-mode.tar.gz 
	tar xf tuareg-caml-mode.tar.gz

.PHONY: download.lua.mode
download.lua.mode:
	rm lua-mode.el
	wget https://raw.githubusercontent.com/immerrr/lua-mode/master/lua-mode.el

.PHONY: download.lang.modes
download.lang.modes: download.lua.mode
	wget https://raw.githubusercontent.com/russel/Emacs-Groovy-Mode/master/groovy-mode.el
	wget http://php-mode.svn.sourceforge.net/svnroot/php-mode/tags/php-mode-1.5.0/php-mode.el
	wget http://www.erlang.org/download/contrib/erlang.el
	wget http://jblevins.org/projects/markdown-mode/markdown-mode.el
	curl 'http://golang.org/misc/emacs/go-mode.el?m=text' > go-mode.el
	wget https://raw.githubusercontent.com/clarkgrubb/latex-input/master/latex.el

.PHONY: download.projectile
download.projectile:
	wget https://raw.githubusercontent.com/bbatsov/projectile/master/projectile.el
	patch < projectile.el.patch
	wget https://raw.githubusercontent.com/magnars/dash.el/master/dash.el
	wget https://raw.githubusercontent.com/magnars/s.el/master/s.el

.PHONY: download
download: download.lang.modes download.projectile
	wget https://raw.githubusercontent.com/leoliu/ggtags/master/ggtags.el
	wget https://raw.githubusercontent.com/sigma/tabulated-list.el/master/tabulated-list.el
	curl http://elpa.gnu.org/packages/cl-lib-0.5.el > cl-lib.el
	wget https://raw.githubusercontent.com/nonsequitur/smex/master/smex.el
	wget http://www.emacswiki.org/emacs/download/multi-term.el

all: lisp

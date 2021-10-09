* ruby (erb) and emacs must be installed
* The `install.sh` script installs everything that starts with a period in the home directory.  This includes directories; directories are installed with `cp -R`.
* The `install.sh` script creates the file ~/.gitconfig if it don't already exist.  Prompts for username and email.
* Dot files are for shells, editors, multiplexers, and version control.  Also .pylintrc
* Dot files should work on Darwin and Linux
* Put code which is only installed on type of system in a subdirectory; e.g `darwin`, `linux`.  Write makefile tasks to install it.
* Make shell dot files should be re-sourceable.  In particular, don't use readonly variables, and don't append to a preexisting variable.
* `.git-prompt.sh` comes from https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh

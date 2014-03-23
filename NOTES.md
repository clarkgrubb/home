* The `install.sh` script installs everything that starts with a period in the home directory.  This includes directories; directories are installed with `cp -R`.
* The `install.sh` script creates the files ~/.gitconfig and ~/.hgrc if they don't already exist.  Prompts for username and email.
* Dot files are for shells, editors, and multiplexers
* Dot files should work on these systems: Darwin, Linux, Windows (Cygwin or MinGW)
* Put code which is only installed on type of system in a subdirectory; e.g `darwin`, `linux`.  Write special makefile tasks to install it.
* Examples of system specific code: GUI scripts (i.e. AppleScript), daemons which start up at boot or login.
* Make shell dot files should be re-sourceable.  In particular, don't use readonly variables, and don't append to a preexisting variable.
* `.git-prompt.sh` comes from https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh

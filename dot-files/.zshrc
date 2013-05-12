# Aliases and Shell Function Definitions
#

export OS_TYPE=`uname -s`
if [[ ${OS_TYPE:0:6} == CYGWIN ]]
then
    export OS_TYPE='Cygwin'
fi

# Instead of running the last command
# launch R statistics environment.
#
alias r='command r'

# Don't launch X Windows to run Emacs.
#
# Put the OS and Editor in tab.
#
# Unfortunately this does not revert the
# tab name when exiting Emacs.
#
alias emacs='tabname $OS_TYPE \(Emacs\) && emacs -nw'

# Makes git tab completion faster
#
__git_files () {
    _wanted files expl 'local files' _files
}

# Used to set the tab name in
# Terminal.app
#
function tabname() {
    echo -n "\033]0;$*\007"
}

# awk with tab as FS
#
function tawk() {
    awk -F $'\t' "$*"
}

if [[ $OS_TYPE == 'Darwin' ]]
then

    # For reading man pages with Preview.app.
    #
    pman() {
        man -t "$@" | open -f -a /Applications/Preview.app
    }

    # Defines function 'itunes'
    #
    . ~/.zsh_itunes


elif [[ $OS_TYPE == 'Linux' ]]
then
    # Linux specific definitions here

    #
    #
    pman() {
        man -t "$@"
    }

elif [[ $OS_TYPE == 'Cygwin' || $OS_TYPE == 'Windows' ]]
then
    # Windows specific definitions here


else

    echo "unrecognized OS:" $OS_TYPE

fi

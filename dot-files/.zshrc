# Environment Variables
#

if [ -e /usr/bin/uname ]
then
    export OS_TYPE=$(/usr/bin/uname -s)
else
    export OS_TYPE=$(/bin/uname -s)
fi

if [[ $OS_TYPE[0,6] == CYGWIN ]]
then
    export OS_TYPE=Cygwin
fi
if [[ $OS_TYPE[0,5] == MinGW ]]
then
    export OS_TYPE=MinGW
fi

export EDITOR='emacs -q'
export HISTSIZE=2000
export HISTFILE=~/.zsh_history
export SAVEHIST=2000
export READNULLCMD=less

# So we can have version control info in the prompt
#
autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats '%s:%b|%a '
zstyle ':vcs_info:*' formats '%s:%b '
precmd () { vcs_info }

#  C-x C-e to edit command line with $EDITOR
#
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^e' edit-command-line

# Set prompt
#
setopt PROMPT_SUBST
PS1="%F{red}$OS_TYPE:zsh%f %F{blue}%3~ %F{green}\${vcs_info_msg_0_}%f%(?,:%),:() "

# Aliases and Shell Function Definitions
#

export OS_TYPE=$(uname -s)
if [[ ${OS_TYPE:0:6} == CYGWIN ]]
then
    export OS_TYPE=Cygwin
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

if [[ $OS_TYPE == Darwin ]]
then

    export PATH=/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
    export JAVA_HOME=/Library/Java/Home
    export ITUNES_PLAYLIST=KGRB

    # For reading man pages with Preview.app.
    #
    pman() {
        man -t "$@" | open -f -a /Applications/Preview.app
    }

    # Defines function 'itunes'
    #
    . ~/.zsh_itunes


elif [[ $OS_TYPE == Linux ]]
then
    # Linux specific definitions here

    export PATH=/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin

    jvm_dir=/usr/lib/jvm
    for jdk in java-6-openjdk java-6-openjdk-i386 java-6-sun
    do
        if [ -e ${jvm_dir}/${jdk} ]
        then
            export JAVA_HOME=${jvm_dir}/${jdk}
        fi
    done

    # send ps file to stdout
    #
    pman() {
        man -t "$@"
    }

elif [[ $OS_TYPE == 'Cygwin' || $OS_TYPE == 'MinGW' ]]
then
    # Windows specific definitions here

    cd $HOME

else

    echo "unrecognized OS:" $OS_TYPE

fi

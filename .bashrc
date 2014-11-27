# UNIVERSAL
#
export OS_TYPE=$(uname -s)
if [[ ${OS_TYPE:0:5} == MINGW ]]
then
    export OS_TYPE=MinGW
fi

export EDITOR='emacs'
export HISTSIZE=2000
export HISTFILE=~/.bash_history
export TERM=xterm-256color
export GIT_CONFIG_NOSYSTEM=1
export VIRTUAL_ENV_DISABLE_PROMPT=1

. ~/.config/shell/git-prompt.sh
. ~/.config/shell/up.sh

function tabname {
    echo -n "$(echo -n $'\033]0;')$*$(echo -n $'\007')"
}

alias ksh ksh -E

export PS1="\[\033[31m\]$OS_TYPE:bash \[\033[34m\]\w \[\033[32m\]"'$(__git_ps1 "git:%s " )'"\[\033[30m\]\`if [ \$? == 0 ]; then echo \:\); else echo \:\(; fi\` "

# OS SPECIFIC
#

if [[ $OS_TYPE == Darwin ]]
then

    export PATH=~/Local/bin:/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
    export MANPATH=~/Local/man:$(MANPATH= manpath)

    pman() {
        man -t "$@" | open -f -a /Applications/Preview.app
    }

    . ~/.config/shell/itunes.sh

elif [[ $OS_TYPE == Linux ]]
then

    # disable apt pkg command-not-found
    unset command_not_found_handle

    export PATH=~/Local/bin:/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
    export MANPATH=~/Local/man:$(MANPATH= manpath)

    # send ps file to stdout
    #
    pman() {
        man -t "$@"
    }

elif [[ $OS_TYPE == MinGW ]]
then

    cd $HOME

    export TERM=msys

else

    echo "unrecognized OS:" $OS_TYPE

fi

# HOST SPECIFIC
#
if [ -e ~/.shell.local ]
then
    . ~/.shell.local
fi

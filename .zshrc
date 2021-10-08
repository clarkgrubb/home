# UNIVERSAL
#
if [ -e /usr/bin/uname ]
then
    export OS_TYPE=$(/usr/bin/uname -s)
else
    export OS_TYPE=$(/bin/uname -s)
fi
if [[ $OS_TYPE[0,5] == MinGW ]]
then
    export OS_TYPE=MinGW
fi

export MANPATH=~/Local/man:$(MANPATH= manpath)
export EDITOR='emacs'
export HISTSIZE=2000
export HISTFILE=~/.zsh_history
export SAVEHIST=2000
export READNULLCMD=less
export TERM=xterm-256color
export GIT_CONFIG_NOSYSTEM=1
export VIRTUAL_ENV_DISABLE_PROMPT=1

alias ksh ksh -E

#  C-x C-e to edit command line with $EDITOR
#
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^e' edit-command-line

# Set prompt
#
autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats '%s:%b|%a '
zstyle ':vcs_info:*' formats '%s:%b '
precmd () { vcs_info }
setopt PROMPT_SUBST
PS1="%F{red}$OS_TYPE:zsh%f %F{blue}%3~ %F{green}\${vcs_info_msg_0_}%f%(?,:%),:() "

# Instead of running the last command launch R statistics environment.
#
alias r='command r'

# Makes git tab completion faster
#
__git_files () {
    _wanted files expl 'local files' _files
}

. ~/.config/shell/last_agent.sh

function tabname() {
    echo -n "\033]0;$*\007"
}

if [[ $OS_TYPE == Darwin ]]
then

    export PATH=~/Local/bin:/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin

    pman() {
        psfile=$(mktemp)
        man -t "$@" | pstopdf -i -o $psfile
        mv $psfile ${psfile}.pdf
        open -a /Applications/Preview.app ${psfile}.pdf
    }

elif [[ $OS_TYPE == Linux ]]
then

    export PATH=~/Local/bin:/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin

    # send ps file to stdout
    #
    pman() {
        man -t "$@"
    }

elif [[ $OS_TYPE == 'MinGW' ]]
then
    # Has zsh even been ported to MinGW?

    cd $HOME

else

    echo "unrecognized OS:" $OS_TYPE

fi

if [ -e ~/.shell.local ]
then
    . ~/.shell.local
fi

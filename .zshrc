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
export MANPATH=~/Local/man:$(MANPATH= manpath)
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

if [[ $OS_TYPE == Darwin ]]
then

    export PATH=~/Local/Bin:/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
    export JAVA7_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_25.jdk/Contents/Home
    export JAVA6_HOME=/Library/Java/Home
    export JAVA_HOME=$JAVA6_HOME
    export PATH=$JAVA_HOME/bin:$PATH

    export ITUNES_PLAYLIST=KGRB

    # For rbenv and virtualenv
    #
    export PATH=~/.rbenv/shims:$PATH
    export VIRTUAL_ENV_DISABLE_PROMPT=1

    # For reading man pages with Preview.app.
    #
    pman() {
        man -t "$@" | open -f -a /Applications/Preview.app
    }

    # Defines function 'itunes'
    #
    . ~/.itunes.sh


elif [[ $OS_TYPE == Linux ]]
then
    # Linux specific definitions here

    export PATH=~/Local/bin:/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin

    jvm_dir=/usr/lib/jvm
    export JAVA7_HOME=/usr/lib/jvm/java-7-openjdk-amd64
    for jdk in java-6-openjdk java-6-openjdk-i386 java-6-sun
    do
        if [ -e ${jvm_dir}/${jdk} ]
        then
            export JAVA6_HOME=${jvm_dir}/${jdk}
            export JAVA_HOME=$JAVA6_HOME
        fi
    done
    if [ $JAVA_HOME ]
    then
        export PATH=$JAVA_HOME/bin:$PATH
    fi

    # For rbenv and virtualenv
    #
    export PATH=~/.rbenv/shims:$PATH
    export VIRTUAL_ENV_DISABLE_PROMPT=1
    if [ -e ~/Local/env/venv/bin/activate ]
    then
        . ~/Local/env/venv/bin/activate
    fi

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

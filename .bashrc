# Environment Variables, Aliases, and Shell Function Definitions
#

export COMMON_PATH=~/Local/bin:/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
export OS_TYPE=`uname -s`
if [[ ${OS_TYPE:0:6} == CYGWIN ]]
then
    export OS_TYPE=Cygwin
fi
if [[ ${OS_TYPE:0:5} == MINGW ]]
then
    export OS_TYPE=MinGW
fi
if [[ $OS_TYPE != MinGW ]]
then
    export MANPATH=~/Local/man:$(MANPATH= manpath)
fi

export EDITOR='emacs -q'
export HISTSIZE=2000
export HISTFILE=~/.bash_history
export TERM=xterm-256color
export GIT_CONFIG_NOSYSTEM=1

. ~/.git-prompt.sh

function _project_test {
    for dir in .git .hg .bzr
    do
        if [ -e $dir ]
        then
            return 0
        fi
    done
    return 1
}

function _home_test {
    if [ $(pwd) = ~ ]
    then
        return 0
    else
        return 1
    fi
}

function _root_test {
    if [ $(pwd) = / ]
    then
        return 0
    else
        return 1
    fi
}

# Move up one directory at-a-time until we are in a project, the
# home directory, or the root directory.
#
function up {
    while ! _project_test && ! _home_test && ! _root_test
    do
        cd ..
    done
}

function tabname {
    echo -n "$(echo -n $'\033]0;')$*$(echo -n $'\007')"
}

export PS1="\[\033[31m\]$OS_TYPE:bash \[\033[34m\]\w \[\033[32m\]"'$(__git_ps1 "git:%s " )'"\[\033[30m\]\`if [ \$? == 0 ]; then echo \:\); else echo \:\(; fi\` "

if [[ $OS_TYPE == Darwin ]]
then

    export PATH=$COMMON_PATH
    export JAVA7_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_25.jdk/Contents/Home
    export JAVA6_HOME=/Library/Java/Home
    export JAVA_HOME=$JAVA6_HOME
    export PATH=$JAVA_HOME/bin:$PATH

    # For rbenv and virtualenv
    #
    export PATH=~/.rbenv/shims:$PATH
    export VIRTUAL_ENV_DISABLE_PROMPT=1

    pman() {
        man -t "$@" | open -f -a /Applications/Preview.app
    }

    # Defines function 'itunes'
    #
    . ~/.itunes.sh

elif [[ $OS_TYPE == Linux ]]
then

    # disable apt pkg command-not-found
    unset command_not_found_handle

    export PATH=$COMMON_PATH

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
    export PATH=~/.rbenv/bin:$PATH
    export VIRTUAL_ENV_DISABLE_PROMPT=1
    if [ -e ~/Local/env/venv/bin/activate ]
    then
        . ~/Local/env/venv/bin/activate
    fi



elif [[ $OS_TYPE == Cygwin || $OS_TYPE == MinGW ]]
then

    export PATH=$PATH:$COMMON_PATH

    cd $HOME

else

    echo "unrecognized OS:" $OS_TYPE

fi

# A place for host specific settings:
#
if [ -e ~/.shell.local ]
then
    . ~/.shell.local
fi

# Environment Variables
#

export COMMON_PATH=/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
export OS_TYPE=`uname -s`
if [[ $OS_TYPE[0,6] == CYGWIN ]]
then
    export OS_TYPE='Cygwin'
fi

export EDITOR='emacs -q'
export HISTSIZE=2000
export HISTFILE=~/.zsh_history
export SAVEHIST=2000

# Needed to put version control info
# in the prompt 
#
autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats '%s:%b|%a '
zstyle ':vcs_info:*' formats '%s:%b '
precmd () { vcs_info }

# Set prompt
#
setopt PROMPT_SUBST
PS1="%F{red}$OS_TYPE%f %F{blue}%3~ %F{green}\${vcs_info_msg_0_}%f%(?,:%),:() "

if [[ $OS_TYPE == 'Darwin' ]]
then

    export HOSTNAME=`hostname -s`
    export PATH=$COMMON_PATH
    export JAVA_HOME=/Library/Java/Home

elif [[ $OS_TYPE == 'Linux' ]]
then

    export HOSTNAME=`hostname -s`
    export PATH=$COMMON_PATH
    
    readonly jvm_dir=/usr/lib/jvm
    for jdk in java-6-openjdk java-6-sun
    do
        if [ -e ${jvm_dir}/${jdk} ]
        then
            export JAVA_HOME=${jvm_dir}/${jdk}
        fi
    done
 
elif [[ $OS_TYPE == 'Cygwin' || $OS_TYPE == 'Windows' ]]
then

    export HOSTNAME=`hostname`
    export OS_PATH_PRIORITY=~/bin
    export OS_PATH='/cygdrive/c/Program Files (x86)'/Git/bin:'/cygdrive/c/Windows/SUA/bin':'/cygdrive/c/Program Files/ghc-7.0.4/bin'
    export PATH=$OS_PATH_PRIORITY:$COMMON_PATH:$OS_PATH

    cd $HOME

else

    echo "unrecognized OS:" $OS_TYPE

fi


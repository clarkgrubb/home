# The purpose of this file is to set the PATH, EDITOR, prompt (PS1), and aliases.
# It is intended to work under Mac (Darwin), Linux, and Windows (Cygwin).
# It sources ~/.bashrc because Cygwin creates a .bashrc file in the user directory
# on installation.  Only personalize this file, not .bashrc.

if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi

export COMMON_PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/sbin:/usr/X11/bin:~/bin
export EDITOR='emacs -q'
export OS_TYPE=`uname -s`
if [[ $OS_TYPE =~ ^CYGWIN ]]
then export OS_TYPE='Cygwin'
fi
PS1="$OS_TYPE:\w $ "

#
# Macintosh personalizations
#
if [[ $OS_TYPE == 'Darwin' ]]
then

    export OS_PATH=/opt/local/lib/postgresql81/bin:~/Source/android-sdk-mac_86/tools
    export PATH=$COMMON_PATH:$OS_PATH
    export HOSTNAME=`hostname -s`

    pman () {
        man -t "${1}" | open -f -a /Applications/Preview.app
    }

    sman () {
        man -t "${1}" | open -f -a /Applications/Skim.app
    }

#
# Linux personalizations
#
elif [[ $OS_TYPE == 'Linux' ]]
then

    export OS_PATH=/usr/local/mercury-0.13.1/bin:/home/clarkgrubb/Source/io/build/_build/binaries
    export PATH=$COMMON_PATH:$OS_PATH
    export HOSTNAME=`hostname -s`

#
# Windows personalizations
#
elif [[ $OS_TYPE == 'Cygwin' ]]
then

    export OS_PATH=~/bin
    export PATH=$PATH:$OS_PATH
    export HOSTNAME=`hostname`

else

    echo "unrecognized OS:" $OS_TYPE

fi


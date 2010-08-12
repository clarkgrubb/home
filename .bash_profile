# The purpose of this file is to set the PATH, EDITOR, prompt (PS1), and aliases.
# It is intended to work under Mac (Darwin), Linux, and Windows (Cygwin).
# It sources ~/.bashrc because Cygwin creates a .bashrc file in the user directory
# on installation.  Only personalize this file, not .bashrc.

if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi

export COMMON_PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/sbin:/usr/X11/bin:~/bin
export EDITOR='emacs -q'
export OS=`uname -s`
if [[ $OS == '^CYGWIN' ]]
then export OS='Cygwin'
fi
PS1="$OS:\w $ "

#
# Macintosh personalizations
#
if [[ $OS == 'Darwin' ]]
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
elif [[ $OS == 'Linux' ]]
then

    export OS_PATH=/usr/local/mercury-0.13.1/bin:/home/clarkgrubb/Source/io/build/_build/binaries
    export PATH=$COMMON_PATH:$OS_PATH
    export HOSTNAME=`hostname -s`

#
# Windows personalizations
#
elif [[ $OS == 'Cygwin' ]]
then

    export OS_PATH=~/bin
    export PATH=$PATH
    export HOSTNAME=`hostname`

else

    echo "unrecognized OS:" $OS

fi


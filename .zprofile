# The purpose of this file is to set the PATH, EDITOR, prompt (PS1), and aliases.
# It is intended to work under Mac (Darwin), Linux, and Windows (Cygwin).
# It sources ~/.bashrc because Cygwin creates a .bashrc file in the user directory
# on installation.  Only personalize this file, not .bashrc.

if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi

export COMMON_PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
export EDITOR='emacs -q'
export OS_TYPE=`uname -s`
if [[ $OS_TYPE =~ ^CYGWIN ]]
then export OS_TYPE='Cygwin'
fi
PS1="$OS_TYPE:%~ $ "

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

    # Provides these commands:
    #
    #   itunes pause
    #   itunes play
    #   itunes previous track
    #   itunes next track
    #
    itunes-debug () {
        osascript -e 'tell application "iTunes"' -e "${*}" -e "end tell"
    }
    itunes () {
        itunes-debug ${*} 2> /dev/null
    }


    # Provides these commands:
    #   
    #   pandora playpause
    #   pandora next track
    #   pandora thumbs up
    #   pandora thumbs down
    #   pandora get name of current track
    #   pandora get artist of current track
    #   pandora get name of current station
    # 
    pandora-debug () {
        osascript -e 'tell application "PandoraBoy"' -e "${*}" -e "end tell"
    }
    pandora () {
        pandora-debug ${*} 2> /dev/null
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

    export OS_PATH_PRIORITY=~/bin
    export OS_PATH='/Program Files (x86)'/Git/bin
    export PATH=$OS_PATH_PRIORITY:$PATH:$OS_PATH
    export HOSTNAME=`hostname`

else

    echo "unrecognized OS:" $OS_TYPE

fi


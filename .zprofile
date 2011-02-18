# The purpose of this file is to set the PATH, EDITOR, prompt (PS1), and aliases.
# It is intended to work under Mac (Darwin), Linux, and Windows (Cygwin).

export EDITOR='emacs -q'
export OS_TYPE=`uname -s`
if [[ $OS_TYPE[0,6] == CYGWIN ]]
then export OS_TYPE='Cygwin'
fi

#
# Macintosh personalizations
#
if [[ $OS_TYPE == 'Darwin' ]]
then

    trash () {
        mv -n ${*} /Users/clark/Trash
    }

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



#
# Windows personalizations
#
elif [[ $OS_TYPE == 'Cygwin' ]]
then


else

    echo "unrecognized OS:" $OS_TYPE

fi


# Aliases and Shell Function Definitions
#

export OS_TYPE=`uname -s`
if [[ $OS_TYPE[0,6] == CYGWIN ]]
then export OS_TYPE='Cygwin'
fi

alias e='emacs -q'
alias r='command r'

#
# Macintosh personalizations
#
if [[ $OS_TYPE == 'Darwin' ]]
then

    trash () {
        mv -n ${*} /Users/$USER/Trash
    }

    pman () {
        man -t "$@" | open -f -a /Applications/Preview.app
    }

    # Provides these commands:
    #

    #

    _itunes() {
        osascript -e 'tell application "iTunes"' -e ${*} -e "end tell"
    }

    _itunes_playlist() {

        itunes_scpt=~/Dropbox/AppleScript/iTunesPlaylist.scpt
        file=/tmp/$$.$1.txt

        osascript $itunes_scpt $1 $file
        cat $file
        rm $file
    }

    _itunes_help() {
        cat <<EOF
USAGE:

  itunes list
  itunes shuffle
  itunes noshuf
  itunes pause
  itunes play
  itunes next
  itunes prev
  itunes 'play track "Disco Lies"'

EOF
    }

    itunes () {

        # iTunes playlist
        #
        list=KGRB

        arg=${*}

        if [ $# -eq 0 ]
        then
            arg='play playlist "'${list}'"'
        elif [ $# -eq 1 ]
        then
            case $1 in
                list) _itunes_playlist $list ; return ;;
                help) _itunes_help ; return ;;
                stop) arg='pause' ;;
                next) arg='next track' ;;
                prev) arg='previous track' ;;
                previous) arg='previous track' ;;
                shuffle) arg='set shuffle of user playlist "'$list'" to true' ;;
                noshuf) arg='set shuffle of user playlist "'$list'" to false' ;;
            esac
        fi

        _itunes $arg
    }

#
# Linux personalizations
#
elif [[ $OS_TYPE == 'Linux' ]]
then

#
# Windows personalizations
#
elif [[ $OS_TYPE == 'Cygwin' || $OS_TYPE == 'Windows' ]]
then

    alias powershell=C:/Windows/SysWOW64/WindowsPowerShell/v1.0/powershell.exe
    alias cmd=C:/Windows/System32/cmd.exe

else

    echo "unrecognized OS:" $OS_TYPE

fi

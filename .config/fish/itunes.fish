#  Defines 'itunes' function for controlling
#  iTunes from the command line.
#

function _itunes 
    osascript -e 'tell application "iTunes"' -e "$argv" -e "end tell"
end

function _itunes_playlist
    set playlist_scpt ~/Library/Scripts/iTunesPlaylist.scpt
    set file /tmp/%self.$argv[1].txt

    osascript $playlist_scpt $argv[1] $file
    # set LC_ALL=C so awk and sort don't barf
    # on non-UTF-8 bytes returned by osascript
    if [ (count $argv) -eq 2 ]
        if [  $argv[2] = artists ]
            env LC_ALL=C awk 'BEGIN { FS = " ::: " } { print($2, ":::", $1) }' < $file | sort -f
        else
            env LC_ALL=C sort -f $file
        end
    else
        env LC_ALL=C sort -f $file
    end
    rm $file
end

function _itunes_output_scpt
    set file /tmp/%self.(basename $argv[1]).txt

    osascript $argv[1] $file
    cat $file
    rm $file
end

function _itunes_help

    echo "
TO LAUNCH ITUNES AND PLAY $ITUNES_PLAYLIST:

  itunes

USAGE:

  itunes list
  itunes artists
  itunes shuffle
  itunes noshuffle
  itunes current_track
  itunes current_playlist
  itunes pause
  itunes play
  itunes next
  itunes prev
  itunes 'play track \"Disco Lies\" of playlist \"KGRB\"'
  itunes 'play playlist \"KGRB\"'
  itunes quit

"
end

function itunes

    set scptdir ~/Library/Scripts
    set current_track_scpt $scptdir/iTunesCurrentTrack.scpt
    set current_playlist_scpt $scptdir/iTunesCurrentPlaylist.scpt

    if [ -z $ITUNES_PLAYLIST ]
        echo 'ITUNES_PLAYLIST not set'
        return 1
    end

    set arg $argv

    if [ (count $argv) -eq 0 ]
        set arg 'play playlist "'$ITUNES_PLAYLIST'"'
    else if [ (count $argv) -eq 1 ]
        switch $argv[1]
            case list
                _itunes_playlist $ITUNES_PLAYLIST ; return
            case current_track
                _itunes_output_scpt $current_track_scpt ; return 
            case current_playlist
                _itunes_output_scpt $current_playlist_scpt ; return 
            case artists
                _itunes_playlist $ITUNES_PLAYLIST artists ; return
            case help
                _itunes_help ; return
            case stop
                set arg 'pause'
            case next
                set arg 'next track'
            case prev
                set arg 'previous track'
            case previous
                set arg 'previous track'
            case shuffle
                set arg 'set shuffle of user playlist "'$ITUNES_PLAYLIST'" to true'
            case noshuffle
                set arg 'set shuffle of user playlist "'$ITUNES_PLAYLIST'" to false'
        end
    end

    #_itunes 'play playlist "'$ITUNES_PLAYLIST'"'
    _itunes $arg
end

#!/usr/bin/env bash

# Which command line tools are missing?
# Which of them have missing man pages?  What are
# the line counts of the man pages that exist?
#
# The list of  command line tools to search
# for is provided as an argument.  The list
# is a file with one tool name per line.

tmp_file=$(mktemp -t cmd_audit)

function int_handler {
    echo
    rm -f $tmp_file
    exit
}

trap int_handler INT

if [ $# -eq 1 ]
then

    echo $PATH | tr ':' ' ' | xargs ls | sort -u > $tmp_file

    sort -u $1 | join -v 1 - $tmp_file

    rm -f $tmp_file

elif [ \( $# -eq 2 \) -a \( "$1" = '--man' \) ]
then

    for f in $(sort -u $2)
    do
        if man $f > /dev/null 2>&1
        then
            :
        else
            echo $f
        fi
    done

elif [ \( $# -eq 2 \) -a \( "$1" = '--man-length' \) ]
then

    tmp_file=$(mktemp -t cmd_audit)

    for f in $(sort -u $2)
    do
        if man $f > /dev/null 2>&1
        then
            echo -n $f $'\t' >> $tmp_file
            man $f 2> /dev/null | wc -l | tr -d ' ' >> $tmp_file
        fi
    done


    total_length=$(awk -F $'\t' '{sum += $2} END { print sum }' $tmp_file)

    sort -nrk2 $tmp_file | awk \
        'BEGIN{FS="\t"}{ printf("%s\t%s\t%.2f\n", $1, $2, 100.0 * $2 / 101046)}'

else

    echo "USAGE: cmd-audit.sh [--man|--man-length] TOOLS_LIST"

fi

rm -f $tmp_file

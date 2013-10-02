#!/bin/sh

readonly src_dir=$(dirname $(dirname $0))
readonly home_dir=$1

cd $src_dir

if [ $# -lt 1 ]
then
    echo "USAGE: install.sh DIR"
    exit 1
fi

if [ ! -d $home_dir ]
then
    echo "NOT A DIRECTORY: " $home_dir
    exit 1
fi

for dot_file in $(echo .[a-zA-Z]*)
do
    if [ \( $dot_file != .git \) -a \( $dot_file != .gitignore \) ]
    then
        if [ -d $dot_file ]
        then
            rm -rf $home_dir/$dot_file
            cp -R $dot_file $home_dir
        else
            cp $dot_file $home_dir
        fi
    fi
done

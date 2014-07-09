#!/bin/bash

readonly src_dir=$(dirname $(dirname $0))
readonly home_dir=$1
readonly gitconfig=${home_dir}/.gitconfig
readonly hgrc=${home_dir}/.hgrc

function prompt_for_username() {
    if [ -z "$username" ]
    then
        read -p "username: " username
    fi
}

function prompt_for_email() {
    if [ -z "$email" ]
    then
        read -p "email: " email
    fi
}

function create_gitconfig() {
    if [ ! -e $gitconfig ]
    then
        prompt_for_username
        prompt_for_email
        echo $'[user]' >> $gitconfig
        echo $'\tname = '$username >> $gitconfig
        echo $'\temail = '$email >> $gitconfig
    fi
}

function create_hgrc() {
    if [ ! -e $hgrc ]
    then
        prompt_for_username
        prompt_for_email
        echo $'[ui]' >> $hgrc
        echo $'username='$username'<'$email'>' >> $hgrc
    fi
}

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

for dot_file in .[a-zA-Z]*
do
    if [ \( $dot_file != .git \) -a \( $dot_file != .gitignore \) ]
    then
        if [ $dot_file = .config ]
        then
            :
        elif [ -d $dot_file ]
        then
            rm -rf $home_dir/$dot_file
            cp -R $dot_file $home_dir
        else
            cp $dot_file $home_dir
        fi
    fi
done

mkdir -p $home_dir/.config/fish
for file in .config/fish/*
do
    cp $file $home_dir/.config/fish
done

mkdir -p $home_dir/Local/share
for file in Local/share/*
do
    sed "s:HOME_DIR:${HOME}:" < $file > $home_dir/$file
done

create_gitconfig
create_hgrc

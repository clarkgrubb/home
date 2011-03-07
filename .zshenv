
export COMMON_PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
export OS_TYPE=`uname -s`
if [[ $OS_TYPE[0,6] == CYGWIN ]]
then export OS_TYPE='Cygwin'
fi

PS1="$OS_TYPE:%~ $ "

if [[ $OS_TYPE == 'Darwin' ]]
then

    export HOSTNAME=`hostname -s`
    export OS_PATH=/usr/local/mysql/bin
    export PATH=$OS_PATH:$COMMON_PATH

elif [[ $OS_TYPE == 'Linux' ]]
then

    export HOSTNAME=`hostname -s`
    export OS_PATH=/home/clarkgrubb/Source/io/build/_build/binaries
    export PATH=$OS_PATH:$COMMON_PATH
 
elif [[ $OS_TYPE == 'Cygwin' || $OS_TYPE == 'Windows' ]]
then

    export HOSTNAME=`hostname`
    export OS_PATH_PRIORITY=~/bin
    export OS_PATH='/cygdrive/c/Program Files (x86)'/Git/bin
    export PATH=$OS_PATH_PRIORITY:$COMMON_PATH:$OS_PATH

    cd $HOME

else

    echo "unrecognized OS:" $OS_TYPE

fi



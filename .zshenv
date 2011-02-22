
export COMMON_PATH=/usr/X11/bin:/usr/sbin:/sbin:/bin:/usr/bin:/opt/local/sbin:/opt/local/bin:/usr/local/bin
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
 
elif [[ $OS_TYPE == 'Cygwin' ]]
then

    export HOSTNAME=`hostname`
    export OS_PATH_PRIORITY=~/bin
    export OS_PATH='/Program Files (x86)'/Git/bin
    export PATH=$OS_PATH:$PATH:$OS_PATH_PRIORITY

else

    echo "unrecognized OS:" $OS_TYPE

fi



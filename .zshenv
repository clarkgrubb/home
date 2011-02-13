
export COMMON_PATH=/usr/local/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
export OS_TYPE=`uname -s`
if [[ $OS_TYPE =~ ^CYGWIN ]]
then export OS_TYPE='Cygwin'
fi

PS1="$OS_TYPE:%~ $ "

if [[ $OS_TYPE == 'Darwin' ]]
then

    export HOSTNAME=`hostname -s`
    export OS_PATH=/opt/local/lib/postgresql81/bin:~/Source/android-sdk-mac_86/tools
    export PATH=$COMMON_PATH:$OS_PATH

elif [[ $OS_TYPE == 'Linux' ]]
then

    export HOSTNAME=`hostname -s`
    export OS_PATH=/usr/local/mercury-0.13.1/bin:/home/clarkgrubb/Source/io/build/_build/binaries
    export PATH=$COMMON_PATH:$OS_PATH
 
elif [[ $OS_TYPE == 'Cygwin' ]]
then

    export HOSTNAME=`hostname`
    export OS_PATH_PRIORITY=~/bin
    export OS_PATH='/Program Files (x86)'/Git/bin
    export PATH=$OS_PATH_PRIORITY:$PATH:$OS_PATH

else

    echo "unrecognized OS:" $OS_TYPE

fi



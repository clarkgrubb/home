if [ ! -z $OS_TYPE ]
then
    if [[ $OS_TYPE != "Cygwin" ]]
    then
        tabname $OS_TYPE
    fi
fi

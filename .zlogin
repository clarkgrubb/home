# zsh loads .zlogin only if it is a login shell and
# after it loads .zshrc.

if [ ! -z $OS_TYPE ]
then
    if [[ $OS_TYPE != "MinGW" ]]
    then
        tabname $OS_TYPE
    fi
fi

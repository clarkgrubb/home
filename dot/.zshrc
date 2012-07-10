# Aliases and Shell Function Definitions
#

export OS_TYPE=`uname -s`
if [[ $OS_TYPE[0,6] == CYGWIN ]]
then
    export OS_TYPE='Cygwin'
fi

alias r='command r'

#
# Macintosh personalizations
#
if [[ $OS_TYPE == 'Darwin' ]]
then

    pman() {
        man -t "$@" | open -f -a /Applications/Preview.app
    }

    # Defines function 'itunes'
    #
    . ~/.zsh_itunes

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

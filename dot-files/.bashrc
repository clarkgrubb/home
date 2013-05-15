# Environment Variables, Aliases, and Shell Function Definitions
#

export COMMON_PATH=/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
export OS_TYPE=`uname -s`
if [[ ${OS_TYPE:0:6} == CYGWIN ]]
then
    export OS_TYPE=Cygwin
fi
if [[ ${OS_TYPE:0:5} == MINGW ]]
then
    export OS_TYPE=MinGW
fi

export EDITOR='emacs -q'
export HISTSIZE=2000
export HISTFILE=~/.bash_history

# No version control info in prompt :(
#

export PS1="\[\033[31m\]$OS_TYPE:bash \[\033[34m\]\w \[\033[30m\]\`if [ \$? == 0 ]; then echo \:\); else echo \:\(; fi\` "

# awk with tab as FS
#
function tawk() {
    awk -F $'\t' "$@"
}

if [[ $OS_TYPE == Darwin ]]
then

    export PATH=$COMMON_PATH
    export JAVA_HOME=/Library/Java/Home

    pman() {
        man -t "$@" | open -f -a /Applications/Preview.app
    }

elif [[ $OS_TYPE == Linux ]]
then

    export PATH=$COMMON_PATH

    jvm_dir=/usr/lib/jvm
    for jdk in java-6-openjdk java-6-openjdk-i386 java-6-sun
    do
        if [ -e ${jvm_dir}/${jdk} ]
        then
            export JAVA_HOME=${jvm_dir}/${jdk}
        fi
    done

elif [[ $OS_TYPE == Cygwin || $OS_TYPE == MinGW ]]
then

    export PATH=$PATH:$COMMON_PATH

    cd $HOME

else

    echo "unrecognized OS:" $OS_TYPE

fi

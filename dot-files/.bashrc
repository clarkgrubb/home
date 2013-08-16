# Environment Variables, Aliases, and Shell Function Definitions
#

export COMMON_PATH=~/Bin:/usr/local/bin:/usr/bin:/bin:/sbin:/usr/sbin:/usr/X11/bin
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
    export JAVA7_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_25.jdk/Contents/Home
    export JAVA6_HOME=/Library/Java/Home
    export JAVA_HOME=$JAVA6_HOME
    export PATH=$JAVA_HOME/bin:$PATH

    # For rbenv and virtualenv
    #
    export PATH=~/.rbenv/shims:$PATH
    export VIRTUAL_ENV_DISABLE_PROMPT=1

    pman() {
        man -t "$@" | open -f -a /Applications/Preview.app
    }

elif [[ $OS_TYPE == Linux ]]
then

    export PATH=$COMMON_PATH

    jvm_dir=/usr/lib/jvm
    JAVA7_HOME=/usr/lib/jvm/java-7-openjdk-amd64
    for jdk in java-6-openjdk java-6-openjdk-i386 java-6-sun
    do
        if [ -e ${jvm_dir}/${jdk} ]
        then
            export JAVA6_HOME=${jvm_dir}/${jdk}
        fi
    done
    JAVA_HOME=$JAVA6_HOME
    export PATH=$JAVA_HOME/bin:$PATH

    # For rbenv and virtualenv
    #
    export PATH=~/.rbenv/shims:$PATH
    export VIRTUAL_ENV_DISABLE_PROMPT=1



elif [[ $OS_TYPE == Cygwin || $OS_TYPE == MinGW ]]
then

    export PATH=$PATH:$COMMON_PATH

    cd $HOME

else

    echo "unrecognized OS:" $OS_TYPE

fi

function _project_test {
    for dir in .git .hg .bzr
    do
        if [ -e $dir ]
        then
            return 0
        fi
    done
    return 1
}

function _home_test {
    if [ $(pwd) = ~ ]
    then
        return 0
    else
        return 1
    fi
}

function _root_test {
    if [ $(pwd) = / ]
    then
        return 0
    else
        return 1
    fi
}

# Move up one directory at-a-time until we are in a project, the
# home directory, or the root directory.
#
function up {
    while ! _project_test && ! _home_test && ! _root_test
    do
        cd ..
    done
}

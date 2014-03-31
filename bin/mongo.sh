#!/bin/bash

set -eu -o pipefail

underscore_url=http://underscorejs.org/underscore.js
underscore_file=~/Local/src/underscore.js

if [ ! -e $underscore_file ]
then
  curl $undescore_url > $underscore_file
fi

mongo $1 $underscore_file --shell

#!/bin/bash


PWD=`pwd`
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd ${DIR}/src

if [ $# -eq 0 ]; then
    ERLS=`ls *.erl`
else
    ERLS=$@
fi

for erl in $ERLS; do
    filename="${erl%.*}"
    erlc ${filename}.erl
    mv ${filename}.beam ../ebin/
    yaws --load ${filename}
done
cd $PWD

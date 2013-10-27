#!/bin/bash

PWD=`pwd`
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd ${DIR}/src
for erl in `ls *.erl`; do
    erlc $erl
    filename="${erl%.*}"
    mv ${filename}.beam ../ebin/
    yaws --load ${filename}
done
cd $PWD

#yaws --load bang
#yaws --load bang_utilities
#yaws --load bang_user
#yaws --load bang_db
#yaws --load bang_json
#yaws --load bang_http

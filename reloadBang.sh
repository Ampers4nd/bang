#!/bin/bash

pwd=`pwd`
srcDir=./src
ebinDir=./ebin
filename="*.erl"

bangincludeDir=./include
yawsincludeDir=/usr/lib/yaws/include

doterlang=~/.erlang
doterlangpatha='code:add_patha("'`pwd`'/ebin").'
doterlangpathamatch=`grep -F "${doterlangpatha}" $doterlang`


# create directory for binaries
if [ ! -d $ebinDir ]; then
    echo "[...] create: ${ebinDir}"
    mkdir $ebinDir
fi

if [ ! -d $bangincludeDir ]; then
    echo "[...] create ${bangincludeDir}"
    ln -s $yawsincludeDir $bangincludeDir    
fi
##/usr/lib/yaws/include

if [ ! -f $doterlang ]; then
    echo "[...] create: ${doterlang}"
    touch $doterlang
fi

if [ ! -n $doterlangpathamatch ]; then
    echo "[...] modify: (add bang) ${doterlang}"
    echo $doterlangpatha >> $doterlang
fi



OLDIFS=$IFS
IFS=$'\n' # save and change IFS
fileArr=($(find $inputDir -name "${filename}"))
IFS=$OLDIFS # restore it

tLen=${#fileArr[@]}
for ((i=0; i<${tLen}; i++)); do
    echo "[...] compile: ${fileArr[$i]}"
    erlc -o $ebinDir ${fileArr[$i]}.erl 
done

for ((i=0; i<${tLen}; i++)); do
    #echo "[...] load: " $(basename ${fileArr[$i]%.*})
    yaws --load $(basename ${fileArr[$i]%.*})
done




#!/bin/bash
NAME=$(mktemp --suffix=.remmina)
cp -f ./remmina.template $NAME
echo server=$2:$3 >> $NAME

echo $NAME

remmina -e $NAME
#rdesktop $2:$3
echo remove $NAME
#rm $NAME

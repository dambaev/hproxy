#!/bin/bash
NAME=$(mktemp --suffix=.remmina)
cp -f ./remmina.template $NAME
echo server=$2:$3 >> $NAME
remmina -e $NAME
#rdesktop $2:$3
#rdesktop $2:$3
rm $NAME

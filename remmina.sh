#!/bin/bash
NAME=$(mktemp --suffix=.remmina)
cp -f /usr/share/hproxy/remmina.template $NAME
echo server=$2:$3 >> $NAME
remmina -e $NAME
rm $NAME

#!/bin/bash
DEST=$(zenity --entry --text="Введите IP-адрес SSH-сервера, пример: 192.168.0.1")
RET=$?
if [ "$DEST" == "" ] || [ "$RET" != "0" ]; then
    zenity --error --text="ввод отклонен"
    exit 1
fi

hproxy.sh "-s 10.255.0.40:22 -d $DEST:22 --conn-count 3 --client-script /usr/share/hproxy/ssh-client.sh"

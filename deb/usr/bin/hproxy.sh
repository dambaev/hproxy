#!/bin/bash

USER=`zenity --entry --text="Введите логин в домене"`

hproxy -u $USER $1

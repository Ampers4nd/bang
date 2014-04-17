#!/bin/bash

IP="localhost"
PORT=4443
RESOURCE=register
UNAME=default@gmail.com
PW=Qwert1234!

curl -v -k -X "POST" -H "Content-Type: application/json" -d "{\"username\":\"${UNAME}\",\"password\":\"${PW}\", \"data\":{\"more\":\"stuff\"}}"  https://${IP}:${PORT}/api/1.0/${RESOURCE};
echo

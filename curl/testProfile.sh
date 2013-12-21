#!/bin/bash

if [ $# -lt 4 ]; then
    echo "Oops. Missing arguments. Call the script like this: $0 IP APP_ID CL_ID TOKEN"
    exit
fi

METHOD=POST
IP=$1
PORT="4443"
RESOURCE="user-profile"
APP_ID=$2
CL_ID=$3

#echo "curl -v -k -X POST -H \"Content-Type: application/json\" -d "{\"application_id\":\"${APP_ID}\", \"client_id\":\"${CL_ID}\", \"redirect_ur\":\"${REDIRECT_URI}\" ,\"credentials\":{\"username\":\"${UNAME}\", \"password\":\"${PW}\"}}" https://${IP}:${PORT}/api/1.0/${RESOURCE};"'
curl -v -k -X ${METHOD} -H "Content-Type: application/json" -d "{\"application_id\":\"${APP_ID}\", \"client_id\":\"${CL_ID}\",}}" https://${IP}:${PORT}/api/1.0/${RESOURCE};


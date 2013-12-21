#!/bin/bash

if [ $# -lt 6 ]; then
    echo "Oops. Missing arguments. Call the script like this: $0 IP APP_ID CL_ID UNAME PW REDIRECT_URI"
    exit
fi

METHOD=POST
IP=$1
PORT="4443"
RESOURCE="add-redirect"
APP_ID=$2
CL_ID=$3
REDIRECT_URI=$4
UNAME=$5
PW=$6

#echo "curl -v -k -X POST -H \"Content-Type: application/json\" -d "{\"application_id\":\"${APP_ID}\", \"client_id\":\"${CL_ID}\", \"redirect_ur\":\"${REDIRECT_URI}\" ,\"credentials\":{\"username\":\"${UNAME}\", \"password\":\"${PW}\"}}" https://${IP}:${PORT}/api/1.0/${RESOURCE};"'
curl -v -k -X ${METHOD} -H "Content-Type: application/json" -d "{\"application_id\":\"${APP_ID}\", \"client_id\":\"${CL_ID}\", \"redirect_uris\":\"\[${REDIRECT_URI}\]\" ,\"credentials\":{\"username\":\"${UNAME}\", \"password\":\"${PW}\"}}" https://${IP}:${PORT}/api/1.0/${RESOURCE};


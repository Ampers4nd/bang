#!/bin/bash

if [ $# -ne 4 ]; then
    echo "Oops. Missing arguments. Call the script like this: $0 APP_ID CL_ID UNAME PW"
    exit
fi

METHOD=POST
IP=localhost
PORT="4443"
RESOURCE="sign-in"
APP_ID=$1
CL_ID=$2
REDIRECT_URI=x #hardcoded for now, redirect doesn't have real add support, but it is required for auth
UNAME=$3
PW=$4

curl -v -k -X ${METHOD} -H "Content-Type: application/json" -d "{\"application_id\":\"${APP_ID}\", \"client_id\":\"${CL_ID}\", \"redirect_uri\":\"${REDIRECT_URI}\" ,\"credentials\":{\"username\":\"${UNAME}\", \"password\":\"${PW}\"}}" https://${IP}:${PORT}/api/1.0/${RESOURCE};
echo

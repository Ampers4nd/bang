#!/bin/bash

if [ $# -ne 4 ]; then
    echo "Oops. Missing arguments. Call the script like this: $0 IP APP_ID CL_ID AUTH_CODE"
    exit
fi

METHOD=POST
IP=$1
PORT="4443"
RESOURCE="session-token"
APP_ID=$2
CL_ID=$3
AUTH_CODE=$4
#REDIRECT_URI=$4

curl -v -k -X ${METHOD} -H "Content-Type: application/json" -d "{\"application_id\":\"${APP_ID}\", \"client_id\":\"${CL_ID}\", \"auth_code\":\"${AUTH_CODE}\"}" https://${IP}:${PORT}/api/1.0/${RESOURCE};


#!/bin/bash

if [ $# -lt 3 ]; then
    echo "Oops. Missing arguments."
fi

METHOD=$1
IP=$2
PORT=4443
RESOURCE=form-register-enterprise

if [ ${METHOD} = "POST" ]; then
    echo "Doing POST..."
    curl -v -k -X $METHOD -H "Content-Type: application/json" -d '{"username":"abcdef","password":"hijklm", "data":{"more":"stuff"}}'  https://${IP}:${PORT}/api/1.0/${RESOURCE};
#elif [ ${METHOD} = "PUT" ]; then
#    echo "X0 METHOD = ${METHOD}";
#    VAL_ID=$3
#    if [ ${VAL_ID} = "" ]; then 
#        echo "Missing val id"
#    else
#        echo "Doing PUT!!!!!!!!!!!!!!!!!!!!"
#        echo "curl -v -k -X ${METHOD} -H "Content-Type: application/json" -d '{"token":"${VAL_ID}","user_type":"1"}' https://${IP}:${PORT}/api/1.0/${RESOURCE}"
#        curl -v -k -X ${METHOD} -H "Content-Type: application/json" -d "{\"token\":\"${VAL_ID}\",\"user_type\":\"1\"}" https://${IP}:${PORT}/api/1.0/${RESOURCE};
#    fi
elif [ ${METHOD} = "GET" ]; then
    echo "X1 METHOD = ${METHOD}";
    VAL_ID=$3
    if [ ${VAL_ID} = "" ]; then 
        echo "Missing val id";
    fi
    echo "Doing GET..."
    echo $VAL_ID
    echo "curl -v -k https://${IP}:${PORT}/api/1.0/validation/${VAL_ID}"
    curl -v -k https://${IP}:${PORT}/api/1.0/validation/${VAL_ID};
else
    echo "Oops. Unsupported Method Parameter: ${METHOD}"
fi

#!/bin/bash

if [ $# -lt 2 ]; then
    echo "Oops. Missing arguments."
fi

METHOD=$1
IP=$2

if [ ${METHOD} = "POST" ]; then
    echo "METHOD = ${METHOD}"
    echo "Doing POST!!!!!!!!!!!!!!!!!!!!"
    curl -v -X $METHOD -H "Content-Type: application/json" -d '{"username":"abdefc","password":"xsdsdyz"}'  http://${IP}:8000/form-register-enterprise;
elif [ ${METHOD} = "PUT" ]; then
    echo "X0 METHOD = ${METHOD}";
    VAL_ID=$3
    if [ ${VAL_ID} = "" ]; then 
        echo "Missing val id"
    else
        echo "Doing PUT!!!!!!!!!!!!!!!!!!!!"
        echo "curl -v -X ${METHOD} -H "Content-Type: application/json" -d '{"val_id":"${VAL_ID}","user_type":"1"}' http://${IP}:8000/form-register-enterprise"
        curl -v -X ${METHOD} -H "Content-Type: application/json" -d "{\"val_id\":\"${VAL_ID}\",\"user_type\":\"1\"}" http://${IP}:8000/form-register-enterprise;
    fi
elif [ ${METHOD} = "GET" ]; then
    echo "X1 METHOD = ${METHOD}";
    VAL_ID=$3
    if [ ${VAL_ID} = "" ]; then 
        echo "Missing val id";
    else
       echo "Doing GET!!!!!!!!!!!!!!!!!!!!"
       echo "curl -v http://${IP}:8000/form-register-enterprise?val_id=${VAL_ID}"
       curl -v http://${IP}:8000/form-register-enterprise?val_id=${VAL_ID};
    fi
else
    echo "Oops. Bad Method Parameter: ${METHOD}"
fi

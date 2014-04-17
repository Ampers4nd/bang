#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Oops. Missing val id. Call like this: $0 VAL_ID"
fi

IP=localhost
PORT=4443
VAL_ID=$1
curl -v -k https://${IP}:${PORT}/api/1.0/validation/${VAL_ID};
echo

#!/bin/bash

if [ $# -eq 0 ]; then
   echo "Oops. You need to specify the port."
   exit
fi

PORT=$1

#setup individual dbs
curl -X PUT "http://localhost:${PORT}/bang_enterprise_users"
curl -X PUT "http://localhost:${PORT}/bang_other_users"
curl -X PUT "http://localhost:${PORT}/bang_redirect"
curl -X PUT "http://localhost:${PORT}/bang_session"

#design documents
curl -X PUT -H "Content-Type: application/json" -d @bang_enterprise_users_design.txt "http://localhost:${PORT}/bang_enterprise_users/_design/users"
curl -X PUT -H "Content-Type: application/json" -d @bang_session_design.txt "http://localhost:${PORT}/bang_session/_design/session"
#curl -X PUT -H "Content-Type: application/json" "http://localhost:${PORT}/bang_other_users/_design/users" >  bang_other_users_design.txt
#curl -X PUT -H "Content-Type: application/json" "http://localhost:${PORT}/bang_redirect/_design/redirect" >  bang_redirect_design.txt

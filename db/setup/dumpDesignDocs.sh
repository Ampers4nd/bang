#!/usr/bin/bash

curl "http://localhost:5984/bang_enterprise_users/_design/users" >  bang_enterprise_users_design.txt
curl "http://localhost:5984/bang_session/_design/session" >  bang_session_design.txt
#curl "http://localhost:5984/bang_other_users/_design/users" >  bang_other_users_design.txt
#curl "http://localhost:5984/bang_redirect/_design/redirect" >  bang_redirect_design.txt


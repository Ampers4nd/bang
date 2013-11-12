#1/usr/bin/bash

#update views like this:
#curl -v -X POST -d '{"map": "function(doc){if (doc.application_id) {emit(doc.application_id, {client_id: doc.client_id});}}"}' -H "Content-Type: application/json" http://localhost:5984/bang_design/xxx/_view/myview

#add update handler
DB="bang_session"
HOST="http://localhost"
PORT="5984"
FILE="session_update.json"
curl -v -X POST -H "Content-type: application/json" -d @session_update.json "${HOST}:${PORT}/${DB}/_bulk_docs"

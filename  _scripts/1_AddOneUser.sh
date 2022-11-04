#!/bin/sh
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{"name":"Алиса", "login":"alica", "password":"alica", "admin": false, "author": true }' \
http://localhost:8080/login/users

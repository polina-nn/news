#!/bin/sh
curl -i \
--header "Cookie: servant-auth-cookie=keyuser" \
--header "Content-Type: application/json"   \
--request POST   \
--data '{"name":"Алиса6", "login":"alica6", "password":"alica6", "admin": false, "author": true }' \
http://localhost:8080/login/users

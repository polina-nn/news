#!/bin/sh
curl -i -POST \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--data '{"name":"Алиса", "login":"alica", "password":"alica", "admin": false, "author": true }' \
http://localhost:8080/login/users \


#!/bin/sh
curl -i -POST \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--data '{"name":"Алиса2", "login":"alica2", "password":"alica2", "admin": false, "author": true }' \
http://localhost:8080/login/users \


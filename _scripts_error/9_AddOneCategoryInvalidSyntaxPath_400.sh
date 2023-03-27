#!/bin/sh
curl -i  \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--request POST   \
--data '{ "path":"1a", "category":"Алина" }' \
http://localhost:8080/login/category
#!/bin/sh
curl -i  \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--request POST   \
--data '{ "parent":1000, "category":"Алиcия" }' \
http://localhost:8080/login/category




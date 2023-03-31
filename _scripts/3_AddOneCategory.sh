#!/bin/sh
curl -i -POST \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--data '{ "path":"1", "category":"Алиcа" }' \
http://localhost:8080/login/category \

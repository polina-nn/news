#!/bin/sh
curl -i -XPUT \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--data '{ "newCategory": "Авелина", "newParent":{ "getId" : 90 }}' \
http://localhost:8080/login/category/1 \

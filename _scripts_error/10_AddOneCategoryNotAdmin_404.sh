#!/bin/sh
curl -i \
--header "Cookie: servant-auth-cookie=keyuser" \
--header "Content-Type: application/json"   \
--request POST   \
--data '{ "parent":{ "getId" : 0 }, "category":"Алевтина" }' \
http://localhost:8080/login/category

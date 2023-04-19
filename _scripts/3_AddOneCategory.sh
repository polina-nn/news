#!/bin/sh
curl -i -POST \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--data '{ "category": "Анюта", "parent": { "getId" : 0 }  }' \
http://localhost:8080/login/category \
#!/bin/sh
curl -i -POST \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--data '{ "category": "Анна", "parent": { "parentId" : 0 }  }' \
http://localhost:8080/login/category \
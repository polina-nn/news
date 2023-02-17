#!/bin/sh
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{"name":"user", "login":"user", "password":"user", "admin": false, "author": false }' \
http://localhost:8080/login/users

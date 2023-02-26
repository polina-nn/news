#!/bin/sh
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{ "path":"1000", "category":"Алина" }' \
http://localhost:8080/login/category
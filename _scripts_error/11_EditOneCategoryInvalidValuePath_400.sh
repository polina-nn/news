#!/bin/sh
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request PUT   \
--data '{ "newPath":"100", "newCategory": "Авелина"}' \
http://localhost:8080/login/category/1


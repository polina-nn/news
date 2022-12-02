#!/bin/sh
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request PUT   \
--data '{ "new_category": "Авелина"}' \
http://localhost:8080/login/category/1


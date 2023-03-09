#!/bin/sh
curl -i -GET \
--header "Cookie: servant-auth-cookie=keypolina" \
http://localhost:8080/login/news/search?text='Сергей' \

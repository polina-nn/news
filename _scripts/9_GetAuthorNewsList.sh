#!/bin/sh
curl -i -GET \
--header "Cookie: servant-auth-cookie=keypolina" \
http://localhost:8080/login/news?content=Сергей&sort_by=photo&limit=10 \


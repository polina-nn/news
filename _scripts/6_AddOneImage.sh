#!/bin/sh
curl -i -POST \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--data '{"file":"pink", "format":"png","image": "./_images/pink_base64" }' \
http://localhost:8080/login/image \

#!/bin/sh
curl -i \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--request POST   \
--data '{"file":"white", "format":"png","image": "./_images/green_bad_base64" }' \
http://localhost:8080/login/image


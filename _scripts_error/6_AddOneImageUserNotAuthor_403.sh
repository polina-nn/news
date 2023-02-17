#!/bin/sh
curl -i --user user:user \
--header "Content-Type: application/json"   \
--request POST   \
--data '{"file":"white", "format":"png","image": "./_images/white_base64" }' \
http://localhost:8080/login/image


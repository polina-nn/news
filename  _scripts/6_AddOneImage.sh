#!/bin/sh"
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{"file":"white", "format":"png","image": "/Users/admin/news/_images/white_base64" }' \
http://localhost:8080/login/image


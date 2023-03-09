#!/bin/sh
curl -i -POST \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--data '{ "title": "Тандыр","newsCategoryId": 1, "text": "Тандыр построил печник Сергей. Работает", "images" : [{"file":"blue", "format":"png","image": "./_images/blue_base64" },{"file":"white", "format":"png","image": "./_images/white_base64" }, {"file":"yellow", "format":"png","image": "./_images/yellow_base64" }], "published" : true  }' \
http://localhost:8080/login/news \
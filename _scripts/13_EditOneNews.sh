#!/bin/sh
curl -i -XPUT \
--header "Cookie: servant-auth-cookie=keypolina" \
--header "Content-Type: application/json"   \
--data '{ "newTitle": "Камин Авелина" , "newImages" : [{"file":"yellow", "format":"png","image": "./_images/yellow_base64" },{"file":"blue", "format":"png","image": "./_images/blue_base64" },{"file":"white", "format":"png","image": "./_images/white_base64" }],  "newText": "Хороший камин построил Сергей.", "newCategory_id": 1}' \
http://localhost:8080/login/news/1 \


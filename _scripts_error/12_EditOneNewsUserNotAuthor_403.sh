#!/bin/sh
curl -i --user user:user \
--header "Content-Type: application/json"   \
--request PUT   \
--data '{ "newTitle": "Камин Авелина" , "newImages" : [{"file":"yellow", "format":"png","image": "./_images/yellow_base64" },{"file":"blue", "format":"png","image": "./_images/blue_base64" },{"file":"white", "format":"png","image": "./_images/white_base64" }],  "newText": "Хороший камин построил Сергей.", "newCategory_id": 1}' \
http://localhost:8080/login/news/10


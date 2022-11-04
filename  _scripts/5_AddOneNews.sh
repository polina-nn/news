#!/bin/sh"
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{ "title": "Тандыр","categoryId": 13,"text": "Тандыр построил печник Сергей. Работает","images" : [{"file":"blue", "format":"png","image": "/Users/admin/news/_images/blue_base64" },{"file":"white", "format":"png","image": "/Users/admin/news/_images/white_base64" }, {"file":"yellow", "format":"png","image": "/Users/admin/news/_images/yellow_base64" }], "published" : true  }' \
http://localhost:8080/login/news
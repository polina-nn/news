#!/bin/sh
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request PUT   \
--data '{ "new_title": "Камин Авелина" , "new_images" : [{"file":"yellow", "format":"png","image": "/Users/admin/news/_images/yellow_base64" },{"file":"blue", "format":"png","image": "/Users/admin/news/_images/blue_base64" },{"file":"white", "format":"png","image": "/Users/admin/news/_images/white_base64" }], "new_published" : false,  "new_text": "Хороший камин построил Сергей.", "new_category_id": 1}' \
http://localhost:8080/login/news/1


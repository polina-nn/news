
# Edit one news

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request PUT   \
--data '{ "new_title": "Камин Авелина" , "new_images" : [{"file":"yellow", "format":"png","image": "/Users/admin/news/_images/yellow_base64" },{"file":"blue", "format":"png","image": "/Users/admin/news/_images/blue_base64" },{"file":"white", "format":"png","image": "/Users/admin/news/_images/white_base64" }], "new_published" : false,  "new_text": "Хороший камин построил Сергей.", "new_category_id": 1}' \
http://localhost:8080/login/news/1

## Parameters of request 

CategoryId, title, text, published -  required.  
Array CreateImageRequest -  not required.

| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| new_title | string | news title |
| new_categoryId | number | news category id |
| new_text | string | news text |
| new_images	| array CreateImageRequest | news images  |
| new_published	| bool | news published or not|

CreateImageRequest parameters at [_docs/AddOneImage.md](_docs/AddOneImage.md) 

## Method 
PUT

## Аuthentication required 
Yes

## Permissions required 
Author

## Success Response

You receive JSON-serialized response with News object as a result.

News object in respons: 
| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| news_title  | string | news title|
| news_created | string | creation date in the format "2022-03-15" |
| news_author | string | author name |
| news_category | array Category | categories array  to root category |
| news_text	| string | news text |
| news_images	| array URI | images URI |
| news_published	| bool | news published or not |

Category object see at [_docs/AddOneCategory.md](_docs/AddOneCategory.md)   
URI object see at [_docs/AddOneImage.md](_docs/AddOneImage.md) 

## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 403 | Invalid permission for add news (user not author)|
| 400 | Invalid category or not png image or not base 64 image or not exist image file |
| 404 | Invalid news id (not exists) |
| 500 | SQL request error (need to contact the developer) |


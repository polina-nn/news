
# Edit one news

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request PUT   \
--data '{ "newTitle": "Камин Авелина" , "newImages" : [{"file":"yellow", "format":"png","image": "./_images/yellow_base64" },{"file":"blue", "format":"png","image": "./_images/blue_base64" },{"file":"white", "format":"png","image": "./_images/white_base64" }], "newPublished" : false,  "newText": "Хороший камин построил Сергей.", "newCategoryId": 1}' \
http://localhost:8080/login/news/1

## Parameters of request 

CategoryId, title, text, published -  required.  
Array CreateImageRequest -  not required.

| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| newTitle | string | news title |
| newCategoryId | number | news category id |
| newText | string | news text |
| newImages	| array CreateImageRequest | news images  |
| newPublished	| bool | news published or not|

CreateImageRequest parameters at [_docs/AddOneImage.md](_docs/AddOneImage.md) 

## Method 
PUT

## Authentication required 
Yes

## Permissions required 
Author

## Success Response

You receive JSON-serialized response with News object as a result.

News object in response: 
| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| newsTitle  | string | news title|
| newsCreated | string | creation date in the format "2022-03-15" |
| newsAuthor | string | author name |
| newsCategory | array Category | categories array  to root category |
| newsText	| string | news text |
| newsImages	| array URI | images URI |
| newsPublished	| bool | news published or not |

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


# Add one news

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{ "title": "Тандыр","newsCategoryId": 13,"text": "Тандыр построил печник Сергей. Работает","images" : [{"file":"blue", "format":"png","image": "./_image/blue_base64" },{"file":"white", "format":"png","image": "./_image/white_base64" }, {"file":"yellow", "format":"png","image": "./_image/yellow_base64" },{"file":"blue", "format":"png","image": "./_image/blue_base64" }], "published" : true  }' \
http://localhost:8080/login/news


## Parameters of request 

CategoryId, title, text, published -  required.  
Array CreateImageRequest -  not required.

| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| title | string | news title |
| newsCategoryId | number | news category id |
| text | string | news text |
| images	| array CreateImageRequest | news images  |
| published	| bool | news published or not|

CreateImageRequest parameters at [_docs/AddOneImage.md](_docs/AddOneImage.md) 

## Method 
POST

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
| 500 | SQL request error (need to contact the developer) |




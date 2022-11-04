# Get news list with search

## Example of request

```
http://localhost:8080/news/search?text='тандыр'
http://polina:polina@localhost:8080/login/news/search?text='Сергей'

```
Search for a phrase in the title, text of the news, in the category name, in the author's name
The request has two versions for authors and for everyone

## Method 
GET

## Аuthentication required 
YES - http://polina:polina@localhost:8080/login/news/search?text='Сергей'
NO - http://localhost:8080/news/search?text='тандыр'

## Permissions required 
YES - author
NO

## Success Response

You receive JSON-serialized response with list of news object as a result.

News object in respons: 

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
| 400 | Invalid limit or offset or filer or searcth params  |
| 403 | Invalid permission get news (for request with authorization)  |
| 500 | SQL request error (need to contact the developer) |

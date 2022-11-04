# Get news list

## Example of request

```
curl -v "http://localhost:8080/news?created_at=2022-10-13&created_until=2022-09-30&limit=5"
curl -v "http://localhost:8080/news?author=Егор&title=Тандыр&content=Сергей&sort_by=foto"
curl -v "http://polina:polina@localhost:8080/login/news?created_at=2022-10-13&created_until=2022-09-30&limit=5"
```
The request has two versions for authors and for everyone

## Parameters of request 
list of query parameters:
| Field        |  Description       |  
| ------------- |-------------------|  
| created_at | news creation date  (in the format 2022-12-31)|  
| created_until | news created before the date (in the format 2022-12-31)|
| created_since |  news created after the date (in the format 2022-12-31) |
| author |  authors name |
| category_id	|  category by id |
| title	| the phrase is in the title |
| content |  the phrase is in the title |
| sort_by |  sort by one of the options (see below) |
| offset	|  offset from start |
| limit	| maximum number of records in result|

By default, data sorting by news creation time (newest news first) by the Postgress.  
Then it sorted accord by haskell code (if sortBy parameter exists in request). This is due to incorrect sorting of Cyrillic by the Postgress. I was unable to set up the correct alphabetical sorting of the data.

Query parameter sort_by:

| Value        | Description       |
| ------------- |-------------------|
| author | authors name alphabetical sorting |
| category | category name  alphabetical sorting |
| data |  sorting by news creation time (newest news first)  |
| foto |  the number of photos in the news|
| category_id	|  category by id |


## Method 
GET

## Аuthentication required 
YES - http://polina:polina@localhost:8080/login/news
NO - http://localhost:8080/news

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
| 400 | Invalid limit or offset or filer   |
| 403 | Invalid permission get news (for request with authorization)  |
| 500 | SQL request error (need to contact the developer) |

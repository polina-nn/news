# Get category list

## Example of request
```
curl -v "http://localhost:8080/category?offset=0&limit=10"
```
 Can use with offset and limit.[See here](OffsetLimit.md)  If limit more then you set in config, server use limit from config.
```
curl -v "http://localhost:8080/category"
curl -v "http://localhost:8080/category?limit=10"
curl -v "http://localhost:8080/category?offset=2&limit=3"
```
Attention please! Offset does not work after limit.
```
... /users?limit=3&offset=4  == ... /users?limit=3 
```

## Method 
GET

## Authentication required 
No

## Permissions required 
No

## Success Response

You receive JSON-serialized response with list of categories object as a result.

Category object in response: 
| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| categoryId | number | Category id is assigned by the server automatically. |
| categoryName | string | Category name|
| categoryPath	 | string | Category path |


## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 400 | Invalid limit or offset  |
| 500 | SQL request error (need to contact the developer) |

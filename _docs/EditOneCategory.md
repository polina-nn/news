# Edit one category

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request PUT   \
--data '{ "newParent":"2.5", "newCategory": "Авелина"}' \
http://localhost:8080/login/category/1


## Example of categories
 
Category id is the last parameter in uri.  
Category name is unique.
Category parent for root categories is 0.

## Parameters of request 

All parameters are not required.

| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| newParent | int | parent to category |
| newCategory	| string | category name |




## Method 
PUT

## Authentication required 
Yes

## Permissions required 
Admin

## Success Response

You receive JSON-serialized response with category object as a result.

User object in response: 
| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| categoryId | number | Category id |
| categoryName | string | Category name|
| categoryParent	 |  number | Category parent |



## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 404 | Invalid permission for add category (user not admin)|
| 400 | Invalid parent or name |
| 404 | Invalid category id (not exists) |
| 500 | SQL request error (need to contact the developer) |






# Add one category

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{ "parent":"1", "category":"Алина" }' \
http://localhost:8080/login/category

## Example of categories
 
Category id is assigned by the server automatically.  
Category name is unique.
Category parent for root categories is 0.

## Parameters of request 

All parameters are required.

| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| parent | int | parent to category |
| category	| string | category name |



## Method 
POST

## Authentication required 
Yes

## Permissions required 
Admin

## Success Response

You receive JSON-serialized response with category object as a result.

Category object in response: 
| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| categoryId | number | Category id is assigned by the server automatically. |
| categoryName | string | Category name|
| categoryParent	 | number | Category parent |



## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 404 | Invalid permission for add category (user not admin)|
| 400 | Invalid parent or name |
| 500 | SQL request error (need to contact the developer) |




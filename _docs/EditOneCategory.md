# Edit one category

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request PUT   \
--data '{ "newPath":"2.5", "newCategory": "Авелина"}' \
http://localhost:8080/login/category/1


## Example of categories
 
CategoryId is the last parameter in uri.  
CategoryPath is unique, category names can be the same.  
The request specifies a new puff and a new category name. Puffs of other categories are rebuilt automatically if necessary.

Use this example to determine the path (syntax and logic. No holes or cycles in the numbering).
Category {categoryPath = 1, categoryId = 1, categoryName = Ася }   
Category {categoryPath = 1.1, categoryId = 11, categoryName = Ася Борисовна }   
Category {categoryPath = 1.2, categoryId = 9, categoryName = Ася Михайловна }   
Category {categoryPath = 1.2.1, categoryId = 10, categoryName = Ася Михайловна Иванова }   
Category {categoryPath = 1.2.1.1, categoryId = 12, categoryName = Ася Михайловна Иванова-Борисова }   
Category {categoryPath = 1.2.1.2, categoryId = 13, categoryName = Ася Михайловна Иванова-Власова }  
Category {categoryPath = 2, categoryId = 2, categoryName = Борис  }  
Category {categoryPath = 3, categoryId = 8, categoryName = Вася }   
Category {categoryPath = 4, categoryId = 3, categoryName = Женя  }   


## Parameters of request 

All parameters are required.

| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| newPath | string | path to category |
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
| categoryPath	 | string | Category path |



## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 404 | Invalid permission for add category (user not admin)|
| 400 | Invalid syntax or value of the path |
| 404 | Invalid category id (not exists) |
| 500 | SQL request error (need to contact the developer) |






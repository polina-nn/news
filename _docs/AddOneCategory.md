# Add one category

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{ "path":"1", "category":"Алина" }' \
http://localhost:8080/login/category

## Example of categories

Use this example to determine the path (syntax and logic. No holes or cycles in the numbering).  
CategoryId is assigned by the server automatically.  
CategoryPath is unique, category names can be the same.  
When adding a category with categoryPath=1, puffs of all categories below will be automatically overwritten.  
(Categories with name = Ася will have categoryPath = 2 and so on)  

Category {categoryPath = 1, category_id = 1, categoryName = Ася }   
Category {categoryPath = 1.1, category_id = 11, categoryName = Ася Борисовна }   
Category {categoryPath = 1.2, category_id = 9, categoryName = Ася Михайловна }   
Category {categoryPath = 1.2.1, category_id = 10, categoryName = Ася Михайловна Иванова }   
Category {categoryPath = 1.2.1.1, category_id = 12, categoryName = Ася Михайловна Иванова-Борисова }   
Category {categoryPath = 1.2.1.2, category_id = 13, categoryName = Ася Михайловна Иванова-Власова }  
Category {categoryPath = 2, category_id = 2, categoryName = Борис  }  
Category {categoryPath = 3, category_id = 8, categoryName = Вася }   
Category {categoryPath = 4, category_id = 3, categoryName = Женя  }   


## Parameters of request 

All parameters are required.

| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| path | string | path to category |
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
| categoryPath	 | string | Category path |



## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 404 | Invalid permission for add category (user not admin)|
| 400 | Invalid syntax or value of the path |
| 500 | SQL request error (need to contact the developer) |




# Add one category

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{ "path":"1", "category":"Алина" }' \
http://localhost:8080/login/category

## Example of categories

Use this example to determine the path (Syntasis and logic. No holes or cycles in the numbering).  
Category_id is assigned by the server automatically.  
Сategory_path is unique, category names can be the same.  
When adding a category with category_path=1, puffs of all categories below will be automatically overwritten.  
(Сategory with name = Ася will have category_path = 2 and so on)  

Category {category_path = 1, category_id = 1, category_name = Ася }   
Category {category_path = 1.1, category_id = 11, category_name = Ася Борисовна }   
Category {category_path = 1.2, category_id = 9, category_name = Ася Михайловна }   
Category {category_path = 1.2.1, category_id = 10, category_name = Ася Михайловна Иванова }   
Category {category_path = 1.2.1.1, category_id = 12, category_name = Ася Михайловна Иванова-Борисова }   
Category {category_path = 1.2.1.2, category_id = 13, category_name = Ася Михайловна Иванова-Власова }  
Category {category_path = 2, category_id = 2, category_name = Борис  }  
Category {category_path = 3, category_id = 8, category_name = Вася }   
Category {category_path = 4, category_id = 3, category_name = Женя  }   


## Parameters of request 

All parameters are required.

| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| path | string | path to category |
| category	| string | category name |



## Method 
POST

## Аuthentication required 
Yes

## Permissions required 
Admin

## Success Response

You receive JSON-serialized response with category object as a result.

User object in respons: 
| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| category_id | number | Category id is assigned by the server automatically. |
| category_name | string | Category name|
| category_path	 | string | Category path |



## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 404 | Invalid permission for add category (user not admin)|
| 400 | Invalid syntax or value of the path |
| 500 | SQL request error (need to contact the developer) |




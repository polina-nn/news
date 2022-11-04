# Edit one category

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request PUT   \
--data '{ "new_path":"2.5", "new_category": "Авелина"}' \
http://localhost:8080/login/category/1


## Example of categories
 
Category_id is the last parameter in uri.  
Сategory_path is unique, category names can be the same.  
The request specifies a new puff and a new category name. Puffs of other categories are rebuilt automatically if necessary.

Use this example to determine the path (Syntasis and logic. No holes or cycles in the numbering).
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
| new_path | string | path to category |
| new_category	| string | category name |



## Method 
PUT

## Аuthentication required 
Yes

## Permissions required 
Admin

## Success Response

You receive JSON-serialized response with category object as a result.

User object in respons: 
| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| category_id | number | Category id |
| category_name | string | Category name|
| category_path	 | string | Category path |



## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 404 | Invalid permission for add category (user not admin)|
| 400 | Invalid syntax or value of the path |
| 404 | Invalid category id (not exists) |
| 500 | SQL request error (need to contact the developer) |






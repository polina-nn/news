# Get users list

## Example of request
```
curl -v "http://localhost:8080/users?offset=2&limit=3"
```
 Can use wiht offset and limit.[See here](OffsetLimit.md)  If limit more then you set in config, server use limit from config.
```
curl -v "http://localhost:8080/users"
curl -v "http://localhost:8080/users?limit=10"
curl -v "http://localhost:8080/users?offset=2&limit=3"
```
Attention please! Offset doesn`t work after limit.
```
... /users?limit=3&offset=4  == ... /users?limit=3 
```

## Method 
GET

## Аuthentication required 
No

## Permissions required 
No

## Success Response

You receive JSON-serialized response with list of users object as a result.

User object in respons: 
| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| user_admin| bool | true then user is admin|
| user_author| bool | true then user is author|
| user_created| string | creation date in the format "2022-03-15" |
| user_login| string | login for Basic Authentication |
| user_name	| string | user name |
| user_password	| null | user password for Basic Authentication. Don`t returned |


## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 400 | Invalid limit or offset  |
| 500 | SQL request error (need to contact the developer) |








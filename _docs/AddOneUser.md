# Add one user

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{"name":"Алиса", "login":"alica", "password":"alica", "admin": false, "author": true }' \
http://localhost:8080/login/users


## Parameters of request 

All parameters are required.

| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| admin| bool | true then user is admin|
| author| bool | true then user is author|
| login| string | login for Basic Authentication |
| name	| string | user name |
| password	| string | user password for Basic Authentication. Don`t returned |


## Method 
 POST

## Authentication required 
Yes

## Permissions required 
Admin

## Success Response

You receive JSON-serialized response with  User object as a result.

User object in response: 
| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| userAdmin| bool | true then user is admin|
| userAuthor| bool | true then user is author|
| userCreated| string | creation date in the format "2022-03-15" |
| userLogin| string | login for Basic Authentication |
| userName	| string | user name |
| userPassword	| null | user password for Basic Authentication. Don`t returned |


## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 404 | Invalid permission for add user (user not admin)|
| 400 | User with this login already existed |
| 500 | SQL request error (need to contact the developer) |


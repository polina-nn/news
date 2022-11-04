# Add one image

## Example of request
curl -i --user polina:polina \
--header "Content-Type: application/json"   \
--request POST   \
--data '{"file":"white", "format":"png","image": "/Users/admin/news/_images/white_base64" }' \
http://localhost:8080/login/image


## Parameters of request 

All parameters are required.  

| Field         | Type 	   | Description       |
| ------------- | ---------|-------------------|
| title | string | file  name  |
| format | string | format - "png" |
| images	| string | file path of image file in base64   |

## Method 
POST

## Аuthentication required 
Yes

## Permissions required 
Author

## Success Response

You receive JSON-serialized response with URI string as a result for example http://localhost:8080/image/11  

Uri scheme ("Http"), uri host ("localhost") , uri port  (8080)  - set in the config.conf file to generate the required uri. See at [config.conf](config.conf)
 
## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 403 | Invalid permission for add news (user not author)|
| 400 | Invalid category or not png image or not base 64 image or not exist image file |
| 500 | SQL request error (need to contact the developer) |

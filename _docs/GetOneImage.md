## Get one image

## Example of request
```
curl -v  "http://localhost:8080/image/1"
```
## Method 
GET

## Аuthentication required 
No

## Permissions required 
No

## Success Response

You receive JSON-serialized response with URI string as a result for example http://localhost:8080/image/11  

Uri scheme ("Http"), uri host ("localhost") , uri port  (8080)  - set in the config.conf file to generate the required uri. See at [config.conf](config.conf)


## Error Responses

Response with error description. 

Possible causes handled on this request with a detailed description in logs. 

| Error code          | Why?                                                                |
| ----------------- | ------------------------------------------------------------------ |
| 500 | SQL request error (need to contact the developer) |







## References for developer
For this endpoint I use Custom Output Format in Servant API: 
https://github.com/sras/servant-examples/blob/master/src/CustomOutput.hs

https://stackoverflow.com/questions/35636829/sending-generic-content-type-in-servant
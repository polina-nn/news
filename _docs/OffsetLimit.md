# Offset and Limit

For some get methods (return list) can use offset and limit. 
If limit more then you set in config, server use limit from config.
This logic tested here [OffsetLimitSpec.hs](../test/OffsetLimitSpec.hs). 


Examples:
```
curl -v "http://localhost:8080/users"
curl -v "http://localhost:8080/users?limit=10"
curl -v "http://localhost:8080/users?offset=2&limit=3"
```
Attention please! Offset doesn`t work after limit.
```
... /users?limit=3&offset=4  == ... /users?limit=3 
```

```
curl -v "http://localhost:8080/users?offset=-2" offset <0, bad request, error message 

curl -v "http://localhost:8080/users?limit=-100" limit <0, bad request, error message 

curl -v "http://localhost:8080/users?offset=-4&limit=2" offset <0, bad request, error message 

curl -v "http://localhost:8080/users?offset=-4&limit=-2"  offset <0, bad request, error message 

```

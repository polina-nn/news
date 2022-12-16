# News - Simple REST Server
------

# Introduction
This is an educational project. The task is here - https://coda.io/@metalamp/education/4-15

I took this example as the basis of the project (use servant and postgresql-simple)  - https://github.com/jinilover/type-safe-ws

I took this example as the basis of the project (use handle pattern for logging and config)  - https://github.com/fullstack-development/haskell-internship/tree/master/echo-bot-template

You can use the links to study (review) some of the topics for this project. They are placed in the text, and in [# References](#references)

What can be improved in [# ToDoList](#todolist)

## Basic libraries

| library           | Used for                                                                |
| ----------------- | ------------------------------------------------------------------ |
| сonfigurator | to work with config |
| time  | to work with time in logging and creation news/users|
| servant|  for writing type-safe web applications |
| postgresql-simple |  for use the PostgreSQL database |\
| cryptonite | for password hashing |

If you have never worked with servant -
https://docs.servant.dev/en/stable/
https://github.com/sras/servant-examples


# Setup environment
------
## Install PostgreSql
Attention please!
You must have a Postgres on your computer! And you must to create a data base, user, password, port. 
You can see the example in **config.conf**  and use the same values.
```
 config {
    # DbConfig
    dbHost = "localhost"       #  "localhost" - dbHost for test server
    dbName = "tiny"            #  "news"      - dbName in postgress
    user = "postgres"          #  "postgres" - user in postgress
    password = "postgres123"   #  "postgres123" - password in n postgress
    dbPort = "5432"            # "5432"  - dbPort in postgress
    noOfStripes = 2             # 2 - stripes https://docs.servant.dev/en/stable/cookbook/db-postgres-pool/PostgresPool.html
    idleTime = 60               # 60 - unused connections are kept open for a minute
    stripeSize = 10             # 10 - max. 10 connections open per stripe
       }
```        
If you have never worked with postgres - 
https://learn.coderslang.com/ru/0119-setting-up-and-getting-started-with-postgresql/

## Set config
Attention please! You must have the **config.conf** file at the root of the project.
If file config.conf not exist, you must not run server. 
File **logs.txt** created automatically for logging (If you choose logging in file)
You must set some parameters in the **config.conf**:
* Application parameters: appPort (I use 8080) and appShowLimit. Maximum array length per get request.
* Database parameters (see above). 
* Parameters for connection pool: noOfStripes, idleTime, stripeSize . Don`t edit. You can see about it - https://docs.servant.dev/en/stable/cookbook/db-postgres-pool/PostgresPool.html
* Logging level (Debug / Info / Warning / Error).
* Logging output  (Terminal / File). 
* Parameters for creation URI: scheme (http or https), host ("localhost" for tests), port  (8080 for tests) 


## Build and start the application

1. Clone this repository.
2. You must use Stack
```haskell
stack build
stack exec news-exe
```
## For test
* for test use. I tested only logic function for working with categories and offset&limit.
```haskell
stack test
```
* how the server works and what we get in response to different requests (valid and not) see in [_requests](_requests).
* one curl request for each "End Point" see in [ _scripts]( _scripts). Use command sh fileName.sh in terminal. Scripts are numbered, it will be executed from the first

You can use it such as curl request. If you have never worked with curl - https://curl.se/docs/manual.html

## Migrations

All migrations are at  [migrations](migrations).
Before starting the server, you need to create a new empty database in the Postgres DB.
During migrations, a user with login "polina" and password "polina" is automatically created (Author and administrator).
You can create other administrators and authors, if you login as "polina", password "polina". 
You can change in [_migrations/01.sql](_migrations/01.sql) the login and password. You must generating a password hash by [app/MainBase64.hs] (app/MainBase64.hs). 

The name of the database and the owner put in the config:
```
config {
    # DbConfig    
    dbName = "tiny"            #  "tiny"      - dbName in postgress
    user = "postgres"          #  "postgres" - user in postgress
       }
```
## DatabaseSchema

Schema is at [DatabaseSchema](_docs/DatabaseSchema.md)



# Project structure 
------
I explain only content of some folders and files. In other places content is standard.
Folders with prefixed are used for documentation and running/testing server (examples requests or content and do not contain haskell code). 

```
news/
│ _docs/ # Depiction EndPoints for readme doc
│ │
│ _image/ # Images in png and base64 for example
│ │
│ _migrations/ # Contain two schemes: with data and empty for Data Base
│ │
│ _scripts/ # One request for each endpoint
│ │
│ _requests/ # Many requests for some endpoints
│ │
│ app/
│ │
│ ├── Main.hs       
│ ├── MainBase64.hs # Convert file from png to base64
│ ├── MainCR.hs     # Generate hash for password
│ │
│ src/
│ │
│ ├── EndPoints/ 
│ │   ├── Lib/ # Lib for End Points
│ │   │  ├── Category/  # Lib for Category End Points
│ │   │  ├── News/  # Lib for News End Points
│ │   │  ├── Lib.hs # Some function for many End Points
│ │   │  ├── OffsetLimit.hs     # Offset limit for End Points 
│ │   │  ├── ToHttpResponse.hs # class ToHttpResponse for throwError by servant
│ │   │  └── ToText.hs # class ToText.hs for present in logging results of requests
│ │   ├── AddOneCategory.hs #  Create one category (authentication admin required)
│ │   ├── AddOneImage.hs #  Create a path to one loaded image (authentication author required)
│ │   ├── AddOneNews.hs #  Create one news  (authentication author required)
│ │   ├── AddOneUser.hs # Create one user (authentication admin required)
│ │   ├── EditOneCategory.hs #  Edit one category
│ │   ├── EditOneNews.hs #  Edit one news
│ │   ├── GetAuthorsNewsList.hs # Get news list after filers and sorts (authentication author required)
│ │   ├── GetAuthorsNewsSearchList.hs # Get news list after search (authentication author required)
│ │   ├── GetCategoryList.hs # Get a list of categories
│ │   ├── GetNewsList.hs # Get news list after filers and sorts
│ │   ├── GetNewsSearchList.hs # Get news list after search  
│ │   ├── GetOneImage.hs # Get a one image
│ │   └── GetUserList.hs # Get a list of users 
│ │
│ ├── Logger/      
│ │   └── Impl.hs   # The default implementation of the Logger interface
│ │
│ ├── Types/        
│ │   ├── ApiTypes.hs   # RestAPI  
│ │   ├── DataTypes.hs  # all types except api, config, error
│ │   └── ErrorTypes.hs # Error for endpoints
│ │
│ ├── Config.hs  # Configuration reader and default config value
│ ├── DbServices.hs # Connection pool to DB used in module Server
│ ├── Logger.hs # The logger interface module. Not define an implementation
│ ├── News.hs # Handle for config and logger
│ └── Server.hs # run server and authorization (basic auth)
│
├── config.conf # config file 
└── logs # You create file logs by yourself for logging

```
# API endpoints
------
## Hellow server
Simple hello from server. Example:
```
curl -v http://localhost:8080/  
```
## GET methods for representation of a resource. Can use a browser for it.
For some get methods (return list) can use [Offset&Limit](_docs/OffsetLimit.md) 

* [Get category list](_docs/GetCategoryList.md) 
* [Get one image](_docs/GetOneImage.md) 
* [Get users list](_docs/GetUserList.md) 
* [Get news list. With filtering and sorting](_docs/GetNewsList.md) 
* [Get news list. With search](_docs/GetNewsSearchList.md) 

## POST methods. Add (create) a new resource in Data Base.

* [Post one category](_docs/AddOneCategory.md) 
* [Post one image](_docs/AddOneImage.md) 
* [Post one news](_docs/AddOneNews.md) 
* [Post one user](_docs/AddOneUser.md) 


## PUT methods to update (edit) mutable resources  in Data Base.
* [Put one category](_docs/EditOneCategory.md) 
* [Put one news](_docs/EditOneNews.md) 

## Error message

I define general list of errors, and errors list for each endpoint or group of endpoints. See here [src/Types/ErrorTypes.hs](src/Types/ErrorTypes.hs) 

| Code          | Description                                                           |
| ----------------- | ------------------------------------------------------------------ |
| 400 | Bad Request (Invalid limit or offset, invalid content by type or value|
| 403 | Forbidden Error (Invalid author permission)|
| 404 | Not Found (Invalid admin permission, Invalid id)|
| 500 | Internal Server Error (Error of executing SQL request) |


# References
------
* Mark Masse "REST API Design Rulebook"
* postgres https://learn.coderslang.com/ru/0119-setting-up-and-getting-started-with-postgresql/
* curl https://curl.se/docs/manual.html
* servant https://docs.servant.dev/en/stable/
* servant examples to use https://github.com/sras/servant-examples
* Determining the Primary Key of an Inserted Record http://samag.ru/archive/article/2140?ysclid=l8el1jdv4m991219764
* Нow postgresql's nextval() work 
  * https://stackoverflow.com/questions/36782006/i-dont-understand-how-postgresqls-nextval-work-can-someone-explain 
  * http://programming-lang.com/html/sql/glava%207/index5.htm


# ToDoList
------
* By default, data sorting by news creation time (newest news first) by the Postgress.
Then it sorted accord by haskell code (if sortBy parameter exists in request). This is due to incorrect sorting of Cyrillic by the Postgress. I was unable to set up the correct alphabetical sorting of the data.
* Нandling exceptions connecting to a non-existent database and work with log file
```
"Exception: libpq: failed (connection to server at "localhost" (::1), port 5432 failed: FATAL:  database "news2" does not exist"
"Exception: logs: openFile: resource busy (file is locked)"

```
* The terminal always displays authorization data, most likely it is from src/Server.hs
```
[]
[("\1050\1072\1084\1080\1085 \1040\1074\1077\1083\1080\1085\1072",2022-12-02,"polina","1","\1040\1074\1077\1083\1080\1085\1072","\1061\1086\1088\1086\1096\1080\1081 \1082\1072\1084\1080\1085 \1087\1086\1089\1090\1088\1086\1080\1083 \1057\1077\1088\1075\1077\1081.",PGArray {fromPGArray = [8,9,10]},3,True)]
[("\1050\1072\1084\1080\1085 \1040\1074\1077\1083\1080\1085\1072",2022-12-02,"polina","1","\1040\1074\1077\1083\1080\1085\1072","\1061\1086\1088\1086\1096\1080\1081 \1082\1072\1084\1080\1085 \1087\1086\1089\1090\1088\1086\1080\1083 \1057\1077\1088\1075\1077\1081.",PGArray {fromPGArray = [8,9,10]},3,True)]
```
* Add monad transformers to do with arrays such us [IO Either a b]



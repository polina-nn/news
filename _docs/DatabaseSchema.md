# Data Base Schema


# Table usr

| Field         | Type 	   | Notes       |
| ------------- | ---------|-------------------|
| usr_login| VARCHAR(50) PRIMARY KEY | Must be unique|
| usr_name| VARCHAR(50) NOT NULL |  |
| usr_password| VARCHAR(50) NOT NULL | Store the hash of the password, do not return it to anyone when asked |
| usr_created	|DATE NOT NULL |Automatically when user is created |
| usr_admin	| BOOLEAN NOT NULL |  |
| usr_author 	| BOOLEAN NOT NULL |  |

# Table image

| Field         | Type 	   | Notes       |
| ------------- | ---------|-------------------|
| image_id| BIGINT NOT NULL PRIMARY KEY| determine by SEQUENCE image_id_seq|
| image_name| VARCHAR(50) NOT NULL |  |
| image_type | VARCHAR(50) NOT NULL | png |
| image_content	|TEXT NOT NULL | |
 
In  image_type  = png . It may be useful if you have to store different types of images in the future.  
For this table define SEQUENCE image_id_seq, by which I assign image_id, when creating a picture

# Table category

| Field         | Type 	   | Notes       |
| ------------- | ---------|-------------------|
| category_id| INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY |  |
| category_path | VARCHAR(50) NOT NULL | Unique, no holes or loops (such us 1.23.12.1) |
| category_name | VARCHAR(50) NOT NULL |  |

Due to the uniqueness of the puff, it is easy to return the category_id

# Table news

| Field         | Type 	   | Notes       |
| ------------- | ---------|-------------------|
| news_id  | BIGINT NOT NULL PRIMARY KEY | determine by SEQUENCE news_id_seq |
| news_title | VARCHAR(100) NOT NULL |  |
| news_created | DATE NOT NULL | Automatically when user is created  |
| news_author_login 	| VARCHAR(50) NOT NULL |I store the  usr_login from the table usr, because login is unique |
| news_category_id 	| INT NOT NULL | I store the category_id from the table category |
| news_text 	| VARCHAR  NOT NULL |  |
| news_images_id 	| INT[] | I store the images_id from the table image  |
| news_published 	| BOOLEAN NOT NULL |  |

For this table define SEQUENCE news_id_seq, by which I assign news_id, when creating a news
News creates an already by existing user-author and in an existing category.  
Photos can be added then news creation. They are added to the image table.
When you create a news, you cannot add an image that has already been loaded into the image table.  
Therefore, there will be a lot of repeating photos in the image table.



CREATE TABLE IF NOT EXISTS usr (
    usr_login  VARCHAR(50) PRIMARY KEY,
    usr_name   VARCHAR(50) NOT NULL, 
    usr_password VARCHAR(50) NOT NULL, 
    usr_created  DATE NOT NULL,
    usr_admin    BOOLEAN NOT NULL,  
    usr_author   BOOLEAN NOT NULL);
    

INSERT INTO usr (usr_name, usr_login , usr_password, usr_created, usr_admin, usr_author )
VALUES 
       ('polina', 'polina',  's0zUqXz8ZkllWgC/qc0/s6y19z5ZSY7gR1tFR9tRYTE=', '2022-04-10', TRUE, TRUE) 
ON CONFLICT (usr_login) DO NOTHING ;

      

CREATE SEQUENCE IF NOT EXISTS image_id_seq; 
CREATE TABLE IF NOT EXISTS image (
    image_id  BIGINT NOT NULL PRIMARY KEY, 
    image_name VARCHAR(50) NOT NULL, 
    image_type VARCHAR(50) NOT NULL,
    image_content TEXT NOT NULL);

 

CREATE TABLE IF NOT EXISTS category (
    category_path  VARCHAR(50) NOT NULL , 
    category_id INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY, 
    category_name VARCHAR(50) NOT NULL);



CREATE SEQUENCE IF NOT EXISTS  news_id_seq; 
CREATE TABLE IF NOT EXISTS news (
    news_id     BIGINT NOT NULL PRIMARY KEY,
    news_title  VARCHAR(100) NOT NULL, 
    news_created  DATE NOT NULL,
    news_author_login VARCHAR(50) NOT NULL, 
    news_category_id INT NOT NULL, 
    news_text VARCHAR  NOT NULL, 
    news_images_id INT[],
    news_published BOOLEAN NOT NULL);









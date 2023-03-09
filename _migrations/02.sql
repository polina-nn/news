CREATE TABLE IF NOT EXISTS cookie (
    cookie_account  VARCHAR(50) PRIMARY KEY,
    cookie_key       VARCHAR(50) NOT NULL
    );
    

INSERT INTO cookie (cookie_account, cookie_key )
VALUES 
       ('polina', 'keypolina') 
ON CONFLICT (cookie_account ) DO NOTHING ;

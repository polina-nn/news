CREATE TABLE IF NOT EXISTS token (
    token_login  VARCHAR(50) PRIMARY KEY,
    token_key    VARCHAR(50) NOT NULL
    );
    

INSERT INTO token (token_login, token_key )
VALUES 
       ('polina', 'Nb/8bj4NLEbMvD472ODaONVLyJ3RM4Q5oWfe89Ctfdo=') 
ON CONFLICT (token_login ) DO NOTHING ;

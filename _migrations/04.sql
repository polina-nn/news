-- Attention !!! This scheme is working only if the news has not yet been deleted from the server 
-- and the current numbering was from 1 without holes.

-- NEWS TABLE changes: 
ALTER TABLE news  DROP COLUMN news_id;
ALTER TABLE news  ADD COLUMN news_id BIGINT GENERATED ALWAYS AS IDENTITY  (START WITH 1 INCREMENT BY 1);

-- IMAGE TABLE changes: 
ALTER TABLE image  DROP COLUMN image_id;
ALTER TABLE image  ADD COLUMN image_id BIGINT GENERATED ALWAYS AS IDENTITY  (START WITH 1 INCREMENT BY 1);


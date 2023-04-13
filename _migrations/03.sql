-- CATEGORY TABLE changes: 

-- 1. Add column category_parent_id and delete category_path. 
-- All previously created categories are transferred to the root category (parent_id = 0)
ALTER TABLE category ADD COLUMN category_parent_id INT;
UPDATE category SET category_parent_id = 0;
ALTER TABLE category ALTER COLUMN category_parent_id SET NOT NULL;
ALTER TABLE category DROP COLUMN category_path;

-- 2. Adding uniqueness for the category_name, we will check the name through the sql query error
ALTER TABLE category ALTER COLUMN category_name TYPE TEXT;
ALTER TABLE category ALTER COLUMN category_name SET NOT NULL;
ALTER TABLE category ADD UNIQUE (category_name); 

-- NEWS TABLE changes: 
ALTER TABLE news  ALTER COLUMN news_id ADD GENERATED ALWAYS AS IDENTITY;
DROP SEQUENCE IF EXISTS  news_id_seq;

-- IMAGE TABLE changes: 
ALTER TABLE image  ALTER COLUMN image_id ADD GENERATED ALWAYS AS IDENTITY;
DROP SEQUENCE IF EXISTS  image_id_seq;

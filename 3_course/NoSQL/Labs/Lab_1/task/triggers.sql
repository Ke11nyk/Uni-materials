-- 1. Trigger for updating UpdatedAt in Users table
CREATE OR REPLACE FUNCTION update_user_timestamp()
RETURNS TRIGGER AS $$
BEGIN
    NEW.UpdatedAt = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_user_timestamp_trigger
BEFORE UPDATE ON Users
FOR EACH ROW
EXECUTE FUNCTION update_user_timestamp();

-- 2. Trigger for updating UpdatedAt in Products table
CREATE OR REPLACE FUNCTION update_product_timestamp()
RETURNS TRIGGER AS $$
BEGIN
    NEW.UpdatedAt = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_product_timestamp_trigger
BEFORE UPDATE ON Products
FOR EACH ROW
EXECUTE FUNCTION update_product_timestamp();
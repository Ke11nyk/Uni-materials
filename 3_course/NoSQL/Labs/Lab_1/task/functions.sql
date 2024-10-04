-- 1. Soft Delete User Function
CREATE OR REPLACE FUNCTION soft_delete_user(p_user_id INT)
RETURNS VOID AS $$
BEGIN
    UPDATE Users
    SET IsDeleted = TRUE, UpdatedAt = CURRENT_TIMESTAMP
    WHERE UserID = p_user_id;
END;
$$ LANGUAGE plpgsql;

-- 2. Restore Soft Deleted User Function
CREATE OR REPLACE FUNCTION restore_soft_deleted_user(p_user_id INT)
RETURNS VOID AS $$
BEGIN
    UPDATE Users
    SET IsDeleted = FALSE, UpdatedAt = CURRENT_TIMESTAMP
    WHERE UserID = p_user_id;
END;
$$ LANGUAGE plpgsql;

-- 3. Soft Delete Product Function
CREATE OR REPLACE FUNCTION soft_delete_product(p_product_id INT, p_admin_id INT)
RETURNS VOID AS $$
BEGIN
    UPDATE Products
    SET IsDeleted = TRUE, UpdatedAt = CURRENT_TIMESTAMP, UpdatedBy = p_admin_id
    WHERE ProductID = p_product_id;
END;
$$ LANGUAGE plpgsql;

-- 4. Update Product with Change Tracking Function
CREATE OR REPLACE FUNCTION update_product(
    p_product_id INT,
    p_name VARCHAR(100),
    p_description TEXT,
    p_price DECIMAL(10, 2),
    p_stock_quantity INT,
    p_category_id INT,
    p_admin_id INT
)
RETURNS VOID AS $$
BEGIN
    UPDATE Products
    SET Name = p_name,
        Description = p_description,
        Price = p_price,
        StockQuantity = p_stock_quantity,
        CategoryID = p_category_id,
        UpdatedAt = CURRENT_TIMESTAMP,
        UpdatedBy = p_admin_id
    WHERE ProductID = p_product_id;
END;
$$ LANGUAGE plpgsql;

-- 5. Soft Delete Category Function
CREATE OR REPLACE FUNCTION soft_delete_category(p_category_id INT, p_admin_id INT)
RETURNS VOID AS $$
BEGIN
    UPDATE Categories
    SET IsDeleted = TRUE, UpdatedAt = CURRENT_TIMESTAMP, UpdatedBy = p_admin_id
    WHERE CategoryID = p_category_id;
END;
$$ LANGUAGE plpgsql;

-- 6. Soft Delete Discount Function
CREATE OR REPLACE FUNCTION soft_delete_discount(p_discount_id INT)
RETURNS VOID AS $$
BEGIN
    UPDATE Discounts
    SET IsDeleted = TRUE
    WHERE DiscountID = p_discount_id;
END;
$$ LANGUAGE plpgsql;

-- 7. Function to get total revenue by date range
CREATE OR REPLACE FUNCTION get_total_revenue(start_date DATE, end_date DATE)
RETURNS DECIMAL(10, 2) AS $$
DECLARE
    total_revenue DECIMAL(10, 2);
BEGIN
    SELECT COALESCE(SUM(TotalAmount), 0)
    INTO total_revenue
    FROM Orders
    WHERE OrderDate BETWEEN start_date AND end_date;
    
    RETURN total_revenue;
END;
$$ LANGUAGE plpgsql;
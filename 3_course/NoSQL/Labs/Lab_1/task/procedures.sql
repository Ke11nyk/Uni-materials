-- 1. Stored Procedure to apply discount to all products in a category
CREATE OR REPLACE PROCEDURE apply_category_discount(
    p_category_id INT,
    p_discount_percentage DECIMAL(5, 2),
    p_admin_id INT
)
LANGUAGE plpgsql
AS $$
BEGIN
    UPDATE Products
    SET Price = Price * (1 - p_discount_percentage / 100),
        UpdatedAt = CURRENT_TIMESTAMP,
        UpdatedBy = p_admin_id
    WHERE CategoryID = p_category_id AND IsDeleted = FALSE;
END;
$$;
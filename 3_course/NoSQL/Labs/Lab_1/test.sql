-- Examples of using stored procedures, custom functions, triggers, and views

-- 1. Soft delete a user
SELECT soft_delete_user(3);

-- 2. Restore a soft-deleted user
SELECT restore_soft_deleted_user(3);

-- 3. Soft delete a product
SELECT soft_delete_product(5, 1);

-- 4. Update a product with change tracking
SELECT update_product(1, 'Smartphone Pro', 'Latest pro model smartphone', 799.99, 120, 1, 2);

-- 5. Soft delete a category
SELECT soft_delete_category(3, 1);

-- 6. Soft delete a discount
SELECT soft_delete_discount(2);

-- 7. View active users
SELECT * FROM active_users;

-- 8. View active products with categories
SELECT * FROM active_products_with_category;

-- 9. Get total revenue for a date range
SELECT get_total_revenue('2023-09-01', '2024-12-31') AS total_revenue;

-- 10. Apply a category discount
CALL apply_category_discount(1, 15.00, 1);

-- 11. Demonstrate trigger usage (update user)
UPDATE Users SET FirstName = 'Johnny' WHERE UserID = 1;
SELECT UserID, FirstName, UpdatedAt FROM Users WHERE UserID = 1;

-- 12. Demonstrate trigger usage (update product)
UPDATE Products SET StockQuantity = 90 WHERE ProductID = 1;
SELECT ProductID, Name, StockQuantity, UpdatedAt FROM Products WHERE ProductID = 1;

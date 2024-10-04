-- 1. View for Active Users
CREATE OR REPLACE VIEW active_users AS
SELECT UserID, Username, Email, FirstName, LastName, CreatedAt
FROM Users
WHERE IsDeleted = FALSE;

-- 2. View for Active Products with Category Name
CREATE OR REPLACE VIEW active_products_with_category AS
SELECT p.ProductID, p.Name AS ProductName, p.Description, p.Price, p.StockQuantity,
       c.Name AS CategoryName, p.CreatedAt, p.UpdatedAt
FROM Products p
JOIN Categories c ON p.CategoryID = c.CategoryID
WHERE p.IsDeleted = FALSE AND c.IsDeleted = FALSE;
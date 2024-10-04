-- Populate the database with sample data

-- Insert Users
INSERT INTO Users (Username, Email, Password, FirstName, LastName, Address, PhoneNumber)
VALUES
('john_doe', 'john@example.com', 'password123', 'John', 'Doe', '123 Main St, City', '1234567890'),
('jane_smith', 'jane@example.com', 'password456', 'Jane', 'Smith', '456 Elm St, Town', '9876543210'),
('bob_johnson', 'bob@example.com', 'password789', 'Bob', 'Johnson', '789 Oak St, Village', '5555555555');

-- Insert Admins
INSERT INTO Admins (UserID, Role)
VALUES
(1, 'Super Admin'),
(2, 'Product Manager');

-- Insert Categories
INSERT INTO Categories (Name, Description, UpdatedBy)
VALUES
('Electronics', 'Electronic devices and accessories', 1),
('Clothing', 'Apparel and fashion items', 2),
('Books', 'Printed and digital books', 1);

-- Insert Products
INSERT INTO Products (Name, Description, Price, StockQuantity, CategoryID, UpdatedBy)
VALUES
('Smartphone', 'Latest model smartphone', 699.99, 100, 1, 1),
('Laptop', 'High-performance laptop', 1299.99, 50, 1, 1),
('T-shirt', 'Cotton t-shirt', 19.99, 200, 2, 2),
('Jeans', 'Denim jeans', 49.99, 150, 2, 2),
('Novel', 'Bestselling fiction novel', 14.99, 300, 3, 1);

-- Insert Warehouses
INSERT INTO Warehouses (Name, Address, Capacity)
VALUES
('Main Warehouse', '100 Storage Blvd, Warehouse City', 10000),
('Secondary Warehouse', '200 Inventory St, Stock Town', 5000);

-- Insert Inventory
INSERT INTO Inventory (ProductID, WarehouseID, Quantity)
VALUES
(1, 1, 75),
(1, 2, 25),
(2, 1, 40),
(2, 2, 10),
(3, 1, 150),
(3, 2, 50),
(4, 1, 100),
(4, 2, 50),
(5, 1, 200),
(5, 2, 100);

-- Insert Suppliers
INSERT INTO Suppliers (Name, ContactPerson, Email, PhoneNumber, Address)
VALUES
('TechSupplier', 'Alice Johnson', 'alice@techsupplier.com', '1112223333', '789 Tech Ave, Supplier City'),
('FashionWholesale', 'Bob Smith', 'bob@fashionwholesale.com', '4445556666', '456 Style St, Fashion Town');

-- Insert ProductSuppliers
INSERT INTO ProductSuppliers (ProductID, SupplierID, SupplyPrice)
VALUES
(1, 1, 500.00),
(2, 1, 1000.00),
(3, 2, 10.00),
(4, 2, 30.00);

-- Insert Discounts
INSERT INTO Discounts (Code, Description, DiscountType, Value, StartDate, EndDate, MinimumPurchase, MaxUsage)
VALUES
('SUMMER10', 'Summer sale 10% off', 'Percentage', 10.00, '2023-06-01', '2023-08-31', 50.00, 1000),
('NEWCUST20', 'New customer 20% off', 'Percentage', 20.00, '2023-01-01', '2023-12-31', 100.00, 500);

-- Insert Orders
INSERT INTO Orders (UserID, TotalAmount, Status, ShippingAddress)
VALUES
(3, 749.98, 'Completed', '789 Oak St, Village'),
(2, 1319.98, 'Processing', '456 Elm St, Town');

-- Insert OrderItems
INSERT INTO OrderItems (OrderID, ProductID, Quantity, Price, DiscountID)
VALUES
(1, 1, 1, 699.99, 1),
(1, 3, 2, 19.99, NULL),
(2, 2, 1, 1299.99, 2),
(2, 4, 1, 49.99, NULL);
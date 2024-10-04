-- Create Users table
CREATE TABLE Users (
    UserID SERIAL PRIMARY KEY,
    Username VARCHAR(50) UNIQUE NOT NULL,
    Email VARCHAR(100) UNIQUE NOT NULL,
    Password VARCHAR(255) NOT NULL,
    FirstName VARCHAR(50),
    LastName VARCHAR(50),
    Address TEXT,
    PhoneNumber VARCHAR(20),
    CreatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UpdatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    IsDeleted BOOLEAN DEFAULT FALSE
);

-- Create Admins table
CREATE TABLE Admins (
    AdminID SERIAL PRIMARY KEY,
    UserID INTEGER REFERENCES Users(UserID),
    Role VARCHAR(50) NOT NULL,
    CreatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UpdatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    IsDeleted BOOLEAN DEFAULT FALSE
);

-- Create Categories table
CREATE TABLE Categories (
    CategoryID SERIAL PRIMARY KEY,
    Name VARCHAR(100) NOT NULL,
    Description TEXT,
    ParentCategoryID INTEGER REFERENCES Categories(CategoryID),
    CreatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UpdatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UpdatedBy INTEGER REFERENCES Admins(AdminID),
    IsDeleted BOOLEAN DEFAULT FALSE
);

-- Create Products table
CREATE TABLE Products (
    ProductID SERIAL PRIMARY KEY,
    Name VARCHAR(100) NOT NULL,
    Description TEXT,
    Price DECIMAL(10, 2) NOT NULL,
    StockQuantity INTEGER NOT NULL,
    CategoryID INTEGER REFERENCES Categories(CategoryID),
    CreatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UpdatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UpdatedBy INTEGER REFERENCES Admins(AdminID),
    IsDeleted BOOLEAN DEFAULT FALSE
);

-- Create Warehouses table
CREATE TABLE Warehouses (
    WarehouseID SERIAL PRIMARY KEY,
    Name VARCHAR(100) NOT NULL,
    Address TEXT,
    Capacity INTEGER,
    CreatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UpdatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create Inventory table
CREATE TABLE Inventory (
    InventoryID SERIAL PRIMARY KEY,
    ProductID INTEGER REFERENCES Products(ProductID),
    WarehouseID INTEGER REFERENCES Warehouses(WarehouseID),
    Quantity INTEGER NOT NULL,
    LastRestocked TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create Suppliers table
CREATE TABLE Suppliers (
    SupplierID SERIAL PRIMARY KEY,
    Name VARCHAR(100) NOT NULL,
    ContactPerson VARCHAR(100),
    Email VARCHAR(100),
    PhoneNumber VARCHAR(20),
    Address TEXT
);

-- Create ProductSuppliers table
CREATE TABLE ProductSuppliers (
    ProductSupplierID SERIAL PRIMARY KEY,
    ProductID INTEGER REFERENCES Products(ProductID),
    SupplierID INTEGER REFERENCES Suppliers(SupplierID),
    SupplyPrice DECIMAL(10, 2) NOT NULL,
    PurchaseDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create Discounts table
CREATE TABLE Discounts (
    DiscountID SERIAL PRIMARY KEY,
    Code VARCHAR(50) UNIQUE NOT NULL,
    Description TEXT,
    DiscountType VARCHAR(20) NOT NULL,
    Value DECIMAL(5, 2) NOT NULL,
    StartDate TIMESTAMP NOT NULL,
    EndDate TIMESTAMP NOT NULL,
    MinimumPurchase DECIMAL(10, 2),
    MaxUsage INTEGER,
    CreatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    IsDeleted BOOLEAN DEFAULT FALSE
);

-- Create Orders table
CREATE TABLE Orders (
    OrderID SERIAL PRIMARY KEY,
    UserID INTEGER REFERENCES Users(UserID),
    OrderDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    TotalAmount DECIMAL(10, 2) NOT NULL,
    Status VARCHAR(20) NOT NULL,
    ShippingAddress TEXT NOT NULL
);

-- Create OrderItems table
CREATE TABLE OrderItems (
    OrderItemID SERIAL PRIMARY KEY,
    OrderID INTEGER REFERENCES Orders(OrderID),
    ProductID INTEGER REFERENCES Products(ProductID),
    Quantity INTEGER NOT NULL,
    Price DECIMAL(10, 2) NOT NULL,
    DiscountID INTEGER REFERENCES Discounts(DiscountID)
);

-- Create Reviews table
CREATE TABLE Reviews (
    ReviewID SERIAL PRIMARY KEY,
    UserID INTEGER REFERENCES Users(UserID),
    ProductID INTEGER REFERENCES Products(ProductID),
    Rating INTEGER CHECK (Rating >= 1 AND Rating <= 5),
    Comment TEXT,
    CreatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);


-- Create UserDiscounts table
CREATE TABLE UserDiscounts (
    UserID INTEGER REFERENCES Users(UserID),
    DiscountID INTEGER REFERENCES Discounts(DiscountID),
    UsedCount INTEGER DEFAULT 0,
    PRIMARY KEY (UserID, DiscountID)
);

-- Create Wishlist table
CREATE TABLE Wishlist (
    WishlistID SERIAL PRIMARY KEY,
    UserID INTEGER REFERENCES Users(UserID),
    ProductID INTEGER REFERENCES Products(ProductID),
    AddedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create ShoppingCart table
CREATE TABLE ShoppingCart (
    CartID SERIAL PRIMARY KEY,
    UserID INTEGER REFERENCES Users(UserID),
    ProductID INTEGER REFERENCES Products(ProductID),
    Quantity INTEGER NOT NULL,
    AddedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create Shipments table
CREATE TABLE Shipments (
    ShipmentID SERIAL PRIMARY KEY,
    OrderID INTEGER REFERENCES Orders(OrderID),
    CarrierName VARCHAR(100) NOT NULL,
    TrackingNumber VARCHAR(100),
    ShippedDate TIMESTAMP,
    EstimatedDeliveryDate TIMESTAMP,
    ActualDeliveryDate TIMESTAMP,
    Status VARCHAR(20) NOT NULL
);
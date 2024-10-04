-- Add indexes for frequently queried columns
CREATE INDEX idx_products_category ON Products(CategoryID);
CREATE INDEX idx_inventory_product ON Inventory(ProductID);
CREATE INDEX idx_orderitems_order ON OrderItems(OrderID);
CREATE INDEX idx_orderitems_product ON OrderItems(ProductID);
CREATE INDEX idx_reviews_product ON Reviews(ProductID);
CREATE INDEX idx_shoppingcart_user ON ShoppingCart(UserID);
CREATE INDEX idx_shipments_order ON Shipments(OrderID);
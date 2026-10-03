-- =========================================================
-- Схема БД
-- =========================================================

-- 1) Директор -- керує (1:1) -- Магазин
CREATE TABLE store (
    name     VARCHAR(150) PRIMARY KEY,
    address  VARCHAR(250) NOT NULL
);

CREATE TABLE director (
    passport    VARCHAR(20) PRIMARY KEY,
    full_name   VARCHAR(150) NOT NULL,
    store_name  VARCHAR(150) NOT NULL UNIQUE
        REFERENCES store(name)                 -- UNIQUE -> 1:1, NOT NULL -> обов'язковість директора
);

-- 2) Магазин -- містить (1:M, ідентифікуючий) -- Відділ (слабка сутність)
CREATE TABLE department (
    store_name   VARCHAR(150) NOT NULL REFERENCES store(name) ON DELETE CASCADE,
    name         VARCHAR(150) NOT NULL,        -- частковий ключ слабкої сутності
    staff_count  INTEGER NOT NULL CHECK (staff_count >= 0),
    PRIMARY KEY (store_name, name)             -- повний ключ = store_name + name
);

-- 3) Відділ -- працює (1:M) -- Продавець; + рекурсивний зв'язок "керівник" (M:1)
CREATE TABLE seller (
    passport            VARCHAR(20) PRIMARY KEY,
    full_name           VARCHAR(150) NOT NULL,
    age                 SMALLINT NOT NULL CHECK (age BETWEEN 16 AND 80),
    gender              VARCHAR(10) NOT NULL CHECK (gender IN ('чоловіча','жіноча')),
    department_store    VARCHAR(150) NOT NULL,
    department_name     VARCHAR(150) NOT NULL,
    supervisor_passport VARCHAR(20)
        REFERENCES seller(passport),           -- рекурсивний зв'язок "керівник"
    FOREIGN KEY (department_store, department_name)
        REFERENCES department(store_name, name)
);

-- 4) Виробник, Пристрій, "володіє" (1:M)
CREATE TABLE manufacturer (
    name     VARCHAR(150) PRIMARY KEY,
    country  VARCHAR(100) NOT NULL
);

CREATE TABLE device (
    name              VARCHAR(150) PRIMARY KEY,
    price             NUMERIC(10,2) NOT NULL CHECK (price >= 0),
    release_year      SMALLINT NOT NULL,
    warranty_months   SMALLINT NOT NULL CHECK (warranty_months >= 0),
    manufacturer_name VARCHAR(150) NOT NULL
        REFERENCES manufacturer(name)          -- "володіє": власник-виробник
);

-- 5) ISA: Пристрій -> Смартфон / Ноутбук / Побутова техніка (повна спеціалізація 1:1)
CREATE TABLE smartphone (
    device_name  VARCHAR(150) PRIMARY KEY REFERENCES device(name) ON DELETE CASCADE,
    screen_size  NUMERIC(4,2) NOT NULL,
    os           VARCHAR(50) NOT NULL
);

CREATE TABLE laptop (
    device_name VARCHAR(150) PRIMARY KEY REFERENCES device(name) ON DELETE CASCADE,
    storage_gb  INTEGER NOT NULL,
    weight_kg   NUMERIC(4,2) NOT NULL
);

CREATE TABLE appliance (
    device_name   VARCHAR(150) PRIMARY KEY REFERENCES device(name) ON DELETE CASCADE,
    power_watts   INTEGER NOT NULL,
    energy_class  VARCHAR(5) NOT NULL
);

-- 6) "спів-виробництво" (M:M, Виробник - Пристрій)
CREATE TABLE co_production (
    device_name       VARCHAR(150) REFERENCES device(name) ON DELETE CASCADE,
    manufacturer_name VARCHAR(150) REFERENCES manufacturer(name) ON DELETE CASCADE,
    PRIMARY KEY (device_name, manufacturer_name)
);

-- 7) "аксесуар до" (M:M, самозв'язок, асиметричний)
CREATE TABLE accessory_for (
    accessory_device VARCHAR(150) REFERENCES device(name) ON DELETE CASCADE,
    main_device      VARCHAR(150) REFERENCES device(name) ON DELETE CASCADE,
    PRIMARY KEY (accessory_device, main_device),
    CHECK (accessory_device <> main_device)
);

-- 8) "конкурує з" (M:M, самозв'язок, симетричний) + обмеження: різні виробники
CREATE TABLE competes_with (
    device1 VARCHAR(150) REFERENCES device(name) ON DELETE CASCADE,
    device2 VARCHAR(150) REFERENCES device(name) ON DELETE CASCADE,
    PRIMARY KEY (device1, device2),
    CHECK (device1 < device2)                  -- зберігаємо пару лише один раз
);

CREATE OR REPLACE FUNCTION check_different_manufacturers()
RETURNS TRIGGER AS $$
DECLARE
    m1 VARCHAR(150);
    m2 VARCHAR(150);
BEGIN
    SELECT manufacturer_name INTO m1 FROM device WHERE name = NEW.device1;
    SELECT manufacturer_name INTO m2 FROM device WHERE name = NEW.device2;
    IF m1 = m2 THEN
        RAISE EXCEPTION 'Пристрої "%" та "%" не можуть конкурувати - однаковий виробник (%)',
            NEW.device1, NEW.device2, m1;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trg_competes_diff_manufacturer
BEFORE INSERT OR UPDATE ON competes_with
FOR EACH ROW EXECUTE FUNCTION check_different_manufacturers();

-- 9) Клієнт
CREATE TABLE client (
    passport   VARCHAR(20) PRIMARY KEY,
    full_name  VARCHAR(150) NOT NULL,
    phone      VARCHAR(20) NOT NULL
);

-- 10) Тернарний зв'язок "продаж" (Продавець, Пристрій, Клієнт)
CREATE TABLE sale (
    seller_passport  VARCHAR(20) NOT NULL REFERENCES seller(passport),
    device_name      VARCHAR(150) NOT NULL REFERENCES device(name),
    client_passport  VARCHAR(20) NOT NULL REFERENCES client(passport),
    sale_date        DATE NOT NULL,
    quantity         INTEGER NOT NULL CHECK (quantity > 0),
    amount           NUMERIC(10,2) NOT NULL CHECK (amount >= 0),
    PRIMARY KEY (seller_passport, device_name, client_passport, sale_date)
);
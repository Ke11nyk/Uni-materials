-- =========================================================
-- Тестові дані
-- =========================================================

INSERT INTO store (name, address) VALUES
('TechnoPlus Хрещатик','м. Київ, вул. Хрещатик, 22'),
('TechnoPlus Оболонь','м. Київ, просп. Оболонський, 15');

INSERT INTO director (passport, full_name, store_name) VALUES
('EK123456','Коваленко Олег Петрович','TechnoPlus Хрещатик'),
('EK234567','Гриценко Ірина Василівна','TechnoPlus Оболонь');

INSERT INTO department (store_name, name, staff_count) VALUES
('TechnoPlus Хрещатик','Відділ смартфонів',4),
('TechnoPlus Хрещатик','Відділ побутової техніки',3),
('TechnoPlus Оболонь','Відділ ноутбуків',3),
('TechnoPlus Оболонь','Відділ сервісу',2);

-- Продавці (частина - керівники відділів, частина - їхні підлеглі)
INSERT INTO seller (passport, full_name, age, gender, department_store, department_name, supervisor_passport) VALUES
('SA100001','Мельник Андрій Сергійович',34,'чоловіча','TechnoPlus Хрещатик','Відділ смартфонів',NULL),
('SA100002','Бондаренко Марія Ігорівна',26,'жіноча','TechnoPlus Хрещатик','Відділ смартфонів','SA100001'),
('SA100003','Ткаченко Віктор Олексійович',23,'чоловіча','TechnoPlus Хрещатик','Відділ смартфонів','SA100001'),
('SA100004','Шевченко Наталія Павлівна',41,'жіноча','TechnoPlus Хрещатик','Відділ побутової техніки',NULL),
('SA100005','Кравець Дмитро Романович',29,'чоловіча','TechnoPlus Оболонь','Відділ ноутбуків',NULL),
('SA100006','Пилипенко Оксана Тарасівна',24,'жіноча','TechnoPlus Оболонь','Відділ ноутбуків','SA100005'),
('SA100007','Романюк Сергій Миколайович',37,'чоловіча','TechnoPlus Оболонь','Відділ сервісу',NULL);

INSERT INTO manufacturer (name, country) VALUES
('Samsung','Республіка Корея'),
('Apple','США'),
('Xiaomi','Китай'),
('LG','Республіка Корея'),
('Dell','США'),
('Bosch','Німеччина');

INSERT INTO device (name, price, release_year, warranty_months, manufacturer_name) VALUES
('Samsung Galaxy S24',34999.00,2024,24,'Samsung'),
('iPhone 15',42999.00,2023,12,'Apple'),
('Xiaomi Redmi Note 13',9999.00,2024,24,'Xiaomi'),
('Apple MacBook Air M2',54999.00,2023,12,'Apple'),
('Dell XPS 13',49999.00,2023,24,'Dell'),
('LG Холодильник GA-B509',24999.00,2022,36,'LG'),
('Bosch Пральна машина WAT28',21999.00,2022,24,'Bosch'),
('Samsung Galaxy Buds FE',3499.00,2024,12,'Samsung'),
('Xiaomi 20000mAh PowerBank',999.00,2023,12,'Xiaomi');

INSERT INTO smartphone (device_name, screen_size, os) VALUES
('Samsung Galaxy S24',6.20,'Android 14'),
('iPhone 15',6.10,'iOS 17'),
('Xiaomi Redmi Note 13',6.67,'Android 13');

INSERT INTO laptop (device_name, storage_gb, weight_kg) VALUES
('Apple MacBook Air M2',256,1.24),
('Dell XPS 13',512,1.17);

INSERT INTO appliance (device_name, power_watts, energy_class) VALUES
('LG Холодильник GA-B509',150,'A+'),
('Bosch Пральна машина WAT28',2000,'A');

-- спів-виробництво: партнерські моделі
INSERT INTO co_production (device_name, manufacturer_name) VALUES
('Samsung Galaxy S24','Xiaomi'),   -- умовна спільна поставка компонентів
('iPhone 15','Samsung');           -- Samsung постачає дисплеї

-- аксесуар до: навушники й павербанк - аксесуари до смартфонів
INSERT INTO accessory_for (accessory_device, main_device) VALUES
('Samsung Galaxy Buds FE','Samsung Galaxy S24'),
('Samsung Galaxy Buds FE','iPhone 15'),
('Xiaomi 20000mAh PowerBank','Xiaomi Redmi Note 13'),
('Xiaomi 20000mAh PowerBank','iPhone 15');

-- конкурує з: пристрої різних виробників в одній ніші
INSERT INTO competes_with (device1, device2) VALUES
('Apple MacBook Air M2','Dell XPS 13'),
('Samsung Galaxy S24','iPhone 15'),
('Xiaomi Redmi Note 13','iPhone 15');

INSERT INTO client (passport, full_name, phone) VALUES
('CL500001','Іваненко Петро Вікторович','+380671112233'),
('CL500002','Литвиненко Ганна Олегівна','+380502223344'),
('CL500003','Соколов Артем Дмитрович','+380932345566'),
('CL500004','Гончарук Юлія Андріївна','+380637654321');

INSERT INTO sale (seller_passport, device_name, client_passport, sale_date, quantity, amount) VALUES
('SA100002','Samsung Galaxy S24','CL500001','2026-07-15',1,34999.00),
('SA100003','iPhone 15','CL500002','2026-07-20',1,42999.00),
('SA100001','Samsung Galaxy Buds FE','CL500001','2026-07-15',1,3499.00),
('SA100004','LG Холодильник GA-B509','CL500003','2026-08-02',1,24999.00),
('SA100004','Bosch Пральна машина WAT28','CL500004','2026-08-10',1,21999.00),
('SA100005','Apple MacBook Air M2','CL500002','2026-08-14',1,54999.00),
('SA100006','Dell XPS 13','CL500003','2026-09-01',1,49999.00),
('SA100002','Xiaomi Redmi Note 13','CL500004','2026-09-05',2,19998.00),
('SA100003','Xiaomi 20000mAh PowerBank','CL500002','2026-09-05',1,999.00);
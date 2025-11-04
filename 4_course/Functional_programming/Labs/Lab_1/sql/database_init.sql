-- Видалення старої бази даних
DROP DATABASE IF EXISTS faculty_newspaper;

-- Створення бази даних з UTF-8
CREATE DATABASE faculty_newspaper CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE faculty_newspaper;

-- Встановлення кодування для сесії
SET NAMES utf8mb4;
SET CHARACTER SET utf8mb4;
SET character_set_connection=utf8mb4;

-- =====================================================
-- ОСНОВНІ ТАБЛИЦІ
-- =====================================================

-- Таблиця авторів
CREATE TABLE IF NOT EXISTS authors (
    author_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    department VARCHAR(200) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_email (email),
    INDEX idx_last_name (last_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Таблиця читачів
CREATE TABLE IF NOT EXISTS readers (
    reader_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    student_group VARCHAR(50),
    registration_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_email (email)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Таблиця категорій
CREATE TABLE IF NOT EXISTS categories (
    category_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    category_name VARCHAR(100) NOT NULL UNIQUE,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_category_name (category_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Таблиця тегів
CREATE TABLE IF NOT EXISTS tags (
    tag_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    tag_name VARCHAR(50) NOT NULL UNIQUE,
    usage_count BIGINT DEFAULT 0,
    INDEX idx_tag_name (tag_name),
    INDEX idx_usage_count (usage_count)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Таблиця статей (використовує category_id)
CREATE TABLE IF NOT EXISTS articles (
    article_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    title VARCHAR(500) NOT NULL,
    author_id BIGINT NOT NULL,
    annotation TEXT,
    file_path VARCHAR(1000) NOT NULL,
    publication_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    category_id BIGINT NOT NULL,
    is_published BOOLEAN DEFAULT FALSE,
    FOREIGN KEY (author_id) REFERENCES authors(author_id) ON DELETE CASCADE,
    FOREIGN KEY (category_id) REFERENCES categories(category_id) ON DELETE RESTRICT,
    INDEX idx_author (author_id),
    INDEX idx_category (category_id),
    INDEX idx_published (is_published),
    INDEX idx_publication_date (publication_date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Таблиця коментарів
CREATE TABLE IF NOT EXISTS comments (
    comment_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    article_id BIGINT NOT NULL,
    reader_id BIGINT NOT NULL,
    comment_text TEXT NOT NULL,
    comment_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    rating INT CHECK (rating >= 1 AND rating <= 5),
    FOREIGN KEY (article_id) REFERENCES articles(article_id) ON DELETE CASCADE,
    FOREIGN KEY (reader_id) REFERENCES readers(reader_id) ON DELETE CASCADE,
    INDEX idx_article (article_id),
    INDEX idx_reader (reader_id),
    INDEX idx_rating (rating)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Таблиця статистики статей
CREATE TABLE IF NOT EXISTS article_statistics (
    stat_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    article_id BIGINT NOT NULL UNIQUE,
    view_count BIGINT DEFAULT 0,
    download_count BIGINT DEFAULT 0,
    comment_count BIGINT DEFAULT 0,
    average_rating DECIMAL(3,2) DEFAULT 0.0,
    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (article_id) REFERENCES articles(article_id) ON DELETE CASCADE,
    INDEX idx_view_count (view_count),
    INDEX idx_average_rating (average_rating)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Таблиця графічних матеріалів
CREATE TABLE IF NOT EXISTS graphic_materials (
    material_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    title VARCHAR(500) NOT NULL,
    author_id BIGINT NOT NULL,
    file_path VARCHAR(1000) NOT NULL,
    material_type ENUM('image', 'infographic', 'chart', 'diagram') NOT NULL,
    upload_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    description TEXT,
    FOREIGN KEY (author_id) REFERENCES authors(author_id) ON DELETE CASCADE,
    INDEX idx_author (author_id),
    INDEX idx_material_type (material_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- ЗВ'ЯЗУЮЧІ ТАБЛИЦІ
-- =====================================================

-- Зв'язуюча таблиця для статей і тегів
CREATE TABLE IF NOT EXISTS article_tags (
    article_id BIGINT NOT NULL,
    tag_id BIGINT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (article_id, tag_id),
    FOREIGN KEY (article_id) REFERENCES articles(article_id) ON DELETE CASCADE,
    FOREIGN KEY (tag_id) REFERENCES tags(tag_id) ON DELETE CASCADE,
    INDEX idx_tag (tag_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Зв'язуюча таблиця для статей і графічних матеріалів
CREATE TABLE IF NOT EXISTS article_materials (
    article_id BIGINT NOT NULL,
    material_id BIGINT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (article_id, material_id),
    FOREIGN KEY (article_id) REFERENCES articles(article_id) ON DELETE CASCADE,
    FOREIGN KEY (material_id) REFERENCES graphic_materials(material_id) ON DELETE CASCADE,
    INDEX idx_material (material_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- ПОЧАТКОВІ ДАНІ
-- =====================================================

-- Вставка базових категорій
INSERT INTO categories (category_name, description) VALUES
('Наука', 'Наукові статті та дослідження'),
('Технології', 'Новини технологій та IT'),
('Освіта', 'Освітні матеріали та методики'),
('Студентське життя', 'Події та новини студентського життя'),
('Факультет', 'Новини та оголошення факультету'),
('Культура', 'Культурні події та мистецтво'),
('Спорт', 'Спортивні новини та досягнення');

-- Вставка базових тегів
INSERT INTO tags (tag_name) VALUES
('програмування'), ('математика'), ('фізика'),
('штучний_інтелект'), ('веб_розробка'), ('бази_даних'),
('алгоритми'), ('data_science'), ('машинне_навчання'),
('хакатон'), ('конференція'), ('олімпіада');

-- Вставка тестових авторів
INSERT INTO authors (first_name, last_name, email, department) VALUES
('Іван', 'Петренко', 'i.petrenko@university.edu', 'Кафедра інформатики'),
('Марія', 'Іваненко', 'm.ivanenko@university.edu', 'Кафедра математики'),
('Олександр', 'Коваленко', 'o.kovalenko@university.edu', 'Кафедра фізики'),
('Олена', 'Сидоренко', 'o.sydorenko@university.edu', 'Кафедра інформатики'),
('Андрій', 'Шевченко', 'a.shevchenko@university.edu', 'Кафедра прикладної математики');

-- Вставка тестових читачів
INSERT INTO readers (first_name, last_name, email, student_group) VALUES
('Петро', 'Мельник', 'p.melnyk@student.edu', 'КН-21'),
('Олена', 'Білоус', 'o.bilous@student.edu', 'МІ-20'),
('Дмитро', 'Гриценко', 'd.hrytsenko@student.edu', 'ФІ-19'),
('Анна', 'Ткаченко', 'a.tkachenko@student.edu', 'КН-22'),
('Михайло', 'Бойко', 'm.boyko@student.edu', 'МІ-21');

-- Вставка тестових статей
INSERT INTO articles (title, author_id, annotation, file_path, category_id, is_published) VALUES
('Основи машинного навчання для початківців', 1, 
 'Стаття розглядає базові концепції машинного навчання та їх застосування', 
 '/articles/ml_basics.pdf', 2, TRUE),
('Нові методи розв\'язання диференціальних рівнянь', 2,
 'Дослідження сучасних числових методів для складних систем рівнянь',
 '/articles/diff_equations.pdf', 1, TRUE),
('Квантова механіка в XXI столітті', 3,
 'Огляд сучасних досягнень у квантовій фізиці та їх практичне застосування',
 '/articles/quantum_mechanics.pdf', 1, TRUE),
('Факультетський хакатон 2025: підсумки', 4,
 'Звіт про проведення щорічного хакатону з програмування',
 '/articles/hackathon_2025.pdf', 4, TRUE),
('Математичні олімпіади: досвід підготовки', 5,
 'Поради та методики підготовки до математичних змагань',
 '/articles/math_olympiad.pdf', 3, FALSE);

-- Створення записів статистики для статей
INSERT INTO article_statistics (article_id) 
SELECT article_id FROM articles;

-- Оновлення тестової статистики
UPDATE article_statistics SET view_count = 150, comment_count = 5, average_rating = 4.5 WHERE article_id = 1;
UPDATE article_statistics SET view_count = 89, comment_count = 3, average_rating = 4.8 WHERE article_id = 2;
UPDATE article_statistics SET view_count = 200, comment_count = 8, average_rating = 4.2 WHERE article_id = 3;
UPDATE article_statistics SET view_count = 320, comment_count = 12, average_rating = 4.9 WHERE article_id = 4;

-- Прив'язка тегів до статей
INSERT INTO article_tags (article_id, tag_id) VALUES
(1, 4), (1, 9),  -- ML стаття: штучний_інтелект, машинне_навчання
(2, 2), (2, 7),  -- Математична стаття: математика, алгоритми
(3, 3),          -- Фізична стаття: фізика
(4, 1), (4, 10), -- Хакатон: програмування, хакатон
(5, 2), (5, 12); -- Олімпіада: математика, олімпіада

-- Оновлення лічильників використання тегів
UPDATE tags SET usage_count = (SELECT COUNT(*) FROM article_tags WHERE article_tags.tag_id = tags.tag_id);

-- Вставка тестових коментарів
INSERT INTO comments (article_id, reader_id, comment_text, rating) VALUES
(1, 1, 'Дуже корисна стаття для початківців! Все зрозуміло пояснено.', 5),
(1, 2, 'Чудовий огляд базових концепцій ML', 4),
(3, 3, 'Захоплююча тема! Хотілося б більше прикладів.', 4),
(4, 4, 'Був на хакатоні, незабутні враження!', 5),
(4, 5, 'Чудова організація заходу', 5);

-- Вставка тестових графічних матеріалів
INSERT INTO graphic_materials (title, author_id, file_path, material_type, description) VALUES
('Схема нейронної мережі', 1, '/materials/neural_network_diagram.png', 'diagram', 
 'Візуалізація архітектури простої нейронної мережі'),
('Інфографіка: Статистика факультету 2025', 4, '/materials/faculty_stats_2025.png', 'infographic',
 'Основні показники роботи факультету за рік'),
('Графік розподілу оцінок', 2, '/materials/grades_chart.png', 'chart',
 'Аналіз успішності студентів за семестр');

-- Прив'язка матеріалів до статей
INSERT INTO article_materials (article_id, material_id) VALUES
(1, 1),  -- Стаття про ML з діаграмою нейронної мережі
(4, 2);  -- Стаття про хакатон з інфографікою

-- =====================================================
-- ПЕРЕВІРОЧНІ ЗАПИТИ
-- =====================================================

-- Показати статистику бази даних
SELECT 
    'Авторів' as Тип, COUNT(*) as Кількість FROM authors
UNION ALL SELECT 'Статей', COUNT(*) FROM articles
UNION ALL SELECT 'Опубліковано', COUNT(*) FROM articles WHERE is_published = TRUE
UNION ALL SELECT 'Читачів', COUNT(*) FROM readers
UNION ALL SELECT 'Коментарів', COUNT(*) FROM comments
UNION ALL SELECT 'Категорій', COUNT(*) FROM categories
UNION ALL SELECT 'Тегів', COUNT(*) FROM tags
UNION ALL SELECT 'Матеріалів', COUNT(*) FROM graphic_materials;
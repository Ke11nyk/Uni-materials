-- Видалення старої бази даних
DROP DATABASE IF EXISTS faculty_newspaper;

-- Створення бази даних з UTF-8
CREATE DATABASE faculty_newspaper CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE faculty_newspaper;

-- Встановлення кодування для сесії
SET NAMES utf8mb4;
SET CHARACTER SET utf8mb4;
SET character_set_connection=utf8mb4;

-- Таблиця авторів
CREATE TABLE IF NOT EXISTS authors (
    author_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(100) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    department VARCHAR(200) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_email (email)
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

-- Таблиця статей
CREATE TABLE IF NOT EXISTS articles (
    article_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    title VARCHAR(500) NOT NULL,
    author_id BIGINT NOT NULL,
    annotation TEXT,
    file_path VARCHAR(1000) NOT NULL,
    publication_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    category VARCHAR(100) NOT NULL,
    is_published BOOLEAN DEFAULT FALSE,
    FOREIGN KEY (author_id) REFERENCES authors(author_id) ON DELETE CASCADE,
    INDEX idx_author (author_id),
    INDEX idx_category (category),
    INDEX idx_published (is_published)
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
    INDEX idx_reader (reader_id)
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
    FOREIGN KEY (article_id) REFERENCES articles(article_id) ON DELETE CASCADE
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
    FOREIGN KEY (author_id) REFERENCES authors(author_id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Таблиця категорій
CREATE TABLE IF NOT EXISTS categories (
    category_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    category_name VARCHAR(100) NOT NULL UNIQUE,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Таблиця тегів
CREATE TABLE IF NOT EXISTS tags (
    tag_id BIGINT AUTO_INCREMENT PRIMARY KEY,
    tag_name VARCHAR(50) NOT NULL UNIQUE,
    usage_count BIGINT DEFAULT 0
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Зв'язуюча таблиця для статей і тегів
CREATE TABLE IF NOT EXISTS article_tags (
    article_id BIGINT NOT NULL,
    tag_id BIGINT NOT NULL,
    PRIMARY KEY (article_id, tag_id),
    FOREIGN KEY (article_id) REFERENCES articles(article_id) ON DELETE CASCADE,
    FOREIGN KEY (tag_id) REFERENCES tags(tag_id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Вставка базових категорій
INSERT IGNORE INTO categories (category_name, description) VALUES
('Наука', 'Наукові статті та дослідження'),
('Технології', 'Новини технологій та IT'),
('Освіта', 'Освітні матеріали та методики'),
('Студентське життя', 'Події та новини студентського життя'),
('Факультет', 'Новини та оголошення факультету');

-- Вставка базових тегів
INSERT IGNORE INTO tags (tag_name) VALUES
('програмування'), ('математика'), ('фізика'),
('штучний_інтелект'), ('веб_розробка'), ('бази_даних');

-- Вставка тестових авторів
INSERT IGNORE INTO authors (first_name, last_name, email, department) VALUES
('Іван', 'Петренко', 'i.petrenko@university.edu', 'Кафедра інформатики'),
('Марія', 'Іваненко', 'm.ivanenko@university.edu', 'Кафедра математики'),
('Олександр', 'Коваленко', 'o.kovalenko@university.edu', 'Кафедра фізики');

-- Вставка тестових читачів
INSERT IGNORE INTO readers (first_name, last_name, email, student_group) VALUES
('Петро', 'Мельник', 'p.melnyk@student.edu', 'КН-21'),
('Олена', 'Білоус', 'o.bilous@student.edu', 'МІ-20'),
('Дмитро', 'Гриценко', 'd.hrytsenko@student.edu', 'ФІ-19');
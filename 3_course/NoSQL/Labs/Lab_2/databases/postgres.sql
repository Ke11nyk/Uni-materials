-- Створення таблиць
CREATE TABLE rock_types (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    description TEXT
);

CREATE TABLE rock_samples (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    type_id INTEGER REFERENCES rock_types(id),
    latitude DECIMAL(9,6),
    longitude DECIMAL(9,6),
    collected_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE chemical_compositions (
    id SERIAL PRIMARY KEY,
    sample_id INTEGER REFERENCES rock_samples(id),
    compound VARCHAR(50) NOT NULL,
    percentage DECIMAL(5,2) NOT NULL
);

CREATE TABLE measurements (
    id SERIAL PRIMARY KEY,
    sample_id INTEGER REFERENCES rock_samples(id),
    hardness DECIMAL(4,2),
    density DECIMAL(4,2),
    measured_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Додавання індексів
CREATE INDEX idx_rock_samples_type ON rock_samples(type_id);
CREATE INDEX idx_measurements_sample ON measurements(sample_id);
CREATE INDEX idx_chemical_comp_sample ON chemical_compositions(sample_id);

-- Функції для роботи з даними
CREATE OR REPLACE FUNCTION add_rock_sample(
    p_name VARCHAR,
    p_type_id INTEGER,
    p_latitude DECIMAL,
    p_longitude DECIMAL
) RETURNS INTEGER AS $$
DECLARE
    new_id INTEGER;
BEGIN
    INSERT INTO rock_samples (name, type_id, latitude, longitude)
    VALUES (p_name, p_type_id, p_latitude, p_longitude)
    RETURNING id INTO new_id;
    RETURN new_id;
END;
$$ LANGUAGE plpgsql;

-- Додавання тестових даних
INSERT INTO rock_types (name, description)
VALUES ('граніт', 'Магматична гірська порода');

-- Вставка зразка
INSERT INTO rock_samples (name, type_id, latitude, longitude)
VALUES ('Граніт-1', 1, 50.4501, 30.5234);
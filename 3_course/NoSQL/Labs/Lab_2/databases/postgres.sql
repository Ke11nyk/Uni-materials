-- PostgreSQL база даних

-- Створення бази даних (якщо все ще не створена)
CREATE DATABASE test_db;

-- Підключення до test_db
\c test_db;

-- Створення таблиць
CREATE TABLE geological_periods (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    start_age INTEGER,
    end_age INTEGER,
    characteristics TEXT
);

CREATE TABLE rock_types (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    formation_process TEXT,
    typical_minerals TEXT
);

CREATE TABLE research_projects (
    id SERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    description TEXT,
    start_date DATE,
    end_date DATE,
    funding_source VARCHAR(200)
);

CREATE TABLE researchers (
    id SERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    specialization VARCHAR(100),
    institution VARCHAR(200),
    contact_info TEXT,
    joined_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE laboratories (
    id SERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    location TEXT,
    certification VARCHAR(100),
    equipment_list TEXT
);

CREATE TABLE equipment (
    id SERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    type VARCHAR(100),
    manufacturer VARCHAR(200),
    last_calibration DATE,
    calibration_certificate VARCHAR(100)
);

CREATE TABLE mining_sites (
    id SERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    status VARCHAR(50),
    operational_since DATE,
    access_permissions TEXT
);

CREATE TABLE locations (
    id SERIAL PRIMARY KEY,
    mining_site_id INTEGER REFERENCES mining_sites(id),
    latitude DECIMAL(9,6),
    longitude DECIMAL(9,6),
    altitude DECIMAL(8,2),
    terrain_type VARCHAR(100)
);

CREATE TABLE rock_samples (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    type_id INTEGER REFERENCES rock_types(id),
    location_id INTEGER REFERENCES locations(id),
    researcher_id INTEGER REFERENCES researchers(id),
    project_id INTEGER REFERENCES research_projects(id),
    geological_period_id INTEGER REFERENCES geological_periods(id),
    collection_method VARCHAR(100),
    preservation_state VARCHAR(100),
    collected_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE chemical_compositions (
    id SERIAL PRIMARY KEY,
    sample_id INTEGER REFERENCES rock_samples(id),
    compound VARCHAR(50) NOT NULL,
    percentage DECIMAL(5,2) NOT NULL,
    measurement_method VARCHAR(100),
    analyzed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE measurements (
    id SERIAL PRIMARY KEY,
    sample_id INTEGER REFERENCES rock_samples(id),
    equipment_id INTEGER REFERENCES equipment(id),
    hardness DECIMAL(4,2),
    density DECIMAL(4,2),
    porosity DECIMAL(5,2),
    permeability DECIMAL(10,6),
    crystal_structure VARCHAR(100),
    measured_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE sample_images (
    id SERIAL PRIMARY KEY,
    sample_id INTEGER REFERENCES rock_samples(id),
    image_url TEXT NOT NULL,
    image_type VARCHAR(50),
    microscope_type VARCHAR(100),
    magnitude INTEGER,
    captured_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE sample_analysis (
    id SERIAL PRIMARY KEY,
    sample_id INTEGER REFERENCES rock_samples(id),
    researcher_id INTEGER REFERENCES researchers(id),
    laboratory_id INTEGER REFERENCES laboratories(id),
    analysis_type VARCHAR(100),
    results TEXT,
    performed_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE project_researchers (
    project_id INTEGER REFERENCES research_projects(id),
    researcher_id INTEGER REFERENCES researchers(id),
    role VARCHAR(100),
    joined_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (project_id, researcher_id)
);

-- Створення індексів, аби пришвидшити запити
CREATE INDEX idx_rock_samples_type ON rock_samples(type_id);
CREATE INDEX idx_rock_samples_location ON rock_samples(location_id);
CREATE INDEX idx_rock_samples_researcher ON rock_samples(researcher_id);
CREATE INDEX idx_rock_samples_project ON rock_samples(project_id);
CREATE INDEX idx_measurements_sample ON measurements(sample_id);
CREATE INDEX idx_chemical_comp_sample ON chemical_compositions(sample_id);
CREATE INDEX idx_sample_images_sample ON sample_images(sample_id);
CREATE INDEX idx_sample_analysis_sample ON sample_analysis(sample_id);
CREATE INDEX idx_locations_mining_site ON locations(mining_site_id);
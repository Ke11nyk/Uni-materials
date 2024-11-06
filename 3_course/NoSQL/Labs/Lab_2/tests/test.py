import psycopg2
import sys
import time
import random
import getpass
from datetime import datetime, timezone
from pymongo import MongoClient
from bson import ObjectId

def check_db_exists(conn, db_name):
    cur = conn.cursor()
    cur.execute("SELECT 1 FROM pg_database WHERE datname = %s", (db_name,))
    exists = cur.fetchone()
    cur.close()
    return exists is not None

def check_active_connections(conn, db_name):
    cur = conn.cursor()
    cur.execute("""
        SELECT COUNT(*)
        FROM pg_stat_activity
        WHERE datname = %s
    """, (db_name,))
    count = cur.fetchone()[0]
    cur.close()
    return count

def terminate_connections(conn, db_name):
    try:
        conn.set_isolation_level(0)
        cur = conn.cursor()
        cur.execute("""
            SELECT pg_terminate_backend(pg_stat_activity.pid)
            FROM pg_stat_activity
            WHERE pg_stat_activity.datname = %s
            AND pid <> pg_backend_pid()
        """, (db_name,))
        cur.close()
        conn.set_isolation_level(1)
        return True
    except Exception as e:
        print(f"Помилка при закритті з'єднань: {e}")
        return False

def setup_postgresql(password):
    DB_NAME = 'rocks_db'
    
    try:
        print("1. Підключення до PostgreSQL...")
        conn = psycopg2.connect(
            host="localhost",
            user="postgres",
            password=password
        )
        conn.autocommit = True
        print("   Підключення успішне!")

        print(f"\n2. Перевірка існування бази даних {DB_NAME}...")
        if check_db_exists(conn, DB_NAME):
            print(f"   База даних {DB_NAME} існує")
            
            print("   Перевірка активних підключень...")
            connections = check_active_connections(conn, DB_NAME)
            if connections > 0:
                print(f"   Знайдено {connections} активних підключень")
                print("   Спроба закрити активні підключення...")
                if terminate_connections(conn, DB_NAME):
                    print("   Підключення успішно закриті")
                time.sleep(1)

            print(f"   Видалення існуючої бази даних {DB_NAME}...")
            cur = conn.cursor()
            cur.execute(f"DROP DATABASE IF EXISTS {DB_NAME}")
            cur.close()
            print("   База даних успішно видалена")
        
        print(f"\n3. Створення нової бази даних {DB_NAME}...")
        cur = conn.cursor()
        cur.execute(f"CREATE DATABASE {DB_NAME}")
        cur.close()
        print("   База даних успішно створена!")
        
        print("\n4. Підключення до нової бази даних...")
        conn.close()
        conn = psycopg2.connect(
            host="localhost",
            database=DB_NAME,
            user="postgres",
            password=password
        )
        conn.autocommit = True
        print("   Підключення успішне!")

        print("\n5. Створення таблиць...")
        cur = conn.cursor()
        
        # Створення таблиць
        tables_sql = """
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
        """
        
        for statement in tables_sql.split(';'):
            if statement.strip():
                cur.execute(statement)
        
        print("\n6. Створення індексів...")
        indices_sql = """
            CREATE INDEX idx_rock_samples_type ON rock_samples(type_id);
            CREATE INDEX idx_rock_samples_location ON rock_samples(location_id);
            CREATE INDEX idx_rock_samples_researcher ON rock_samples(researcher_id);
            CREATE INDEX idx_rock_samples_project ON rock_samples(project_id);
            CREATE INDEX idx_measurements_sample ON measurements(sample_id);
            CREATE INDEX idx_chemical_comp_sample ON chemical_compositions(sample_id);
            CREATE INDEX idx_sample_images_sample ON sample_images(sample_id);
            CREATE INDEX idx_sample_analysis_sample ON sample_analysis(sample_id);
            CREATE INDEX idx_locations_mining_site ON locations(mining_site_id);
        """
        
        for statement in indices_sql.split(';'):
            if statement.strip():
                cur.execute(statement)
        
        print("\n7. Додавання початкових даних...")
        initial_data_sql = """
            INSERT INTO geological_periods (name, start_age, end_age, characteristics) 
            VALUES ('палеозой', 541, 252, 'Ера древнього життя');
            
            INSERT INTO rock_types (name, description, formation_process) 
            VALUES ('граніт', 'Магматична гірська порода', 'Кристалізація магми');
            
            INSERT INTO researchers (name, specialization, institution) 
            VALUES ('Тестовий Дослідник', 'Петрологія', 'Інститут геології');
        """
        
        for statement in initial_data_sql.split(';'):
            if statement.strip():
                cur.execute(statement)
        
        conn.commit()
        cur.close()
        conn.close()
        
        print("\nНалаштування PostgreSQL успішно завершено!")
        return True
        
    except Exception as e:
        print(f"\nПомилка при налаштуванні PostgreSQL: {e}")
        print(f"Тип помилки: {type(e).__name__}")
        return False

def setup_mongodb():
    try:
        print("\nНалаштування MongoDB...")
        client = MongoClient('mongodb://localhost:27017/')
        db = client['rocks_db']
        
        # Очищення існуючих колекцій
        collections = ['rock_samples', 'researchers', 'laboratories',
                       'research_projects', 'equipment']
        for collection in collections:
            db[collection].drop()
        
        # Створення колекцій
        db.create_collection('rock_samples')
        db.create_collection('researchers')
        db.create_collection('laboratories')
        db.create_collection('research_projects')
        db.create_collection('equipment')
        
        # Створення індексів
        print("Створення індексів для MongoDB...")
        db.rock_samples.create_index([("location.coordinates", "2dsphere")])
        db.rock_samples.create_index([("type", 1)])
        db.rock_samples.create_index([("geological_period", 1)])
        db.researchers.create_index([("specialization", 1)])
        db.laboratories.create_index([("location", "2dsphere")])
        
        # Додавання початкових даних
        print("Додавання початкових даних...")
        initial_researcher = {
            "name": "Тестовий Дослідник",
            "specialization": "Петрологія",
            "institution": "Інститут геології",
            "contact_info": "test@geology.org",
            "joined_at": datetime.now(timezone.utc),
            "projects": []
        }
        db.researchers.insert_one(initial_researcher)
        
        initial_project = {
            "name": "Дослідження гранітів",
            "description": "Вивчення гранітних утворень",
            "start_date": datetime.now(timezone.utc),
            "end_date": None,
            "funding_source": "Науковий фонд",
            "researchers": [],
            "samples": []
        }
        db.research_projects.insert_one(initial_project)
        
        print("MongoDB налаштована успішно!")
        return True
        
    except Exception as e:
        print(f"Помилка при налаштуванні MongoDB: {e}")
        return False

def test_nosql_performance(num_samples=1000):
    print(f"\nТестування MongoDB (кількість зразків: {num_samples})...")
    client = MongoClient('mongodb://localhost:27017/')
    db = client['rocks_db']
    
    # Створення тестових дослідників та проектів
    researcher_id = db.researchers.insert_one({
        "name": "Test Researcher",
        "specialization": "geology",
        "institution": "Test University",
        "contact_info": "test@test.com"
    }).inserted_id
    
    project_id = db.research_projects.insert_one({
        "name": "Test Project",
        "description": "Performance testing project",
        "start_date": datetime.now(timezone.utc),
        "end_date": datetime.now(timezone.utc),
        "funding_source": "Test Fund"
    }).inserted_id
    
    # Тест запису
    start_time = time.time()
    for i in range(num_samples):
        # Створення зразка
        sample = {
            "name": f"Sample-{i}",
            "type": "граніт",
            "location": {
                "coordinates": [random.uniform(45, 55), random.uniform(25, 35)],
                "type": "Point"
            },
            "researcher_id": researcher_id,
            "project_id": project_id,
            "geological_period": "палеозой",
            "chemical_composition": {
                "SiO2": random.uniform(70, 75),
                "Al2O3": random.uniform(13, 15),
                "K2O": random.uniform(4, 5)
            },
            "measurements": [{
                "hardness": random.uniform(6, 7),
                "density": random.uniform(2.6, 2.8),
                "porosity": random.uniform(0.1, 0.5),
                "permeability": random.uniform(0.001, 0.01),
                "crystal_structure": "гранітна",
                "equipment_id": str(ObjectId()),
                "date": datetime.now(timezone.utc)
            }],
            "collection_method": "standard",
            "collected_at": datetime.now(timezone.utc),
            "images": [],
            "analyses": []
        }
        db.rock_samples.insert_one(sample)
    
    write_time = time.time() - start_time
    print(f"Час запису MongoDB: {write_time:.2f} секунд")
    
    # Тест читання
    start_time = time.time()
    results = list(db.rock_samples.find({
        "type": "граніт",
        "geological_period": "палеозой"
    }).limit(num_samples))
    read_time = time.time() - start_time
    print(f"Час читання MongoDB: {read_time:.2f} секунд")
    print(f"Кількість знайдених записів: {len(results)}")
    
    return write_time, read_time

def test_sql_performance(password, num_samples=1000):
    print(f"\nТестування PostgreSQL (кількість зразків: {num_samples})...")
    conn = psycopg2.connect(
        host="localhost",
        database="rocks_db",
        user="postgres",
        password=password
    )
    cur = conn.cursor()
    
    # Створення тестових даних
    cur.execute("""
        INSERT INTO geological_periods (name, start_age, end_age)
        VALUES ('палеозой', 541, 252)
        RETURNING id
    """)
    period_id = cur.fetchone()[0]
    
    cur.execute("""
        INSERT INTO rock_types (name, description)
        VALUES ('граніт', 'Магматична гірська порода')
        RETURNING id
    """)
    type_id = cur.fetchone()[0]
    
    cur.execute("""
        INSERT INTO researchers (name, specialization, institution, contact_info)
        VALUES ('Test Researcher', 'geology', 'Test University', 'test@test.com')
        RETURNING id
    """)
    researcher_id = cur.fetchone()[0]
    
    cur.execute("""
        INSERT INTO research_projects (name, description, start_date, end_date, funding_source)
        VALUES ('Test Project', 'Performance testing project', CURRENT_DATE, CURRENT_DATE, 'Test Fund')
        RETURNING id
    """)
    project_id = cur.fetchone()[0]
    
    cur.execute("""
        INSERT INTO equipment (name, type, manufacturer)
        VALUES ('Test Equipment', 'measurement', 'Test Manufacturer')
        RETURNING id
    """)
    equipment_id = cur.fetchone()[0]
    
    # Тест запису
    start_time = time.time()
    for i in range(num_samples):
        # Додавання локації
        cur.execute("""
            INSERT INTO locations (latitude, longitude, altitude)
            VALUES (%s, %s, %s)
            RETURNING id
        """, (random.uniform(45, 55), random.uniform(25, 35), random.uniform(100, 1000)))
        location_id = cur.fetchone()[0]
        
        # Додавання зразка
        cur.execute("""
            INSERT INTO rock_samples (
                name, type_id, location_id, researcher_id, project_id,
                geological_period_id, collection_method
            )
            VALUES (%s, %s, %s, %s, %s, %s, %s)
            RETURNING id
        """, (f"Sample-{i}", type_id, location_id, researcher_id, project_id,
              period_id, 'standard'))
        sample_id = cur.fetchone()[0]
        
        # Додавання хімічного складу
        cur.execute("""
            INSERT INTO chemical_compositions (sample_id, compound, percentage)
            VALUES 
                (%s, 'SiO2', %s),
                (%s, 'Al2O3', %s),
                (%s, 'K2O', %s)
        """, (
            sample_id, random.uniform(70, 75),
            sample_id, random.uniform(13, 15),
            sample_id, random.uniform(4, 5)
        ))
        
        # Додавання вимірювань
        cur.execute("""
            INSERT INTO measurements (
                sample_id, equipment_id, hardness, density,
                porosity, permeability, crystal_structure
            )
            VALUES (%s, %s, %s, %s, %s, %s, %s)
        """, (
            sample_id, equipment_id,
            random.uniform(6, 7), random.uniform(2.6, 2.8),
            random.uniform(0.1, 0.5), random.uniform(0.001, 0.01),
            'гранітна'
        ))
    
    conn.commit()
    write_time = time.time() - start_time
    print(f"Час запису PostgreSQL: {write_time:.2f} секунд")
    
    # Тест читання
    start_time = time.time()
    cur.execute("""
        SELECT
            rs.*,
            rt.name as rock_type,
            gp.name as geological_period,
            cc.compound,
            cc.percentage,
            m.hardness,
            m.density,
            m.porosity,
            m.permeability,
            l.latitude,
            l.longitude
        FROM rock_samples rs
        JOIN rock_types rt ON rs.type_id = rt.id
        JOIN geological_periods gp ON rs.geological_period_id = gp.id
        JOIN locations l ON rs.location_id = l.id
        JOIN chemical_compositions cc ON rs.id = cc.sample_id
        JOIN measurements m ON rs.id = m.sample_id
        WHERE rt.name = 'граніт'
        AND gp.name = 'палеозой'
    """)
    results = cur.fetchall()
    read_time = time.time() - start_time
    print(f"Час читання PostgreSQL: {read_time:.2f} секунд")
    print(f"Кількість знайдених записів: {len(results)}")
    
    cur.close()
    conn.close()
    
    return write_time, read_time

def run_performance_comparison(password, num_samples=1000):
    print(f"\nПорівняння продуктивності баз даних")
    print("=" * 50)
    
    # Тестування MongoDB
    mongo_write, mongo_read = test_nosql_performance(num_samples)
    
    # Тестування PostgreSQL
    sql_write, sql_read = test_sql_performance(password, num_samples)
    
    print("\nРезультати порівняння:")
    print("=" * 50)
    print(f"Кількість зразків: {num_samples}")
    print("\nЧас запису:")
    print(f"MongoDB:    {mongo_write:.2f} секунд")
    print(f"PostgreSQL: {sql_write:.2f} секунд")
    print(f"Різниця:    {abs(mongo_write - sql_write):.2f} секунд")
    
    print("\nЧас читання:")
    print(f"MongoDB:    {mongo_read:.2f} секунд")
    print(f"PostgreSQL: {sql_read:.2f} секунд")
    print(f"Різниця:    {abs(mongo_read - sql_read):.2f} секунд")

def main():
    password = getpass.getpass("Введіть пароль для користувача postgres: ")
    num_samples = int(input("Введіть кількість зразків для тестування (за замовчуванням 1000): ") or 1000)
    
    print("\nПочинаємо налаштування баз даних...")
    
    if setup_postgresql(password) and setup_mongodb():
        print("\nБази даних успішно налаштовані!")
        # Імпортуємо функцію з попереднього артефакту
        run_performance_comparison(password, num_samples)
    else:
        print("\nВиникла помилка при налаштуванні баз даних.")
        sys.exit(1)

if __name__ == "__main__":
    main()
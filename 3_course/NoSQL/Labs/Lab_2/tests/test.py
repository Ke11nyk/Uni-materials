import psycopg2
import sys
import time
from pymongo import MongoClient
import random
from datetime import datetime

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
        
        cur.execute("""
            CREATE TABLE rock_types (
                id SERIAL PRIMARY KEY,
                name VARCHAR(100) NOT NULL,
                description TEXT
            )
        """)
        
        cur.execute("""
            CREATE TABLE rock_samples (
                id SERIAL PRIMARY KEY,
                name VARCHAR(100) NOT NULL,
                type_id INTEGER REFERENCES rock_types(id),
                latitude DECIMAL(9,6),
                longitude DECIMAL(9,6),
                collected_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        cur.execute("""
            CREATE TABLE chemical_compositions (
                id SERIAL PRIMARY KEY,
                sample_id INTEGER REFERENCES rock_samples(id),
                compound VARCHAR(50) NOT NULL,
                percentage DECIMAL(5,2) NOT NULL
            )
        """)
        
        cur.execute("""
            CREATE TABLE measurements (
                id SERIAL PRIMARY KEY,
                sample_id INTEGER REFERENCES rock_samples(id),
                hardness DECIMAL(4,2),
                density DECIMAL(4,2),
                measured_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """)
        
        print("\n6. Створення індексів...")
        cur.execute("CREATE INDEX idx_rock_samples_type ON rock_samples(type_id)")
        cur.execute("CREATE INDEX idx_measurements_sample ON measurements(sample_id)")
        cur.execute("CREATE INDEX idx_chemical_comp_sample ON chemical_compositions(sample_id)")
        
        print("\n7. Додавання початкових даних...")
        cur.execute("""
            INSERT INTO rock_types (name, description) 
            VALUES ('граніт', 'Магматична гірська порода')
        """)
        
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
        db.rock_samples.drop()  # Очищаємо колекцію якщо вона існує
        
        # Створення індексів
        db.rock_samples.create_index([("location", "2dsphere")])
        db.rock_samples.create_index([("type", 1)])
        
        print("MongoDB налаштована успішно!")
        return True
    except Exception as e:
        print(f"Помилка при налаштуванні MongoDB: {e}")
        return False

def test_nosql_performance(num_samples=1000):
    print(f"\nТестування MongoDB (кількість зразків: {num_samples})...")
    client = MongoClient('mongodb://localhost:27017/')
    db = client['rocks_db']
    samples = db.rock_samples
    
    # Тест запису
    start_time = time.time()
    for i in range(num_samples):
        sample = {
            "name": f"Sample-{i}",
            "type": "граніт",
            "location": [random.uniform(45, 55), random.uniform(25, 35)],
            "chemical_composition": {
                "SiO2": random.uniform(70, 75),
                "Al2O3": random.uniform(13, 15)
            },
            "measurements": [
                {"hardness": random.uniform(6, 7), 
                 "density": random.uniform(2.6, 2.8),
                 "date": datetime.utcnow()}
            ]
        }
        samples.insert_one(sample)
    write_time = time.time() - start_time
    print(f"Час запису MongoDB: {write_time:.2f} секунд")
    
    # Тест читання
    start_time = time.time()
    results = list(samples.find({"type": "граніт"}))
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
    
    # Тест запису
    start_time = time.time()
    for i in range(num_samples):
        # Додавання зразка
        cur.execute("""
            INSERT INTO rock_samples (name, type_id, latitude, longitude)
            VALUES (%s, 1, %s, %s) RETURNING id
        """, (f"Sample-{i}", random.uniform(45, 55), random.uniform(25, 35)))
        sample_id = cur.fetchone()[0]
        
        # Додавання хімічного складу
        cur.execute("""
            INSERT INTO chemical_compositions (sample_id, compound, percentage)
            VALUES (%s, 'SiO2', %s), (%s, 'Al2O3', %s)
        """, (sample_id, random.uniform(70, 75), sample_id, random.uniform(13, 15)))
        
        # Додавання вимірювань
        cur.execute("""
            INSERT INTO measurements (sample_id, hardness, density)
            VALUES (%s, %s, %s)
        """, (sample_id, random.uniform(6, 7), random.uniform(2.6, 2.8)))
    
    conn.commit()
    write_time = time.time() - start_time
    print(f"Час запису PostgreSQL: {write_time:.2f} секунд")
    
    # Тест читання
    start_time = time.time()
    cur.execute("""
        SELECT rs.*, cc.compound, cc.percentage, m.hardness, m.density
        FROM rock_samples rs
        JOIN chemical_compositions cc ON rs.id = cc.sample_id
        JOIN measurements m ON rs.id = m.sample_id
        WHERE rs.type_id = 1
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
    password = input("Введіть пароль для користувача postgres: ")
    num_samples = int(input("Введіть кількість зразків для тестування (за замовчуванням 1000): ") or 1000)
    
    print("\nПочинаємо налаштування баз даних...")
    
    if setup_postgresql(password) and setup_mongodb():
        print("\nБази даних успішно налаштовані!")
        run_performance_comparison(password, num_samples)
    else:
        print("\nВиникла помилка при налаштуванні баз даних.")
        sys.exit(1)

if __name__ == "__main__":
    main()
import psycopg2

def test_connection(password):
    try:
        # Спроба підключення
        conn = psycopg2.connect(
            host="localhost",
            user="postgres",
            password=password
        )
        
        # Перевірка версії PostgreSQL
        cur = conn.cursor()
        cur.execute('SELECT version()')
        db_version = cur.fetchone()
        
        print("PostgreSQL успішно підключено!")
        print(f"Версія бази даних: {db_version[0]}")
        
        cur.close()
        conn.close()
        
        return True
    except Exception as e:
        print(f"Помилка підключення до PostgreSQL: {e}")
        return False

if __name__ == "__main__":
    password = input("Введіть пароль для користувача postgres: ")
    test_connection(password)
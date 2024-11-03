from pymongo import MongoClient

def test_connection():
    try:
        # Спроба підключення
        client = MongoClient('mongodb://localhost:27017/')
        
        # Перевірка підключення
        client.admin.command('ping')
        
        print("MongoDB успішно підключено!")
        
        # Спробуємо створити тестову базу даних
        db = client['test_db']
        collection = db['test_collection']
        
        # Вставимо тестовий документ
        test_document = {"test": "success"}
        collection.insert_one(test_document)
        
        print("Тестовий документ успішно створено!")
        
        # Видалимо тестовий документ
        collection.delete_one({"test": "success"})
        
        return True
    except Exception as e:
        print(f"Помилка підключення до MongoDB: {e}")
        return False

if __name__ == "__main__":
    test_connection()
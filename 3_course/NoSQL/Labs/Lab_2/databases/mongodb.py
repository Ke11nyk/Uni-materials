from pymongo import MongoClient
import datetime

# Підключення до MongoDB
client = MongoClient('mongodb://localhost:27017/')
db = client['rocks_db']
samples_collection = db['rock_samples']

# Функція для додавання нового зразка
def add_rock_sample(name, type, location, chemical_composition, measurements):
    sample = {
        "name": name,
        "type": type,
        "location": {
            "coordinates": location,
            "type": "Point"
        },
        "chemical_composition": chemical_composition,
        "measurements": measurements,
        "collected_at": datetime.datetime.utcnow()
    }
    return samples_collection.insert_one(sample)

# Функція для пошуку зразків за типом
def find_samples_by_type(rock_type):
    return samples_collection.find({"type": rock_type})

# Функція для оновлення даних вимірювань
def update_measurements(sample_id, new_measurements):
    return samples_collection.update_one(
        {"_id": sample_id},
        {"$push": {"measurements": new_measurements}}
    )

# Приклад використання
test_sample = {
    "name": "Граніт-1",
    "type": "граніт",
    "location": [50.4501, 30.5234],
    "chemical_composition": {
        "SiO2": 72.5,
        "Al2O3": 14.2,
        "K2O": 4.1
    },
    "measurements": [
        {"hardness": 6.5, "density": 2.7, "date": datetime.datetime.utcnow()}
    ]
}

# Створення індексів для оптимізації
samples_collection.create_index([("location", "2dsphere")])
samples_collection.create_index([("type", 1)])
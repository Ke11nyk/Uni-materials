from pymongo import MongoClient
from datetime import datetime, timezone

# Підключення до MongoDB
client = MongoClient('mongodb://localhost:27017/')
db = client['rocks_db']

# Створення колекцій
rock_samples = db['rock_samples']
researchers = db['researchers']
laboratories = db['laboratories']
research_projects = db['research_projects']
equipment = db['equipment']

# Створення індексів
rock_samples.create_index([("location.coordinates", "2dsphere")])
rock_samples.create_index([("type", 1)])
rock_samples.create_index([("geological_period", 1)])
researchers.create_index([("specialization", 1)])
laboratories.create_index([("location", "2dsphere")])

# Функції для роботи з зразками
def add_rock_sample(name, type, location, researcher_id, project_id, geological_period,
                    chemical_composition, measurements, collection_method="standard"):
    sample = {
        "name": name,
        "type": type,
        "location": {
            "coordinates": location,
            "type": "Point"
        },
        "researcher_id": researcher_id,
        "project_id": project_id,
        "geological_period": geological_period,
        "chemical_composition": chemical_composition,
        "measurements": measurements,
        "collection_method": collection_method,
        "collected_at": datetime.now(timezone.utc),
        "images": [],
        "analyses": []
    }
    return rock_samples.insert_one(sample)

def add_sample_image(sample_id, image_url, image_type, microscope_type=None, magnitude=None):
    return rock_samples.update_one(
        {"_id": sample_id},
        {"$push": {"images": {
            "url": image_url,
            "type": image_type,
            "microscope_type": microscope_type,
            "magnitude": magnitude,
            "captured_at": datetime.now(timezone.utc)
        }}}
    )

def add_sample_analysis(sample_id, researcher_id, laboratory_id, analysis_type, results):
    analysis = {
        "researcher_id": researcher_id,
        "laboratory_id": laboratory_id,
        "analysis_type": analysis_type,
        "results": results,
        "performed_at": datetime.now(timezone.utc)
    }
    return rock_samples.update_one(
        {"_id": sample_id},
        {"$push": {"analyses": analysis}}
    )

# Функції для роботи з дослідниками
def add_researcher(name, specialization, institution, contact_info):
    researcher = {
        "name": name,
        "specialization": specialization,
        "institution": institution,
        "contact_info": contact_info,
        "joined_at": datetime.now(timezone.utc),
        "projects": []
    }
    return researchers.insert_one(researcher)

# Функції для роботи з лабораторіями
def add_laboratory(name, location, certification, equipment_list):
    laboratory = {
        "name": name,
        "location": location,
        "certification": certification,
        "equipment_list": equipment_list,
        "created_at": datetime.now(timezone.utc)
    }
    return laboratories.insert_one(laboratory)

# Функції для роботи з проектами
def create_research_project(name, description, start_date, end_date, funding_source):
    project = {
        "name": name,
        "description": description,
        "start_date": start_date,
        "end_date": end_date,
        "funding_source": funding_source,
        "created_at": datetime.now(timezone.utc),
        "researchers": [],
        "samples": []
    }
    return research_projects.insert_one(project)

# Функції для роботи з обладнанням
def add_equipment(name, type, manufacturer, calibration_date, calibration_certificate):
    equipment_doc = {
        "name": name,
        "type": type,
        "manufacturer": manufacturer,
        "last_calibration": calibration_date,
        "calibration_certificate": calibration_certificate,
        "added_at": datetime.now(timezone.utc)
    }
    return equipment.insert_one(equipment_doc)

# Приклад використання
test_sample = {
    "name": "Граніт-1",
    "type": "граніт",
    "location": [50.4501, 30.5234],
    "researcher_id": "researcher_1",
    "project_id": "project_1",
    "geological_period": "палеозой",
    "chemical_composition": {
        "SiO2": 72.5,
        "Al2O3": 14.2,
        "K2O": 4.1
    },
    "measurements": [
        {
            "hardness": 6.5,
            "density": 2.7,
            "porosity": 0.3,
            "permeability": 0.001,
            "crystal_structure": "гранітна",
            "equipment_id": "equipment_1",
            "date": datetime.now(timezone.utc)
        }
    ]
}
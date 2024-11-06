```mermaid
erDiagram
    rock_types ||--o{ rock_samples : contains
    rock_samples ||--o{ chemical_compositions : has
    rock_samples ||--o{ measurements : has
    rock_samples ||--o{ sample_images : has
    rock_samples ||--o{ sample_analysis : has
    researchers ||--o{ rock_samples : collects
    researchers ||--o{ sample_analysis : performs
    laboratories ||--o{ sample_analysis : conducts
    locations ||--o{ rock_samples : found_at
    mining_sites ||--o{ locations : contains
    geological_periods ||--o{ rock_samples : formed_in
    equipment ||--o{ measurements : measured_with
    research_projects ||--o{ rock_samples : studies
    research_projects ||--o{ researchers : involves

    research_projects ||--o{ project_researchers : has
    researchers ||--o{ project_researchers : participates

    rock_types {
        int id PK
        string name
        string description
        string formation_process
        string typical_minerals
    }
    
    rock_samples {
        int id PK
        string name
        int type_id FK "References rock_types(id)"
        int location_id FK "References locations(id)"
        int researcher_id FK "References researchers(id)"
        int project_id FK "References research_projects(id)"
        int geological_period_id FK "References geological_periods(id)"
        date collected_at
        string collection_method
        string preservation_state
    }
    
    chemical_compositions {
        int id PK
        int sample_id FK "References rock_samples(id)"
        string compound
        decimal percentage
        string measurement_method
        date analyzed_at
    }
    
    measurements {
        int id PK
        int sample_id FK "References rock_samples(id)"
        int equipment_id FK "References equipment(id)"
        decimal hardness
        decimal density
        decimal porosity
        decimal permeability
        string crystal_structure
        date measured_at
    }
    
    sample_images {
        int id PK
        int sample_id FK "References rock_samples(id)"
        string image_url
        string image_type
        string microscope_type
        int magnitude
        date captured_at
    }
    
    researchers {
        int id PK
        string name
        string specialization
        string institution
        string contact_info
        date joined_at
    }
    
    laboratories {
        int id PK
        string name
        string location
        string certification
        string equipment_list
    }
    
    locations {
        int id PK
        int mining_site_id FK "References mining_sites(id)"
        decimal latitude
        decimal longitude
        decimal altitude
        string terrain_type
    }
    
    mining_sites {
        int id PK
        string name
        string status
        date operational_since
        string access_permissions
    }
    
    geological_periods {
        int id PK
        string name
        int start_age
        int end_age
        string characteristics
    }
    
    equipment {
        int id PK
        string name
        string type
        string manufacturer
        date last_calibration
        string calibration_certificate
    }
    
    sample_analysis {
        int id PK
        int sample_id FK "References rock_samples(id)"
        int researcher_id FK "References researchers(id)"
        int laboratory_id FK "References laboratories(id)"
        string analysis_type
        text results
        date performed_at
    }
    
    research_projects {
        int id PK
        string name
        string description
        date start_date
        date end_date
        string funding_source
    }

    project_researchers {
        int project_id PK "References research_projects(id)"
        int researcher_id PK "References researchers(id)"
        string role
        date joined_at
    }
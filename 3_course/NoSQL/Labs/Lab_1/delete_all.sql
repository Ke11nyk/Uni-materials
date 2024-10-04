DO $$ 
DECLARE 
    r RECORD;
BEGIN
    -- Disable triggers
    EXECUTE 'SET session_replication_role = replica';
    
    -- Drop all tables in the current schema
    FOR r IN (SELECT tablename FROM pg_tables WHERE schemaname = current_schema()) LOOP
        EXECUTE 'DROP TABLE IF EXISTS ' || quote_ident(r.tablename) || ' CASCADE';
    END LOOP;
    
    -- Re-enable triggers
    EXECUTE 'SET session_replication_role = DEFAULT';
END $$;
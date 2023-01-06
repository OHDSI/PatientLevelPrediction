-- Database migrations for verion 6.0.10
-- This migration updates the schema:
 -- 1. to store the patient level prediction version
 -- 2. Add a migrations table for supporting database migrations

{DEFAULT @package_version = package_version}
{DEFAULT @migration = migration}
{DEFAULT @table_prefix = ''}

-- Create table indicating version number of ddl
DROP TABLE IF EXISTS @database_schema.@table_prefix@package_version;

--HINT DISTRIBUTE ON RANDOM
CREATE TABLE @database_schema.@table_prefix@package_version (
    version_number VARCHAR(50) PRIMARY KEY
);
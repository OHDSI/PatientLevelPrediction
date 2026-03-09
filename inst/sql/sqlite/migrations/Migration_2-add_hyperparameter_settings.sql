-- Database migration to persist hyperparameter settings as model design settings

{DEFAULT @table_prefix = ''}

CREATE TABLE IF NOT EXISTS @database_schema.@table_prefixhyperparameter_settings (
    hyperparameter_setting_id INTEGER PRIMARY KEY AUTOINCREMENT,
    hyperparameter_settings_json text NOT NULL
);

INSERT INTO @database_schema.@table_prefixhyperparameter_settings (hyperparameter_settings_json)
SELECT '{}'
WHERE NOT EXISTS (
    SELECT 1 FROM @database_schema.@table_prefixhyperparameter_settings
);

ALTER TABLE @database_schema.@table_prefixmodel_designs
ADD COLUMN hyperparameter_setting_id int;

UPDATE @database_schema.@table_prefixmodel_designs
SET hyperparameter_setting_id = (
    SELECT MIN(hyperparameter_setting_id)
    FROM @database_schema.@table_prefixhyperparameter_settings
)
WHERE hyperparameter_setting_id IS NULL;

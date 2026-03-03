-- Database migration to persist hyperparameter settings as model design settings

{DEFAULT @table_prefix = ''}

IF OBJECT_ID('@database_schema.@table_prefixhyperparameter_settings', 'U') IS NULL
BEGIN
  CREATE TABLE @database_schema.@table_prefixhyperparameter_settings (
      hyperparameter_setting_id int IDENTITY(1,1) NOT NULL PRIMARY KEY,
      hyperparameter_settings_json VARCHAR(MAX) NOT NULL
  );
END;

IF NOT EXISTS (
  SELECT 1
  FROM @database_schema.@table_prefixhyperparameter_settings
)
BEGIN
  INSERT INTO @database_schema.@table_prefixhyperparameter_settings (hyperparameter_settings_json)
  VALUES ('{}');
END;

IF COL_LENGTH('@database_schema.@table_prefixmodel_designs', 'hyperparameter_setting_id') IS NULL
BEGIN
  ALTER TABLE @database_schema.@table_prefixmodel_designs
  ADD hyperparameter_setting_id int;
END;

UPDATE md
SET md.hyperparameter_setting_id = hp.hyperparameter_setting_id
FROM @database_schema.@table_prefixmodel_designs md
CROSS JOIN (
  SELECT MIN(hyperparameter_setting_id) AS hyperparameter_setting_id
  FROM @database_schema.@table_prefixhyperparameter_settings
) hp
WHERE md.hyperparameter_setting_id IS NULL;

-- Database migration to persist model algorithm name in model settings

{DEFAULT @table_prefix = ''}

ALTER TABLE @database_schema.@table_prefixmodel_settings
ADD COLUMN model_name VARCHAR(255);

UPDATE @database_schema.@table_prefixmodel_settings
SET model_name = (
  SELECT MIN(m.model_type)
  FROM @database_schema.@table_prefixmodel_designs md
  INNER JOIN @database_schema.@table_prefixmodels m
    ON md.model_design_id = m.model_design_id
  WHERE md.model_setting_id = @database_schema.@table_prefixmodel_settings.model_setting_id
    AND m.model_type IS NOT NULL
    AND TRIM(m.model_type) <> ''
)
WHERE model_name IS NULL
   OR TRIM(model_name) = '';

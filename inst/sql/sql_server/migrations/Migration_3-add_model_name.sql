-- Database migration to persist model algorithm name in model settings

{DEFAULT @table_prefix = ''}

IF COL_LENGTH('@database_schema.@table_prefixmodel_settings', 'model_name') IS NULL
BEGIN
  ALTER TABLE @database_schema.@table_prefixmodel_settings
  ADD model_name VARCHAR(255);
END;

UPDATE ms
SET ms.model_name = src.model_name
FROM @database_schema.@table_prefixmodel_settings ms
INNER JOIN (
  SELECT md.model_setting_id,
         MIN(m.model_type) AS model_name
  FROM @database_schema.@table_prefixmodel_designs md
  INNER JOIN @database_schema.@table_prefixmodels m
    ON md.model_design_id = m.model_design_id
  WHERE m.model_type IS NOT NULL
    AND LTRIM(RTRIM(m.model_type)) <> ''
  GROUP BY md.model_setting_id
) src
  ON ms.model_setting_id = src.model_setting_id
WHERE ms.model_name IS NULL
   OR LTRIM(RTRIM(ms.model_name)) = '';

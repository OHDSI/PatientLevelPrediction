-- Database migration to persist model algorithm name in model settings

{DEFAULT @table_prefix = ''}

ALTER TABLE @database_schema.@table_prefixmodel_settings
ADD COLUMN IF NOT EXISTS model_name VARCHAR(255);

UPDATE @database_schema.@table_prefixmodel_settings ms
SET model_name = src.model_name
FROM (
  SELECT md.model_setting_id,
         MIN(m.model_type) AS model_name
  FROM @database_schema.@table_prefixmodel_designs md
  INNER JOIN @database_schema.@table_prefixmodels m
    ON md.model_design_id = m.model_design_id
  WHERE m.model_type IS NOT NULL
    AND BTRIM(m.model_type) <> ''
  GROUP BY md.model_setting_id
) src
WHERE ms.model_setting_id = src.model_setting_id
  AND (ms.model_name IS NULL OR BTRIM(ms.model_name) = '');

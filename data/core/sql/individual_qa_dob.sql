SELECT 'to_check' AS ACTION, count(*) AS COUNT FROM reg_entity_individuals WHERE DATE_OF_BIRTH IS NULL 
UNION 
SELECT 'ok' AS ACTION, count(*) AS COUNT FROM reg_entity_individuals WHERE DATE_OF_BIRTH IS NOT NULL
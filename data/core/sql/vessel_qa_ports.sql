SELECT 'to_check' AS ACTION, count(*) AS COUNT, 'homeport' AS CHARA FROM reg_vessels WHERE CL_FISH_HOME_PORT_LANDING_SITE_ID LIKE '%9999%'
UNION 
SELECT 'ok' AS ACTION, count(*) AS COUNT, 'homeport' AS CHARA FROM reg_vessels WHERE CL_FISH_HOME_PORT_LANDING_SITE_ID NOT LIKE '%9999%' 
UNION 
SELECT 'to_check' AS ACTION, count(*) AS COUNT, 'regport' AS CHARA FROM reg_vessels WHERE CL_FISH_REG_PORT_LANDING_SITE_ID LIKE '%9999%' 
UNION 
SELECT 'ok' AS ACTION, count(*) AS COUNT, 'regport' AS CHARA FROM reg_vessels WHERE CL_FISH_REG_PORT_LANDING_SITE_ID NOT LIKE '%9999%'
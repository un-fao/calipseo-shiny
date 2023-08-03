SELECT TEXT INTO @sql FROM lcacalipseo.ad_country_param where CODE = 'ISFISHER'; 
PREPARE sql_query FROM @sql; 
EXECUTE sql_query 
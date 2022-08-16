SELECT gend.CODE, gend.I18n_DEFAULT AS NAME, count(*) AS COUNT FROM reg_entity_individuals AS ind 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN cl_app_genders AS gend ON CL_APP_GENDER_ID = gend.ID 
WHERE ent.ENABLED = 1 
GROUP BY ind.CL_APP_GENDER_ID;
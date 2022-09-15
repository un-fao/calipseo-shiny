SELECT edulevel.CODE, edulevel.I18n_DEFAULT as NAME,fr.I18n_DEFAULT AS FSH_ROLE FROM reg_entity_individual_education AS edu 
LEFT JOIN reg_entities AS ent ON edu.REG_ENTITY_ID = ent.ID 
LEFT JOIN cl_app_education_levels AS edulevel ON edu.CL_APP_EDUCATION_LEVEL_ID = edulevel.ID 
LEFT JOIN reg_entity_individuals AS ind ON edu.ID = ind.ID 
LEFT JOIN cl_app_genders as gend ON CL_APP_GENDER_ID = gend.ID 
LEFT JOIN reg_entity_individual_role_fishery AS entfr ON ind.ID = entfr.REG_ENTITY_ID 
LEFT JOIN cl_fish_role_in_fishery AS fr ON entfr.CL_FISH_ROLE_IN_FISHERY_ID = fr.ID 
WHERE ent.ENABLED = 1
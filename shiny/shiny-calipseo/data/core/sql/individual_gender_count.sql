SELECT fr.I18n_DEFAULT AS FSH_ROLE,gend.I18n_DEFAULT AS NAME FROM reg_entity_individuals AS ind 
LEFT JOIN reg_entities AS ent ON ind.REG_ENTITY_ID = ent.ID 
LEFT JOIN cl_app_genders AS gend ON CL_APP_GENDER_ID = gend.ID 
LEFT JOIN reg_entity_individual_role_fishery AS entfr ON ind.ID = entfr.REG_ENTITY_ID 
LEFT JOIN cl_fish_role_in_fishery AS fr ON entfr.CL_FISH_ROLE_IN_FISHERY_ID = fr.ID 
WHERE ent.ENABLED = 1
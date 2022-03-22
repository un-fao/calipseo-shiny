SELECT entype.I18n_DEFAULT AS ENTITY_TYPE, ent.NAME, ind.FIRST_NAME, ind.MIDDLE_NAME, ind.ENTITY_DOCUMENT_NUMBER, 
ent.ADDRESS, ent.ADDRESS_CITY, ent.ADDRESS_ZIP_CODE, 
ent.PHONE_NUMBER, ent.MOBILE_NUMBER, 
v.REGISTRATION_NUMBER FROM reg_vessel_owners AS vo 
LEFT JOIN reg_entities AS ent 
ON vo.REG_ENTITY_OWNER = ent.id 
LEFT JOIN cl_app_entity_types AS entype 
ON ent.CL_APP_ENTITY_TYPE_ID = entype.id 
LEFT JOIN reg_entity_individuals AS ind 
ON vo.REG_ENTITY_OWNER = ind.REG_ENTITY_ID 
LEFT JOIN reg_entity_companies AS entcpy ON vo.REG_ENTITY_OWNER = entcpy.REG_ENTITY_ID 
LEFT JOIN reg_vessels AS v ON vo.REG_VESSEL_ID = v.ID
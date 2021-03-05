SELECT vo.*, ent.*, ind.* FROM reg_vessel_owners AS vo 
LEFT JOIN reg_entities AS ent 
ON vo.REG_ENTITY_OWNER = ent.id 
LEFT JOIN reg_entity_individuals AS ind 
ON vo.REG_ENTITY_OWNER = ind.REG_ENTITY_ID
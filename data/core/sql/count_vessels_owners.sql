select count(*) as COUNT from (select distinct reg.REG_ENTITY_OWNER from reg_vessel_owners reg 
left join reg_entities ent ON (reg.REG_ENTITY_OWNER = ent.ID) 
where ent.CL_APP_ENTITY_TYPE_ID = 1) as owners
SELECT 
count(*) as COUNT 
FROM reg_vessel_owners as vo 
LEFT JOIN reg_vessels as v ON vo.REG_VESSEL_ID = v.ID
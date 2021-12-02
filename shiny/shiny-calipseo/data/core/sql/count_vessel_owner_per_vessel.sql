SELECT 
count(*) AS COUNT 
FROM reg_vessel_owners AS vo 
LEFT JOIN reg_vessels AS v ON vo.REG_VESSEL_ID = v.ID
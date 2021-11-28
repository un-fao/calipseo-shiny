SELECT 
count(*) as COUNT 
FROM reg_vessel_gears as fg 
LEFT JOIN reg_vessels as v ON fg.REG_VESSEL_ID = v.ID
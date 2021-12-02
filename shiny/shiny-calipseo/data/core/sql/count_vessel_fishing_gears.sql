SELECT 
count(*) AS COUNT 
FROM reg_vessel_gears AS fg 
LEFT JOIN reg_vessels as v ON fg.REG_VESSEL_ID = v.ID
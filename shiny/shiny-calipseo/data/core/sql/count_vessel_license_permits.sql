SELECT 
count(*) as COUNT 
FROM reg_vessel_license_permits as vlp 
LEFT JOIN reg_vessels as v ON vlp.REG_VESSEL_ID = v.ID 

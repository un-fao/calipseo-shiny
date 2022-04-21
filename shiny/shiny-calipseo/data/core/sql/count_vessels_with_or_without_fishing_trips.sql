SELECT 
v.REGISTRATION_NUMBER,ft.DATE_FROM, ft.DATE_TO 
FROM dt_fishing_trip AS ft 
LEFT JOIN reg_vessels AS v ON ft.REG_VESSEL_ID = v.ID 

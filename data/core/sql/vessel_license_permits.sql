SELECT 
v.ID AS ID, vlp.PERMIT_NUMBER, vlp.APPLICATION_DATE, vlp.PERMIT_DATE,
vlp.DATE_FROM AS Valid_from_date, vlp.DATE_TO AS Valid_to_date,
rg.I18n_DEFAULT AS Gears, v.REGISTRATION_NUMBER 
FROM reg_vessels AS v 
LEFT JOIN reg_vessel_license_permits AS vlp ON v.ID = vlp.REG_VESSEL_ID  
LEFT JOIN reg_vessel_gears AS vg ON vlp.REG_VESSEL_ID = vg.REG_VESSEL_ID 
LEFT JOIN cl_ref_gears AS rg ON vg.CL_REF_GEAR_ID = rg.ID 


SELECT 
vlp.PERMIT_NUMBER, vlp.APPLICATION_DATE, vlp.PERMIT_DATE,
vlp.DATE_FROM AS Valid_from_date, vlp.DATE_TO AS Valid_to_date,
rg.I18n_DEFAULT AS Gears, v.REGISTRATION_NUMBER 
FROM reg_vessel_gears AS vg 
LEFT JOIN reg_vessels AS v ON vg.REG_VESSEL_ID = v.ID 
LEFT JOIN cl_ref_gears AS rg ON vg.CL_REF_GEAR_ID = rg.ID 
LEFT JOIN reg_vessel_license_permits AS vlp ON vg.REG_VESSEL_ID = vlp.REG_VESSEL_ID 



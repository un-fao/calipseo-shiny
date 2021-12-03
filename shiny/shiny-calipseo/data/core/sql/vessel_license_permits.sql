SELECT 
vlp.PERMIT_NUMBER, vlp.APPLICATION_DATE, vlp.PERMIT_DATE,
vlp.DATE_FROM as Valid_from_date, vlp.DATE_TO as Valid_to_date,
rg.NAME as Gears 
FROM reg_vessel_gears as vg 
LEFT JOIN reg_vessels as v ON vg.REG_VESSEL_ID = v.ID 
LEFT JOIN cl_ref_gears as rg ON vg.CL_REF_GEAR_ID = rg.ID 
LEFT JOIN reg_vessel_license_permits as vlp ON vg.REG_VESSEL_ID = vlp.REG_VESSEL_ID 



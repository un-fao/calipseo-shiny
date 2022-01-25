SELECT 
ft.ID as trip_id, 
ft.DATE_TO as date,
sp.I18n_DEFAULT as species_desc, sp.SCIENTIFIC_NAME as species_sci, sp.ASFIS_CODE as species_asfis,
fas.QUANTITY as quantity 
FROM dt_fishing_activities as fa 
LEFT JOIN dt_fishing_activities_species as fas ON fa.ID = fas.DT_FISHING_ACTIVITY_ID 
LEFT JOIN cl_ref_species as sp ON fas.CL_REF_SPECIES_ID = sp.ID 
LEFT JOIN dt_fishing_trip as ft ON fa.DT_FISHING_TRIP_ID = ft.ID 
LEFT JOIN reg_vessels as v ON ft.REG_VESSEL_ID = v.ID 


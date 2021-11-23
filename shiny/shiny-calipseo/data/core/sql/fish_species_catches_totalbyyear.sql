SELECT 
year(ft.DATE_TO) as year,
sp.NAME as species_desc,
fas.QUANTITY as quantity /* round(sum(fas.QUANTITY),2) as catches*/
FROM dt_fishing_activities as fa 
LEFT JOIN dt_fishing_activities_species as fas ON fa.ID = fas.DT_FISHING_ACTIVITY_ID 
LEFT JOIN cl_ref_species as sp ON fas.CL_REF_SPECIES_ID = sp.ID 
LEFT JOIN dt_fishing_trip as ft ON fa.DT_FISHING_TRIP_ID = ft.ID 
/*group by year(ft.DATE_TO), sp.NAME;*/

SELECT * FROM 
 (SELECT 
 ft.ID as id, 
 year(ft.DATE_FROM) as year,month(ft.DATE_FROM) as month,day(ft.DATE_FROM) as days, 
 ft.CL_FISH_FISHING_UNIT_ID as fishing_unit, 
 sp.ID as species, 
 fas.QUANTITY as quantity, fas.TOTAL_VALUE as value, fas.CATCH_NUMBER as number, fas.PRICE_PER_UNIT_CATCH as price
 FROM dt_fishing_activities as fa 
 LEFT JOIN dt_fishing_activities_species as fas ON fa.ID = fas.DT_FISHING_ACTIVITY_ID 
 LEFT JOIN cl_ref_species as sp ON fas.CL_REF_SPECIES_ID = sp.ID 
 LEFT JOIN dt_fishing_trip as ft ON fa.DT_FISHING_TRIP_ID = ft.ID ) as l 

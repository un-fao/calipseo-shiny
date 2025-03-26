SELECT * FROM 
 (SELECT 
 ft.ID as fishing_trip, 
 year(ft.DATE_FROM) as year,
 month(ft.DATE_FROM) as month,
 day(ft.DATE_FROM) as day,
 ft.CL_TO_PORT_SITE_ID as landing_site,
 site.CL_STAT_STRATA_ID as minor_strata,
 ft.CL_FISH_FISHING_UNIT_ID as fishing_unit,
 ft.TIME_SPENT_FISHING_ZONE as effort_fishing_duration,
 ft.CL_TIME_SPENT_FISHING_UNIT_ID as effort_fishing_duration_unit,
 sp.ID as species, 
 fas.CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT as catch_nominal_landed, 
 fas.TOTAL_VALUE as trade_value, 
 fas.CATCH_NUMBER as catch_number 
 FROM dt_fishing_activities as fa 
 LEFT JOIN dt_fishing_activities_species as fas ON fa.ID = fas.DT_FISHING_ACTIVITY_ID 
 LEFT JOIN cl_ref_species as sp ON fas.CL_REF_SPECIES_ID = sp.ID 
 LEFT JOIN dt_fishing_trip as ft ON fa.DT_FISHING_TRIP_ID = ft.ID
 LEFT JOIN cl_fish_landing_sites as site ON ft.CL_TO_PORT_SITE_ID = site.ID) as l 
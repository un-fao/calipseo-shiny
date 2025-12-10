SELECT 
    ft.ID AS trip_id, 
    ft.CL_FISH_FISHING_TRIP_TYPE_ID as trip_type, 
    ft.DATE_FROM AS date_from, 
    ft.DATE_TO AS date_to, 
    SUM(fas.CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT) AS total_catch, 
    SUM(fas.DISCARD_QUANTITY) AS total_discard 
FROM dt_fishing_trip AS ft 
LEFT JOIN dt_fishing_activities AS fa ON fa.DT_FISHING_TRIP_ID = ft.ID 
LEFT JOIN dt_fishing_activities_species AS fas ON fas.DT_FISHING_ACTIVITY_ID = fa.ID 
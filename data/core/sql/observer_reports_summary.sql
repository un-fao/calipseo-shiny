SELECT 
    r.ID AS report_id, 
    ft.REG_VESSEL_ID AS vessel_id, 
    vi.VESSEL_NAME AS vessel_name, 
    r.OBSERVER_FULL_NAME AS observer_name, 
    r.OBSERVATION_PERIOD_START AS observation_start, 
    r.OBSERVATION_PERIOD_END AS observation_end, 
    r.BOARDING_DATE AS embarkation_start, 
    r.DISEMBARK_DATE AS embarkation_end, 
    ft.DATE_FROM AS trip_start, 
    ft.DATE_TO AS trip_end, 
    DATEDIFF(r.DISEMBARK_DATE, r.BOARDING_DATE) AS days_at_sea, 
    COUNT(DISTINCT fa.ID) AS nb_fishing_operations, 
    CASE
        WHEN EXISTS ( 
            SELECT 1 
            FROM dt_fishing_trip AS ft2 
            WHERE ft2.REG_VESSEL_ID = ft.REG_VESSEL_ID 
              AND ft2.DATE_FROM = ft.DATE_FROM 
              AND ft2.DATE_TO = ft.DATE_TO 
              AND ft2.CL_FISH_FISHING_TRIP_TYPE_ID = 1 
        ) THEN 1 ELSE 0 
    END AS logbook_linked, 
    CASE 
        WHEN lf.DT_FISHING_TRIP_ID IS NOT NULL THEN 1 ELSE 0 
    END AS biological_present, 
    SUM(fas.CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT) AS total_catch, 
    SUM(fas.DISCARD_QUANTITY) AS total_discard 
FROM dt_observer_reports AS r
LEFT JOIN dt_fishing_trip ft ON ft.ID = r.DT_FISHING_TRIP_ID 
LEFT JOIN dt_observer_report_vessel_information AS vi ON vi.DT_OBSERVER_REPORT_ID = r.ID 
LEFT JOIN dt_fishing_activities AS fa ON fa.DT_FISHING_TRIP_ID = ft.ID 
LEFT JOIN dt_fishing_activities_species AS fas ON fas.DT_FISHING_ACTIVITY_ID = fa.ID 
LEFT JOIN dt_length_frequencies AS lf  ON lf.DT_FISHING_TRIP_ID = r.DT_FISHING_TRIP_ID 
GROUP BY 
    r.ID, 
    ft.REG_VESSEL_ID, 
    vi.VESSEL_NAME, 
    r.OBSERVER_FULL_NAME, 
    r.OBSERVATION_PERIOD_START, 
    r.OBSERVATION_PERIOD_END, 
    r.BOARDING_DATE, 
    r.DISEMBARK_DATE, 
    ft.DATE_FROM, 
    ft.DATE_TO; 
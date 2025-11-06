SELECT * 
FROM dt_observer_reports AS orep 
LEFT JOIN (SELECT DT_OBSERVER_REPORT_ID, VESSEL_NAME,VESSEL_OWNER FROM dt_observer_report_vessel_information) AS orvi ON orvi.DT_OBSERVER_REPORT_ID = orep.ID 
LEFT JOIN (SELECT DT_OBSERVER_REPORT_ID, OBSERVER_FULL_NAME FROM dt_observer_report_environment_conditions) AS orec ON orec.DT_OBSERVER_REPORT_ID = orep.ID
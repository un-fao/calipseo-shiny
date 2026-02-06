SELECT 
  r.ID AS REPORT_ID, 
  ft_report.ID AS TRIP_ID, 
  CASE WHEN ft_logbook.ID IS NOT NULL THEN 1 ELSE 0 END AS LINKED_TO_LOGBOOK, 
  ft_logbook.ID AS LOGBOOK_ID 
FROM dt_observer_reports r 
INNER JOIN dt_fishing_trip ft_report ON ft_report.ID = r.DT_FISHING_TRIP_ID AND ft_report.CL_FISH_FISHING_TRIP_TYPE_ID = 3 
LEFT JOIN dt_fishing_trip ft_logbook ON ft_logbook.REG_VESSEL_ID = ft_report.REG_VESSEL_ID AND ft_logbook.DATE_FROM = ft_report.DATE_FROM AND ft_logbook.DATE_TO = ft_report.DATE_TO AND ft_logbook.CL_FISH_FISHING_TRIP_TYPE_ID = 1
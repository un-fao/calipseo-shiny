SELECT trip.vesstype, trip.year, 
ROUND(sum(trip.sum),2) as sum, 
ROUND(avg(trip.sum),2) as mean,
COUNT(trip.sum) as number,
(SUBSTRING_INDEX(SUBSTRING_INDEX((SELECT GROUP_CONCAT(trip.sum ORDER BY trip.sum ASC SEPARATOR ',')), ',', CEILING((SELECT COUNT(trip.sum)) * 0.25)), ',', -1)) as q1,
(SUBSTRING_INDEX(SUBSTRING_INDEX((SELECT GROUP_CONCAT(trip.sum ORDER BY trip.sum ASC SEPARATOR ',')), ',', CEILING((SELECT COUNT(trip.sum)) * 0.5)), ',', -1)) as median,
(SUBSTRING_INDEX(SUBSTRING_INDEX((SELECT GROUP_CONCAT(trip.sum ORDER BY trip.sum ASC SEPARATOR ',')), ',', CEILING((SELECT COUNT(trip.sum)) * 0.75)), ',', -1)) as q3,
IF(COUNT(trip.sum)%2=1,
       SUBSTRING_INDEX(substring_index((SELECT GROUP_CONCAT(trip.sum ORDER BY trip.sum ASC SEPARATOR ',')), ',', CEILING((SELECT COUNT(trip.sum)) * 0.5)),",",-1),
       (SUBSTRING_INDEX(substring_index((SELECT GROUP_CONCAT(trip.sum ORDER BY trip.sum ASC SEPARATOR ',')), ',', CEILING((SELECT COUNT(trip.sum)) * 0.5)),",",-1) 
         + SUBSTRING_INDEX(substring_index((SELECT GROUP_CONCAT(trip.sum ORDER BY trip.sum ASC SEPARATOR ',')), ',', CEILING((SELECT COUNT(trip.sum)) * 0.5)+1),",",-1))/2) 
    as median2 ,
ROUND(STDDEV_SAMP(trip.sum),2) as sd
FROM
(SELECT q.landing_id, q.vesstype, q.year, sum(q.quantity) as sum FROM
(SELECT 
ft.ID as landing_id, 
ft.DATE_FROM as dep_datetime, ft.DATE_TO as ret_datetime, year(ft.DATE_TO) as year, month(ft.DATE_TO) as month, yearweek(ft.DATE_TO),
vt.I18n_DEFAULT as vesstype, 
fas.QUANTITY as quantity
FROM dt_fishing_activities as fa 
LEFT JOIN dt_fishing_activities_species as fas ON fa.ID = fas.DT_FISHING_ACTIVITY_ID 
LEFT JOIN dt_fishing_trip as ft ON fa.DT_FISHING_TRIP_ID = ft.ID 
LEFT JOIN reg_vessels as v ON ft.REG_VESSEL_ID = v.ID 
LEFT JOIN cl_ref_vessel_types as vt ON vt.ID = v.CL_REF_VESSEL_TYPE_ID 
WHERE v.CL_APP_VESSEL_STAT_TYPE_ID = 2
) as q
GROUP BY q.landing_id, q.vesstype, q.year) as trip
WHERE trip.sum IS NOT NULL
GROUP BY trip.vesstype, trip.year
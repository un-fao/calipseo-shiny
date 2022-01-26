SELECT 
CONCAT(t.year, '-', LPAD(t.month, 2, '0')) as date,
t.strat_id, 
t.strat_name 

from
(SELECT distinct 
 f.YEAR as year, 
 f.CL_APP_MONTH_ID as month, 
 f.CL_FISH_FISHING_UNIT_ID as strat_id, 
 fu.NAME as strat_name 
 
FROM dt_frame_survey as f 
LEFT JOIN cl_fish_fishing_units as fu ON fu.ID = f.CL_FISH_FISHING_UNIT_ID 
)as t
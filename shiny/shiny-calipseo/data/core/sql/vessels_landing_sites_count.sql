select ref.NAME, ref.LONGITUDE, ref.LATITUDE, count(*) as COUNT from reg_vessels reg 
left join cl_fish_landing_sites as ref ON CL_FISH_HOME_PORT_LANDING_SITE_ID = ref.ID 
WHERE ref.LONGITUDE != '' AND ref.LATITUDE != '' AND ref.NAME IS NOT NULL 
group by reg.CL_FISH_HOME_PORT_LANDING_SITE_ID;
select ref.I18n_DEFAULT as NAME, ref.LONGITUDE, ref.LATITUDE, count(*) as COUNT from reg_vessels reg 
LEFT JOIN cl_fish_landing_sites as ref ON CL_FISH_HOME_PORT_LANDING_SITE_ID = ref.ID 
WHERE ref.I18n_DEFAULT IS NOT NULL 
GROUP BY reg.CL_FISH_HOME_PORT_LANDING_SITE_ID;
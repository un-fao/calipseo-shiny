SELECT v.ID, 
v.REGISTRATION_NUMBER, 
ls.ID AS LANDING_SITE_ID, 
ls.I18n_DEFAULT AS LANDING_SITE 
FROM reg_vessels AS v 
LEFT JOIN (SELECT * FROM cl_fish_landing_sites) AS ls ON v.CL_FISH_REG_PORT_LANDING_SITE_ID = ls.ID 
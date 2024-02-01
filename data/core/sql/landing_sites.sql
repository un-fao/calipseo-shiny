select ID, CODE, I18n_DEFAULT as NAME, 
LONGITUDE, LATITUDE from cl_fish_landing_sites 
where LONGITUDE != '' AND LATITUDE != '' 
order by NAME;
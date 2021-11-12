select ref.NAME as HOME_PORT_LANDING_SITE, vt.name as VESSEL_TYPE, 
ref.LONGITUDE, ref.LATITUDE,count(*) as COUNT from reg_vessels as reg 
left join cl_ref_vessel_types as vt ON reg.CL_REF_VESSEL_TYPE_ID = vt.ID 
left join cl_fish_landing_sites as ref ON reg.CL_FISH_HOME_PORT_LANDING_SITE_ID = ref.ID 
where ref.LONGITUDE != '' and ref.LATITUDE != '' and ref.NAME IS not null 
group by reg.CL_FISH_HOME_PORT_LANDING_SITE_ID,reg.CL_REF_VESSEL_TYPE_ID;

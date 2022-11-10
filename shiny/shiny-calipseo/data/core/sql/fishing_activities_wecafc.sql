SELECT 
cou.ISO_3_CODE as flagstate,
t.year,t.period,t.geographical_identifier,t.species,t.catch_retained,t.catch_discarded,t.catch_nominal,t.catch_unit 
FROM (SELECT 
year(fa.DATE_TO) as year,
month(fa.DATE_TO) as period,
'WCA' as geographical_identifier,
sp.ASFIS_CODE as species,
fas.QUANTITY as catch_retained,
fas.DISCARD_QUANTITY as catch_discarded,
fas.QUANTITY as catch_nominal,
uom.I18n_DEFAULT as catch_unit,
v.CL_APP_VESSEL_STAT_TYPE_ID 
FROM dt_fishing_activities as fa 
LEFT JOIN dt_fishing_activities_species as fas ON fa.ID = fas.DT_FISHING_ACTIVITY_ID 
LEFT JOIN cl_ref_species as sp ON fas.CL_REF_SPECIES_ID = sp.ID 
LEFT JOIN cl_app_quantity_units uom ON fas.CL_APP_QUANTITY_UNIT_ID = uom.ID 
LEFT JOIN dt_fishing_trip as ft ON fa.DT_FISHING_TRIP_ID = ft.ID 
LEFT JOIN reg_vessels as v ON ft.REG_VESSEL_ID = v.ID 
)as t , ad_country_param as cp 
LEFT JOIN cl_ref_countries as cou ON cp.VALUE_NO_UNIT = cou.ID 
WHERE cp.CODE = 'ISOCODE' 
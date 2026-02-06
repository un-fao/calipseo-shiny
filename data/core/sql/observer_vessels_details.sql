SELECT 
  rvi.DT_OBSERVER_REPORT_ID AS REPORT_ID, 
  rvi.VESSEL_NAME AS VESSEL_NAME, 
  rvi.VESSEL_OWNER AS VESSEL_OWNER, 
  ctry.I18n_DEFAULT AS NATIONALITY, 
  rvi.CONSTRUCTION_YEAR AS CONSTRUCTION_YEAR, 
  rvi.REGISTRATION_NUMBER AS REGISTRATION_NUMBER, 
  rvi.LICENCE_NUMBER AS LICENCE_NUMBER, 
  rvi.LOA AS LOA, 
  loa_unit.I18n_DEFAULT AS LOA_UNIT, 
  rvi.DRA AS DRA, 
  dra_unit.I18n_DEFAULT AS DRA_UNIT, 
  rvi.WIDTH AS WIDTH, 
  width_unit.I18n_DEFAULT AS WIDTH_UNIT, 
  rvi.GT AS GT, 
  gt_unit.I18n_DEFAULT AS GT_UNIT, 
  rvi.NET_TONNAGE AS NET_TONNAGE, 
  net_ton_unit.I18n_DEFAULT AS NET_TONNAGE_UNIT, 
  rvi.SPEED AS SPEED, 
  speed_unit.I18n_DEFAULT AS SPEED_UNIT, 
  rvi.ENGINE_POWER AS ENGINE_POWER, 
  engine_power_unit.I18n_DEFAULT AS ENGINE_POWER_UNIT, 
  nav.VHF_RADIO, 
  nav.HF_RADIO, 
  nav.RADAR, 
  nav.SOUNDER, 
  nav.SONAR, 
  nav.AUTOPILOT, 
  nav.SATELITE, 
  nav.ROUTE_TRACER, 
  nav.NET_PROBE, 
  cons.I18n_DEFAULT AS FISH_CONSERVATION_METHODS, 
  pres.NUMBER_OF_STORES, 
  pres.STORAGE_CAPACITY, 
  storage_unit.I18n_DEFAULT AS STORAGE_CAPACITY_UNIT, 
  pres.FREEZING_CAPACITY, 
  freezing_unit.I18n_DEFAULT AS FREEZING_CAPACITY_UNIT, 
  call_type.I18n_DEFAULT AS VESSEL_CALL_TYPE 
FROM dt_observer_report_vessel_information rvi 
INNER JOIN dt_observer_reports r ON r.ID = rvi.DT_OBSERVER_REPORT_ID 
LEFT JOIN cl_ref_countries ctry ON ctry.ID = rvi.CL_REF_COUNTRY_NATIONALITY_ID 
LEFT JOIN cl_app_quantity_units loa_unit ON loa_unit.ID = rvi.CL_APP_QUANTITY_UNIT_ID_LOA 
LEFT JOIN cl_app_quantity_units dra_unit ON dra_unit.ID = rvi.CL_APP_QUANTITY_UNIT_ID_DRA 
LEFT JOIN cl_app_quantity_units gt_unit ON gt_unit.ID = rvi.CL_APP_QUANTITY_UNIT_ID_GT 
LEFT JOIN cl_app_quantity_units net_ton_unit ON net_ton_unit.ID = rvi.CL_APP_NET_TONNAGE_UNIT_ID 
LEFT JOIN cl_app_quantity_units speed_unit ON speed_unit.ID = rvi.CL_SPEED_UNIT_ID 
LEFT JOIN cl_app_quantity_units engine_power_unit ON engine_power_unit.ID = rvi.ENGINE_POWER_UNIT_ID 
LEFT JOIN cl_app_quantity_units width_unit ON width_unit.ID = rvi.CL_APP_WIDTH_UNIT_ID 
LEFT JOIN dt_observer_report_vessel_navigation_equipment nav ON nav.DT_OBSERVER_REPORT_VESSEL_INFO_ID = rvi.ID 
LEFT JOIN dt_observer_report_vessel_conservation_methods rvc ON rvc.DT_OBSERVER_REPORT_VESSEL_INFO_ID = rvi.ID 
LEFT JOIN cl_fish_conservation_methods cons ON cons.ID = rvc.CL_FISH_CONSERVATION_METHODS_ID 
LEFT JOIN dt_observer_report_vessel_fish_preservation pres ON pres.DT_OBSERVER_REPORT_VESSEL_INFO_ID = rvi.ID 
LEFT JOIN cl_app_quantity_units storage_unit ON storage_unit.ID = pres.CL_APP_QUANTITY_UNIT_ID_STORAGE_CAPACITY 
LEFT JOIN cl_app_quantity_units freezing_unit ON freezing_unit.ID = pres.CL_APP_QUANTITY_UNIT_ID_FREEZING_CAPACITY 
LEFT JOIN dt_observer_report_vessel_call_type_frequency vct ON vct.DT_OBSERVER_REPORT_VESSEL_INFO_ID = rvi.ID 
LEFT JOIN cl_app_vessel_call_types call_type ON call_type.ID = vct.CL_APP_VESSEL_CALL_TYPE_ID
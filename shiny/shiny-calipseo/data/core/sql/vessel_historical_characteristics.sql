SELECT 
vhct.NAME AS Type,
rgvhch.OLD_VALUE_DATE,rgvhch.NEW_VALUE_DATE,rgvhch.OLD_VALUE_FLOAT,rgvhch.NEW_VALUE_FLOAT,
rgvhch.OLD_ID_LABEL,rgvhch.NEW_ID_LABEL,rgvhch.OLD_VALUE_STRING,rgvhch.NEW_VALUE_STRING,rgvhch.DESCRIPTION,rgvhch.UPDATED_AT 
FROM reg_vessel_historical_characteristics AS rgvhch 
LEFT JOIN (SELECT * FROM reg_vessels) AS v ON rgvhch.REG_VESSEL_ID = v.ID 
LEFT JOIN (SELECT * FROM cl_app_vessel_historical_characteristic_types) AS vhct ON rgvhch.CL_APP_VESSEL_HISTORICAL_CHARACTERISTIC_ID = vhct.ID 
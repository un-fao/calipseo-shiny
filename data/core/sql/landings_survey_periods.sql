SELECT distinct 
year(f.DATE_TO) as year, 
month(f.DATE_TO) as month 
FROM dt_fishing_trip as f
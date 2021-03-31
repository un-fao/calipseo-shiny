select year(ft.DATE_TO) as YEAR, month(ft.DATE_TO) as MONTH, round(sum(fas.QUANTITY),2) as QUANTITY from dt_fishing_activities_species fas 
left join dt_fishing_trip ft on (ft.ID = fas.DT_FISHING_ACTIVITY_ID) 
group by year(ft.DATE_TO), month(ft.DATE_TO);
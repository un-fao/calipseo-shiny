# shiny-calipseo v1.0.0.9999 (DEV)

## New features

## Enhancements

## Corrections

** Core corrections**

* [CALR-67](https://sdlc.review.fao.org/jira/browse/CALR-67) Routes do not work systematically

** Module corrections**

_Individuals -list_

* [CALR-91](https://sdlc.review.fao.org/jira/browse/CALR-91) individuals list - Wrong SQL join with reg_entities

_Vessels - details (info)_

* [CALR-92](https://sdlc.review.fao.org/jira/browse/CALR-92) Vessel-info-module Number-of-license-infobox-display-more-counts
* [CALR-93](https://sdlc.review.fao.org/jira/browse/CALR-93) Vessel-info-module Removing extra column name from dataframe
* [CALR-94](https://sdlc.review.fao.org/jira/browse/CALR-94) Vessel-info-module Vessel Operational Status infobox throws error when invalide registration number is input
* [CALR-95](https://sdlc.review.fao.org/jira/browse/CALR-95) Vessel-info-module vessel characteristics tab throws error when invalide registration number is input


# shiny-calipseo  [v1.0.0](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.0.0.zip) - 2022-04-22

## New features

**Core features**

* [CALR-6](https://sdlc.review.fao.org/jira/browse/CALR-6) Prepare Dockerfile for calipseo-shiny
* [CALR-29](https://sdlc.review.fao.org/jira/browse/CALR-29) Add capacity for internal module to be loaded
* [CALR-17](https://sdlc.review.fao.org/jira/browse/CALR-17) Add R function to load country profile from DB
* [CALR-32](https://sdlc.review.fao.org/jira/browse/CALR-32) i18n multi-lingual support - base skeleton for i18 app labels
* [CALR-49](https://sdlc.review.fao.org/jira/browse/CALR-49) Apply i18n English ('en') terms to all existing modules
* [CALR-41](https://sdlc.review.fao.org/jira/browse/CALR-41) Faciliate the set-up of local shiny configs
* [CALR-55](https://sdlc.review.fao.org/jira/browse/CALR-55) i18n multi-lingual support - retrieval of DB i18n labels
* [CALR-57](https://sdlc.review.fao.org/jira/browse/CALR-57) Switch to RMariaDB driver instead of RMySQL
* [CALR-61](https://sdlc.review.fao.org/jira/browse/CALR-61) Inherit country params
* [CALR-62](https://sdlc.review.fao.org/jira/browse/CALR-62) Create 'HAS_REGMANGT' R variable based on country-specific parameter
* [CALR-68](https://sdlc.review.fao.org/jira/browse/CALR-68) Externalize package dependencies to json file
* [CALR-75](https://sdlc.review.fao.org/jira/browse/CALR-75) Add version/date footer for the shiny-calipseo app
* [CALR-84](https://sdlc.review.fao.org/jira/browse/CALR-84) Improve country's logo handling
* [CALR-89](https://sdlc.review.fao.org/jira/browse/CALR-89) Refactor shiny module server calls from 'callModule' to 'moduleServer'

**Module features**

_Vessels - list_

* [CALR-80](https://sdlc.review.fao.org/jira/browse/CALR-) Vessel list - Add a column for License status

_Vessels - details (info)_

* [CALR-14](https://sdlc.review.fao.org/jira/browse/CALR-14) Vessel Details module - Vessel image fetching/display from Vessel finder
* [CALR-15](https://sdlc.review.fao.org/jira/browse/CALR-15) Vessel Details module - add catches sub tab 'Breakdown by species'
* [CALR-21](https://sdlc.review.fao.org/jira/browse/CALR-21) Vessel Details module - add indicators
* [CALR-24](https://sdlc.review.fao.org/jira/browse/CALR-24) Vessel Details module - Apply generic function to display advanced species linechart
* [CALR-25](https://sdlc.review.fao.org/jira/browse/CALR-25) Vessel Details module - add catches sub tab 'Breakdown by species group'
* [CALR-31](https://sdlc.review.fao.org/jira/browse/CALR-31) Vessel Details module - Licenses tab
* [CALR-45](https://sdlc.review.fao.org/jira/browse/CALR-45) Vessel Details module - add vertical tab 'Fishing trips'
* [CALR-46](https://sdlc.review.fao.org/jira/browse/CALR-46) Vessel Details module - implement vessel characteristics history
* [CALR-66](https://sdlc.review.fao.org/jira/browse/CALR-66) Vessel Details module - add info box for license status

_Vessels - QA (checks)_

* [CALR-71](https://sdlc.review.fao.org/jira/browse/CALR-71) Vessel QA module -New module
* [CALR-73](https://sdlc.review.fao.org/jira/browse/CALR-73) Vessel QA module - Add tab on vessel characteristics
* [CALR-74](https://sdlc.review.fao.org/jira/browse/CALR-74) Vessel QA module - vessels ports - switch to a single table for countings
* [CALR-77](https://sdlc.review.fao.org/jira/browse/CALR-77) Vessel QA module - Add tab for counting vessels with expired license for current year
* [CALR-78](https://sdlc.review.fao.org/jira/browse/CALR-78) Vessel QA module - Add tab for counting vessels with unknown operational status
* [CALR-86](https://sdlc.review.fao.org/jira/browse/CALR-86) Vessel QA module - Add tab for counting vessels with/without activity 

_Individuals - list_

* [CALR-69](https://sdlc.review.fao.org/jira/browse/CALR-69) Individuals list - new core module inception

_Logbooks - overview_

* [CALR-4](https://sdlc.review.fao.org/jira/browse/CALR-4) Logbooks - Overview - Add statistical timeseries as linechart

_Logbooks - trips_

* [CALR-44](https://sdlc.review.fao.org/jira/browse/CALR-) New Trip Monitoring Module

_Logbooks - data upload_

* [CALR-5](https://sdlc.review.fao.org/jira/browse/CALR-5) Logbooks - Data upload module (new)

_Artfish_

* [CALR-42](https://sdlc.review.fao.org/jira/browse/CALR-42) New module for Artfish statistical methodology
* [CALR-40](https://sdlc.review.fao.org/jira/browse/CALR-40) R Functions in support of Artfish methodology / statistical computation
* [CALR-56](https://sdlc.review.fao.org/jira/browse/CALR-56) Apply i18n DB labels to ArtFish module
* [CALR-58](https://sdlc.review.fao.org/jira/browse/CALR-) Apply i18n English ('en') terms to Artfish modules

**Utilities**

* [CALR-2](https://sdlc.review.fao.org/jira/browse/CALR-2) R function in support of Suriname logbooks data integration
* [CALR-12](https://sdlc.review.fao.org/jira/browse/CALR-12) R functions in support of Lebanon FLOUCA data integration
* [CALR-13](https://sdlc.review.fao.org/jira/browse/CALR-13) R function to fetch free information from Vessel Finder
* [CALR-27](https://sdlc.review.fao.org/jira/browse/CALR-27) Compute Confidence Intervals (percentiles 2.5 and 97.5) associated to data series mean
* [CALR-50](https://sdlc.review.fao.org/jira/browse/CALR-50) Extend shinydashboard 'infoBox' function for more flexibility

## Enhancements

**Core enhancements**

* [CALR-16](https://sdlc.review.fao.org/jira/browse/CALR-16) Evaluate needs to add 'shinydashboardplus' to improve user-friendliness
* [CALR-28](https://sdlc.review.fao.org/jira/browse/CALR-28) Load ASFIS reference dataset enriched with ISSCAAP groups at app starting
* [CALR-83](https://sdlc.review.fao.org/jira/browse/CALR-83) Improve country's logo handling
* [CALR-48](https://sdlc.review.fao.org/jira/browse/CALR-48) Reduce size (in particular height) of vertical tab titles

**Module enhancements**

_Home_

* [CALR-7](https://sdlc.review.fao.org/jira/browse/CALR-7) Home module - Improve UI dynamicity
* [CALR-63](https://sdlc.review.fao.org/jira/browse/CALR-63) Home module | Display vessel-related info boxes only when HAS_REGMANGT is TRUE

_Vessels - list_

* [CALR-10](https://sdlc.review.fao.org/jira/browse/CALR-10) Vessels - Register module improvements

_Vessels - details (info)_

* [CALR-11](https://sdlc.review.fao.org/jira/browse/CALR-11) Vessel Details module - UI refactoring
* [CALR-20](https://sdlc.review.fao.org/jira/browse/CALR-20) Test 'shinydashboardplus' for vessel identity card
* [CALR-36](https://sdlc.review.fao.org/jira/browse/CALR-36) Vessel Details module - apply different background color vessel operational status info box
* [CALR-38](https://sdlc.review.fao.org/jira/browse/CALR-38) Vessel Details module - additional information (vessel characteristics) to put in vessel identity card
* [CALR-39](https://sdlc.review.fao.org/jira/browse/CALR-39) Vessel Details module - enrich vessel identity card 'Info' tab
* [CALR-47](https://sdlc.review.fao.org/jira/browse/CALR-47) Vessel Details module - Add warning on 'catches' and 'fishing trips' report for artisanal vessels
* [CALR-52](https://sdlc.review.fao.org/jira/browse/CALR-52) Vessel Details module - remove page title
* [CALR-60](https://sdlc.review.fao.org/jira/browse/CALR-60) Extent vessel owners list to target companies
* [CALR-72](https://sdlc.review.fao.org/jira/browse/CALR-72) Vessel Details module - switch from js_render_for_license_table to native R shiny implementation
* [CALR-81](https://sdlc.review.fao.org/jira/browse/CALR-81) Vessel Details module - add explicit logs on main code runs

_Vessels - breakdown_

* [CALR-8](https://sdlc.review.fao.org/jira/browse/CALR-8) Vessels - Breakdown - UI improvements

_Logbooks - overview_

* [CALR-22](https://sdlc.review.fao.org/jira/browse/CALR-22) Logbooks - Overview - Convert kgs to tons
* [CALR-26](https://sdlc.review.fao.org/jira/browse/CALR-26) Logbooks - Overview - Layout improvement for species / species group
* [CALR-34](https://sdlc.review.fao.org/jira/browse/CALR-34) Logbook overview lineplot Function - add capability to include summary table
* [CALR-35](https://sdlc.review.fao.org/jira/browse/CALR-35) Logbooks - Overview -  Simplify UI
* [CALR-70](https://sdlc.review.fao.org/jira/browse/CALR-70) Improve Logbook overview

_Linechart module (internal)_

* [CALR-23](https://sdlc.review.fao.org/jira/browse/CALR-23) Factorize generic function(s) to display statistics as linechart with shinydashboardPlus
* [CALR-43](https://sdlc.review.fao.org/jira/browse/CALR-43) Generic line_chart module - set a landscape view for PDF data export


## Corrections

** Core corrections**

* [CALR-9](https://sdlc.review.fao.org/jira/browse/CALR-) Fix hrefs to shiny module relative paths
* [CALR-18](https://sdlc.review.fao.org/jira/browse/CALR-) Add missing URL params for modules without paging

** Module corrections**

_Vessels - list_

* [CALR-87](https://sdlc.review.fao.org/jira/browse/CALR-87) Vessel-list enabling all the vessels with valid license permits display as intended

_Vessels - details (info)_

* [CALR-1](https://sdlc.review.fao.org/jira/browse/CALR-1) SUR - Vessel info fail for some vessels
* [CALR-51](https://sdlc.review.fao.org/jira/browse/CALR-51) vesselFindeR doesn't work anymore
* [CALR-53](https://sdlc.review.fao.org/jira/browse/CALR-53) Vessel Details module - fix typos
* [CALR-59](https://sdlc.review.fao.org/jira/browse/CALR-59) Vessels details - Check that data is properly filtered by vessel / aggregated
* [CALR-76](https://sdlc.review.fao.org/jira/browse/CALR-76) License status infobox broken
* [CALR-85](https://sdlc.review.fao.org/jira/browse/CALR-85) Vessel details - Issue with vessel history (when no data?)

_Vessels - QA (checks_

* [CALR-88](https://sdlc.review.fao.org/jira/browse/CALR-88) Vessel QA-vessel license tab not having the right counts
* [CALR-90](https://sdlc.review.fao.org/jira/browse/CALR-90) Vessel QA-vessel activity tab not having the right counts

_Logbooks - overview_

* [CALR-3](https://sdlc.review.fao.org/jira/browse/CALR-3) Logbooks - Overview - Check accuracy of 'Logbook Reporting ratio'


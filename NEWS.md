# **shiny-calipseo [v1.5.0](Ongoing) - 2024-02-20**

## Corrections

**Core corrections**

* [#188](https://github.com/un-fao/calipseo-shiny/issues/188) Counting of landing sites too restrictive on geographic coordinates availability
* [#189](https://github.com/un-fao/calipseo-shiny/issues/189) Bad filering inside tables of vessel and individual list modules
* [#190](https://github.com/un-fao/calipseo-shiny/issues/190) Select2 js filter not sorting first column

## Enhancements

**Core enhancements**

* [#178](https://github.com/un-fao/calipseo-shiny/issues/178) Make artfish method compatible for multiple sampling strategy
* [#181](https://github.com/un-fao/calipseo-shiny/issues/181) Make computation module more flexible
* [#186](https://github.com/un-fao/calipseo-shiny/issues/186) Make the shiny app rely on external calipseo-data
* [#187](https://github.com/un-fao/calipseo-shiny/issues/187) Move statistical indicators defs to calipseo-data repository
* [#185](https://github.com/un-fao/calipseo-shiny/issues/185) add capability to filter variable categories for table module

**Module enhancements**

_Individuals list_

* [#191](https://github.com/un-fao/calipseo-shiny/issues/191) Full i18n implementation for individuals list
* [#192](https://github.com/un-fao/calipseo-shiny/issues/192) Full i18n implementation for individuals overview

_Vessels list_

* [#194](https://github.com/un-fao/calipseo-shiny/issues/194) Vessel list - Support vessel status (active/inactive)

_Vessels overview_

* [#184](https://github.com/un-fao/calipseo-shiny/issues/184) Vessel overview, update table categories
* [#193](https://github.com/un-fao/calipseo-shiny/issues/193) Full i18n implementation for vessels overview

## New features

**Core new features**

**Stat new features**

_Saint Lucia (LCA)_

* [#182](https://github.com/un-fao/calipseo-shiny/issues/182) LCA - add statistical indicator for count number of active fisher by category
* [#183](https://github.com/un-fao/calipseo-shiny/issues/183) LCA - add statistical indicator to count active vessel

# **shiny-calipseo [v1.4.0](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.4.0.zip) - 2023-12-05**

## Corrections

**Core corrections**

* [#162](https://github.com/un-fao/calipseo-shiny/issues/162) Computation module - Error with handling local datasets
* [#163](https://github.com/un-fao/calipseo-shiny/issues/163) Computation module - Error with passing temporal args to `computeIndicator`
* [#175](https://github.com/un-fao/calipseo-shiny/issues/175) js_select2_filter_provider.js fails when some item is null

## Enhancements

**Core enhancements**

* [#156](https://github.com/un-fao/calipseo-shiny/issues/156) Computation module - Add support for mode (release/staging) selection for inter-dependent indicators
* [#160](https://github.com/un-fao/calipseo-shiny/issues/160) Computation module - Add message to inform if all upstream indicators are going to be computed (case of inter-dependent indicators)
* [#164](https://github.com/un-fao/calipseo-shiny/issues/164) Make individual modules generic
* [#170](https://github.com/un-fao/calipseo-shiny/issues/170) LBN - adjust digit number of statistical indicators

**Module enhancements**

_Individuals_

* [#174](https://github.com/un-fao/calipseo-shiny/issues/174) Individual Overview - Restructure visual and content to make the module more generic
* [#176](https://github.com/un-fao/calipseo-shiny/issues/176) Implement IS_FISHER and IS_FISHER ACTIVE query and apply them in individuals_overview

_Vessels_

* [#177](https://github.com/un-fao/calipseo-shiny/issues/177) Refactor vessel breakdown to vessel overview
* [#180](https://github.com/un-fao/calipseo-shiny/issues/180) Enable Stats breakdowns even with no map is available

## New features

**Core new features**

* [#157](https://github.com/un-fao/calipseo-shiny/issues/157) Computation module -  add support for triggering upstream indicators in cascade 
* [#161](https://github.com/un-fao/calipseo-shiny/issues/161) Computation module -  add capability to release staging indicators in cascade if a toplevel indicator is released
* [#165](https://github.com/un-fao/calipseo-shiny/issues/165) Create config file for LCA
* [#171](https://github.com/un-fao/calipseo-shiny/issues/171) Create generic widget module for sunburst chart
* [#172](https://github.com/un-fao/calipseo-shiny/issues/172) Create generic widget module for pyramid chart
* [#173](https://github.com/un-fao/calipseo-shiny/issues/173) Create generic widget module to display table with wide view

**Module new features**

* [#167](https://github.com/un-fao/calipseo-shiny/issues/167) LBN - Add statistical indicator to compute GFCM Task I
* [#158](https://github.com/un-fao/calipseo-shiny/issues/158) LBN - Add statistical indicator to compute GFCM Task II.1
* [#159](https://github.com/un-fao/calipseo-shiny/issues/159) LBN - Add statistical indicator to compute GFCM Task II.2
* [#169](https://github.com/un-fao/calipseo-shiny/issues/169) LBN - Add statistical indicator to compute GFCM Task III
* [#168](https://github.com/un-fao/calipseo-shiny/issues/168) LBN - Add statistical indicator to compute FAO report

**New R dependencies**

* `sortable` --> [#171](https://github.com/un-fao/calipseo-shiny/issues/171)

# **shiny-calipseo [v1.3.0](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.3.0.zip) - 2022-02-02**

## Enhancements

**Core enhancements**

* [#143](https://github.com/un-fao/calipseo-shiny/issues/143) Improve units of measures handling / conversion
* [#144](https://github.com/un-fao/calipseo-shiny/issues/144) Systematize use of CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT for target quantity field
* [#146](https://github.com/un-fao/calipseo-shiny/issues/146) Make the computation module generic (initially done for TTO)
* [#147](https://github.com/un-fao/calipseo-shiny/issues/147) Use computation module (statistics storage) for Artfish module
* [#152](https://github.com/un-fao/calipseo-shiny/issues/152) Computation module needs to trigger changes for dependent modules
* [#153](https://github.com/un-fao/calipseo-shiny/issues/153) Computation module - refactor periods handling (to check actual file existence)
* [#155](https://github.com/un-fao/calipseo-shiny/issues/155) Use ASFIS species / species groups from fdiwg repository instead of RefData

## New features

**Core features**

* [#150](https://github.com/un-fao/calipseo-shiny/issues/150) Computation module - enable external store for storing computation results
* [#154](https://github.com/un-fao/calipseo-shiny/issues/154) Computation module - need to be permissive for visualizing statistics (staging/release)

**Module features**

* [#142](https://github.com/un-fao/calipseo-shiny/issues/142) New module for SUR - export to CWP RH / WECAFC-FIRMS RDB formats
* [#148](https://github.com/un-fao/calipseo-shiny/issues/148) LBN - Add statistical indicator to compute Artfish estimates by fleet segment

**Miscs**

* [#145](https://github.com/un-fao/calipseo-shiny/issues/145) Introduce dotenv package for easier switch between local instance configs

**New R dependencies**

* lubridate --> [#142](https://github.com/un-fao/calipseo-shiny/issues/142)
* units --> [#143](https://github.com/un-fao/calipseo-shiny/issues/143)
* measurements --> [#143](https://github.com/un-fao/calipseo-shiny/issues/143)


# **shiny-calipseo [v1.2.0](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.2.0.zip) - 2022-11-03**

## Corrections

**Core corrections**

* [#128](https://github.com/un-fao/calipseo-shiny/issues/128) Vessel-info vesselfinder returns less variables

**Module corrections**

_Individual Breakdown and List_

* [#133](https://github.com/un-fao/calipseo-shiny/issues/133) Modify list of items used like fisher/non fisher roles

_Vessels - list / details_

* [#137](https://github.com/un-fao/calipseo-shiny/issues/137) Modify query to search fishing type of vessel

_Logbook trips_

* [#118](https://github.com/un-fao/calipseo-shiny/issues/118) Logbook trips-select inputs do not respond

_Vessel details_

* [#122](https://github.com/un-fao/calipseo-shiny/issues/122) Vessel QA-handle vessels with no license

## Improvements

**Core enhancements**

* [#127](https://github.com/un-fao/calipseo-shiny/issues/127) VesselFindeR - add type to restrain search to fishing vessels

**Module enhancements**

_Individual Info_

* [#129](https://github.com/un-fao/calipseo-shiny/issues/129) Add information about age and entity document of individu profil card

_Individual List_

* [#126](https://github.com/un-fao/calipseo-shiny/issues/126) Add dropdown list for filter individuals based on fishery roles

* [#131](https://github.com/un-fao/calipseo-shiny/issues/131) Use INFO function

_Individual Breakdown_

* [#130](https://github.com/un-fao/calipseo-shiny/issues/130) Add dropdown list for filter individuals based on fishery roles

_Individual Quality Assessment_

* [#134](https://github.com/un-fao/calipseo-shiny/issues/134) Add quality information for gender and educational level

_Logbook details_

* [#136](https://github.com/un-fao/calipseo-shiny/issues/136) Force to organize years and connect dots in chronological order on plot 

_Vessel list_

* [#119](https://github.com/un-fao/calipseo-shiny/issues/119) Vessel-list enable vessel list table combine with license permit table with no data

_Vessel details_

* [#123](https://github.com/un-fao/calipseo-shiny/issues/123) Vessel-info make an expection for Vessel Operational Status for dominica

## New features

**Core features**

* [#141](https://github.com/un-fao/calipseo-shiny/issues/141) New strategy to catch and dissiminate currency preference use to store data 

* [#140](https://github.com/un-fao/calipseo-shiny/issues/140) New strategy to catch and dissiminate weight unit preference use to store data 

**Module features**

* [#120](https://github.com/un-fao/calipseo-shiny/issues/120) New module for individuals QA
* [#121](https://github.com/un-fao/calipseo-shiny/issues/121) New module for individuals breakdown
* [#124](https://github.com/un-fao/calipseo-shiny/issues/124) Individual breakdown: Breaking down educational level by gender
* [#125](https://github.com/un-fao/calipseo-shiny/issues/125) New module for individuals details
* [#132](https://github.com/un-fao/calipseo-shiny/issues/132) New module for individuals overview

# **shiny-calipseo [v1.1.3](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.1.3.zip) - 2022-07-13**

## Corrections

**Module corrections**

_Artfish method (summary by fishing units)_

* [#117](https://github.com/un-fao/calipseo-shiny/issues/117) bugs in calculation break the "By fishing unit' module  


# **shiny-calipseo [v1.1.2](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.1.2.zip) - 2022-06-24**

## Corrections

**Module corrections**

_Logbook validation (Suriname)_

* [#110](https://github.com/un-fao/calipseo-shiny/issues/110) Logbooks validation / SQL - add inclusion of fishery products - **Fix regression with SQL**

# **shiny-calipseo [v1.1.1](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.1.1.zip) - 2022-06-22**

## Enhancements

**Module enhancements**

_Fishing trips_

* [#115](https://github.com/un-fao/calipseo-shiny/issues/115) Fishing trips - detail by vessel - need to round (2 digits) quantities

_Logbook validation (Suriname)_

* [#110](https://github.com/un-fao/calipseo-shiny/issues/110) Logbooks validation / SQL - add inclusion of fishery products
* [#116](https://github.com/un-fao/calipseo-shiny/issues/116) Logbook validation (Suriname) - implement RD-DD+1 rule for fishing trip duration

## Corrections

**Module corrections**

_Logbook overview_

* [#111](https://github.com/un-fao/calipseo-shiny/issues/111) Logbooks overview - CI intervals with normal and student dists

_Logbook details_

* [#113](https://github.com/un-fao/calipseo-shiny/issues/113) Logbooks - details - download buttons do not work in "By landing site" tab tables


# **shiny-calipseo [v1.1.0](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.1.0.zip) - 2022-05-12**

## New features

## Enhancements

**Module enhancements**

_Vessels - list / details_


* [#96](https://github.com/un-fao/calipseo-shiny/issues/96) Adopting a function to determine vessel license validity across vessel list module and vessel info module
* [#97](https://github.com/un-fao/calipseo-shiny/issues/97) Refractoring Vessel list module-moving from using column indexes to column names
* [#98](https://github.com/un-fao/calipseo-shiny/issues/98) Using the gicon calipseo function to handle glyphicon icons in vessel list module
* [#99](https://github.com/un-fao/calipseo-shiny/issues/99) Refractoring Vessel info module-moving from using column indexes to column names

_Logbook validation (Suriname)_

* [#107](https://github.com/un-fao/calipseo-shiny/issues/107) Rename module for more clarity on business processes (validation, SQL generation) vs. upload
* [#108](https://github.com/un-fao/calipseo-shiny/issues/108) Increase validation rules for Logbook upload modules

## Corrections

**Core corrections**

* [#67](https://github.com/un-fao/calipseo-shiny/issues/67) Routes do not work systematically
* [#100](https://github.com/un-fao/calipseo-shiny/issues/100) vessel license permits data not accurate

** Module corrections**

_Individuals -list_

* [#91](https://github.com/un-fao/calipseo-shiny/issues/91) individuals list - Wrong SQL join with reg_entities

_Vessels - details (info)_

* [#92](https://github.com/un-fao/calipseo-shiny/issues/92) Vessel-info-module Number-of-license-infobox-display-more-counts
* [#93](https://github.com/un-fao/calipseo-shiny/issues/93) Vessel-info-module Removing extra column name from dataframe
* [#94](https://github.com/un-fao/calipseo-shiny/issues/94) Vessel-info-module Vessel Operational Status infobox throws error when invalide registration number is input
* [#95](https://github.com/un-fao/calipseo-shiny/issues/95) Vessel-info-module vessel characteristics tab throws error when invalide registration number is input
* [#101](https://github.com/un-fao/calipseo-shiny/issues/101) VesselFindeR returns unstandard data without image link for some vessels

**Miscs**

* [#106](https://github.com/un-fao/calipseo-shiny/issues/106) Fix logbook SQL generation function for Suriname logbook data

# **shiny-calipseo  [v1.0.0](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.0.0.zip) - 2022-04-22**

## New features

**Core features**

* [#6](https://github.com/un-fao/calipseo-shiny/issues/6) Prepare Dockerfile for calipseo-shiny
* [#29](https://github.com/un-fao/calipseo-shiny/issues/29) Add capacity for internal module to be loaded
* [#17](https://github.com/un-fao/calipseo-shiny/issues/17) Add R function to load country profile from DB
* [#32](https://github.com/un-fao/calipseo-shiny/issues/32) i18n multi-lingual support - base skeleton for i18 app labels
* [#49](https://github.com/un-fao/calipseo-shiny/issues/49) Apply i18n English ('en') terms to all existing modules
* [#41](https://github.com/un-fao/calipseo-shiny/issues/41) Faciliate the set-up of local shiny configs
* [#55](https://github.com/un-fao/calipseo-shiny/issues/55) i18n multi-lingual support - retrieval of DB i18n labels
* [#57](https://github.com/un-fao/calipseo-shiny/issues/57) Switch to RMariaDB driver instead of RMySQL
* [#61](https://github.com/un-fao/calipseo-shiny/issues/61) Inherit country params
* [#62](https://github.com/un-fao/calipseo-shiny/issues/62) Create 'HAS_REGMANGT' R variable based on country-specific parameter
* [#68](https://github.com/un-fao/calipseo-shiny/issues/68) Externalize package dependencies to json file
* [#75](https://github.com/un-fao/calipseo-shiny/issues/75) Add version/date footer for the shiny-calipseo app
* [#84](https://github.com/un-fao/calipseo-shiny/issues/84) Improve country's logo handling
* [#89](https://github.com/un-fao/calipseo-shiny/issues/89) Refactor shiny module server calls from 'callModule' to 'moduleServer'

**Module features**

_Vessels - list_

* [#80](https://github.com/un-fao/calipseo-shiny/issues/) Vessel list - Add a column for License status

_Vessels - details (info)_

* [#14](https://github.com/un-fao/calipseo-shiny/issues/14) Vessel Details module - Vessel image fetching/display from Vessel finder
* [#15](https://github.com/un-fao/calipseo-shiny/issues/15) Vessel Details module - add catches sub tab 'Breakdown by species'
* [#21](https://github.com/un-fao/calipseo-shiny/issues/21) Vessel Details module - add indicators
* [#24](https://github.com/un-fao/calipseo-shiny/issues/24) Vessel Details module - Apply generic function to display advanced species linechart
* [#25](https://github.com/un-fao/calipseo-shiny/issues/25) Vessel Details module - add catches sub tab 'Breakdown by species group'
* [#31](https://github.com/un-fao/calipseo-shiny/issues/31) Vessel Details module - Licenses tab
* [#45](https://github.com/un-fao/calipseo-shiny/issues/45) Vessel Details module - add vertical tab 'Fishing trips'
* [#46](https://github.com/un-fao/calipseo-shiny/issues/46) Vessel Details module - implement vessel characteristics history
* [#66](https://github.com/un-fao/calipseo-shiny/issues/66) Vessel Details module - add info box for license status

_Vessels - QA (checks)_

* [#71](https://github.com/un-fao/calipseo-shiny/issues/71) Vessel QA module -New module
* [#73](https://github.com/un-fao/calipseo-shiny/issues/73) Vessel QA module - Add tab on vessel characteristics
* [#74](https://github.com/un-fao/calipseo-shiny/issues/74) Vessel QA module - vessels ports - switch to a single table for countings
* [#77](https://github.com/un-fao/calipseo-shiny/issues/77) Vessel QA module - Add tab for counting vessels with expired license for current year
* [#78](https://github.com/un-fao/calipseo-shiny/issues/78) Vessel QA module - Add tab for counting vessels with unknown operational status
* [#86](https://github.com/un-fao/calipseo-shiny/issues/86) Vessel QA module - Add tab for counting vessels with/without activity 

_Individuals - list_

* [#69](https://github.com/un-fao/calipseo-shiny/issues/69) Individuals list - new core module inception

_Logbooks - overview_

* [#4](https://github.com/un-fao/calipseo-shiny/issues/4) Logbooks - Overview - Add statistical timeseries as linechart

_Logbooks - trips_

* [#44](https://github.com/un-fao/calipseo-shiny/issues/) New Trip Monitoring Module

_Logbooks - data upload_

* [#5](https://github.com/un-fao/calipseo-shiny/issues/5) Logbooks - Data upload module (new)

_Artfish_

* [#42](https://github.com/un-fao/calipseo-shiny/issues/42) New module for Artfish statistical methodology
* [#40](https://github.com/un-fao/calipseo-shiny/issues/40) R Functions in support of Artfish methodology / statistical computation
* [#56](https://github.com/un-fao/calipseo-shiny/issues/56) Apply i18n DB labels to ArtFish module
* [#58](https://github.com/un-fao/calipseo-shiny/issues/) Apply i18n English ('en') terms to Artfish modules

**Utilities**

* [#2](https://github.com/un-fao/calipseo-shiny/issues/2) R function in support of Suriname logbooks data integration
* [#12](https://github.com/un-fao/calipseo-shiny/issues/12) R functions in support of Lebanon FLOUCA data integration
* [#13](https://github.com/un-fao/calipseo-shiny/issues/13) R function to fetch free information from Vessel Finder
* [#27](https://github.com/un-fao/calipseo-shiny/issues/27) Compute Confidence Intervals (percentiles 2.5 and 97.5) associated to data series mean
* [#50](https://github.com/un-fao/calipseo-shiny/issues/50) Extend shinydashboard 'infoBox' function for more flexibility

## Enhancements

**Core enhancements**

* [#16](https://github.com/un-fao/calipseo-shiny/issues/16) Evaluate needs to add 'shinydashboardplus' to improve user-friendliness
* [#28](https://github.com/un-fao/calipseo-shiny/issues/28) Load ASFIS reference dataset enriched with ISSCAAP groups at app starting
* [#83](https://github.com/un-fao/calipseo-shiny/issues/83) Improve country's logo handling
* [#48](https://github.com/un-fao/calipseo-shiny/issues/48) Reduce size (in particular height) of vertical tab titles

**Module enhancements**

_Home_

* [#7](https://github.com/un-fao/calipseo-shiny/issues/7) Home module - Improve UI dynamicity
* [#63](https://github.com/un-fao/calipseo-shiny/issues/63) Home module | Display vessel-related info boxes only when HAS_REGMANGT is TRUE

_Vessels - list_

* [#10](https://github.com/un-fao/calipseo-shiny/issues/10) Vessels - Register module improvements

_Vessels - details (info)_

* [#11](https://github.com/un-fao/calipseo-shiny/issues/11) Vessel Details module - UI refactoring
* [#20](https://github.com/un-fao/calipseo-shiny/issues/20) Test 'shinydashboardplus' for vessel identity card
* [#36](https://github.com/un-fao/calipseo-shiny/issues/36) Vessel Details module - apply different background color vessel operational status info box
* [#38](https://github.com/un-fao/calipseo-shiny/issues/38) Vessel Details module - additional information (vessel characteristics) to put in vessel identity card
* [#39](https://github.com/un-fao/calipseo-shiny/issues/39) Vessel Details module - enrich vessel identity card 'Info' tab
* [#47](https://github.com/un-fao/calipseo-shiny/issues/47) Vessel Details module - Add warning on 'catches' and 'fishing trips' report for artisanal vessels
* [#52](https://github.com/un-fao/calipseo-shiny/issues/52) Vessel Details module - remove page title
* [#60](https://github.com/un-fao/calipseo-shiny/issues/60) Extent vessel owners list to target companies
* [#72](https://github.com/un-fao/calipseo-shiny/issues/72) Vessel Details module - switch from js_render_for_license_table to native R shiny implementation
* [#81](https://github.com/un-fao/calipseo-shiny/issues/81) Vessel Details module - add explicit logs on main code runs

_Vessels - breakdown_

* [#8](https://github.com/un-fao/calipseo-shiny/issues/8) Vessels - Breakdown - UI improvements

_Logbooks - overview_

* [#22](https://github.com/un-fao/calipseo-shiny/issues/22) Logbooks - Overview - Convert kgs to tons
* [#26](https://github.com/un-fao/calipseo-shiny/issues/26) Logbooks - Overview - Layout improvement for species / species group
* [#34](https://github.com/un-fao/calipseo-shiny/issues/34) Logbook overview lineplot Function - add capability to include summary table
* [#35](https://github.com/un-fao/calipseo-shiny/issues/35) Logbooks - Overview -  Simplify UI
* [#70](https://github.com/un-fao/calipseo-shiny/issues/70) Improve Logbook overview

_Linechart module (internal)_

* [#23](https://github.com/un-fao/calipseo-shiny/issues/23) Factorize generic function(s) to display statistics as linechart with shinydashboardPlus
* [#43](https://github.com/un-fao/calipseo-shiny/issues/43) Generic line_chart module - set a landscape view for PDF data export


## Corrections

**Core corrections**

* [#9](https://github.com/un-fao/calipseo-shiny/issues/) Fix hrefs to shiny module relative paths
* [#18](https://github.com/un-fao/calipseo-shiny/issues/) Add missing URL params for modules without paging

**Module corrections**

_Vessels - list_

* [#87](https://github.com/un-fao/calipseo-shiny/issues/87) Vessel-list enabling all the vessels with valid license permits display as intended

_Vessels - details (info)_

* [#1](https://github.com/un-fao/calipseo-shiny/issues/1) SUR - Vessel info fail for some vessels
* [#51](https://github.com/un-fao/calipseo-shiny/issues/51) vesselFindeR doesn't work anymore
* [#53](https://github.com/un-fao/calipseo-shiny/issues/53) Vessel Details module - fix typos
* [#59](https://github.com/un-fao/calipseo-shiny/issues/59) Vessels details - Check that data is properly filtered by vessel / aggregated
* [#76](https://github.com/un-fao/calipseo-shiny/issues/76) License status infobox broken
* [#85](https://github.com/un-fao/calipseo-shiny/issues/85) Vessel details - Issue with vessel history (when no data?)

_Vessels - QA (checks_

* [#88](https://github.com/un-fao/calipseo-shiny/issues/88) Vessel QA-vessel license tab not having the right counts
* [#90](https://github.com/un-fao/calipseo-shiny/issues/90) Vessel QA-vessel activity tab not having the right counts

_Logbooks - overview_

* [#3](https://github.com/un-fao/calipseo-shiny/issues/3) Logbooks - Overview - Check accuracy of 'Logbook Reporting ratio'


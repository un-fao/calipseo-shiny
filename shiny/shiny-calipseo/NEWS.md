# **shiny-calipseo [v1.3.20230819] ONGOING DEVELOPMENT - 2023-08-19**

## Corrections

**Core corrections**

* [CALR-162](https://sdlc.review.fao.org/jira/browse/CALR-162) Computation module - Error with handling local datasets
* [CALR-163](https://sdlc.review.fao.org/jira/browse/CALR-163) Computation module - Error with passing temporal args to computeIndicator
* [CALR-175](https://sdlc.review.fao.org/jira/browse/CALR-175) js_select2_filter_provider.js fails when some item is null

## Enhancements

**Core enhancements**

* [CALR-156](https://sdlc.review.fao.org/jira/browse/CALR-156) Computation module - Add support for mode (release/staging) selection for inter-dependent indicators
* [CALR-160](https://sdlc.review.fao.org/jira/browse/CALR-160) Computation module - Add message to inform if all upstream indicators are going to be computed (case of inter-dependent indicators)
* [CALR-164](https://sdlc.review.fao.org/jira/browse/CALR-164) Make individual modules generic
* [CALR-170](https://sdlc.review.fao.org/jira/browse/CALR-170) LBN - adjust digit number of statistical indicators

**Module enhancements**

_Individuals_

* [CALR-174](https://sdlc.review.fao.org/jira/browse/CALR-174) Individual Overview - Restructurate visual and content to make the module more generic
* [CALR-176](https://sdlc.review.fao.org/jira/browse/CALR-176) Implement IS_FISHER and IS_FISHER ACTIVE query and apply them in individuals_overview

_Vessels_

* [CALR-177](https://sdlc.review.fao.org/jira/browse/CALR-177) Refactor vessel breakdown to vessel overview

## New features

**Core new features**

* [CALR-157](https://sdlc.review.fao.org/jira/browse/CALR-157) Computation module -  add support for triggering upstream indicators in cascade 
* [CALR-161](https://sdlc.review.fao.org/jira/browse/CALR-161) Computation module -  add capability to release staging indicators in cascade if a toplevel indicator is released
* [CALR-165](https://sdlc.review.fao.org/jira/browse/CALR-165) Create config file for LCA
* [CALR-171](https://sdlc.review.fao.org/jira/browse/CALR-171) Create generic widget module for sunburst chart
* [CALR-172](https://sdlc.review.fao.org/jira/browse/CALR-172) Create generic widget module for pyramid chart
* [CALR-173](https://sdlc.review.fao.org/jira/browse/CALR-173) Create generic widget module to display table with wide view

**Module new features**

* [CALR-167](https://sdlc.review.fao.org/jira/browse/CALR-167) LBN - Add statistical indicator to compute GFCM Task I
* [CALR-158](https://sdlc.review.fao.org/jira/browse/CALR-158) LBN - Add statistical indicator to compute GFCM Task II.1
* [CALR-159](https://sdlc.review.fao.org/jira/browse/CALR-159) LBN - Add statistical indicator to compute GFCM Task II.2
* [CALR-169](https://sdlc.review.fao.org/jira/browse/CALR-169) LBN - Add statistical indicator to compute GFCM Task III
* [CALR-168](https://sdlc.review.fao.org/jira/browse/CALR-168) LBN - Add statistical indicator to compute FAO report

# **shiny-calipseo [v1.3.0](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.3.0.zip) - 2022-02-02**

## Enhancements

**Core enhancements**

* [CALR-143](https://sdlc.review.fao.org/jira/browse/CALR-143) Improve units of measures handling / conversion
* [CALR-144](https://sdlc.review.fao.org/jira/browse/CALR-144) Systematize use of CATCH_QUANTITY_LIVE_WEIGHT_EQUIVALENT for target quantity field
* [CALR-146](https://sdlc.review.fao.org/jira/browse/CALR-146) Make the computation module generic (initially done for TTO)
* [CALR-147](https://sdlc.review.fao.org/jira/browse/CALR-147) Use computation module (statistics storage) for Artfish module
* [CALR-152](https://sdlc.review.fao.org/jira/browse/CALR-152) Computation module needs to trigger changes for dependent modules
* [CALR-153](https://sdlc.review.fao.org/jira/browse/CALR-153) Computation module - refactor periods handling (to check actual file existence)
* [CALR-155](https://sdlc.review.fao.org/jira/browse/CALR-155) Use ASFIS species / species groups from fdiwg repository instead of RefData

## New features

**Core features**

* [CALR-150](https://sdlc.review.fao.org/jira/browse/CALR-150) Computation module - enable external store for storing computation results
* [CALR-154](https://sdlc.review.fao.org/jira/browse/CALR-154) Computation module - need to be permissive for visualizing statistics (staging/release)

**Module features**

* [CALR-142](https://sdlc.review.fao.org/jira/browse/CALR-142) New module for SUR - export to CWP RH / WECAFC-FIRMS RDB formats
* [CALR-148](https://sdlc.review.fao.org/jira/browse/CALR-148) LBN - Add statistical indicator to compute Artfish estimates by fleet segment

**Miscs**

* [CALR-145](https://sdlc.review.fao.org/jira/browse/CALR-145) Introduce dotenv package for easier switch between local instance configs

**New R dependencies**

* lubridate --> [CALR-142](https://sdlc.review.fao.org/jira/browse/CALR-142)
* units --> [CALR-143](https://sdlc.review.fao.org/jira/browse/CALR-143)
* measurements --> [CALR-143](https://sdlc.review.fao.org/jira/browse/CALR-143)


# **shiny-calipseo [v1.2.0](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.2.0.zip) - 2022-11-03**

## Corrections

**Core corrections**

* [CALR-128](https://sdlc.review.fao.org/jira/browse/CALR-128) Vessel-info vesselfinder returns less variables

**Module corrections**

_Individual Breakdown and List_

* [CALR-133](https://sdlc.review.fao.org/jira/browse/CALR-133) Modify list of items used like fisher/non fisher roles

_Vessels - list / details_

* [CALR-137](https://sdlc.review.fao.org/jira/browse/CALR-137) Modify query to search fishing type of vessel

_Logbook trips_

* [CALR-118](https://sdlc.review.fao.org/jira/browse/CALR-118) Logbook trips-select inputs do not respond

_Vessel details_

* [CALR-122](https://sdlc.review.fao.org/jira/browse/CALR-122) Vessel QA-handle vessels with no license

## Improvements

**Core enhancements**

* [CALR-127](https://sdlc.review.fao.org/jira/browse/CALR-127) VesselFindeR - add type to restrain search to fishing vessels

**Module enhancements**

_Individual Info_

* [CALR-129](https://sdlc.review.fao.org/jira/browse/CALR-129) Add information about age and entity document of individu profil card

_Individual List_

* [CALR-126](https://sdlc.review.fao.org/jira/browse/CALR-126) Add dropdown list for filter individuals based on fishery roles

* [CALR-131](https://sdlc.review.fao.org/jira/browse/CALR-131) Use INFO function

_Individual Breakdown_

* [CALR-130](https://sdlc.review.fao.org/jira/browse/CALR-130) Add dropdown list for filter individuals based on fishery roles

_Individual Quality Assessment_

* [CALR-134](https://sdlc.review.fao.org/jira/browse/CALR-134) Add quality information for gender and educational level

_Logbook details_

* [CALR-136](https://sdlc.review.fao.org/jira/browse/CALR-136) Force to organize years and connect dots in chronological order on plot 

_Vessel list_

* [CALR-119](https://sdlc.review.fao.org/jira/browse/CALR-119) Vessel-list enable vessel list table combine with license permit table with no data

_Vessel details_

* [CALR-123](https://sdlc.review.fao.org/jira/browse/CALR-123) Vessel-info make an expection for Vessel Operational Status for dominica

## New features

**Core features**

* [CALR-141](https://sdlc.review.fao.org/jira/browse/CALR-141) New strategy to catch and dissiminate currency preference use to store data 

* [CALR-140](https://sdlc.review.fao.org/jira/browse/CALR-140) New strategy to catch and dissiminate weight unit preference use to store data 

**Module features**

* [CALR-120](https://sdlc.review.fao.org/jira/browse/CALR-120) New module for individuals QA
* [CALR-121](https://sdlc.review.fao.org/jira/browse/CALR-121) New module for individuals breakdown
* [CALR-124](https://sdlc.review.fao.org/jira/browse/CALR-124) Individual breakdown: Breaking down educational level by gender
* [CALR-125](https://sdlc.review.fao.org/jira/browse/CALR-125) New module for individuals details
* [CALR-132](https://sdlc.review.fao.org/jira/browse/CALR-132) New module for individuals overview

# **shiny-calipseo [v1.1.3](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.1.3.zip) - 2022-07-13**

## Corrections

**Module corrections**

_Artfish method (summary by fishing units)_

* [CALR-117](https://sdlc.review.fao.org/jira/browse/CALR-117) bugs in calculation break the "By fishing unit' module  


# **shiny-calipseo [v1.1.2](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.1.2.zip) - 2022-06-24**

## Corrections

**Module corrections**

_Logbook validation (Suriname)_

* [CALR-110](https://sdlc.review.fao.org/jira/browse/CALR-110) Logbooks validation / SQL - add inclusion of fishery products - **Fix regression with SQL**

# **shiny-calipseo [v1.1.1](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.1.1.zip) - 2022-06-22**

## Enhancements

**Module enhancements**

_Fishing trips_

* [CALR-115](https://sdlc.review.fao.org/jira/browse/CALR-115) Fishing trips - detail by vessel - need to round (2 digits) quantities

_Logbook validation (Suriname)_

* [CALR-110](https://sdlc.review.fao.org/jira/browse/CALR-110) Logbooks validation / SQL - add inclusion of fishery products
* [CALR-116](https://sdlc.review.fao.org/jira/browse/CALR-116) Logbook validation (Suriname) - implement RD-DD+1 rule for fishing trip duration

## Corrections

**Module corrections**

_Logbook overview_

* [CALR-111](https://sdlc.review.fao.org/jira/browse/CALR-111) Logbooks overview - CI intervals with normal and student dists

_Logbook details_

* [CALR-113](https://sdlc.review.fao.org/jira/browse/CALR-113) Logbooks - details - download buttons do not work in "By landing site" tab tables


# **shiny-calipseo [v1.1.0](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.1.0.zip) - 2022-05-12**

## New features

## Enhancements

**Module enhancements**

_Vessels - list / details_


* [CALR-96](https://sdlc.review.fao.org/jira/browse/CALR-96) Adopting a function to determine vessel license validity across vessel list module and vessel info module
* [CALR-97](https://sdlc.review.fao.org/jira/browse/CALR-97) Refractoring Vessel list module-moving from using column indexes to column names
* [CALR-98](https://sdlc.review.fao.org/jira/browse/CALR-98) Using the gicon calipseo function to handle glyphicon icons in vessel list module
* [CALR-99](https://sdlc.review.fao.org/jira/browse/CALR-99) Refractoring Vessel info module-moving from using column indexes to column names

_Logbook validation (Suriname)_

* [CALR-107](https://sdlc.review.fao.org/jira/browse/CALR-107) Rename module for more clarity on business processes (validation, SQL generation) vs. upload
* [CALR-108](https://sdlc.review.fao.org/jira/browse/CALR-108) Increase validation rules for Logbook upload modules

## Corrections

**Core corrections**

* [CALR-67](https://sdlc.review.fao.org/jira/browse/CALR-67) Routes do not work systematically
* [CALR-100](https://sdlc.review.fao.org/jira/browse/CALR-100) vessel license permits data not accurate

** Module corrections**

_Individuals -list_

* [CALR-91](https://sdlc.review.fao.org/jira/browse/CALR-91) individuals list - Wrong SQL join with reg_entities

_Vessels - details (info)_

* [CALR-92](https://sdlc.review.fao.org/jira/browse/CALR-92) Vessel-info-module Number-of-license-infobox-display-more-counts
* [CALR-93](https://sdlc.review.fao.org/jira/browse/CALR-93) Vessel-info-module Removing extra column name from dataframe
* [CALR-94](https://sdlc.review.fao.org/jira/browse/CALR-94) Vessel-info-module Vessel Operational Status infobox throws error when invalide registration number is input
* [CALR-95](https://sdlc.review.fao.org/jira/browse/CALR-95) Vessel-info-module vessel characteristics tab throws error when invalide registration number is input
* [CALR-101](https://sdlc.review.fao.org/jira/browse/CALR-101) VesselFindeR returns unstandard data without image link for some vessels

**Miscs**

* [CALR-106](https://sdlc.review.fao.org/jira/browse/CALR-106) Fix logbook SQL generation function for Suriname logbook data

# **shiny-calipseo  [v1.0.0](https://bitbucket.org/cioapps/fao-calipseo-stats/get/v1.0.0.zip) - 2022-04-22**

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

**Core corrections**

* [CALR-9](https://sdlc.review.fao.org/jira/browse/CALR-) Fix hrefs to shiny module relative paths
* [CALR-18](https://sdlc.review.fao.org/jira/browse/CALR-) Add missing URL params for modules without paging

**Module corrections**

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


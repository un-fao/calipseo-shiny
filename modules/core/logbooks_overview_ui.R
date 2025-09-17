#logbooks_overview_ui
logbooks_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "logbooks_overview",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("logbooks_overview_info"))
            )
          ),
          uiOutput(ns("nb_infos")),
          fluidRow(
            div(
              class = "col-md-12",
              shinyWidgets::verticalTabsetPanel(
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_GLOBAL_QUANTITY"),box_height='70px', line_chart_ui(ns("gq"),sliderWidth =25)),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_GLOBAL_VESSEL"),box_height='70px', 
                                                 box(
                                                   title="",
                                                   width = 12,
                                                   sidebar = shinydashboardPlus::boxSidebar(
                                                     id=ns("gv_box"),
                                                     width = 25,
                                                     style = 'font-size:14px;',
                                                     selectInput(ns("gv_granu"),label = paste0(i18n("TEMPORAL_RESOLUTION")," :"), choices = c(i18n("YEARLY"),i18n("MONTHLY"),i18n("WEEKLY")))
                                                     ),
                                                   uiOutput(ns("gv_result"))
                                                 )
                                               ),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_BREAKDOWN_BY_VESSEL_TYPES"),box_height='70px', line_chart_ui(ns("vt"),sliderWidth =25)),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_BREAKDOWN_BY_GEAR_TYPES"),box_height='70px' ,line_chart_ui(ns("gt"),sliderWidth =25)),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_BREAKDOWN_BY_SPECIES"),box_height='70px',
                                               tabsetPanel(type="pills",
                                                 tabPanel(i18n("TABPANEL_BREAKDOWN_BY_SPECIES"),line_chart_ui(ns("sp"),sliderWidth =25)),
                                                 tabPanel(i18n("TABPANEL_BREAKDOWN_BY_SPECIES_GROUPS"),line_chart_ui(ns("fg"),sliderWidth =25))
                                                 )),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_BREAKDOWN_BY_LANDING_SITES"),box_height='70px',contentWidth=11 ,line_chart_ui(ns("ls"),sliderWidth =25)),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_BREAKDOWN_BY_FISHING_ZONE"),box_height='70px',contentWidth=11 ,line_chart_ui(ns("fz"),sliderWidth =25))
              )
            )
          )
)
  
}
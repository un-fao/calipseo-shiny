#vessel_qa_ui
vessel_qa_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "vessel_qa",
          fluidRow(
            column(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("vessel_qa_info"))
            )
          ),
          
          bs4Dash::tabsetPanel(
            vertical = TRUE,
            type = "pills",
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_VESSEL_QA_NAMES")), box_height = '70px',
              bs4Dash::box(
                width = 12, height = 480, 
                title = i18n("VESSEL_QA_TITLE_NAMES"),
                DT::dataTableOutput(ns("vessel_names")),
                collapsible = FALSE
              )
            ) ,
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_VESSEL_QA_VOP_STATUS")), box_height = '70px',
              bs4Dash::box(
                width = 12, height = 480, 
                title = i18n("VESSEL_QA_TITLE_VOP_STATUS"),
                DT::dataTableOutput(ns("vessel_operational_status")),
                collapsible = FALSE
              )
            ),
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_VESSEL_QA_PORTS")), box_height = '70px',
              bs4Dash::box(
                width = 12, height = 480, 
                title = i18n("VESSEL_QA_TITLE_PORTS"), 
                DT::dataTableOutput(ns("vessel_ports")),
                collapsible = FALSE
              ),
              sliderWidth = 25
            ),
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_VESSEL_QA_CHARACTERISTICS")), box_height = '70px',
              bs4Dash::box(
                width = 12, height = 480,
                title = i18n("VESSEL_QA_TITLE_CHARACTERISTICS"),
                DT::dataTableOutput(ns("vessel_characteristics")),
                collapsible = FALSE
              )
            ),
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_VESSEL_QA_LICENSE")), box_height = '70px',
              bs4Dash::box(
                width = 12, height = 480, 
                title = i18n("VESSEL_QA_TITLE_LICENSE"),
                DT::dataTableOutput(ns("vessel_license")),
                collapsible = FALSE
              )
            ),
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_VESSEL_QA_ACTIVITY")), box_height='70px',
              bs4Dash::box(
                width = 12, height = 480, 
                title = i18n("VESSEL_QA_TITLE_ACTIVITY"),
                DT::dataTableOutput(ns("vessel_activity")),
                collapsible = FALSE
              )
            )
          )
  )
  
}


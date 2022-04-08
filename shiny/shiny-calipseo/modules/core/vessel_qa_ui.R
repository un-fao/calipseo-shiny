#vessel_qa_ui
vessel_qa_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_qa",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("vessel_qa_info"))
            )
          ),
          
          div(class='row',style = "margin:0px;", id='vertical-panels-vessel-qa',
              shinyWidgets::verticalTabsetPanel(
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_VESSEL_QA_NAMES"),box_height='70px',box(width = 12, height = 480, title = i18n("VESSEL_QA_TITLE_NAMES"),DT::dataTableOutput(ns("vessel_names")))) ,
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_VESSEL_QA_VOP_STATUS"),box_height='70px',box(width = 12, height = 480, title = i18n("VESSEL_QA_TITLE_VOP_STATUS"),DT::dataTableOutput(ns("vessel_operational_status")))),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_VESSEL_QA_PORTS"),box_height='70px' ,box(width = 12, height = 480, title = i18n("VESSEL_QA_TITLE_PORTS"), DT::dataTableOutput(ns("vessel_ports"))),sliderWidth =25),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_VESSEL_QA_CHARACTERISTICS"),box_height='70px',box(width = 12, height = 480, title = i18n("VESSEL_QA_TITLE_CHARACTERISTICS"),DT::dataTableOutput(ns("vessel_characteristics")))),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_VESSEL_QA_LICENSE"),box_height='70px',box(width = 12, height = 480, title = i18n("VESSEL_QA_TITLE_LICENSE"),DT::dataTableOutput(ns("vessel_license"))))))
  )
  
}


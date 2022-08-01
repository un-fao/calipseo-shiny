#individual_qa_ui
individual_qa_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "individual_qa",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("individual_qa_info"))
            )
          ),
          
          div(class='row',style = "margin:0px;", id='vertical-panels-individual-qa',
              shinyWidgets::verticalTabsetPanel(
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_INDIVIDUAL_QA_DOB"),box_height='70px',box(width = 12, height = 480, title = i18n("INDIVIDUAL_QA_TITLE_DOB"),DT::dataTableOutput(ns("individual_dob"))),sliderWidth =25)
                
                ))
  )
  
}

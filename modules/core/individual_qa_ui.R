#individual_qa_ui
individual_qa_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "individual_qa",
          fluidRow(
            column(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("individual_qa_info"))
            )
          ),
          
          bs4Dash::tabsetPanel(
            vertical = TRUE,
            type = "pills",
            tabPanel(
              title = tags$h5(i18n("VERTICALTABPANEL_INDIVIDUAL_QA_DOB")), box_height='70px',
              bs4Dash::box(
                width = 12, height = 480, 
                title = i18n("INDIVIDUAL_QA_TITLE_DOB"),
                DT::dataTableOutput(ns("individual_dob"))
              ),
              sliderWidth =25
            )
          )
  )
  
}

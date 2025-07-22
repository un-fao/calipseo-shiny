#artisanal_validation_ui
artisanal_validation_ui <- function(id){
  
  # ui_load_count <<- ui_load_count + 1
  # print(sprintf("UI loaded %s time(s)", ui_load_count))
  
  ns <- NS(id)
  
  tabItem(
    tabName = "artisanal_validation",
    useShinyjs(),
     tags$head(
       tags$style(
         HTML(".shiny-notification {
               height: 100px;
               width: 280px;
               position:fixed;
               top: calc(50% - 50px);;
               left: calc(50% - 140px);;
             }
            "
         )
       )
     ),
    tags$h2(i18n("LOGBOOKS_VALIDATION_TITLE")),
    fluidRow(
      column(3,uiOutput(ns("file_input_wrapper"))),     
      column(2,style = "margin-top: 25px;",
             uiOutput(ns("validity_btn")))
             #actionButton(ns("check_validity"), "Run validation"))
    ),
    fluidRow(
      uiOutput(ns("validity_result"))
    ),
    fluidRow(
      uiOutput(ns("generate_SQL_btn"))
    ),
    fluidRow(
      uiOutput(ns("generate_report_btn"))
    )
  )
}
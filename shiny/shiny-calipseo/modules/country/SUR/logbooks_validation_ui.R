#logbooks_validation_ui
logbooks_validation_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(
    tabName = "logbooks_validation",
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
      column(3,
      fileInput(inputId = ns("file_to_validate"), label = paste0(i18n("LOGBOOK_VALIDATION_FILEINPUT_LABEL_TITLE"),":"),multiple = FALSE,accept = c(".xlsx"))),
      column(2,style = "margin-top: 25px;",uiOutput(ns("validity_btn")))
    ),
    fluidRow(
      uiOutput(ns("validity_result"))
    ),
    fluidRow(
      uiOutput(ns("generate_SQL_btn"))
    )
  )
}
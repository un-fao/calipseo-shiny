#logbooks_upload_ui
logbooks_upload_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(
    tabName = "logbooks_upload",
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
    tags$h2(i18n("LOGBOOKS_UPLOAD_TITLE")),
    fluidRow(
      column(3,
             fileInput(inputId = ns("file_to_upload"), label = paste0(i18n("LOGBOOK_UPLOAD_FILEINPUT_LABEL_TITLE"),":"),multiple = FALSE,accept = c(".xlsx"))),
      column(2,style = "margin-top: 25px;",uiOutput(ns("validity_btn")))
    ),
    fluidRow(
      uiOutput(ns("validity_result"))
    ),
    fluidRow(
      uiOutput(ns("generate_SQL_btn"))
    ),
    uiOutput(ns("upload_error_message"))
  )
}
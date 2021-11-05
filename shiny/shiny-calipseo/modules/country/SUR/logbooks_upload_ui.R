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
    h2("Upload Logbooks"),
    fluidRow(
      column(3,
      fileInput(inputId = ns("file_to_upload"), label = "Input data logbooks:",multiple = FALSE,accept = c(".xlsx"))),
      column(3,uiOutput(ns("validity_btn")))
    ),
    fluidRow(
      uiOutput(ns("validity_result"))
    ),
    fluidRow(
      uiOutput(ns("generate_SQL_btn"))
    )
  )
}
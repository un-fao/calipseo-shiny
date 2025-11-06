#observor_biological_ui
observor_biological_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "observor_biological",
          tags$h2(i18n("TITLE")),
          fluidRow(
            uiOutput(ns("species_selector"))
          ),
          fluidRow(
              plotlyOutput(ns("correlationPlot"))
          )
  )
  
}
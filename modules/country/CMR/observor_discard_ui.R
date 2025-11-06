#observor_discard_ui
observor_discard_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "observor_discard",
          tags$h2(i18n("TITLE")),
          fluidRow(
            uiOutput(ns("species_selector"))
          )
  )
  
}
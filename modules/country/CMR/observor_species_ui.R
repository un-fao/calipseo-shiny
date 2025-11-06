#observor_species_ui
observor_species_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "observor_species",
          tags$h2(i18n("TITLE")),
          fluidRow(
            uiOutput(ns("species_selector"))
          )
  )
  
}
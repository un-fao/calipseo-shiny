#artfish_explorer_species_ui
artfish_explorer_species_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "artfish_explorer_species",
    tagList(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('update_progress_label', function(data) {
          $('#progress_percent').text(data.percent);
          $('#progress_label').text(data.text);
        });"
      )),
      artfishr::artfish_shiny_species_ui(ns("artfish_explorer_species"))
    )
  )
  
}
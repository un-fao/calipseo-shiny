#artfish_fishing_unit_explorer_ui
artfish_fishing_unit_explorer_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "artfish_fishing_unit_explorer",
    tagList(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('update_progress_label', function(data) {
          $('#progress_percent').text(data.percent);
          $('#progress_label').text(data.text);
        });"
      )),
        artfishr::artfish_shiny_fishing_unit_ui(ns("artfish_fishing_unit_explorer"))
    )
  )
}
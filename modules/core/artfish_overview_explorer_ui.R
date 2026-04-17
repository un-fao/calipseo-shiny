#artfish_overview_explorer_ui
artfish_overview_explorer_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "artfish_overview_explorer",
    tagList(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('update_progress_label', function(data) {
          $('#progress_percent').text(data.percent);
          $('#progress_label').text(data.text);
        });"
      )),
      artfishr::artfish_shiny_overview_ui(ns("artfish_overview_explorer"))
    )
  )
}

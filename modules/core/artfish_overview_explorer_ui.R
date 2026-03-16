#artfish_overview_explorer_ui
artfish_overview_explorer_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "artfish_overview_explorer",
    tagList(
      waiter::useWaiter(),
      waiter::useHostess(),
      artfishr::artfish_shiny_overview_ui(ns("artfish_overview_explorer"))
    )
  )
}

#artfish_fishing_unit_ui
artfish_fishing_unit_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "artfish_fishing_unit",
                   fluidRow(
                     div(
                       width = 12, style = "margin:12px;",
                       tags$h2(i18n("ARTFISH_FISHING_UNIT_TITLE")),tags$h3(class = "text-muted", i18n("ARTFISH_FISHING_UNIT_SUBTITLE"))
                     )
                   ),
                   uiOutput(ns("no_release")),
                   uiOutput(ns("filter_selectors")),
                   uiOutput(ns("indicators")),
                   uiOutput(ns("results"))
          
  )
  
  
}
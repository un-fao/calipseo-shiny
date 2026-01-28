#artfish_overview_ui
artfish_overview_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "artfish_overview",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              
              tags$h2(i18n("ARTFISH_OVERVIEW_TITLE")),tags$h3(class = "text-muted", i18n("ARTFISH_OVERVIEW_SUBTITLE"))
            )
          ),
          uiOutput(ns("no_release")),
          uiOutput(ns("filter_selectors")),
          uiOutput(ns("indicators")),
          uiOutput(ns("results"))
  )
}
         
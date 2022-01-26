#artfish_fishing_unit_ui
artfish_fishing_unit_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "artfish_fishing_unit",
          uiOutput(ns("urlPage")),
          fluidRow(
            tags$h2("Detail by Fishing Unit")
          ),
          fluidRow(
            column(6,uiOutput(ns("selectors"))),
            column(3,uiOutput(ns("value"))),
            #column(3,uiOutput(ns("accuracy")))
            column(3,plotlyOutput(ns("accuracy2"),height=200))
          ),
          uiOutput(ns("bg_results")),
          uiOutput(ns("sp_results"))
  
  )
          

}
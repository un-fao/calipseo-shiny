#artfish_species_ui
artfish_species_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "artfish_species",
          uiOutput(ns("urlPage")),
            div(class="row",
            column(3,
              tags$h2("Detail by Species"),
              uiOutput(ns("species_selector")),
              uiOutput(ns("fishing_unit_selector"))
            ),
            column(5,
                   plotlyOutput(ns("donut"), width = "auto", height = "auto")
            ),
            column(4,
                   plotlyOutput(ns("rank"), width = "auto", height = "auto")
            )
            ,height=200),
          uiOutput(ns("indicators")),
          fluidRow(plotlyOutput(ns("timeline")))
          )
  
}
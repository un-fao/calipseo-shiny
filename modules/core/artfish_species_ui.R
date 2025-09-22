#artfish_species_ui
artfish_species_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "artfish_species",
            div(class="row",
            column(3,
              tags$h3(i18n("ARTFISH_SPECIES_TITLE")),
              uiOutput(ns("no_release")),
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
#artfish_species_ui
artfish_species_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "artfish_species",
            fluidRow(
              div(
                width = 12, style = "margin:12px;",
                tags$h2(i18n("ARTFISH_SPECIES_TITLE")),tags$h3(class = "text-muted", i18n("ARTFISH_SPECIES_SUBTITLE"))
              )
            ),            
            div(class="row",
            column(3,
              uiOutput(ns("no_release")),
              uiOutput(ns("species_selector")),
              uiOutput(ns("time_selector")),
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
          uiOutput(ns("results")),
  )
  
}
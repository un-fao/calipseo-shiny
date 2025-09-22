#landings1_charts_ui
landings1_charts_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "landings1_charts",
    fluidRow(
      bs4Dash::box(
        width = 6,
        htmlOutput(ns("landings1_charts_info")),
        collapsible = FALSE
      ),
      bs4Dash::box(width = 6,
          selectizeInput(ns("bch_name"), label = i18n("LANDING_SITE_LABEL"), choices = accessLandingSiteNames(pool),
                         options = list(placeholder = i18n("LANDING_SITE_PLACEHOLDER_LABEL"),
                                        onInitialize = I('function() { this.setValue(""); }'))),
          collapsible = FALSE
      )
    ),
    fluidRow(
      bs4Dash::box(width = 4, height = 345, title = "LAN", status = "primary", solidHeader= TRUE, plotlyOutput(ns("plot_LAN")), collapsible = FALSE),
      bs4Dash::box(width = 4, height = 345, title = "VAL", status = "primary", solidHeader= TRUE, plotlyOutput(ns("plot_VAL")), collapsible = FALSE),
      bs4Dash::box(width = 4, height = 345, title = "TRP", status = "primary", solidHeader= TRUE, plotlyOutput(ns("plot_TRP")), collapsible = FALSE)
    ),
    fluidRow(
      bs4Dash::box(width = 4, height = 345, title = "L/T", status = "primary", solidHeader= TRUE, plotlyOutput(ns("plot_LT")), collapsible = FALSE),
      bs4Dash::box(width = 4, height = 345, title = "V/T", status = "primary", solidHeader= TRUE, plotlyOutput(ns("plot_VT")), collapsible = FALSE),
      bs4Dash::box(width = 4, height = 345, title = "P/K", status = "primary", solidHeader= TRUE, plotlyOutput(ns("plot_PK")), collapsible = FALSE)
    )
  )
  
}
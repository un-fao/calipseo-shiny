#computation_ui
computation_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "computation",
    fluidRow(
      column(
        width = 6,
        htmlOutput(ns("computation_info"))
      )
    ),
    fluidRow(
      style="display:flex;",
      box(width = 6, title = i18n("LABEL_BOX_INDICATOR"),
          uiOutput(ns("computation_by"))
      ),
      box(width = 6, title = i18n("LABEL_BOX_PLOT"),
          #maximizable = T,
          uiOutput(ns("plot_wrapper"))
      )
    ),
    uiOutput(ns("computation_summary"))
  )
}
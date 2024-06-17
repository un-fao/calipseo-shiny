#computation2_ui
computation2_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(
    tabName = "computation2",
    fluidRow(
      div(
        width = 6, style = "margin:12px;",
        htmlOutput(ns("computation_info"))
      )
    ),
    fluidRow(
      style="display:flex;",
      box(width = 6, title = i18n("LABEL_BOX_INDICATOR"),
          uiOutput(ns("computation_by"))
      ),
      box(width = 6, title = i18n("LABEL_BOX_PLOT"),
          uiOutput(ns("plot_wrapper"))
      )
    ),
    uiOutput(ns("computation_summary"))
  )
}
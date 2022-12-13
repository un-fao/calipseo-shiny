#computation_ui
computation_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(
    tabName = "computation",
    fluidRow(
      div(
        width = 6, style = "margin:12px;",
        htmlOutput(ns("computation_info"))
      ),
      box(width = 6,
          selectizeInput(
            ns("computation_indicator"), label = i18n("COMPUTATION_INDICATOR_LABEL"), 
            choices = sapply(getLocalCountryDataset("statistical_indicators"), function(x){x$label}), selected = NULL,
            options = list(
              placeholder = i18n("COMPUTATION_INDICATOR_PLACEHOLDER_LABEL"),
              render = I('{
                option: function(item, escape) {
                  return "<div><strong>" + escape(item.label) + "</strong>"
                }
              }')
            )
          ),
          uiOutput(ns("computation_by")),
          actionButton(ns("computeButton"), label = i18n("COMPUTATION_ACTIONBUTTON_LABEL"), class = "btn-primary"),
          uiOutput(ns("releaseInfoShortcut"))
      ),
      box(width = 6,
          p(tags$b(i18n("COMPUTATION_STATUS_LABEL"))), hr(),
          DT::dataTableOutput(ns("computation_summary"))   
      )
    ),
    fluidRow(
      box(width = 12, uiOutput(ns("results")))
    )
  )
}
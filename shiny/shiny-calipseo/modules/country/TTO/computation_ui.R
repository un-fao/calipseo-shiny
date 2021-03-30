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
            ns("computation_indicator"), label = "Statistical indicator", 
            choices = sapply(getLocalCountryDataset("statistical_indicators"), function(x){x$label}), selected = NULL,
            options = list(
              placeholder = 'Select an indicator',
              render = I('{
                option: function(item, escape) {
                  return "<div><strong>" + escape(item.label) + "</strong>"
                }
              }')
            )
          ),
          selectizeInput(
            ns("computation_year"), label = "Year", 
            choices = accessAvailableYears(pool), selected = NULL, 
            options = list(placeholder = 'Select a year')),
          actionButton(ns("computeButton"), label = "Compute", class = "btn-primary"),
          uiOutput(ns("releaseInfoShortcut"))
      ),
      box(width = 6,
          p(tags$b("Computation status")), hr(),
          DT::dataTableOutput(ns("computation_summary"))   
      )
    ),
    fluidRow(
      box(width = 12, uiOutput(ns("results")))
    )
  )
}
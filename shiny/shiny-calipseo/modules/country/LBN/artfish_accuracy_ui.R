#artfish_ui
artfish_accuracy_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "artfish_accuracy",
          uiOutput(ns("urlPage")),
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              
              tags$h2("Estimate accuracy and uniformity of sampling strategy"),
            )
          ),
          tabsetPanel(
            tabPanel("Accuracy",
              fluidRow(
                div(
                  class = "col-md-4",
                  box(id=ns("global_box"),title="Global",width = 12,
                    numericInputIcon(ns("boats"),"Number of boats",value=NULL,icon=icon("ship")),
                    numericInputIcon(ns("days"),"Number of fishing days",value=NULL,icon=icon("calendar"))
                  )
                ),
                div(
                  class = "col-md-4",
                  box(id=ns("effort_box"),title="Effort",width = 12,
                    numericInputIcon(ns("effort_smp"),"Number of effort samples",value=NULL,icon=icon("ship")),
                    numericInputIcon(ns("effort_days_smp"),"Number of days sampled (effort)",value=NULL,icon=icon("calendar"))
                  )
                ),
                div(
                  class = "col-md-4",
                  box(id=ns("landing_box"),title="Landing",width = 12,
                    numericInputIcon(ns("landing_smp"),"Number of landing samples",value=NULL,icon=icon("ship")),
                    numericInputIcon(ns("landing_days_smp"),"Number of days sampled (landing)",value=NULL,icon=icon("calendar"))
                  )
                )
              ),
              fluidRow(uiOutput(ns("button"))),
              br(),
              uiOutput(ns("result"))
            ),
            tabPanel("Uniformity",
              fluidRow(
                p("Click on 'New Column' to add a day and double click on cell to edit"),
                actionButton(ns("addColumn"), "New Column"),
                DTOutput(ns("table")),
                actionButton(ns("compute"), "Compute")
              ),
              fluidRow(uiOutput(ns("index")))
            )
          )
          
  )
  
}
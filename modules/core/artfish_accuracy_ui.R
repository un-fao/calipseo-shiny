#artfish_ui
artfish_accuracy_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "artfish_accuracy",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              
              tags$h2(i18n("ARTFISH_ACCURACY_TITLE")),
            )
          ),
          tabsetPanel(
            tabPanel(i18n("TABPANEL_ACCURACY"),
              fluidRow(
                div(
                  class = "col-md-4",
                  box(id=ns("global_box"),title=i18n("GLOBAL_BOX_TITLE"),width = 12,
                    numericInputIcon(ns("boats"),i18n("NUMERIC_INPUT_NUMBER_OF_BOATS_TITLE"),value=NULL,icon=icon("ship")),
                    numericInputIcon(ns("days"),i18n("NUMERIC_INPUT_NUMBER_OF_FISHING_DAYS_TITLE"),value=NULL,icon=icon("calendar"))
                  )
                ),
                div(
                  class = "col-md-4",
                  box(id=ns("effort_box"),title=i18n("EFFORT_BOX_TITLE"),width = 12,
                    numericInputIcon(ns("effort_smp"),i18n("NUMERIC_INPUT_NUMBER_OF_EFFORT_SAMPLES_TITLE"),value=NULL,icon=icon("ship")),
                    numericInputIcon(ns("effort_days_smp"),i18n("NUMERIC_INPUT_NUMBER_OF_DAYS_SAMPLED_EFFORTS_TITLE"),value=NULL,icon=icon("calendar"))
                  )
                ),
                div(
                  class = "col-md-4",
                  box(id=ns("landing_box"),title=i18n("LANDING_BOX_TITLE"),width = 12,
                    numericInputIcon(ns("landing_smp"),i18n("NUMERIC_INPUT_NUMBER_OF_LANDING_SAMPLES_TITLE"),value=NULL,icon=icon("ship")),
                    numericInputIcon(ns("landing_days_smp"),i18n("NUMERIC_INPUT_NUMBER_OF_DAYS_SAMPLED_LANDING_TITLE"),value=NULL,icon=icon("calendar"))
                  )
                )
              ),
              fluidRow(uiOutput(ns("button"))),
              br(),
              uiOutput(ns("result"))
            ),
            tabPanel(i18n("TABPANEL_UNIFORMITY"),
              fluidRow(
                p(i18n("UNIFORMITY_HINT")),
                actionButton(ns("addColumn"), i18n("ACTIONBUTTON_ACTIVATE_NEWCOLUMN_LABEL")),
                DTOutput(ns("table")),
                actionButton(ns("compute"), i18n("ACTIONBUTTON_COMPUTE_LABEL"))
              ),
              fluidRow(uiOutput(ns("index")))
            )
          )
          
  )
  
}
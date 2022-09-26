#individual_list_ui
individual_list_ui <- function(id){
  
  ns <- NS(id)
  
  Owner <- as.character(i18n("INDIVIDUAL_LABEL_OWNER"))
  Captain <- as.character(i18n("INDIVIDUAL_LABEL_CAPTAIN"))
  `Holder of fishing Id` <- as.character(i18n("INDIVIDUAL_LABEL_HOLDER_FISHING_ID"))
  `Holder of fishing License` <- as.character(i18n("INDIVIDUAL_LABEL_HOLDER_FISHING_LICENSE"))
  
  
  
  tabItem(tabName = "individual_list",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("individual_list_info"))
            )
          ),
          
          fluidRow(
            
            box(width = 12,
                div(selectInput(ns("ctry_display_cntrl"),label="", choices = appConfig[["country_profile"]]$data$CODE),style='display:none;'),
                conditionalPanel(
                  condition = "input.ctry_display_cntrl =='DM'",ns = ns,
                  h4(i18n("INDIVIDUAL_TYPE")),
                  span(checkboxInput(ns("fisher_chck"), label = i18n("INDIVIDUAL_BREAKDOWN_TITLE_FISHER")),checkboxInput(ns("nonfisher_chck"), label = i18n("INDIVIDUAL_BREAKDOWN_TITLE_NONFISHER"))),
                  conditionalPanel(
                    condition = "input.fisher_chck == 1",ns = ns,
                    selectizeInput(ns("filter_fisher_category_list"),label = i18n("SELECT_FISHER_CATEGORY"),choices = c(Owner,Captain,`Holder of fishing Id`,`Holder of fishing License`),multiple = FALSE, width = '40%'))),
                withSpinner(DT::dataTableOutput(ns("individual_list"))))
            
            
          )
  )
  
}


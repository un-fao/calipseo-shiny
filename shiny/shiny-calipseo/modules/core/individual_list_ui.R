#individual_list_ui
individual_list_ui <- function(id){
  
  ns <- NS(id)
  
  
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
                  selectizeInput(ns("filter_individual_roles"),label = i18n("SELECT_INDIVIDUAL_ROLES"),choices =NULL,options = list(placeholder='All'),multiple = TRUE, width = '30%')),
                withSpinner(DT::dataTableOutput(ns("individual_list"))))
            
            
          )
  )
  
}


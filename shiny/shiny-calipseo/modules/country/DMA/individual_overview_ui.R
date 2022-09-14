#individual_overview_ui
individual_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "individual_overview",
            div(class = 'row',style = "margin:12px;",htmlOutput(ns("individual_overview_info"))),
    div(uiOutput(ns('descrip_title')),br(),
    uiOutput(ns('fisher_dash')),
    uiOutput(ns('nonfisher_dash'))),
    div(class = 'row',box(width = 12,title = i18n("INDIVIDUAL_OVERVIEW_TITLE_INDIVIDUAL_DETAIL"), 
                          withSpinner(DT::dataTableOutput(ns("category_fishery_dt"))))),br(),
  div(class = 'row',div(class = 'col-md-6',box(width = 12,title = i18n("INDIVIDUAL_OVERVIEW_TITLE_FISHER"),
                        plotlyOutput(ns("fisher_age_gender")))),
      div(class = 'col-md-6',box(width = 12,title =i18n("INDIVIDUAL_OVERVIEW_TITLE_NONFISHER"),
          plotlyOutput(ns("non_fisher_age_gender")))))
          
  )
  
}
#logbooks_overview_ui
logbooks_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "logbooks_overview",
          uiOutput(ns("urlPage")),
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("logbooks_overview_info"))
            )
          ),
          uiOutput(ns("nb_infos")),
          fluidRow(
            div(
              class = "col-md-6",
              h3(sprintf("Statistics (kg) by vessel type - %s", as.integer(format(Sys.Date(), "%Y"))-1)), hr(),
              box(
                width = 12,
                DT::dataTableOutput(ns("stats_by_type_lastyear_table"))
              )
            ),
            div(
              class = "col-md-6",
              h3(sprintf("Statistics (kg) by vessel type - %s", format(Sys.Date(), "%Y"))), hr(),
              box(
                width = 12,
                DT::dataTableOutput(ns("stats_by_type_currentyear_table"))
              )
            )
          ),
          fluidRow(
            div(
              class = "col-md-6",
              h3(sprintf("Overview of quantity catched by vessel type")), hr(),
              box(
                title="",
                width = 12,
                sidebar = shinydashboardPlus::boxSidebar(
                  id=ns("vt_sidebar"),
                  width = 25,
                  style = 'font-size:14px;',
                selectInput(ns("vt_granu"),"Temporal resolution :",choices=c("Yearly"="%Y","Monthly"="%Y-%m","Weekly"="%Y-%U")),
                selectInput(ns("vt_stat"),"Statistic :",choices=c("Total"="sum","Average"="mean","Median"="median")),
                uiOutput(ns("vt_additional"))),
                plotlyOutput(ns("vt_plot")))
                
              ),
            div(
              class = "col-md-6",
              h3(sprintf("Overview of quantity catched by species"), hr(),
                 box(
                   title="",
                   width = 12,
                   sidebar = shinydashboardPlus::boxSidebar(
                     id=ns("sp_sidebar"),
                     width = 30,
                     style = 'font-size:14px;',
                     uiOutput(ns("sp_nb_selector")),
                     selectInput(ns("sp_rank_method"),"Rank method :",choices=c("Total catch over the period"="sum","Last year total catch"="last_year","Annual catch average"="year_avg")),
                     selectInput(ns("sp_granu"),"Temporal resolution :",choices=c("Yearly"="%Y","Monthly"="%Y-%m","Weekly"="%Y-%U")),
                     selectInput(ns("sp_stat"),"Statistic :",choices=c("Total"="sum","Average"="mean","Median"="median")),
                     uiOutput(ns("sp_additional"))),
                   plotlyOutput(ns("sp_plot")))
              )
            )
          ),
        fluidRow(
          div(
            class = "col-md-6",
            h3(sprintf("Overview of quantity catched by fish group")), hr(),
              box(
                title="",
                width = 12,
                sidebar = shinydashboardPlus::boxSidebar(
                  id=ns("fg_sidebar"),
                  width = 25,
                  style = 'font-size:14px;',
                  selectInput(ns("fg_granu"),"Temporal resolution :",choices=c("Yearly"="%Y","Monthly"="%Y-%m","Weekly"="%Y-%U")),
                  selectInput(ns("fg_stat"),"Statistic :",choices=c("Total"="sum","Average"="mean","Median"="median")),
                  uiOutput(ns("fg_additional"))),
                plotlyOutput(ns("fg_plot")))
            )
        )
          
  )
  
}
#logbooks_trips_ui
logbooks_trips_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "logbooks_trips",
          uiOutput(ns("urlPage")),
          box(
            title="",
            width = 12,
            sidebar = shinydashboardPlus::boxSidebar(
              id=ns("box"),
              width = 25,
              startOpen = T,
              style = 'font-size:14px;',
              uiOutput(ns("selector"))
            ),
            fluidRow(
              column(3,offset=1,uiOutput(ns("page_selector"))),
              column(8,p("click on trip to see more informations"))
            ),
            plotlyOutput(ns("gantt"))%>%withSpinner(type = 4)
          )
  )

}
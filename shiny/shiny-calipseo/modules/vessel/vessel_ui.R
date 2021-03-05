#vesselListUI
vesselListUI <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_list",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("vessel_list_info"))
            )
          ),
          fluidRow(
            box(width = 12, DT::dataTableOutput(ns("vessel_list")))
          )
  )
  
}

#vesselRepartition
vesselRepartitionUI <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_repartition",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("vessel_repartition_info"))
            )
          ),
          fluidRow(
            box(width = 6, height = 540, title = "Repartition of vessels in Trinidad per vessel type", status = "primary", solidHeader= TRUE, plotlyOutput(ns("rep_vessels"))),
            box(width = 6, height = 540, title = "Repartition of vessels in Trinidad per home port", status = "primary", solidHeader= TRUE, leafletOutput(ns("map_vessels"), height = 490))
          )
  )
  
}

#vesselInfoUI
vesselInfoUI <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_info",
          
       fluidRow(
         div(
           width = 12, style = "margin:12px;",
           htmlOutput(ns("vessel_header")),
           uiOutput(ns("vessel_description"))
         )
       ),
       fluidRow(
         div(
           class = "col-md-5",
           h3("Ownership"), hr(),
           box(
             width = 12,
             DT::dataTableOutput(ns("vessel_owners"))
           ),
           h3("Licenses"), hr(),
           box(
             width = 12
           ),
           h3("Catch summary"), hr(),
           box(
             width = 12,
             DT::dataTableOutput(ns("vessel_catch_summary"))
           )
         ),
         div(
           class = "col-md-7",
           h3("Catch history"),hr(),
           DT::dataTableOutput(ns("vessel_catch_history"))
         )
       )
  )
  
}
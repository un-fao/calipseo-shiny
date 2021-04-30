#logbooks_details_ui
logbooks_details_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "logbooks_details",
    fluidRow(
      div(
        width = 12, style = "margin:12px;",
        tabsetPanel(
          id = "logbooks_details_tabs",
          type = "tabs",
          #BY VESSEL
          #------------------------------------------------------------------------------------
          tabPanel(title = "By vessel",
                   fluidRow(
                     br(),
                     div(
                       class = "col-md-6",
                       selectizeInput(ns("logbooks_vessel"), label = "Vessel", 
                                      choice = accessVesselsWithLogBooks(pool), selected = NULL, 
                                      options = list(
                                        placeholder = 'Select a vessel',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )),
                     )
                   ),
                   fluidRow(
                     div(
                       class = "col-md-6",
                       h3("Total quantities by year"), hr(),
                       box(
                         width = 12,
                         tabsetPanel(
                           id = "logbooks_yearly_totals_by_vessel_tabs", type = "pills",
                           tabPanel(title = "Graph", plotlyOutput(ns("plot_vessel"))),
                           tabPanel(title = "Data", tags$div(DTOutput(ns("data_vessel"))), style = "margin:6px;")
                         )
                       )
                     )
                   )              
          ),
          #BY OWNER
          #------------------------------------------------------------------------------------
          tabPanel(title = "By owner",
                   fluidRow(
                     br(),
                     div(
                       class = "col-md-6",
                       selectizeInput(ns("logbooks_vessel_owner"), label = "Vessel owner (company)", 
                                      choice = accessVesselsOwnersWithLogBooks(pool), selected = NULL, 
                                      options = list(
                                        placeholder = 'Select a vessel owner',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )),
                     )
                   ),
                   fluidRow(
                     div(
                       class = "col-md-6",
                       h3("Total quantities by year"), hr(),
                       box(
                         width = 12,
                         tabsetPanel(
                           id = "logbooks_yearly_totals_by_vessel_owner_tabs", type = "pills",
                           tabPanel(title = "Graph", plotlyOutput(ns("plot_vessel_owner"))),
                           tabPanel(title = "Data", tags$div(DTOutput(ns("data_vessel_owner"))), style = "margin:6px;")
                         )
                       )
                     )
                   )                    
          ),
          #BY LANDING SITE
          #------------------------------------------------------------------------------------
          tabPanel(title = "By landing site",
                   fluidRow(
                     br(),
                     div(
                       class = "col-md-6",
                       selectizeInput(ns("logbooks_year"), label = "Year", 
                                      choice = accessAvailableYears(pool), selected = NULL, 
                                      options = list(
                                        placeholder = 'Select a year',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )),
                     )
                   ),
                   fluidRow(
                     div(
                       class = "col-md-6",
                       h3("Breakdown of total quantities by landing site / year"), hr(),
                       box(
                         width = 12,
                         tabsetPanel(
                          id = "logbooks_yearly_totals_tabs", type = "pills",
                          tabPanel(title = "Map", leafletOutput(ns("map_total"))),
                          tabPanel(title = "Data", tags$div(DTOutput(ns("data_total"))), style = "margin:6px;")
                         )
                       )
                     ),
                     div(
                       class = "col-md-6",
                       h3("Breakdown of total quantities by landing site / species / year"), hr(),
                       box(
                         width = 12,
                         tabsetPanel(
                           id = "logbooks_yearly_speciestotals_tabs", type = "pills",
                           tabPanel(title = "Map", leafletOutput(ns("map_species"))),
                           tabPanel(title = "Data", tags$div(DTOutput(ns("data_species"))), style = "margin:6px;")
                         )
                       )
                     )
                   )         
          )
        )
      )
    )
  )
  
}
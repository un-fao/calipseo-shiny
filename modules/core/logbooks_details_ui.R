#logbooks_details_ui
logbooks_details_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "logbooks_details",
    fluidRow(
      column(
        width = 12,
        bs4Dash::tabsetPanel(
          id = "logbooks_details_tabs",
          type = "tabs",
          #BY VESSEL
          #------------------------------------------------------------------------------------
          tabPanel(title = i18n("TABPANEL_BY_VESSEL"),
                   fluidRow(
                     br(),
                     div(
                       class = "col-md-6",
                       selectizeInput(ns("logbooks_vessel"), label = i18n("TABPANEL_BY_VESSEL_SELECT_LABEL"), 
                                      choice = accessVesselsWithLogBooks(pool), selected = NULL, 
                                      options = list(
                                        placeholder = i18n("TABPANEL_BY_VESSEL_SELECT_PLACEHOLDER_LABEL"),
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )),
                     )
                   ),
                   fluidRow(
                     div(
                       class = "col-md-6",
                       bs4Dash::box(
                         title = h3(i18n("TOTAL_QUANTITIES_GRAPH_TITLE")),
                         width = 12,
                         tabsetPanel(
                           id = "logbooks_yearly_totals_by_vessel_tabs", type = "pills",
                           tabPanel(title = i18n("GRAPH_TABPANEL_TITLE"), plotlyOutput(ns("plot_vessel"))),
                           tabPanel(title = i18n("DATA_TABPANEL_TITLE"), tags$div(DTOutput(ns("data_vessel"))), style = "margin:6px;")
                         ),
                         collapsible = FALSE,
                         maximizable = TRUE
                       )
                     )
                   )              
          ),
          #BY OWNER
          #------------------------------------------------------------------------------------
          tabPanel(title = i18n("TABPANEL_BY_OWNER"),
                   fluidRow(
                     br(),
                     div(
                       class = "col-md-6",
                       selectizeInput(ns("logbooks_vessel_owner"), label = i18n("TABPANEL_BY_OWNER_SELECT_LABEL"), 
                                      choice = accessVesselsOwnersWithLogBooks(pool), selected = NULL, 
                                      options = list(
                                        placeholder = i18n("TABPANEL_BY_OWNER_SELECT_PLACEHOLDER_LABEL"),
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )),
                     )
                   ),
                   fluidRow(
                     div(
                       class = "col-md-6",
                       bs4Dash::box(
                         title = h3(i18n("TOTAL_QUANTITIES_GRAPH_TITLE")),
                         width = 12,
                         tabsetPanel(
                           id = "logbooks_yearly_totals_by_vessel_owner_tabs", type = "pills",
                           tabPanel(title = i18n("GRAPH_TABPANEL_TITLE"), plotlyOutput(ns("plot_vessel_owner"))),
                           tabPanel(title = i18n("DATA_TABPANEL_TITLE"), tags$div(DTOutput(ns("data_vessel_owner"))), style = "margin:6px;")
                         ),
                         collapsible = FALSE,
                         maximizable = TRUE
                       )
                     )
                   )                    
          ),
          #BY LANDING SITE
          #------------------------------------------------------------------------------------
          tabPanel(title = i18n("TABPANEL_BY_LANDING_SITE"),
                   fluidRow(
                     br(),
                     div(
                       class = "col-md-6",
                       selectizeInput(ns("logbooks_year"), label = i18n("TABPANEL_BY_LANDING_SITE_SELECT_LABEL"), 
                                      choice = accessAvailableYears(pool)$year, selected = NULL, 
                                      options = list(
                                        placeholder = i18n("TABPANEL_BY_LANDING_SITE_SELECT_PLACEHOLDER_LABEL"),
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )),
                     )
                   ),
                   fluidRow(
                     div(
                       class = "col-md-6",
                       bs4Dash::box(
                         title = h3(i18n("BREAKDOWN_OF_TOTAL_QUANTITIES_LANDINGSITE_YEAR_TITLE")),
                         width = 12,
                         tabsetPanel(
                          id = "logbooks_yearly_totals_tabs", type = "pills",
                          tabPanel(title = i18n("MAP_TABPANEL_TITLE"), leafletOutput(ns("map_total"))),
                          tabPanel(title = i18n("DATA_TABPANEL_TITLE"), tags$div(DTOutput(ns("data_total"))), style = "margin:6px;")
                         ),
                         collapsible = FALSE,
                         maximizable = TRUE
                       )
                     ),
                     div(
                       class = "col-md-6",
                       bs4Dash::box(
                         title = h3(i18n("BREAKDOWN_OF_TOTAL_QUANTITIES_LANDINGSITE_SPICIES_YEAR_TITLE")),
                         width = 12,
                         tabsetPanel(
                           id = "logbooks_yearly_speciestotals_tabs", type = "pills",
                           tabPanel(title = i18n("MAP_TABPANEL_TITLE"), leafletOutput(ns("map_species"))),
                           tabPanel(title = i18n("DATA_TABPANEL_TITLE"), tags$div(DTOutput(ns("data_species"))), style = "margin:6px;")
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
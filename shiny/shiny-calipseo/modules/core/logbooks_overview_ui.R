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
          # fluidRow(
          #   div(
          #     class = "col-md-6",
          #     h3(sprintf("Statistics (tons) by vessel type - %s", as.integer(format(Sys.Date(), "%Y"))-1)), hr(),
          #     box(
          #       width = 12,
          #       DT::dataTableOutput(ns("stats_by_type_lastyear_table"))
          #     )
          #   ),
          #   div(
          #     class = "col-md-6",
          #     h3(sprintf("Statistics (tons) by vessel type - %s", format(Sys.Date(), "%Y"))), hr(),
          #     box(
          #       width = 12,
          #       DT::dataTableOutput(ns("stats_by_type_currentyear_table"))
          #     )
          #   )
          #),
          fluidRow(
            div(
              class = "col-md-12",
             # h3(sprintf("Breakdown of quantity caught")), hr(),
              shinyWidgets::verticalTabsetPanel(
                shinyWidgets::verticalTabPanel('Breakdown by vessel types',box_height='70px', line_chart_ui(ns("vt"),sliderWidth =25)),
                shinyWidgets::verticalTabPanel('Breakdown by gear types',box_height='70px' ,line_chart_ui(ns("gt"),sliderWidth =25)),
                shinyWidgets::verticalTabPanel('Breakdown by species',box_height='70px',
                                               tabsetPanel(type="pills",
                                                 tabPanel('Species',line_chart_ui(ns("sp"),sliderWidth =25)),
                                                 tabPanel('Species groups',line_chart_ui(ns("fg"),sliderWidth =25))
                                                 )),
                shinyWidgets::verticalTabPanel('Breakdown by landing sites',box_height='70px',contentWidth=11 ,line_chart_ui(ns("ls"),sliderWidth =25)),
                shinyWidgets::verticalTabPanel('Breakdown by fishing zones',box_height='70px',contentWidth=11 ,line_chart_ui(ns("fz"),sliderWidth =25))
              )
            )
          )
)
  
}
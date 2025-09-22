#individual_overview_ui
individual_overview_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "individual_overview",
                   
    fluidRow(
      column(
        width = 12,
        htmlOutput(ns("individual_overview_info"))
      )
    ),
    uiOutput(ns("indicators")),
    bs4Dash::tabsetPanel(
      vertical = TRUE,
      type = "pills",
      tabPanel(
        title = tags$h5(i18n("INDIVIDUAL_OVERVIEW_VERTICALTABPANEL_GRAPHS")), box_height='70px',
        div(class = 'row',
            div(class = 'col-md-6',
                sunburst_chart_ui(
                  id = ns("sb"),
                  title = i18n("INDIVIDUAL_OVERVIEW_SUNBURST_BOX_LABEL"),
                  sliderWidth =35
                )
            ),
            div(class = 'col-md-6',
                pyramid_chart_ui(
                  id = ns("py"),
                  title = i18n("INDIVIDUAL_OVERVIEW_PYRAMID_BOX_LABEL"),
                  sliderWidth = 35
                )
            )
        )
      ),
      tabPanel(
        title = tags$h5(i18n("INDIVIDUAL_OVERVIEW_VERTICALTABPANEL_TABLE")), box_height='70px',
        div(class = 'row',
          div(class = 'col-md-12', height = '800px',
              pretty_table_ui(
                id = ns("pt"),
                title = i18n("INDIVIDUAL_OVERVIEW_TABLE_BOX_LABEL"),
                sliderWidth = 35,
                sliderOpen = TRUE
              )
            )
          )
        )
      )
    )
}
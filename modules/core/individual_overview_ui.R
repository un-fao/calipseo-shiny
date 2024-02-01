#individual_overview_ui
individual_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "individual_overview",
          div(class = 'row',style = "margin:12px;",htmlOutput(ns("individual_overview_info"))),
          div(class = 'row',
              div(class = 'col-md-12',uiOutput(ns("indicators")))
          ),
          shinyWidgets::verticalTabsetPanel(
            shinyWidgets::verticalTabPanel(i18n("INDIVIDUAL_OVERVIEW_VERTICALTABPANEL_GRAPHS"),box_height='70px',
              div(class = 'row',
                  div(class = 'col-md-6',sunburst_chart_ui(ns("sb"),title=i18n("INDIVIDUAL_OVERVIEW_SUNBURST_BOX_LABEL"),sliderWidth =35)),
                  div(class = 'col-md-6',pyramid_chart_ui(ns("py"),title=i18n("INDIVIDUAL_OVERVIEW_PYRAMID_BOX_LABEL"),sliderWidth =35))
              )
            ),
            shinyWidgets::verticalTabPanel(i18n("INDIVIDUAL_OVERVIEW_VERTICALTABPANEL_TABLE"),box_height='70px',
              div(class = 'row',
                div(class = 'col-md-12',height = '800px',pretty_table_ui(ns("pt"),title=i18n("INDIVIDUAL_OVERVIEW_TABLE_BOX_LABEL"),sliderWidth =35,sliderOpen=T))
              )
            )
          )
          
  )
  
}
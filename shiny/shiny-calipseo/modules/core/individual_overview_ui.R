#individual_overview_ui
individual_overview_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "individual_overview",
          div(class = 'row',style = "margin:12px;",htmlOutput(ns("individual_overview_info"))),
          div(class = 'row',
              div(class = 'col-md-6',
                box(width = 12,title = i18n("INDIVIDUAL_OVERVIEW_INDICATORS_BOX_LABEL"),uiOutput(ns("indicators")))
                ),
              div(class = 'col-md-6',
                box(width = 12,title = i18n("INDIVIDUAL_OVERVIEW_MAP_BOX_LABEL"),p("Map place holder"))
                )
              ),
          div(class = 'row',
              div(class = 'col-md-6',sunburst_chart_ui(ns("sb"),title=i18n("INDIVIDUAL_OVERVIEW_SUNBURST_BOX_LABEL"),sliderWidth =25)),
              div(class = 'col-md-6',pyramid_chart_ui(ns("py"),title=i18n("INDIVIDUAL_OVERVIEW_PYRAMID_BOX_LABEL"),sliderWidth =25))
          ),
          div(class = 'row',
              div(class = 'col-md-12',pretty_table_ui(ns("pt"),title=i18n("INDIVIDUAL_OVERVIEW_TABLE_BOX_LABEL"),sliderWidth =25,sliderOpen=T))
          )
          
  )
  
}
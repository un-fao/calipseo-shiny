line_chart_ui <- function(id,sliderWidth = 25) {
  ns<-NS(id)
  tagList(
    box(
      title="",
      width = 12,
      sidebar = shinydashboardPlus::boxSidebar(
        id=ns("box"),
        width = sliderWidth,
        style = 'font-size:14px;',
        uiOutput(ns("rank_params")),
        selectInput(ns("granu"),"Temporal resolution :",choices=c("Yearly"="%Y","Monthly"="%Y-%m","Weekly"="%Y-%U")),
        selectInput(ns("stat"),"Statistic :",choices=c("Total"="sum","Average"="mean","Median"="median")),
        uiOutput(ns("additional"))),
        plotlyOutput(ns("plot"))
    )
  )
  
}
#' @name line_chart_ui
#' @aliases line_chart_ui
#' @title line_chart_ui
#' @description \code{line_chart_ui} UI part of line_chart module
#'
#' @usage line_chart_ui(id, sliderWidth)
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param sliderWidth numeric, width of slider

#'    

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
        uiOutput(ns("result"))
    )
    )
  
  
}
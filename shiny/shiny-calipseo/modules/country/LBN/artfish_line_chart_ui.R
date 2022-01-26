#' @name artfish_line_chart_ui
#' @aliases artfish_line_chart_ui
#' @title artfish_line_chart_ui
#' @description \code{artfish_line_chart_ui} UI part of artfish_line_chart module
#'
#' @usage artfish_line_chart_ui(id, sliderWidth)
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param title title of the box
#' @param sliderWidth numeric, width of slider

#'    

artfish_line_chart_ui <- function(id,title="",sliderWidth = 25) {
  ns<-NS(id)
  tagList(
    box(
      title=title,
      width = 12,
      sidebar = shinydashboardPlus::boxSidebar(
        id=ns("box"),
        width = sliderWidth,
        style = 'font-size:14px;',
        uiOutput(ns("levels_selector")),
        uiOutput(ns("rank_params"))),
        uiOutput(ns("result"))
    )
  )
  
}
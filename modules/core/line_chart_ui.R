#' @name line_chart_ui
#' @aliases line_chart_ui
#' @title line_chart_ui
#' @description \code{line_chart_ui} UI part of line_chart module
#'
#' @usage line_chart_ui(id, sliderWidth)
#'                 
#' @param id specific id of module to be able to link ui and server part

#'    

line_chart_ui <- function(id) {
  ns<-NS(id)
 
  uiOutput(ns("main"))
  
  
}
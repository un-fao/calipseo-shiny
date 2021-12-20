#' @name trip_gantt_ui
#' @aliases trip_gantt_ui
#' @title trip_gantt_ui
#' @description \code{trip_gantt_ui} UI part of trip_gantt module
#'
#' @usage trip_gantt_ui_ui(id, sliderWidth)
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param sliderWidth numeric, width of slider

#'    

trip_gantt_ui <- function(id,sliderWidth = 25) {
  ns<-NS(id)
  tagList(
    uiOutput(ns("content")),
  )
  
}
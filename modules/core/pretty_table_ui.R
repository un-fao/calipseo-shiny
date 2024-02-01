#' @name pretty_table_ui
#' @aliases pretty_table_ui
#' @title pretty_table_ui
#' @description \code{pretty_table_ui} UI part of pretty_table module
#'
#' @usage pretty_table_ui(id,title,sliderWidth,backgroundColor,sliderOpen)
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param title title of th box
#' @param sliderWidth numeric, width of slider
#' @param backgroundColor color of slider background
#' @param sliderOpen whether the sidebar is open at start. FALSE by default.

#'    

pretty_table_ui <- function(id,title,sliderWidth = 25,backgroundColor="#333a40",sliderOpen=FALSE) {
  ns<-NS(id)
  
  tagList(
    tags$style(
      HTML("
    .rank-list-container.default-sortable {
      background-color: #c6c6c6;
    }
    .default-sortable .rank-list-item {
      background-color: #62a0ca;
    }
  ")
    ),
    shinydashboardPlus::box(
      title=title,
      width = 12,
      style= "min-height:600px",
      sidebar = shinydashboardPlus::boxSidebar(
        id=ns("box"),
        width = sliderWidth,
        background = backgroundColor,
        startOpen = sliderOpen,
        style = 'font-size:14px;',
        uiOutput(ns("select_variable")),
        uiOutput(ns("grandTotal_wrapper")),
        
      ),
      uiOutput(ns("result"))
    )
  )
  
}
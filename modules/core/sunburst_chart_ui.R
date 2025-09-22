#' @name sunburst_chart_ui
#' @aliases sunburst_chart_ui
#' @title sunburst_chart_ui
#' @description \code{sunburst_chart_ui} UI part of sunburst_chart module
#'
#' @usage sunburst_chart_ui(id,title,sliderWidth,backgroundColor,sliderOpen)
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param title title of th box
#' @param sliderWidth numeric, width of slider
#' @param backgroundColor color of slider background
#' @param sliderOpen whether the sidebar is open at start. FALSE by default.

#'    

sunburst_chart_ui <- function(id,title="",sliderWidth = 25,backgroundColor="#333a40",sliderOpen=FALSE) {
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
    bs4Dash::box(
      title = title,
      width = 12,
      sidebar = bs4Dash::boxSidebar(
        id=ns("box"),
        width = sliderWidth,
        background = backgroundColor,
        startOpen = sliderOpen,
        style = 'font-size:14px;',
        uiOutput(ns("select_variable")),
      ),
      uiOutput(ns("result")),
      collapsible = FALSE,
      maximizable = TRUE
    )
  )
  
  
}
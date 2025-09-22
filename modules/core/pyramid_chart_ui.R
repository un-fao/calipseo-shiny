#' @name pyramid_chart_ui
#' @aliases pyramid_chart_ui
#' @title pyramid_chart_ui
#' @description \code{pyramid_chart_ui} UI part of pyramid_chart module
#'
#' @usage pyramid_chart_ui(id,title,sliderWidth,backgroundColor,sliderOpen)
#'                 
#' @param id specific id of module to be able to link ui and server part
#' @param title title of th box
#' @param sliderWidth numeric, width of slider
#' @param backgroundColor color of slider background
#' @param sliderOpen whether the sidebar is open at start. FALSE by default.

#'    

pyramid_chart_ui <- function(id,title="",sliderWidth = 25,backgroundColor="#333a40",sliderOpen=FALSE) {
  ns<-NS(id)
  
  bs4Dash::box(
      title = title,
      width = 12,
      sidebar = bs4Dash::boxSidebar(
        id=ns("box"),
        width = sliderWidth,
        background = backgroundColor,
        startOpen = sliderOpen,
        style = 'font-size:14px;',
        uiOutput(ns("mode_selector")),
        uiOutput(ns("fill_selector")),
        uiOutput(ns("age_slider")),
        uiOutput(ns("step_selector")),
        uiOutput(ns("filter_selector")),
        uiOutput(ns("filter_item_selector")),
        
      ),
      uiOutput(ns("result")),
      collapsible = FALSE,
      maximizable = TRUE
    )
  
  
}
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
  granu_choices <- c(i18n("YEARLY"),i18n("MONTHLY"),i18n("WEEKLY"))
  stat_choices <- c(i18n("TOTAL"),i18n("AVERAGE"),i18n("MEDIAN"))
  
  tagList(
    shinydashboardPlus::box(
      title="",
      width = 12,
      sidebar = shinydashboardPlus::boxSidebar(
        id=ns("box"),
        width = sliderWidth,
        style = 'font-size:14px;',
        uiOutput(ns("rank_params")),
        selectInput(ns("granu"),label = paste0(i18n("TEMPORAL_RESOLUTION")," :"), choices = granu_choices),
        selectInput(ns("stat"),label = paste0(i18n("STATISTIC")," :"),choices=stat_choices),
        uiOutput(ns("additional"))
      ),
      uiOutput(ns("result"))
    )
    )
  
  
}
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
  
  level_choices <- c(i18n("LEVEL_LABEL_GLOBAL"),i18n("LEVEL_LABEL_DETAIL"))
  
  ranK_choices <- c(i18n("RANK_LABEL_TOTAL_CATCH_OVER_THE_PERIOD"),i18n("RANK_LABEL_LAST_YEAR_TOTAL_CATCH"),
                    i18n("RANK_LABEL_ANNUAL_CATCH_AVERAGE"))
  
  cond <- paste0("input.levels =='",i18n("LEVEL_LABEL_DETAIL"),"'")
  
  tagList(
    box(
      title=title,
      width = 12,
      sidebar = shinydashboardPlus::boxSidebar(
        id=ns("box"),
        width = sliderWidth,
        style = 'font-size:14px;',
        selectInput(ns("levels"),paste0(i18n("LABEL_LEVELS")," :"),choices=level_choices),
        
        conditionalPanel(
          cond ,ns = ns,
          uiOutput(ns("rank_params")),
          selectInput(ns("rank_method"),paste0(i18n("SELECT_INPUT_TITLE_RANK_METHOD")," :"),choices=ranK_choices))
        ),
       
        uiOutput(ns("result"))
    )
  )
  
}
generic_chart_ui <- function(id, title="",sliderWidth = 25) {
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      title = title,
      width = 12,
      
      dropdownMenu = bs4Dash::boxDropdown(
        icon = icon("download"),
        status = "primary",
        size = "sm",
        right = TRUE,
        downloadLink(ns("dl_png"),  "Download PNG"),
        tags$br(),
        downloadLink(ns("dl_html"), "Download HTML")
      ),
      
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        width = sliderWidth,
        style = 'font-size:14px;',
        uiOutput(ns("plot_style_wrapper")),
        uiOutput(ns("granularity_wrapper")),
        uiOutput(ns("error_wrapper"))
      ),
      
      uiOutput(ns("result")),
      collapsible = FALSE,
      maximizable = TRUE
    )
  )
}

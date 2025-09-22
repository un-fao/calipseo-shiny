#logbooks_overview_ui
logbooks_overview_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(tabName = "logbooks_overview",
          fluidRow(
            column(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("logbooks_overview_info"))
            )
          ),
          uiOutput(ns("nb_infos")),
          fluidRow(
            div(
              class = "col-md-12",
              bs4Dash::tabsetPanel(
                vertical = TRUE,
                type = "pills",
                tabPanel(
                  title = tags$h5(i18n("VERTICALTABPANEL_GLOBAL_QUANTITY")), box_height = '70px', 
                  line_chart_ui(
                    id = ns("gq"),
                    sliderWidth =25
                  )
                ),
                tabPanel(
                  title = tags$h5(i18n("VERTICALTABPANEL_GLOBAL_VESSEL")), box_height = '70px', 
                  bs4Dash::box(
                     title = NULL,
                     width = 12,
                     sidebar = bs4Dash::boxSidebar(
                       id = ns("gv_box"),
                       width = 25,
                       style = 'font-size:14px;',
                       selectInput(ns("gv_granu"),label = paste0(i18n("TEMPORAL_RESOLUTION")," :"), choices = c(i18n("YEARLY"),i18n("MONTHLY"),i18n("WEEKLY")))
                     ),
                     uiOutput(ns("gv_result"))
                   )
                  ),
                tabPanel(
                  title = tags$h5(i18n("VERTICALTABPANEL_BREAKDOWN_BY_VESSEL_TYPES")), box_height = '70px', 
                  line_chart_ui(
                    id = ns("vt"),
                    sliderWidth = 25
                  )
                ),
                tabPanel(
                  title = tags$h5(i18n("VERTICALTABPANEL_BREAKDOWN_BY_GEAR_TYPES")), box_height = '70px',
                  line_chart_ui(
                    id = ns("gt"),
                    sliderWidth = 25
                  )
                ),
                tabPanel(
                  title = tags$h5(i18n("VERTICALTABPANEL_BREAKDOWN_BY_SPECIES")), box_height = '70px',
                  bs4Dash::tabsetPanel(
                    type = "pills",
                    tabPanel(
                      title = i18n("TABPANEL_BREAKDOWN_BY_SPECIES"),
                      line_chart_ui(
                        id = ns("sp"),
                        sliderWidth =25
                      )
                    ),
                    tabPanel(
                      title = i18n("TABPANEL_BREAKDOWN_BY_SPECIES_GROUPS"),
                      line_chart_ui(
                        id = ns("fg"),
                        sliderWidth = 25
                      )
                    )
                   )
                ),
                tabPanel(
                  title = tags$h5(i18n("VERTICALTABPANEL_BREAKDOWN_BY_LANDING_SITES")), box_height = '70px',
                  line_chart_ui(
                    id = ns("ls"),
                    sliderWidth = 25
                  )
                ),
                tabPanel(
                  title = tags$h5(i18n("VERTICALTABPANEL_BREAKDOWN_BY_FISHING_ZONE")), box_height = '70px',
                  line_chart_ui(
                    id = ns("fz"),
                    sliderWidth = 25
                  )
                )
              )
            )
          )
)
  
}
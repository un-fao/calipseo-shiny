#individual_breakdown_ui
individual_breakdown_ui <- function(id){
  
  ns <- NS(id)
  
  tabItem(tabName = "individual_breakdown",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("individual_breakdown_info"))
            )
          ),
          
          div(class='row',style = "margin:0px;", id='vertical-panels-individual-breakdown',
              shinyWidgets::verticalTabsetPanel(
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_INDIVIDUAL_BREAKDOWN_GENDER"),box_height='70px',
                                               div(h4(i18n("INDIVIDUAL_BREAKDOWN_TITLE_GENDER")),
                                                   box(width = 12, height = 480, title = sprintf(i18n("BREAKDOWN_INDIVIDUAL_TITLE_PIECHART_GENDER"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE, plotlyOutput(ns("individual_gender"))),
                                                   box(width = 12, height = 480, title = sprintf(i18n("BREAKDOWN_INDIVIDUAL_TITLE_PYRAMID"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE, plotlyOutput(ns("individual_age_gender"))))),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_INDIVIDUAL_BREAKDOWN_EDUCATION"),box_height='70px',
                                               div(h4(i18n("INDIVIDUAL_BREAKDOWN_TITLE_EDUCATION")),
                                                   fluidRow(box(width = 4, height = 480, title = sprintf(i18n("BREAKDOWN_INDIVIDUAL_TITLE_PIECHART_EDUCATION"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE,plotlyOutput(ns("individual_edulevel"))),
                                                            box(width = 4, height = 480, title = sprintf(i18n("BREAKDOWN_MALE_TITLE_PIECHART_EDUCATION"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE,plotlyOutput(ns("individual_edulevel_male"))),
                                                            box(width = 4, height = 480, title = sprintf(i18n("BREAKDOWN_FEMALE_TITLE_PIECHART_EDUCATION"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE,plotlyOutput(ns("individual_edulevel_female"))))
                                                   
                                               )
                )
                
                
              ))
  )
  
}
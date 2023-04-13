#individual_breakdown_ui
individual_breakdown_ui <- function(id){
  
  ns <- NS(id)
  
  
  tabItem(tabName = "individual_breakdown",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("fisher_breakdown_info"))
            )
          ),
          
          div(class='row',style = "margin:0px;", id='vertical-panels-individual-breakdown',
              shinyWidgets::verticalTabsetPanel(
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_INDIVIDUAL_BREAKDOWN_GENDER"),box_height='70px',
                                               div(h4(i18n("INDIVIDUAL_BREAKDOWN_TITLE_GENDER")),
                                                   h4(i18n("INDIVIDUAL_TYPE")),
                                                   span(checkboxInput(ns("fisher_chck"), label = i18n("INDIVIDUAL_BREAKDOWN_TITLE_FISHER")),checkboxInput(ns("nonfisher_chck"), label = i18n("INDIVIDUAL_BREAKDOWN_TITLE_NONFISHER"))),
                                                   conditionalPanel(
                                                     condition = "input.fisher_chck == 1",ns = ns,
                                                     selectizeInput(ns("filter_fisher_category"),label = i18n("SELECT_FISHER_CATEGORY"),choices = c(i18n("INDIVIDUAL_LIST_LABEL_OWNER"),i18n("INDIVIDUAL_LIST_LABEL_CAPTAIN"),i18n("INDIVIDUAL_LIST_LABEL_HOLDER_FISHING_ID"),i18n("INDIVIDUAL_LIST_LABEL_HOLDER_FISHING_LICENSE")),multiple = FALSE, width = '40%')),
                                                   box(width = 12, height = 480, title = uiOutput(ns('title_box_gender')), status = "primary", solidHeader= TRUE, plotlyOutput(ns("fisher_gender"))),
                                                   box(width = 12, height = 480, title = uiOutput(ns('title_box_pyramid')), status = "primary", solidHeader= TRUE,
                                                       plotlyOutput(ns("fisher_age_gender"))))),
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_INDIVIDUAL_BREAKDOWN_EDUCATION"),box_height='70px',
                                               div(h4(i18n("INDIVIDUAL_BREAKDOWN_TITLE_EDUCATION")),
                                                   span(checkboxInput(ns("fisher_chck_edu"), label = i18n("INDIVIDUAL_BREAKDOWN_TITLE_FISHER")),checkboxInput(ns("nonfisher_chck_edu"), label = i18n("INDIVIDUAL_BREAKDOWN_TITLE_NONFISHER"))),
                                                   conditionalPanel(
                                                     condition = "input.fisher_chck_edu == 1",ns = ns,
                                                     selectizeInput(ns("filter_fisher_category_edu"),label = i18n("SELECT_FISHER_CATEGORY"),choices = c(i18n("INDIVIDUAL_LIST_LABEL_OWNER"),i18n("INDIVIDUAL_LIST_LABEL_CAPTAIN"),i18n("INDIVIDUAL_LIST_LABEL_HOLDER_FISHING_ID"),i18n("INDIVIDUAL_LIST_LABEL_HOLDER_FISHING_LICENSE")),multiple = FALSE, width = '40%')),
                                                   fluidRow(box(width = 4, height = 480, title = sprintf(i18n("BREAKDOWN_INDIVIDUAL_TITLE_PIECHART_EDUCATION"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE,plotlyOutput(ns("individual_edulevel"))),
                                                            box(width = 4, height = 480, title = sprintf(i18n("BREAKDOWN_MALE_TITLE_PIECHART_EDUCATION"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE,plotlyOutput(ns("individual_edulevel_male"))),
                                                            box(width = 4, height = 480, title = sprintf(i18n("BREAKDOWN_FEMALE_TITLE_PIECHART_EDUCATION"), appConfig$country_profile$data$NAME), status = "primary", solidHeader= TRUE,plotlyOutput(ns("individual_edulevel_female"))))
                                                   
                                               )
                )
                
                
                
              ))
  )
  
}
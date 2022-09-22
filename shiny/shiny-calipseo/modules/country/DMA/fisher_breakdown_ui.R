#fisher_breakdown_ui
fisher_breakdown_ui <- function(id){
  
  ns <- NS(id)
  
  
  
  Owner <- as.character(i18n("FISHER_LABEL_OWNER"))
  Captain <- as.character(i18n("FISHER_LABEL_CAPTAIN"))
  `Holder of fishing Id` <- as.character(i18n("FISHER_LABEL_HOLDER_FISHING_ID"))
  `Holder of fishing License` <- as.character(i18n("FISHER_LABEL_HOLDER_FISHING_LICENSE"))
  
  tabItem(tabName = "fisher_breakdown",
          fluidRow(
            div(
              width = 12, style = "margin:12px;",
              htmlOutput(ns("fisher_breakdown_info"))
            )
          ),
          
          div(class='row',style = "margin:0px;", id='vertical-panels-individual-breakdown',
              shinyWidgets::verticalTabsetPanel(
                shinyWidgets::verticalTabPanel(i18n("VERTICALTABPANEL_FISHER_BREAKDOWN_GENDER"),box_height='70px',
                                               div(h4(i18n("FISHER_BREAKDOWN_TITLE_GENDER")),
                                                   h4(i18n("FISHER_TITLE_TYPE_INDIVIDUAL")),
                                                   div(class = 'row', div(class = 'col-md-4',checkboxInput(ns("fisher_chck"), label = i18n("FISHER_BREAKDOWN_TITLE_FISHER"))),div(class='col-md-4',checkboxInput(ns("nonfisher_chck"), label = i18n("FISHER_BREAKDOWN_TITLE_NONFISHER")))),
                                                   conditionalPanel(
                                                   condition = "input.fisher_chck == 1",ns = ns,
                                                   selectizeInput(ns("filter_fisher_category"),label = i18n("SELECT_FISHER_CATEGORY"),choices = c(Owner="OWN",Captain= "CAP",`Holder of fishing Id`="FIS_ID",`Holder of fishing License`="FIS_LS"),multiple = FALSE, width = '40%')),
                                                   box(width = 12, height = 480, title = uiOutput(ns('title_box_gender')), status = "primary", solidHeader= TRUE, plotlyOutput(ns("fisher_gender"))),
                                                   box(width = 12, height = 480, title = uiOutput(ns('title_box_pyramid')), status = "primary", solidHeader= TRUE,
                                                   plotlyOutput(ns("fisher_age_gender")))))
               


              ))
  )
  
}
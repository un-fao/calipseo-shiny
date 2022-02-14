#artfish_fishing_unit_ui
artfish_fishing_unit_ui <- function(id){
  
  ns <- NS(id)
  
  indicator_choices <- c(i18n("EFFORT_LABEL"),i18n("CATCH_LABEL"),i18n("VALUE_LABEL"),i18n("CPUE_LABEL"))
  level_choices <- c(i18n("LEVEL_LABLE_GLOBAL"),i18n("LEVEL_LABLE_DETAIL"))
 
  
  DAYS_LABEL <- c(i18n("DAYS_LABEL"))
  KILOGRAM_LABEL <- c(i18n("KILOGRAM_LABEL"))
  TONNE_LABEL <- c(i18n("TONNE_LABEL"))
  DOLLAR_LABEL <- c(i18n("DOLLAR_LABEL"))
  THOUSAND_DOLLAR_LABEL <- c(i18n("THOUSAND_DOLLAR_LABEL"))
  MILLION_DOLLAR_LABEL <- c(i18n("MILLION_DOLLAR_LABEL"))
  CPUE_KILOGRAM_DAY_LABEL <- c(i18n("CPUE_KILOGRAM_DAY_LABEL"))
  
  cond_1 <- paste0("input.indicator =='",i18n("EFFORT_LABEL"),"'")
  cond_2 <- paste0("input.indicator =='",i18n("CATCH_LABEL"),"'")
  cond_3 <- paste0("input.indicator =='",i18n("VALUE_LABEL"),"'")
  cond_4 <- paste0("input.indicator =='",i18n("CPUE_LABEL"),"'")
  cond_5 <- paste0("input.indicator !='",i18n("EFFORT_LABEL"),"'")

  
  tabItem(tabName = "artfish_fishing_unit",
          uiOutput(ns("urlPage")),
          fluidRow(
            tags$h2(i18n("ARTFISH_FISHING_UNIT_TITLE"))
          ),
          fluidRow(
            column(6,
                   
                   
                   tagList(
                     fluidRow(
                       column(6,
                              uiOutput(ns("year_selector")),
                              uiOutput(ns("fishing_unit_selector"))
                       ),
                       column(6,
                              selectizeInput(ns("indicator"),paste0(i18n("SELECT_INPUT_TITLE_INDICATOR")," :"),choices=indicator_choices,multiple = F),
                              conditionalPanel(
                                condition = cond_1,ns = ns,
                                selectizeInput(ns("unit"),paste0(i18n("SELECT_INPUT_TITLE_UNIT")," :"),choices= DAYS_LABEL,multiple = F) 
                                
                              ),
                              conditionalPanel(
                                condition = cond_2,ns = ns,
                                selectizeInput(ns("unit_1"),paste0(i18n("SELECT_INPUT_TITLE_UNIT")," :"),choices=c(KILOGRAM_LABEL,TONNE_LABEL),multiple = F) 
                                
                              ),
                              conditionalPanel(
                                condition = cond_3,ns = ns,
                                selectizeInput(ns("unit_2"),paste0(i18n("SELECT_INPUT_TITLE_UNIT")," :"),choices=c(DOLLAR_LABEL,THOUSAND_DOLLAR_LABEL,MILLION_DOLLAR_LABEL),multiple = F) 
                                
                              ),
                              conditionalPanel(
                                condition = cond_4,ns = ns,
                                selectizeInput(ns("unit_3"),paste0(i18n("SELECT_INPUT_TITLE_UNIT")," :"),choices=CPUE_KILOGRAM_DAY_LABEL,multiple = F) 
                                
                              )
                              
                       )
                     )
                   )     
                   
                   
                   ),
            column(3,uiOutput(ns("value"))),
            column(3,plotlyOutput(ns("accuracy"),height=200))
          ),
          tagList(
            fluidRow(
              div(
                class = "col-md-6",
                box(
                  title= i18n("BOX_TITLE_MONTHLY_REPARTITION_BY_FISHING_UNITS"),
                  width = 12,
                  tabsetPanel(type="pills",
                              tabPanel(i18n("PLOT_LABEL"),
                                       box(
                                         title='',
                                         width = 12,
                                         sidebar = shinydashboardPlus::boxSidebar(
                                           id=ns("bg_sum_box"),
                                           width = 25,
                                           style = 'font-size:14px;',
                                           selectizeInput(ns("bg_level"),paste0(i18n("SELECT_INPUT_TITLE_LEVEL")," :"),choices=level_choices,multiple = F)
                                         ),
                                         plotlyOutput(ns("bg_sum_plot"))%>%withSpinner(type = 4))
                              ),
                              tabPanel(i18n("TABLE_LABEL"),DTOutput(ns("bg_sum_table"))%>%withSpinner(type = 4))
                  )
                )
              ),
              div(
                class = "col-md-6",
                box(
                  title= i18n("BOX_TITLE_TOP_RANKING_FISHING_UNITS"),
                  width = 12,
                  tabsetPanel(type="pills",
                              tabPanel(i18n("PLOT_LABEL"),plotlyOutput(ns("bg_rank_plot"))%>%withSpinner(type = 4)),
                              tabPanel(i18n("TABLE_LABEL"),DTOutput(ns("bg_rank_table"))%>%withSpinner(type = 4))
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = cond_5,ns = ns,
            
            
            tagList(
              fluidRow(
                div(
                  class = "col-md-6",
                  box(
                    title= i18n("BOX_TITLE_MONTHLY_REPARTITION_BY_SPECIES"),
                    width = 12,
                    tabsetPanel(type="pills",
                                tabPanel(i18n("PLOT_LABEL"),
                                         box(
                                           title='',
                                           width = 12,
                                           sidebar = shinydashboardPlus::boxSidebar(
                                             id=ns("sp_sum_box"),
                                             width = 25,
                                             style = 'font-size:14px;',
                                             selectizeInput(ns("sp_level"),paste0(i18n("SELECT_INPUT_TITLE_LEVEL")," :"),choices=level_choices,selected="Detail",multiple = F),
                                             uiOutput(ns("sp_number_selector"))
                                           ),
                                           plotlyOutput(ns("sp_sum_plot"))%>%withSpinner(type = 4))
                                ),
                                tabPanel(i18n("TABLE_LABEL"),DTOutput(ns("sp_sum_table"))%>%withSpinner(type = 4))
                    )
                  )
                ),
                div(
                  class = "col-md-6",
                  box(
                    title= i18n("BOX_TITLE_TOP_RANKING_SPECIES"),
                    width = 12,
                    tabsetPanel(type="pills",
                                tabPanel(i18n("PLOT_LABEL"),plotlyOutput(ns("sp_rank_plot"))%>%withSpinner(type = 4)),
                                tabPanel(i18n("TABLE_LABEL"),DTOutput(ns("sp_rank_table"))%>%withSpinner(type = 4))
                    )
                  )
                )
              )
            )
            
            )
          
  
  )
          

}
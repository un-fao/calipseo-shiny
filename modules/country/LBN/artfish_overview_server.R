#artfish_overview_server
artfish_overview_server <- function(id, pool){

 moduleServer(id, function(input, output, session){   
    
  ns<-session$ns
  
  data_bg<-reactiveVal(NULL)
  
  level_choices <- c(i18n("LEVEL_LABLE_GLOBAL"),i18n("LEVEL_LABLE_DETAIL"))
  
  ref_species<-accessRefSpecies(pool)
  ref_fishing_units<-accessRefFishingUnits(pool)
  
  
  estimate<-artfish_estimates(con=pool,data_effort=accessEffortData(pool),data_landing=accessLandingData(pool))
  
  output$fishing_unit_selector<-renderUI({
        
    bg_sp<-unique(estimate$EST_BGC)
        
    ref_bg_sp<-subset(ref_fishing_units,ID %in% bg_sp)
        
    bg<-setNames(c(0,ref_bg_sp$ID),c(i18n("ALL_FISHING_UNITS_LABEL"),ref_bg_sp$NAME))
        
    selectizeInput(ns("bg"),paste0(i18n("SELECT_INPUT_TITLE_FISHING_UNIT")," :"),choices=bg,multiple = F,selected=bg[1])
    })
  
  observeEvent(input$bg,{
    
    req(!is.null(input$bg))
    
    if(as.integer(input$bg)>0){
      
      selection<-subset(estimate,EST_BGC==input$bg,
                        select=c(EST_YEAR,EST_MONTH,EST_BGC,EST_BGC_NAME,EST_SPC,EST_SPC_NAME,EST_EFF_EFFORT,EST_EFF_NBOATS,EST_LND_CATCH,EST_LND_PRICE,EST_LND_VALUE,EST_LND_CPUE))
    }else{
      selection<-subset(estimate,
                        select=c(EST_YEAR,EST_MONTH,EST_BGC,EST_BGC_NAME,EST_SPC,EST_SPC_NAME,EST_EFF_EFFORT,EST_EFF_NBOATS,EST_LND_CATCH,EST_LND_PRICE,EST_LND_VALUE,EST_LND_CPUE))
    }
    data_bg(selection)
    
  })
  
  observeEvent(data_bg(),{
    req(!is.null(data_bg()))
    
    data<-data_bg()%>%
     mutate(DATE=as.character(format(as.Date(sprintf("%04d-%02d-01",EST_YEAR,EST_MONTH)),format = "%Y-%m")))%>%
    #left_join(ref_species%>%select(ID,NAME),by=c('EST_SPC'='ID'))%>%
      ungroup()
    
    data_landing<-data%>%
                  select(DATE,EST_BGC,EST_BGC_NAME,EST_EFF_EFFORT,EST_EFF_NBOATS)%>%
                  distinct()%>%
                  #left_join(ref_fishing_units%>%select(ID,NAME),by=c('EST_BGC'='ID'))%>%
                  ungroup()
    
    artfish_line_chart_server("boats", label=i18n("BOAT_GEAR_LABEL"),df=data_landing, colDate = "DATE",colTarget="EST_BGC_NAME",colValue="EST_EFF_NBOATS",ylab=i18n("NUMBER_OF_BOATS_LABEL"),levels=level_choices,stat="sum", rank=TRUE,mode='plot+table',prefered_colnames=c(i18n("TABEL_COLNAME_DATE"),i18n("TABEL_COLNAME_AGG"),i18n("TABEL_COLNAME_BOAT_GEAR")))
    artfish_line_chart_server("effort", label=i18n("BOAT_GEAR_LABEL"),df=data_landing, colDate = "DATE",colTarget="EST_BGC_NAME",colValue="EST_EFF_EFFORT",ylab=i18n("EFFORT_DAYS_LABEL"),levels=level_choices,stat="sum", rank=TRUE,mode='plot+table',prefered_colnames=c(i18n("TABEL_COLNAME_DATE"),i18n("TABEL_COLNAME_AGG"),i18n("TABEL_COLNAME_BOAT_GEAR")))
    artfish_line_chart_server("catch", label=i18n("SPECIES_LABEL"),df=data, colDate = "DATE",colTarget="EST_SPC_NAME",colValue="EST_LND_CATCH",ylab=i18n("CATCH_LABEL"),levels=level_choices,stat="sum", rank=TRUE,mode='plot+table',prefered_colnames=c(i18n("TABEL_COLNAME_DATE"),i18n("TABEL_COLNAME_AGG"),i18n("TABEL_COLNAME_SPECIES")))
    artfish_line_chart_server("value", label=i18n("SPECIES_LABEL"),df=data, colDate = "DATE",colTarget="EST_SPC_NAME",colValue="EST_LND_VALUE",ylab=i18n("VALUE_LABEL"),levels=level_choices,stat="sum", rank=TRUE,mode='plot+table',prefered_colnames=c(i18n("TABEL_COLNAME_DATE"),i18n("TABEL_COLNAME_AGG"),i18n("TABEL_COLNAME_SPECIES")))
    artfish_line_chart_server("cpue", label=i18n("SPECIES_LABEL"),df=data, colDate = "DATE",colTarget="EST_SPC_NAME",colValue="EST_LND_CPUE",ylab=i18n("CPUE_LABEL"),levels=level_choices,stat="mean", rank=TRUE,mode='plot+table',prefered_colnames=c(i18n("TABEL_COLNAME_DATE"),i18n("TABEL_COLNAME_AGG"),i18n("TABEL_COLNAME_SPECIES")))
    
    output$results<-renderUI({
    tagList(
      fluidRow(
        column(6,
               artfish_line_chart_ui(ns("boats"),title=i18n("TITLE_CUMULATE_NUMBER_OF_BOATS"),sliderWidth =25)
        ),
        column(6,
               artfish_line_chart_ui(ns("effort"),title=i18n("TITLE_CUMULATE_EFFORT"),sliderWidth =25)
        )
      ),
      fluidRow(
        column(6,
          artfish_line_chart_ui(ns("catch"),title=i18n("TITLE_CUMULATE_CATCH"),sliderWidth =25)
        ),
        column(6,
          artfish_line_chart_ui(ns("value"),title=i18n("TITLE_CUMULATE_VALUE"),sliderWidth =25)
        )
        ),
      fluidRow(
        column(6,        
          artfish_line_chart_ui(ns("cpue"),title=i18n("AVERAGE_CPUE"),sliderWidth =25)
         )  
      )
    )
    })
    })
  
 })
  
}
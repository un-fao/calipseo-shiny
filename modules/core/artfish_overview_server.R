#artfish_overview_server
artfish_overview_server <- function(id, pool){

 moduleServer(id, function(input, output, session){   
    
  ns<-session$ns
  
  data_bg<-reactiveVal(NULL)
  
  level_choices <- c(i18n("LEVEL_LABEL_GLOBAL"),i18n("LEVEL_LABEL_DETAIL"))
  
  ref_species<-accessRefSpecies(pool)
  ref_fishing_units<-accessRefFishingUnits(pool)
  
  files<-getStatPeriods(config=appConfig, "artfish_estimates",target = "release")
  
  output$no_release<-renderUI({
    div(
    if(nrow(files)>0){
      NULL
    }else{
    p(i18n("NO_RELEASE"))
    }
    )
  })
  
  req(nrow(files)>0)
  
  estimate<-do.call(rbind,lapply(files$file, readr::read_csv))
  
  estimate<-estimate%>%
    merge(ref_fishing_units%>%
    select(CODE,NAME)%>%
    rename(fishing_unit=CODE,
           fishing_unit_label=NAME)
    )%>%
    ungroup()
  
  estimate<-estimate%>%
    merge(ref_species%>%
            select(CODE,NAME)%>%
            rename(species=CODE,
                   species_label=NAME)
    )%>%
    ungroup()
  
  output$fishing_unit_selector<-renderUI({
        
    bg_sp<-unique(estimate$fishing_unit)
        
    ref_bg_sp<-subset(ref_fishing_units,ID %in% bg_sp)
        
    bg<-setNames(c(0,ref_bg_sp$ID),c(i18n("ALL_FISHING_UNITS_LABEL"),ref_bg_sp$NAME))
        
    selectizeInput(ns("bg"),paste0(i18n("SELECT_INPUT_TITLE_FISHING_UNIT")," :"),choices=bg,multiple = F,selected=bg[1])
    })
  
  observeEvent(input$bg,{
    
    req(!is.null(input$bg))
    
    if(as.integer(input$bg)>0){
      
      selection<-subset(estimate,fishing_unit==input$bg,
                        select=c(year,month,fishing_unit,fishing_unit_label,species_label,effort_nominal,fleet_engagement_number,catch_nominal_landed,trade_value,catch_cpue))
    }else{
      selection<-subset(estimate,
                        select=c(year,month,fishing_unit,fishing_unit_label,species_label,effort_nominal,fleet_engagement_number,catch_nominal_landed,trade_value,catch_cpue))
    }
    data_bg(selection)
    
  })
  
  observeEvent(data_bg(),{
    req(!is.null(data_bg()))
    
    data<-data_bg()%>%
     mutate(DATE=as.Date(sprintf("%04d-%02d-01",year,month)))%>%
      ungroup()
    
    data_landing<-data%>%
                  select(DATE,fishing_unit,fishing_unit_label,effort_nominal,fleet_engagement_number)%>%
                  distinct()%>%
                  ungroup()
    
    print("TEST-START")
    print(names(data))
    print("TEST-END")
    
    artfish_line_chart_server("boats", label=i18n("BOAT_GEAR_LABEL"),df=data_landing, colDate = "DATE",colTarget="fishing_unit_label",colValue="fleet_engagement_number",ylab=i18n("NUMBER_OF_BOATS_LABEL"),levels=level_choices,stat="sum", rank=TRUE,mode='plot+table',prefered_colnames=c(i18n("TABEL_COLNAME_DATE"),i18n("TABEL_COLNAME_AGG"),i18n("TABEL_COLNAME_BOAT_GEAR")))
    artfish_line_chart_server("effort", label=i18n("BOAT_GEAR_LABEL"),df=data_landing, colDate = "DATE",colTarget="fishing_unit_label",colValue="effort_nominal",ylab=i18n("EFFORT_DAYS_LABEL"),levels=level_choices,stat="sum", rank=TRUE,mode='plot+table',prefered_colnames=c(i18n("TABEL_COLNAME_DATE"),i18n("TABEL_COLNAME_AGG"),i18n("TABEL_COLNAME_BOAT_GEAR")))
    artfish_line_chart_server("catch", label=i18n("SPECIES_LABEL"),df=data, colDate = "DATE",colTarget="species_label",colValue="catch_nominal_landed",ylab=i18n("CATCH_LABEL"),levels=level_choices,stat="sum", rank=TRUE,mode='plot+table',prefered_colnames=c(i18n("TABEL_COLNAME_DATE"),i18n("TABEL_COLNAME_AGG"),i18n("TABEL_COLNAME_SPECIES")))
    artfish_line_chart_server("value", label=i18n("SPECIES_LABEL"),df=data, colDate = "DATE",colTarget="species_label",colValue="trade_value",ylab=i18n("VALUE_LABEL"),levels=level_choices,stat="sum", rank=TRUE,mode='plot+table',prefered_colnames=c(i18n("TABEL_COLNAME_DATE"),i18n("TABEL_COLNAME_AGG"),i18n("TABEL_COLNAME_SPECIES")))
    artfish_line_chart_server("cpue", label=i18n("SPECIES_LABEL"),df=data, colDate = "DATE",colTarget="species_label",colValue="catch_cpue",ylab=i18n("CPUE_LABEL"),levels=level_choices,stat="mean", rank=TRUE,mode='plot+table',prefered_colnames=c(i18n("TABEL_COLNAME_DATE"),i18n("TABEL_COLNAME_AGG"),i18n("TABEL_COLNAME_SPECIES")))
    
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
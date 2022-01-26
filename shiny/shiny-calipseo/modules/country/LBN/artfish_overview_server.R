#artfish_overview_server
artfish_overview_server <- function(input, output, session, pool){
  
  ns<-session$ns
  
  data_bg<-reactiveVal(NULL)
  
  output$urlPage<-renderUI({
    session$userData$page("artfish-overview")
    updatePageUrl("artfish-overview", session)
  })
  
  ref_species<-accessRefSpecies(pool)
  ref_fishing_units<-accessRefFishingUnits(pool)
  
  estimate<-artfish_estimates(data_effort=accessEffortData(pool),data_landing=accessLandingData(pool))
  
  output$fishing_unit_selector<-renderUI({
        
    bg_sp<-unique(estimate$EST_BGC)
        
    ref_bg_sp<-subset(ref_fishing_units,ID %in% bg_sp)
        
    bg<-setNames(c(0,ref_bg_sp$ID),c("Total",ref_bg_sp$NAME))
        
    selectizeInput(ns("bg"),"Selection of boat-gear",choices=bg,multiple = F,selected=bg[1],
                       options = list(
                         placeholder = 'select a boat-gear'
                       )
        )
    })
  
  observeEvent(input$bg,{
    
    req(!is.null(input$bg))
    
    if(as.integer(input$bg)>0){
      
      selection<-subset(estimate,EST_BGC==input$bg,
                        select=c(EST_YEAR,EST_MONTH,EST_BGC,EST_SPC,EST_EFF_EFFORT,EST_EFF_NBOATS,EST_LND_CATCH,EST_LND_PRICE,EST_LND_VALUE,EST_LND_CPUE))
    }else{
      selection<-subset(estimate,
                        select=c(EST_YEAR,EST_MONTH,EST_BGC,EST_SPC,EST_EFF_EFFORT,EST_EFF_NBOATS,EST_LND_CATCH,EST_LND_PRICE,EST_LND_VALUE,EST_LND_CPUE))
    }
    data_bg(selection)
    
  })
  
  observeEvent(data_bg(),{
    req(!is.null(data_bg()))
    
    data<-data_bg()%>%
     mutate(DATE=as.character(format(as.Date(sprintf("%04d-%02d-01",EST_YEAR,EST_MONTH)),format = "%Y-%m")))%>%
    left_join(ref_species%>%select(ID,NAME),by=c('EST_SPC'='ID'))%>%
      ungroup()
    
    data_landing<-data%>%
                  distinct()%>%
                  select(DATE,EST_BGC,EST_EFF_EFFORT,EST_EFF_NBOATS)%>%
                  left_join(ref_fishing_units%>%select(ID,NAME),by=c('EST_BGC'='ID'))%>%
                  ungroup()
    
    artfish_line_chart_server("boats", label="Boat-gear",df=data_landing, colDate = "DATE",colTarget="NAME",colValue="EST_EFF_NBOATS",ylab="Number of boats",levels=c("Global"="global","Detail"="detail"),stat="sum", rank=TRUE,mode='plot+table')
    artfish_line_chart_server("effort", label="Boat_gear",df=data_landing, colDate = "DATE",colTarget="NAME",colValue="EST_EFF_EFFORT",ylab="Effort (days)",levels=c("Global"="global","Detail"="detail"),stat="sum", rank=TRUE,mode='plot+table')
    artfish_line_chart_server("catch", label="Species",df=data, colDate = "DATE",colTarget="NAME",colValue="EST_LND_CATCH",ylab="Catch (kg)",levels=c("Global"="global","Detail"="detail"),stat="sum", rank=TRUE,mode='plot+table')
    artfish_line_chart_server("value", label="Species",df=data, colDate = "DATE",colTarget="NAME",colValue="EST_LND_VALUE",ylab="Value ($)",levels=c("Global"="global","Detail"="detail"),stat="sum", rank=TRUE,mode='plot+table')
    artfish_line_chart_server("cpue", label="Species",df=data, colDate = "DATE",colTarget="NAME",colValue="EST_LND_CPUE",ylab="CPUE (kg/day)",levels=c("Global"="global","Detail"="detail"),stat="mean", rank=TRUE,mode='plot+table')
    
    output$results<-renderUI({
    tagList(
      fluidRow(
        column(6,
               artfish_line_chart_ui(ns("boats"),title="Cumulate Number of Boats",sliderWidth =25)
        ),
        column(6,
               artfish_line_chart_ui(ns("effort"),title="Cumulate Effort",sliderWidth =25)
        )
      ),
      fluidRow(
        column(6,
          artfish_line_chart_ui(ns("catch"),title="Cumulate Catch",sliderWidth =25)
        ),
        column(6,
          artfish_line_chart_ui(ns("value"),title="Cumulate Value",sliderWidth =25)
        )
        ),
      fluidRow(
        column(6,        
          artfish_line_chart_ui(ns("cpue"),title="Average CPUE",sliderWidth =25)
         )  
      )
    )
    })
    })
  
    }
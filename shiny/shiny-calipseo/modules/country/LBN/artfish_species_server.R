#artfish_species_server
artfish_species_server <- function(id, pool){
 
 moduleServer(id, function(input, output, session){   
   
  ns<-session$ns
  
  data_sp<-reactiveVal(NULL)
  data_sp_bg<-reactiveVal(NULL)
  
  output$urlPage<-renderUI({
    session$userData$page("artfish-species")
    updatePageUrl("artfish-species", session)
  })
  
  ref_species<-accessRefSpecies(pool)
  ref_fishing_units<-accessRefFishingUnits(pool)
  
  estimate<-artfish_estimates(data_effort=accessEffortData(pool),data_landing=accessLandingData(pool))
  
  output$species_selector<-renderUI({
    
    species<-setNames(ref_species$ID, sprintf("%s [%s]",ref_species$NAME,ref_species$SCIENTIFIC_NAME))
    
    selectizeInput(ns("species"),paste0(i18n("SELECT_INPUT_TITLE_SPECIES")," :"),choices=species,multiple = F,selected=NULL,
                   options = list(
                     placeholder = i18n("SELECT_INPUT_SPECIES_PLACEHOLDER"),
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })

  #Subset data with species
  observeEvent(input$species,{
    
    print(input$species)
    if(input$species!=""){
      
      selection<-subset(estimate,EST_SPC==input$species)
      data_sp(selection)
    }
  })
    
  observeEvent(data_sp(),{
    
    if(!is.null(data_sp())){ 
  
      output$fishing_unit_selector<-renderUI({
        
        selection<-data_sp()
        bg_sp<-unique(selection$EST_BGC)
        
        ref_bg_sp<-subset(ref_fishing_units,ID %in% bg_sp)
          
        bg<-setNames(c(0,ref_bg_sp$ID),c(i18n("ALL_FISHING_UNITS_LABEL"),ref_bg_sp$NAME))
        
        selectizeInput(ns("bg"),paste0(i18n("SELECT_INPUT_TITLE_FISHING_UNIT")," :"),choices=bg,multiple = F,selected=bg[1])
      })
    }
  })
  
  #Subset data with boat gear
  observeEvent(input$bg,{
    
    req(!is.null(input$bg))
    req(!is.null(data_sp()))
    selection<-data_sp()
    
    if(as.integer(input$bg)>0){
      
      selection<-subset(selection,EST_BGC==input$bg,
                   select=c(EST_YEAR,EST_MONTH,EST_BGC,EST_LND_CATCH,EST_LND_PRICE,EST_LND_VALUE,EST_EFF_EFFORT,EST_LND_CPUE))
    }else{
      selection<-subset(selection,
                   select=c(EST_YEAR,EST_MONTH,EST_BGC,EST_LND_CATCH,EST_LND_PRICE,EST_LND_VALUE,EST_EFF_EFFORT,EST_LND_CPUE))
    }
    data_sp_bg(selection)

  })
  
  #Indicators and timeline
  
  observeEvent(data_sp_bg(),{
    
    req(!is.null(data_sp_bg()))
     
    data<-data_sp_bg()
      
    #Multi plot timeline
      
    output$timeline<-renderPlotly({
          
          data<-data%>%
            mutate(DATE=as.Date(sprintf("%04d-%02d-01",EST_YEAR,EST_MONTH)))%>%
            group_by(DATE)%>%
            summarise(
              EST_LND_CATCH=sum(EST_LND_CATCH,na.rm=T),
              EST_LND_PRICE=mean(EST_LND_PRICE,na.rm=T),
              EST_LND_VALUE=sum(EST_LND_VALUE,na.rm=T),
              EST_EFF_EFFORT=sum(EST_EFF_EFFORT,na.rm=T),
              EST_LND_CPUE=mean(EST_LND_CPUE,na.rm=T)
            )%>%
            ungroup()
          
          fig1 <- data%>%plot_ly(x = ~DATE, y = ~EST_LND_VALUE, type = 'scatter', mode = 'lines',fill = 'tozeroy',name=i18n("LABEL_VALUE"))%>%  
            layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_VALUE"), showarrow = F, xref='paper', yref='paper')))
          
          fig2 <- data%>%plot_ly(x = ~DATE, y = ~EST_LND_CATCH, type = 'bar',name=i18n("LABEL_CATCH"))%>%  
            layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_CATCH"), showarrow = F, xref='paper', yref='paper')))
          
          fig3 <- data%>%plot_ly(x = ~DATE, y = ~EST_LND_PRICE, type = 'scatter', mode = 'lines',name=i18n("LABEL_PRICE"))%>%  
            layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_PRICE"), showarrow = F, xref='paper', yref='paper')))
          
          fig4 <- data%>%plot_ly(x = ~DATE, y = ~EST_LND_CPUE, type = 'scatter', mode = 'lines',name=i18n("LABEL_CPUE"))%>%  
            layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_CPUE"), showarrow = F, xref='paper', yref='paper')))
          
          fig5 <- data%>%plot_ly(x = ~DATE, y = ~EST_EFF_EFFORT, type = 'bar',name=i18n("LABEL_EFFORT"))%>%  
            layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_EFFORT"), showarrow = F, xref='paper', yref='paper'))) 
  
          fig <- subplot(fig1, fig2, fig3,fig4,fig5, nrows = 5, shareX = TRUE)%>% 
            layout(showlegend=F,
                   height=620,
                   hovermode ='closest',
                   plot_bgcolor='#e5ecf6', 
                   xaxis = list(dtick = "M1", tickformat = '%m-%Y',showgrid = T,title = NULL,tickfont = list(color = "#333333"),
                                rangeslider = list(visible = F),
                                rangeselector=list(
                                  buttons=list(
                                    list(count=3, label="3y", step="year", stepmode="backward"),
                                    list(count=2, label="2y", step="year", stepmode="backward"),
                                    list(count=1, label="1y", step="year", stepmode="backward"),
                                    list(step="all")
                                  ))),
                   yaxis = list( 
                     zerolinecolor = '#ffff', 
                     zerolinewidth = 2, 
                     gridcolor = 'ffff')) 
          fig
        })
        
       #Indicators
        
        output$indicators<-renderUI({
          
          tagList(
            fluidRow(
              valueBox(value=tags$p(i18n("VALUEBOX_TITLE_TOTAL_VALUE"),style="font-size: 40%"), subtitle=round(sum(data$EST_LND_VALUE,na.rm=T),2), icon = tags$i(class = "fas fa-dollar-sign", style="font-size: 30px"), width = 2,color = "blue" ),
              valueBox(value=tags$p(i18n("VALUEBOX_TITLE_TOTAL_CATCH"),style="font-size: 40%"), subtitle=round(sum(data$EST_LND_CATCH,na.rm=T),2), icon = tags$i(class = "fas fa-fish", style="font-size: 30px"), width = 2,color="orange"),
              valueBox(value=tags$p(i18n("VALUEBOX_TITLE_AVERAGE_PRICE"),style="font-size: 40%"), subtitle=round(mean(data$EST_LND_PRICE,na.rm=T),2), icon = tags$i(class = "fas fa-dollar-sign", style="font-size: 30px"), width = 2,color="green"),
              valueBox(value=tags$p(i18n("VALUEBOX_TITLE_AVERAGE_CPUE"),style="font-size: 40%"), subtitle=round(mean(data$EST_LND_CPUE,na.rm=T),5), icon = tags$i(class = "fas fa-ship", style="font-size: 30px"), width = 2,color="red"),
              valueBox(value=tags$p(i18n("VALUEBOX_TITLE_TOTAL_EFFORT"),style="font-size: 40%"), subtitle=round(sum(data$EST_EFF_EFFORT,na.rm=T),0), icon = tags$i(class = "fas fa-clock", style="font-size: 30px"), width = 2,color="purple")
              )
          )
          
        })
  })
        
  #donuts and rank
  observeEvent(data_sp(),{
    
    req(!is.null(data_sp()))
    
        output$donut<-renderPlotly({
          
          selection<-data_sp()
          
          bg_sp<-unique(selection$EST_BGC)
          
          ref_bg_sp<-subset(ref_fishing_units,ID %in% bg_sp)
          
          selection%>%
            group_by(EST_BGC)%>%
            summarise(
              EST_LND_CATCH=sum(EST_LND_CATCH,na.rm=T),
              EST_LND_PRICE=mean(EST_LND_PRICE,na.rm=T),
              EST_LND_VALUE=sum(EST_LND_VALUE,na.rm=T),
              EST_EFF_EFFORT=sum(EST_EFF_EFFORT,na.rm=T),
              EST_LND_CPUE=mean(EST_LND_CPUE,na.rm=T)
            )%>%
            left_join(ref_bg_sp%>%select(ID,NAME),by=c('EST_BGC'='ID'))%>%
            ungroup()%>%
            mutate(PERCENT=EST_LND_CATCH/sum(EST_LND_CATCH,na.rm=T))%>%
          plot_ly(labels = ~NAME, values = ~PERCENT)%>% 
            add_pie(hole = 0.6)%>% 
            layout(showlegend = T,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                plot_bgcolor  = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)")

        })
        
        #Rank
        
        output$rank<-renderPlotly({
          rank<-estimate%>%
            group_by(EST_SPC)%>%
            summarise(
              EST_LND_CATCH=sum(EST_LND_CATCH,na.rm=T)
            )%>%
            mutate(rank = rank(-EST_LND_CATCH)) %>%
            arrange(rank)%>%
            mutate(color=ifelse(EST_SPC==input$species,"target","others"))%>%
            ungroup()
          
          target_rank<-subset(rank,color=="target")$rank
          
          rank<-rank%>%
            filter(rank%in%c(seq(target_rank-5,target_rank+5,1)))%>%
            left_join(ref_species%>%select(ID,NAME),by=c('EST_SPC'='ID'))%>%
            ungroup()
          
          rank%>%
            plot_ly(y = ~rank, x = ~EST_LND_CATCH, type = "bar",  orientation = "h",color=~factor(color),colors = c("grey","orange"),hoverinfo='none',text = ~NAME,
                    textposition = "auto",textfont = list(color = "black")) %>%
              layout(showlegend = FALSE,
                     uniformtext=list(minsize=8, mode='show'),
                     yaxis = list(title=i18n("YLAB_TITLE_RANK"),autorange = "reversed",tickmode = "array", tickvals = unique(rank$rank), ticktext = unique(rank$rank)),
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,title = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)")
          
        })
        
  })

 })  
  
}
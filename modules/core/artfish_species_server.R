#artfish_species_server
artfish_species_server <- function(id, parent.session, pool, reloader){
 
 moduleServer(id, function(input, output, session){   
   
  ns<-session$ns
  
  data_sp<-reactiveVal(NULL)
  data_sp_bg<-reactiveVal(NULL)
  
  
  ref_species<-accessRefSpecies(pool)
  ref_species$ID<-as.character(ref_species$ID)
  ref_fishing_units<-accessRefFishingUnits(pool)
  ref_fishing_units$ID<-as.character(ref_fishing_units$ID)
  
  # estimate<-artfish_estimates(con=pool,
  #                             effort_source="fisher_interview",
  #                             active_vessels=accessArtfishA(pool),
  #                             effort = accessArtfishB1(pool),
  #                             active_vessels_strategy="latest",
  #                             active_days=accessArtfishC(pool),
  #                             landings=accessArtfishD(pool),
  #                             minor_strata=NULL)
  
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
  
  ref_species<-subset(ref_species,ID%in%unique(na.omit(estimate$species)))
  
  print("DEBUG-1")
  print(unique(estimate$species))
  print(unique(ref_species$ID))
  
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
      
      selection<-subset(estimate,species==input$species)
      data_sp(selection)
    }
  })
    
  observeEvent(data_sp(),{
    
    if(!is.null(data_sp())){ 
  
      output$fishing_unit_selector<-renderUI({
        
        selection<-data_sp()
        bg_sp<-unique(selection$fishing_unit)
        
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
      
      selection<-subset(selection,fishing_unit==input$bg,
                   select=c(year,month,fishing_unit,catch_nominal_landed,trade_price,trade_value,effort_nominal,catch_cpue))
    }else{
      selection<-subset(selection,
                   select=c(year,month,fishing_unit,catch_nominal_landed,trade_price,trade_value,effort_nominal,catch_cpue))
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
            mutate(DATE=as.Date(sprintf("%04d-%02d-01",year,month)))%>%
            group_by(DATE)%>%
            summarise(
              catch_nominal_landed=sum(catch_nominal_landed,na.rm=T),
              trade_price=mean(trade_price,na.rm=T),
              trade_value=sum(trade_value,na.rm=T),
              effort_nominal=unique(effort_nominal,na.rm=T),
              catch_cpue=mean(catch_cpue,na.rm=T)
            )%>%
            ungroup()
          
          fig1 <- data%>%plot_ly(x = ~DATE, y = ~trade_value, type = 'scatter', mode = 'lines',fill = 'tozeroy',name=i18n("LABEL_VALUE"))%>%  
            layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_VALUE"), showarrow = F, xref='paper', yref='paper')))
          
          fig2 <- data%>%plot_ly(x = ~DATE, y = ~catch_nominal_landed, type = 'bar',name=i18n("LABEL_CATCH"))%>%  
            layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_CATCH"), showarrow = F, xref='paper', yref='paper')))
          
          fig3 <- data%>%plot_ly(x = ~DATE, y = ~trade_price, type = 'scatter', mode = 'lines',name=i18n("LABEL_PRICE"))%>%  
            layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_PRICE"), showarrow = F, xref='paper', yref='paper')))
          
          fig4 <- data%>%plot_ly(x = ~DATE, y = ~catch_cpue, type = 'scatter', mode = 'lines',name=i18n("LABEL_CPUE"))%>%  
            layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_CPUE"), showarrow = F, xref='paper', yref='paper')))
          
          fig5 <- data%>%plot_ly(x = ~DATE, y = ~effort_nominal, type = 'bar',name=i18n("LABEL_EFFORT"))%>%  
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
              valueBox(value=tags$p(i18n("VALUEBOX_TITLE_TOTAL_VALUE"),style="font-size: 40%"), subtitle=round(sum(data$trade_value,na.rm=T),2), icon = tags$i(class = "fas fa-dollar-sign", style="font-size: 30px"), width = 2,color = "blue" ),
              valueBox(value=tags$p(i18n("VALUEBOX_TITLE_TOTAL_CATCH"),style="font-size: 40%"), subtitle=round(sum(data$catch_nominal_landed,na.rm=T),2), icon = tags$i(class = "fas fa-fish", style="font-size: 30px"), width = 2,color="orange"),
              valueBox(value=tags$p(i18n("VALUEBOX_TITLE_AVERAGE_PRICE"),style="font-size: 40%"), subtitle=round(mean(data$trade_price,na.rm=T),2), icon = tags$i(class = "fas fa-dollar-sign", style="font-size: 30px"), width = 2,color="green"),
              valueBox(value=tags$p(i18n("VALUEBOX_TITLE_AVERAGE_CPUE"),style="font-size: 40%"), subtitle=round(mean(data$catch_cpue,na.rm=T),5), icon = tags$i(class = "fas fa-ship", style="font-size: 30px"), width = 2,color="red"),
              valueBox(value=tags$p(i18n("VALUEBOX_TITLE_TOTAL_EFFORT"),style="font-size: 40%"), subtitle=round(sum(data$effort_nominal,na.rm=T),0), icon = tags$i(class = "fas fa-clock", style="font-size: 30px"), width = 2,color="purple")
              )
          )
          
        })
  })
        
  #donuts and rank
  observeEvent(data_sp(),{
    
    req(!is.null(data_sp()))
    
        output$donut<-renderPlotly({
          
          selection<-data_sp()
          
          bg_sp<-unique(selection$fishing_unit)
          
          ref_bg_sp<-subset(ref_fishing_units,ID %in% bg_sp)
          
          selection%>%
            group_by(fishing_unit)%>%
            summarise(
              catch_nominal_landed=sum(catch_nominal_landed,na.rm=T),
              trade_price=mean(trade_price,na.rm=T),
              trade_value=sum(trade_value,na.rm=T),
              effort_nominal=sum(effort_nominal,na.rm=T),
              catch_cpue=mean(catch_cpue,na.rm=T)
            )%>%
            merge(ref_bg_sp%>%select(ID,NAME)%>%rename(fishing_unit=ID))%>%
            ungroup()%>%
            mutate(PERCENT=catch_nominal_landed/sum(catch_nominal_landed,na.rm=T))%>%
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
            group_by(species)%>%
            summarise(
              catch_nominal_landed=sum(catch_nominal_landed,na.rm=T)
            )%>%
            mutate(rank = rank(-catch_nominal_landed)) %>%
            arrange(rank)%>%
            mutate(color=ifelse(species==input$species,"target","others"))%>%
            ungroup()
          
          target_rank<-subset(rank,color=="target")$rank
          
          rank<-rank%>%
            filter(rank%in%c(seq(target_rank-5,target_rank+5,1)))%>%
            merge(ref_species%>%select(ID,NAME)%>%rename(species=ID))%>%
            ungroup()
          
          rank%>%
            plot_ly(y = ~rank, x = ~catch_nominal_landed, type = "bar",  orientation = "h",color=~factor(color),colors = c("grey","orange"),hoverinfo='none',text = ~NAME,
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
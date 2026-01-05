#artfish_species_server
artfish_species_server <- function(id, parent.session, pool, reloader){
 
 moduleServer(id, function(input, output, session){   
   
  INFO("artfish-species: START")
  MODULE_START_TIME <- Sys.time()  
   
  ns<-session$ns
  
  data_sp<-reactiveVal(NULL)
  data_sp_bg<-reactiveVal(NULL)
  
  ref_species <- accessRefSpecies(pool)
  ref_species$ID <- as.character(ref_species$ID)
  ref_fishing_units <- accessRefFishingUnits(pool)
  ref_fishing_units$ID <- as.character(ref_fishing_units$ID)
  
  files<-getStatPeriods(config = appConfig, id = "artfish_estimates",target = "release")
  
  output$no_release<-renderUI({
    div(
      if(nrow(files)>0){
        NULL
      }else{
        p(i18n("ARTFISH_SPECIES_NO_RELEASE"))
      }
    )
  })
  
  req(nrow(files)>0)
  
  estimate <- do.call(rbind,lapply(files$file, readr::read_csv))
  
  estimate <- estimate %>%
    merge(ref_fishing_units %>%
            select(ID,NAME) %>%
            rename(fishing_unit = ID,
                   fishing_unit_label = NAME)
    ) %>%
    ungroup()
  
  estimate <- estimate %>%
    merge(ref_species %>%
            select(ID,NAME) %>%
            rename(species = ID,
                   species_label = NAME)
    )%>%
    ungroup()
  
  estimate <-estimate%>%
    mutate(date = as.Date(sprintf("%04d-%02d-01",year,month)))
  
  ref_species <- subset(ref_species,ID %in% unique(na.omit(estimate$species)))
  
  output$species_selector <- renderUI({
    species <- setNames(ref_species$ID, sprintf("%s [%s]",ref_species$NAME,ref_species$SCIENTIFIC_NAME))
    species <- species[order(names(species))]
    selectizeInput(ns("species"),paste0(i18n("ARTFISH_SPECIES_SPECIES_SELECTOR_LABEL")," :"),choices = species,multiple = F,selected = NULL,
                   options = list(
                     placeholder = i18n("ARTFISH_SPECIES_SPECIES_SELECTOR_PLACEHOLDER"),
                     onInitialize = I('function() { this.setValue(""); }')
                   )
    )
  })

  #Subset data with species
  observeEvent(input$species,{
    if(input$species!=""){
      selection <- subset(estimate, species == input$species)
      data_sp(selection)
    }
  })
    
  observeEvent(data_sp(),{
    if(!is.null(data_sp())){ 
      
      output$time_selector <- renderUI({
        selection <- data_sp()
        sliderInput(
          ns("time"),
          label = i18n("ARTFISH_SPECIES_TIME_SLIDER_LABEL"),
          min = min(selection$date, na.rm = TRUE),
          max = max(selection$date, na.rm = TRUE),
          value = c(
            min(selection$date, na.rm = TRUE),
            max(selection$date, na.rm = TRUE)
          ),
          timeFormat = "%b %Y"
        )
        
      })
      
      output$fishing_unit_selector <- renderUI({
        selection <- data_sp()
        bg_ids <- unique(selection$fishing_unit)
        ref_bg_sp <- subset(ref_fishing_units, ID %in% bg_ids)
        
        choices <- setNames(ref_bg_sp$ID, ref_bg_sp$NAME)
        
        shinyWidgets::pickerInput(
          inputId = ns("fishing_unit"),
          label   = i18n("ARTFISH_SPECIES_FISHING_UNIT_SELECTOR_LABEL"),
          choices = choices,
          selected = ref_bg_sp$ID,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `select-all-text` = i18n("ARTFISH_SPECIES_FISHING_UNIT_SELECTOR_SELECT_ALL"),
            `deselect-all-text` = i18n("ARTFISH_SPECIES_FISHING_UNIT_SELECTOR_DESELECT_ALL"),
            `selected-text-format` = "count > 3",
            `count-selected-text` = paste0("{0} ",i18n("ARTFISH_SPECIES_FISHING_UNIT_SELECTOR_SELECTED")),
            `none-selected-text` = i18n("ARTFISH_SPECIES_FISHING_UNIT_SELECTOR_NO_SELECTION"),
            `live-search` = TRUE
          )
        )
      })
    }
  })
  
  
  observeEvent(c(input$fishing_unit,input$time), {
    
    req(!is.null(input$fishing_unit))
    req(!is.null(data_sp()))
    selection<-data_sp()
    
    data<-selection%>%
      filter(
        date >= input$time[1],
        date <= input$time[2]
      )
    
    if (length(input$fishing_unit) == 0) {
      selection <- data[0, ]
    } else {
      selection <- subset(
        data,
        fishing_unit %in% input$fishing_unit
      )
    }
    
    data_sp_bg(selection)
    
  })
  
  #Indicators and timeline
  observeEvent(data_sp_bg(),{
    
    req(!is.null(data_sp_bg()))
    data<-data_sp_bg()
    data_effort<-data%>%
      select(date,fishing_unit,fishing_unit_label,species,species_label,effort_nominal,catch_species_ratio) %>%
      distinct() %>%
      ungroup()
    
    total_effort<-data_effort%>%
      summarise(effort_nominal=sum(effort_nominal,na.rm=T),
                catch_species_ratio=mean(catch_species_ratio,na.rm=T)
      )
    
    total_catch<-data%>%
      summarise(catch_nominal_landed=sum(catch_nominal_landed,na.rm=T),
                trade_value=sum(trade_value,na.rm=T),
                trade_price=mean(trade_price,na.rm=T)
      )
    
    
    output$indicators <- renderUI({
      fluidRow(
        bs4InfoBox(
          title = i18n("ARTFISH_SPECIES_INFOBOX_CATCH_TITLE"),
          value = sprintf("%s (%s)",formatC(total_catch$catch_nominal_landed, format = "f", digits = 0, big.mark = "\u202F"),i18n("ARTFISH_SPECIES_INFOBOX_CATCH_UNIT")),
          icon = icon("fish"),
          color = "primary",
          width = 2
        ),
        bs4InfoBox(
          title = i18n("ARTFISH_SPECIES_INFOBOX_VALUE_TITLE"),
          value = sprintf("%s (%s)",formatC(total_catch$trade_value, format = "f", digits = 0, big.mark = "\u202F"),i18n("ARTFISH_SPECIES_INFOBOX_VALUE_UNIT")),
          icon = icon("dollar-sign"),
          color = "primary",
          width = 2
        ),
        bs4InfoBox(
          title = i18n("ARTFISH_SPECIES_INFOBOX_PRICE_TITLE"),
          value = sprintf("%s (%s)",formatC(total_catch$trade_price, format = "f", digits = 2, big.mark = "\u202F"),i18n("ARTFISH_SPECIES_INFOBOX_PRICE_UNIT")),
          icon = icon("dollar-sign"),
          color = "primary",
          width = 2
        ),
        bs4InfoBox(
          title = i18n("ARTFISH_SPECIES_INFOBOX_EFFORT_TITLE"),
          value = sprintf("%s (%s)",formatC(total_effort$effort_nominal, format = "f", digits = 0, big.mark = "\u202F"),i18n("ARTFISH_SPECIES_INFOBOX_EFFORT_UNIT")),
          icon = icon("ship"),
          color = "primary",
          width = 2
        ),
        bs4InfoBox(
          title = i18n("ARTFISH_SPECIES_INFOBOX_SP_RATE_TITLE"),
          value = formatC(total_effort$catch_species_ratio, format = "f", digits = 2, big.mark = "\u202F"),
          icon = icon("fish"),
          color = "primary",
          width = 2
        )
      )
    })
    

    generic_chart_server(
      id = "catch",
      df = data,
      col_date = "date",
      col_group = "fishing_unit_label",
      col_value = "catch_nominal_landed",
      stat = "sum",
      time_label = "",
      value_label = i18n("ARTFISH_SPECIES_PLOT_CATCH_VALUE_LABEL"),
      group_label = i18n("ARTFISH_SPECIES_PLOT_CATCH_GROUP_LABEL"),
      #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
    )
    
    generic_chart_server(
      id = "cpue",
      df = data,
      col_date = "date",
      col_group = "fishing_unit_label",
      col_value = "catch_cpue",
      time_label = "",
      value_label = i18n("ARTFISH_SPECIES_PLOT_CPUE_VALUE_LABEL"),
      group_label = i18n("ARTFISH_SPECIES_PLOT_CPUE_GROUP_LABEL"),
      stat = "mean",
      time_choices = "month"
      #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
    )
    
    generic_chart_server(
      id = "effort",
      df = data_effort,
      col_date = "date",
      col_group = "fishing_unit_label",
      col_value = "effort_nominal",
      time_label = "",
      value_label = i18n("ARTFISH_SPECIES_PLOT_EFFORT_VALUE_LABEL"),
      group_label = i18n("ARTFISH_SPECIES_PLOT_EFFORT_GROUP_LABEL"),
      stat = "sum"
      #plot_types = c("line","line_cumulate","area_stack","area_stack_pct"),
      #time_choices = c("month")
    )
    
    generic_chart_server(
      id = "value",
      df = data,
      col_date = "date",
      col_group = "fishing_unit_label",
      col_value = "trade_value",
      time_label = "",
      value_label = i18n("ARTFISH_SPECIES_PLOT_VALUE_VALUE_LABEL"),
      group_label = i18n("ARTFISH_SPECIES_PLOT_VALUE_GROUP_LABEL"),
      stat = "sum"
      #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
    )
    
    generic_chart_server(
      id = "price",
      df = data,
      col_date = "date",
      col_group = "fishing_unit_label",
      col_value = "trade_price",
      time_label = "",
      value_label = i18n("ARTFISH_SPECIES_PLOT_PRICE_VALUE_LABEL"),
      group_label = i18n("ARTFISH_SPECIES_PLOT_PRICE_GROUP_LABEL"),
      stat = "mean",
      time_choices = "month"
      #plot_types = c("line","line_cumulate","area_stack","area_stack_pct")
    )
    
    #Multi plot timeline
      
    # output$timeline<-renderPlotly({
    #       
    #       data<-data%>%
    #         mutate(DATE=as.Date(sprintf("%04d-%02d-01",year,month)))%>%
    #         group_by(DATE)%>%
    #         summarise(
    #           catch_nominal_landed=sum(catch_nominal_landed,na.rm=T),
    #           trade_price=mean(trade_price,na.rm=T),
    #           trade_value=sum(trade_value,na.rm=T),
    #           effort_nominal=unique(effort_nominal,na.rm=T),
    #           catch_cpue=mean(catch_cpue,na.rm=T)
    #         )%>%
    #         ungroup()
    #       
    #       fig1 <- data%>%plot_ly(x = ~DATE, y = ~trade_value, type = 'scatter', mode = 'lines',fill = 'tozeroy',name=i18n("LABEL_VALUE"))%>%  
    #         layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_VALUE"), showarrow = F, xref='paper', yref='paper')))
    #       
    #       fig2 <- data%>%plot_ly(x = ~DATE, y = ~catch_nominal_landed, type = 'bar',name=i18n("LABEL_CATCH"))%>%  
    #         layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_CATCH"), showarrow = F, xref='paper', yref='paper')))
    #       
    #       fig3 <- data%>%plot_ly(x = ~DATE, y = ~trade_price, type = 'scatter', mode = 'lines',name=i18n("LABEL_PRICE"))%>%  
    #         layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_PRICE"), showarrow = F, xref='paper', yref='paper')))
    #       
    #       fig4 <- data%>%plot_ly(x = ~DATE, y = ~catch_cpue, type = 'scatter', mode = 'lines',name=i18n("LABEL_CPUE"))%>%  
    #         layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_CPUE"), showarrow = F, xref='paper', yref='paper')))
    #       
    #       fig5 <- data%>%plot_ly(x = ~DATE, y = ~effort_nominal, type = 'bar',name=i18n("LABEL_EFFORT"))%>%  
    #         layout(height= 120,annotations = list( list(x = 0 , y = 1, text = i18n("VALUE_LABEL_EFFORT"), showarrow = F, xref='paper', yref='paper'))) 
    # 
    #       fig <- subplot(fig1, fig2, fig3,fig4,fig5, nrows = 5, shareX = TRUE)%>% 
    #         layout(showlegend=F,
    #                height=620,
    #                hovermode ='closest',
    #                plot_bgcolor='#e5ecf6', 
    #                xaxis = list(dtick = "M1", tickformat = '%m-%Y',showgrid = T,title = NULL,tickfont = list(color = "#333333"),
    #                             rangeslider = list(visible = F),
    #                             rangeselector=list(
    #                               buttons=list(
    #                                 list(count=3, label="3y", step="year", stepmode="backward"),
    #                                 list(count=2, label="2y", step="year", stepmode="backward"),
    #                                 list(count=1, label="1y", step="year", stepmode="backward"),
    #                                 list(step="all")
    #                               ))),
    #                yaxis = list( 
    #                  zerolinecolor = '#ffff', 
    #                  zerolinewidth = 2, 
    #                  gridcolor = 'ffff')) 
    #       fig
    #     })
        
       #Indicators
    
    output$results<-renderUI({
      
      tagList(
        fluidRow(generic_chart_ui(ns("catch"),title=i18n("ARTFISH_SPECIES_PLOT_CATCH_TITLE"),sliderWidth =25)),
        fluidRow(generic_chart_ui(ns("cpue"),title=i18n("ARTFISH_SPECIES_PLOT_CPUE_TITLE"),sliderWidth =25)),
        fluidRow(generic_chart_ui(ns("value"),title=i18n("ARTFISH_SPECIES_PLOT_VALUE_TITLE"),sliderWidth =25)),
        fluidRow(generic_chart_ui(ns("price"),title=i18n("ARTFISH_SPECIES_PLOT_PRICE_TITLE"),sliderWidth =25)),
        fluidRow(generic_chart_ui(ns("effort"),title=i18n("ARTFISH_SPECIES_PLOT_EFFORT_TITLE"),sliderWidth =25))
      )
    })
        
        # output$indicators<-renderUI({
        #   
        #   tagList(
        #     fluidRow(
        #       valueBox(
        #         value = round(sum(data$trade_value,na.rm=T),2), 
        #         subtitle = i18n("VALUEBOX_TITLE_TOTAL_VALUE"), 
        #         icon = tags$i(class = "fas fa-dollar-sign", style="font-size: 30px"), 
        #         width = 2,color = "navy"
        #       ),
        #       valueBox(
        #         value = round(sum(data$catch_nominal_landed,na.rm=T),2),
        #         subtitle = i18n("VALUEBOX_TITLE_TOTAL_CATCH"), 
        #         icon = tags$i(class = "fas fa-fish", style="font-size: 30px"),
        #         width = 2,color="orange"
        #       ),
        #       valueBox(
        #         value = round(mean(data$trade_price,na.rm=T),2),
        #         subtitle = i18n("VALUEBOX_TITLE_AVERAGE_PRICE"),
        #         icon = tags$i(class = "fas fa-dollar-sign", style="font-size: 30px"),
        #         width = 2,color = "success"
        #       ),
        #       valueBox(
        #         value = round(mean(data$catch_cpue,na.rm=T),5),
        #         subtitle = i18n("VALUEBOX_TITLE_AVERAGE_CPUE"),
        #         icon = tags$i(class = "fas fa-ship", style="font-size: 30px"),
        #         width = 2,color = "danger"
        #       ),
        #       valueBox(
        #         value = round(sum(data$effort_nominal,na.rm=T),0),
        #         subtitle = i18n("VALUEBOX_TITLE_TOTAL_EFFORT"),
        #         icon = tags$i(class = "fas fa-clock", style="font-size: 30px"),
        #         width = 2,color = "fuchsia"
        #       )
        #     )
        #   )
        #   
        # })
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
            plot_ly(y = ~rank, x = ~catch_nominal_landed, type = "bar",  orientation = "h",color=~factor(color),colors = c("gray","orange"),hoverinfo='none',text = ~NAME,
                    textposition = "auto",textfont = list(color = "black")) %>%
              layout(showlegend = FALSE,
                     uniformtext=list(minsize=8, mode='show'),
                     yaxis = list(title=i18n("ARTFISH_SPECIES_RANK_GROUP_LABEL"),autorange = "reversed",tickmode = "array", tickvals = unique(rank$rank), ticktext = unique(rank$rank)),
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,title = FALSE),
                     plot_bgcolor  = "rgba(0, 0, 0, 0)",
                     paper_bgcolor = "rgba(0, 0, 0, 0)")
          
        })
        
  })
  
  observeEvent(data_sp(), {
    req(data_sp())
    
    sel <- data_sp()
    
    if(is.null(input$fishing_unit) || as.integer(input$fishing_unit) == 0){
      data_sp_bg(sel)
    } else {
      data_sp_bg(subset(sel, fishing_unit == input$fishing_unit))
    }
  })
  
  MODULE_END_TIME <- Sys.time()
  INFO("artfish-species: END")
  DEBUG_MODULE_PROCESSING_TIME("Artfish-species", MODULE_START_TIME, MODULE_END_TIME)

 })  
  
}
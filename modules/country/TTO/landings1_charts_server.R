#landings1_charts_server
landings1_charts_server <- function(id, pool, reloader){

 moduleServer(id, function(input, output, session){    
    
  output$landings1_charts_info <- renderText({
    text <- paste0("<h2>", i18n("LANDINGS1_CHARTS_TITLE")," <small>", i18n("LANDINGS1_CHARTS_SUBTITLE"),
                   userTooltip("These charts represent the different statistical descriptors by year including the 1st raised landings (LAN), value (VAL), number of fishing trips (TRP) and ratios such as Landings/Trip (L/T), Value/Trip (V/T), and Value/Landing (P/K)",
                               style = "font-size: 75%;"),"</small></h2><hr>")
    text
  })
  
  #plotDescriptorTotal
  plotDescriptorTotal <- function(ts, bch_name, descriptor){
    if(is.null(ts)) return(NULL)
    bch_ts <- ts[is.na(ts$gear_id)&is.na(ts$species_id)&is.na(ts$month)&ts$bch_name==bch_name,]
    bch_ts_descriptor <- bch_ts[bch_ts$descriptor == descriptor,]
    plot_ly(bch_ts_descriptor, x = ~year, y = ~value, mode = 'lines+markers') %>%  layout(autosize = TRUE, height = 290)
  }
  
  #CONTROLLERS
  #TOTAL LANDINGS plots
  #---------------------
  
  tsr <- reactiveValues(
    data = NULL 
  )
  
  observeEvent(input$bch_name,{
    releases <- list.files(file.path(appConfig$store, "release/artisanal_fisheries_landings1"), full.names = T, recursive = TRUE)
    if(length(releases)>0){
      ts <- as.data.frame(do.call("rbind", lapply(releases, readr::read_csv)))
      ts <- ts[order(ts$bch_name),]
      tsr$data <- ts
    }
  })
  
  output$plot_LAN <- renderPlotly({
    plotDescriptorTotal(tsr$data, input$bch_name, "LAN")
  })
  output$plot_VAL <- renderPlotly({
    plotDescriptorTotal(tsr$data, input$bch_name, "VAL")
  })
  output$plot_TRP <- renderPlotly({
    plotDescriptorTotal(tsr$data, input$bch_name, "TRP")
  })
  output$plot_LT <- renderPlotly({
    plotDescriptorTotal(tsr$data, input$bch_name, "L/T")
  })
  output$plot_VT <- renderPlotly({
    plotDescriptorTotal(tsr$data, input$bch_name, "V/T")
  })
  output$plot_PK <- renderPlotly({
    plotDescriptorTotal(tsr$data, input$bch_name, "P/K")
  })
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) i18n("HOVER_ON_POINT_LABEL") else d
  })
  
 })
}
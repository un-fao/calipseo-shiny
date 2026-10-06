#market_trade_data_exporter_ui
market_trade_data_exporter_ui <- function(id){
  
  ns <- NS(id)
  
  bs4Dash::tabItem(
    tabName = "market_trade_data_exporter",
    uiOutput(ns("result_wrapper"))
  )
  
}
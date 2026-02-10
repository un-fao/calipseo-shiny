#artfish_accuracy_server
artfish_accuracy_server <- function(id, parent.session, pool, reloader){
 
 moduleServer(id, function(input, output, session){   
   
  INFO("artfish-accuracy: START")
  MODULE_START_TIME <- Sys.time()
   
  ns<-session$ns
  
  #we call the Accuracy server module from artfishr
  #for now language is passed statically (from appConfig), no need of observe
  artfishr::artfish_shiny_accuracy_server("artfish_toolbox", lang = appConfig$language)
  
  MODULE_END_TIME <- Sys.time()
  INFO("artfish-accuracy: END")
  DEBUG_MODULE_PROCESSING_TIME("Artfish-accuracy", MODULE_START_TIME, MODULE_END_TIME)
  
 })
}
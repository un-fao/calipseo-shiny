#artfish_accuracy_server
artfish_accuracy_server <- function(id, parent.session, lang = NULL, pool, reloader){
 
 moduleServer(id, function(input, output, session){   
  
  ns<-session$ns
    
  INFO("artfish-accuracy: START")
  MODULE_START_TIME <- Sys.time()
   
  #i18n
  #-----------------------------------------------------------------------------
  i18n_translator <- get_reactive_translator(lang)
  i18n <- function(key){ i18n_translator()$t(key) }
  #-----------------------------------------------------------------------------
  
  #we call the Accuracy server module from artfishr
  #for now language is passed statically (from appConfig), no need of observe
  artfishr::artfish_shiny_accuracy_server("artfish_toolbox", lang = appConfig$language)
  
  MODULE_END_TIME <- Sys.time()
  INFO("artfish-accuracy: END")
  DEBUG_MODULE_PROCESSING_TIME("Artfish-accuracy", MODULE_START_TIME, MODULE_END_TIME)
  
 })
}
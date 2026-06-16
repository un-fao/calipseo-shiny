test_internal_server <- function(id, parent.session, lang = NULL, pool, reloader) {
  
  moduleServer(id, function(input, output, session) {
    #just a test internal server
    
    ns <- session$ns
    
    #i18n
    #-----------------------------------------------------------------------------
    i18n_translator <- get_reactive_translator(lang)
    i18n <- function(key){ i18n_translator()$t(key) }
    #-----------------------------------------------------------------------------
    
  })
}
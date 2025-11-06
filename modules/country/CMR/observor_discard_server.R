#observor_discard_server
observor_discard_server <- function(id, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  
    
    ns <- session$ns
    
    output$species_selector<-renderUI({
      
      species<-NULL
      
      selectizeInput(ns("species"),paste0(i18n("SELECT_TITLE_SPECIES")," :"),choices=species,multiple = F,selected=NULL,
                     options = list(
                       placeholder = i18n("SELECT_SPECIES_PLACEHOLDER"),
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      )
    })
    
  })
}

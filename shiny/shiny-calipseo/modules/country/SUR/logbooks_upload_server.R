#logbooks_upload_server
logbooks_upload_server <- function(id, pool) {
  
 moduleServer(id, function(input, output, session){  
  
  ns <- session$ns

  out<-reactiveValues(
    result=NULL,
    errors=NULL,
    referentials=NULL,
    valid=NULL
  )
  
  
  #Validity to file in input
  output$validity_btn<-renderUI({
    if(!is.null(input$file_to_upload)){
      actionButton(ns("check_validity"),i18n("ACTIONBUTTON_LABEL_TEST_VALIDITY"),style = "color:green; background-color:#b8efa0")
    }else{
      disabled(actionButton(ns("check_validity"),i18n("ACTIONBUTTON_LABEL_TEST_VALIDITY")))
    }
  })

  shinyMonitor = function(value,step,max,trip_id){
    shiny::setProgress(value = value, 
                       message = i18n("UPLOAD_PROGRESS_MESSAGE"),
                       detail = sprintf(paste0(i18n("UPLOAD_PROGRESS_MESSAGE_LABEL_TRIPID"),": %s - [%s ",i18n("UPLOAD_PROGRESS_MESSAGE_LABEL_ON")," %s ",i18n("UPLOAD_PROGRESS_MESSAGE_LABEL_TRIPS"),"]"),trip_id,step,max))
  }
  
  #Validity to content
  observeEvent(input$check_validity,{
    file<-input$file_to_upload
    print(file$name)
    
    outt<-shiny::withProgress(
      value = 0,
      min=0,
      max=1,
      message = i18n("UPLOAD_PROGRESS_MESSAGE"),
      detail = "" , 
      convertTripToSQL(file,pool,monitor=shinyMonitor)
    )

    out$result<-outt$result
    out$errors<-outt$errors
    out$referentials<-outt$referentials
    out$valid<-outt$valid
  
    #Display errors, warning and referential to add
    output$validity_result<-renderUI({
      
      req(!is.null(out$errors)&!is.null(out$referentials))
      
      fluidRow(
       box(width=6,title=paste0(i18n("REFERENTIAL_UPDATE_LABEL")," :"),
          if(nrow(out$referentials)>0){
            tagList(
            p(sprintf(paste0(i18n("NUMBER_REFERENTIAL_INSPECT")," : %s"),nrow(subset(out$referentials,type=="WARNING"))),style = "color:orange"),
            p(sprintf(paste0(i18n("NUMBER_REFERENTIAL_UPDATE")," : %s"),nrow(subset(out$referentials,type=="ERROR"))),style = "color:red"),
            DTOutput(ns('referentials'))
            )
          }else{
            p(i18n("NUMBER_REFERENTIAL_UPTODATE"),style = "color:green")
          }
           ),
       box(width=6,title=paste0(i18n("ERROR_IN_DATA_TITLE")," :"),
           if(nrow(out$errors)>0){
             tagList(
             p(sprintf(paste0(i18n("NUMBER_NON_BLOCKING_ISSUES")," : %s"),nrow(subset(out$errors,type=="WARNING"))),style = "color:orange"),
             p(sprintf(paste0(i18n("NUMBER_BLOCKING_ISSUES")," : %s"),nrow(subset(out$errors,type=="ERROR"))),style = "color:red"),
             DTOutput(ns('errors'))
             )
           }else{
             p(i18n("NO_ISSUES_DETECTED"),style = "color:green")
           }
           )
      )
    })
    
    out$referentials$type<-as.factor(out$referentials$type)
    
    output$referentials<-DT::renderDT(server = FALSE, {
      if(nrow(out$referentials)>0){
        DT::datatable(
          out$referentials,
          colnames = c(i18n("REFERENTIAL_TABLE_COLNAME_1"), i18n("REFERENTIAL_TABLE_COLNAME_2"),i18n("REFERENTIAL_TABLE_COLNAME_3"), i18n("REFERENTIAL_TABLE_COLNAME_4")), 
          extensions = c("Buttons"),
          escape = FALSE,
          filter = list(position = 'top',clear =FALSE),
          options = list(
            dom = 'Bfrtip',
            scrollX=TRUE,
            pageLength=5,
            buttons = list(
              list(extend = 'csv', filename =  paste0(i18n("REFERENTIAL_TABLE_DATA_FILENAME"),strsplit(input$file_to_upload$name,".xlsx")[[1]]), title = NULL, header = TRUE)
            ),
            exportOptions = list(
              modifiers = list(page = "all",selected=TRUE)
            ),
            language = list(url = i18n("TABLE_LANGUAGE"))
          )
        )%>%
          formatStyle("type",target = 'row',backgroundColor = styleEqual(c("WARNING","ERROR"), c("#FDEBD0","#F2D7D5")))
      }
    })
  
    out$errors$type<-as.factor(out$errors$type)
    out$errors$category<-as.factor(out$errors$category)
    
    output$errors<-DT::renderDT(server = FALSE, {
      if(nrow(out$errors)>0){
        DT::datatable(
          out$errors,
          colnames = c(i18n("ERROR_TABLE_COLNAME_1"),i18n("ERROR_TABLE_COLNAME_2"),i18n("ERROR_TABLE_COLNAME_3"),i18n("ERROR_TABLE_COLNAME_4"),i18n("ERROR_TABLE_COLNAME_5")), 
          extensions = c("Buttons"),
          escape = FALSE,
          filter = list(position = 'top',clear =FALSE),
          options = list(
            dom = 'Bfrtip',
            scrollX=TRUE,
            pageLength=5,
            buttons = list(
              list(extend = 'csv', filename =  paste0(i18n("ERROR_TABLE_DATA_FILENAME"),strsplit(input$file_to_upload$name,".xlsx")[[1]]), title = NULL, header = TRUE)
            ),
            exportOptions = list(
              modifiers = list(page = "all",selected=TRUE)
            ),
            language = list(url = i18n("TABLE_LANGUAGE"))
          )
        )%>%
          formatStyle("type",target = 'row',backgroundColor = styleEqual(c("WARNING","ERROR"), c("#FDEBD0","#F2D7D5")))
      }
    })
  
    #Generate SQL
    output$generate_SQL_btn<-renderUI({
      if(isTRUE(out$valid)){
        downloadButton(ns("generate_SQL"),i18n("GENERATE_SQL_FILE"))
      }
    })
    
    #Download SQL
    output$generate_SQL <- downloadHandler(
      filename = function(){ 
        paste0(strsplit(input$file_to_upload$name,".xlsx")[[1]],".sql")  },
      content = function(file){
        writeLines(out$result,file)
      }
    )
  })
  
  
  observeEvent(input$file_to_upload, {
    output$validity_result<-renderUI(NULL)
    output$generate_SQL_btn<-renderUI(NULL)
  })

 })
  
}

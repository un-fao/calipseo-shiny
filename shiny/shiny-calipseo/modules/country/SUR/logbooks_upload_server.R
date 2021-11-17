#logbooks_upload_server
logbooks_upload_server <- function(input, output, session, pool) {
  
  ns <- session$ns

  out<-reactiveValues(
    result=NULL,
    errors=NULL,
    referentials=NULL,
    valid=NULL
  )
  
  output$urlPage<-renderUI({
    session$userData$page("logbooks-upload")
    updatePageUrl("logbooks-upload", session)
  })
  
  #Validity to file in input
  output$validity_btn<-renderUI({
    if(!is.null(input$file_to_upload)){
      actionButton(ns("check_validity"),"Test file validity",style = "color:green; background-color:#b8efa0")
    }else{
      disabled(actionButton(ns("check_validity"),"Test file validity"))
    }
  })

  shinyMonitor = function(value,step,max,trip_id){
    shiny::setProgress(value = value, 
                       message = "Dataset validation in progress ... ",
                       detail = sprintf("Trip ID : %s - [%s on %s trips]",trip_id,step,max))
  }
  
  
  #Validity to content
  observeEvent(input$check_validity,{
    file<-input$file_to_upload
    print(file$name)
    
    outt<-shiny::withProgress(
      value = 0,
      min=0,
      max=1,
      message = "Dataset validation in progress ... ",
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
       box(width=6,title="Referential(s) to update :",
          if(nrow(out$referentials)>0){
            tagList(
            p(sprintf("Number of referential(s) to inspect : %s",nrow(subset(out$referentials,type=="WARNING"))),style = "color:orange"),
            p(sprintf("Number of referential(s) to update : %s",nrow(subset(out$referentials,type=="ERROR"))),style = "color:red"),
            DTOutput(ns('referentials'))
            )
          }else{
            p("All referentials used in this dataset are up-to-date",style = "color:green")
          }
           ),
       box(width=6,title="Error(s) in data :",
           if(nrow(out$errors)>0){
             tagList(
             p(sprintf("Number of non-blocking issue(s) : %s",nrow(subset(out$errors,type=="WARNING"))),style = "color:orange"),
             p(sprintf("Number of blocking issue(s) : %s",nrow(subset(out$errors,type=="ERROR"))),style = "color:red"),
             DTOutput(ns('errors'))
             )
           }else{
             p("No issues detected in this dataset",style = "color:green")
           }
           )
      )
    })
    
    out$referentials$type<-as.factor(out$referentials$type)
    
    output$referentials<-DT::renderDT(server = FALSE, {
      if(nrow(out$referentials)>0){
        DT::datatable(
          out$referentials,
          colnames = c('Table', 'Value','Issue Level', 'Description'), 
          extensions = c("Buttons"),
          escape = FALSE,
          filter = list(position = 'top',clear =FALSE),
          options = list(
            dom = 'Bfrtip',
            scrollX=TRUE,
            pageLength=5,
            buttons = list(
              list(extend = 'csv', filename =  paste0("ref_to_add_",strsplit(input$file_to_upload$name,".xlsx")[[1]]), title = NULL, header = TRUE)
            ),
            exportOptions = list(
              modifiers = list(page = "all",selected=TRUE)
            )
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
          colnames = c('Trip ID', 'Vessel Registration', 'Issue Level','Issue Domain','Description'), 
          extensions = c("Buttons"),
          escape = FALSE,
          filter = list(position = 'top',clear =FALSE),
          options = list(
            dom = 'Bfrtip',
            scrollX=TRUE,
            pageLength=5,
            buttons = list(
              list(extend = 'csv', filename =  paste0("errors_",strsplit(input$file_to_upload$name,".xlsx")[[1]]), title = NULL, header = TRUE)
            ),
            exportOptions = list(
              modifiers = list(page = "all",selected=TRUE)
            )
          )
        )%>%
          formatStyle("type",target = 'row',backgroundColor = styleEqual(c("WARNING","ERROR"), c("#FDEBD0","#F2D7D5")))
      }
    })
  
    #Generate SQL
    output$generate_SQL_btn<-renderUI({
      if(isTRUE(out$valid)){
        downloadButton(ns("generate_SQL"),"Generate SQL file")
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

}

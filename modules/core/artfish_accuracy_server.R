#artfish_accuracy_server
artfish_accuracy_server <- function(id, pool){
 
 moduleServer(id, function(input, output, session){   
   
  ns<-session$ns
  
  
  output$button<-renderUI({
    if(!is.na(input$days)&!is.na(input$boats)&!is.na(input$effort_smp)&!is.na(input$effort_days_smp)&!is.na(input$landing_smp)&!is.na(input$landing_days_smp)){
    actionButton(ns("run"),i18n("ACTIONBUTTON_COMPUTE_LABEL"))}else{NULL}
  })
  
  iconChoice<-function(x){ifelse(x<0.9,"exclamation-triangle","check-circle")}
  colorChoice<-function(x){ifelse(x<0.9,"orange","green")}
  
  observeEvent(input$run,{
    pop<-input$boats*input$days
    SAE<-artfish_accuracy(n=input$effort_smp,N=pop,method="higher")
    TAE<-1
    SAC<-artfish_accuracy(n=input$landing_smp,N=pop,method="higher")
    TAC<-artfish_accuracy(n=input$landing_days_smp,N=input$days,method="higher")
    OAC<-min(SAE,TAE,SAC,TAC)
    output$result<-renderUI({
      tagList(
        fluidRow(
          valueBox(paste(format(round(SAE*100,1), nsmall = 1),"%"),i18n("VALUEBOX_TITLE_SPATIAL_ACCURACY_EFFORT"),width = 3,icon=icon(iconChoice(SAE)),color=colorChoice(SAE)),
          valueBox(paste(format(round(TAE*100,1), nsmall = 1),"%"),i18n("VALUEBOX_TITLE_TEMPORAL_ACCURACY_EFFORT"),width= 3,icon=icon(iconChoice(TAE)),color=colorChoice(TAE)),
          valueBox(paste(format(round(SAC*100,1), nsmall = 1),"%"),i18n("VALUEBOX_TITLE_SPATIAL_ACCURACY_CATCH"),width=3,icon=icon(iconChoice(SAC)),color=colorChoice(SAC)),
          valueBox(paste(format(round(TAC*100,1), nsmall = 1),"%"),i18n("VALUEBOX_TITLE_TEMPORAL_ACCURACY_CATCH"),width=3,icon=icon(iconChoice(TAC)),color=colorChoice(TAC))
        ),
        fluidRow(
          column(8,offset=4,valueBox(paste(format(round(OAC*100,1), nsmall = 1),"%"),i18n("VALUEBOX_TITLE_OVERALL_ACCURACY"),width=4,icon=icon(iconChoice(OAC)),color=colorChoice(OAC)))
        )
      )
    })
  })
  
  reactiveData<- reactiveVal()
  Day1 <- paste0(i18n("DAY_LABEL"),'1')
  table<-data.table(Day1=1)
  row.names(table)<- i18n("SAMPLES_LABEL")
  reactiveData(table)
  
  observeEvent(input$table_cell_edit, {
    info = input$table_cell_edit
    newData <- reactiveData()
    newData[info$row, info$col] <- info$value
    reactiveData(newData)
  })
  
  observeEvent(input$addColumn,{
    newData <- reactiveData()
    newData[[paste0(i18n("DAY_LABEL"),ncol(newData)+1)]] <- 1
    reactiveData(newData)
  })
  
  output$table<-renderDT(server = FALSE,{
    DT::datatable(
      reactiveData(),
      escape = FALSE,
      rownames = TRUE,
      selection = 'none',
      editable = 'cell',
      options = list(
        pageLength = 1, dom = 't', 
        autoWidth = TRUE,
        language = list(url = i18n("TABLE_LANGUAGE"))
      )
    )
  })
  
  observeEvent(input$compute,{
    newData <- reactiveData()
    
    samples<-as.numeric(newData[1,])
    nbdays<-length(samples)
    ratio<-ifelse(samples/mean(samples)>1,1,samples/mean(samples))
    index<-round(sum(ratio),0)/nbdays

     output$index<-renderUI({
       tagList(
          valueBox(index,i18n("VALUEBOX_TITLE_UNIFORMITY_INDEX"),icon=icon(ifelse(index<0.6,"exclamation-triangle","check-circle")),color=ifelse(icon<0.6,"orange","green"),width = 3)
       )
     })
  })
  
 })
}
#artisanal_validation_server
artisanal_validation_server <- function(id, parent.session, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  

    ns <- session$ns
    
    #Initialize object
    out<-reactiveValues(
      data=NULL,
      result=NULL,
      errors=NULL,
      referentials=NULL,
      valid=NULL,
      message=NULL
    )
    
    required_cols <- c(
      "identifier", "_validation_status", "recorder_name",
      "recording_date", "landing_site_name", "arrival_district",
      "vessel_registration_nr", "vessel_name", "fishing_unit_name",
      "gear_name", "fyke_type", "trip_nr",
      "departure_date", "arrival_date", "trip_type",
      "trip_duration_days", "time_spent_fishing", "unit_time_spent_fishing",
      "time_spent_fishing_days", "boat_activity_days", "species_code",
      "species_name_local", "species_name_english", "species_name_scientific",
      "processing_name", "fish_product_code", "landed_weight",
      "unit_landed_weight", "landed_weight_kg", "catch_price",
      "currency_catch_price", "nr_fishers", "food_cost",
      "currency_food_cost", "fuel_consumed", "unit_volume_fuel_consumed",
      "fuel_consumed_liter", "fuel_type", "fuel_price",
      "currency_fuel_price", "unit_volume_fuel_price", "ice_bought",
      "unit_volume_ice_bought", "ice_bought_m3", "ice_price",
      "currency_ice_price", "unit_volume_ice_price", "departure_site_name",
      "departure_district", "fishing_zone_name", "notes"
    )
    
    go_visualisation<-reactiveVal(FALSE)
    
    #File input
    output$file_input_wrapper<-renderUI({
      fileInput(inputId = ns("file_to_validate"), label = paste0(i18n("LOGBOOK_VALIDATION_FILEINPUT_LABEL_TITLE"),":"),multiple = FALSE,accept = c(".xlsx"))
    })
    
    #Run validity test
    output$validity_btn<-renderUI({
      req(input$file_to_validate)
      req(!is.null(input$file_to_validate))
        actionButton(ns("check_validity"),i18n("ACTIONBUTTON_LABEL_TEST_VALIDITY"),style = "color:green; background-color:#b8efa0")
    })
    
    #Display validity test status
     shinyMonitor = function(value,message){
       shiny::incProgress(amount = value, 
                          message = i18n("VALIDATION_PROGRESS_MESSAGE"),
                          detail = message)
     }
    
    #Update object with validity test results
    observeEvent(input$check_validity,{
      req(input$file_to_validate)
      shinyjs::disable(ns("check_validity"))
      file<-input$file_to_validate
      print(file$name)
      
      ext <- tools::file_ext(file$name)
      
      if (!tolower(ext) %in% c("xlsx", "xls")) {
        
        showModal(modalDialog(
          title = i18n("MODAL_NO_EXCEL"),
          i18n("MODAL_NO_EXCEL_MESSAGE"),
          easyClose = TRUE
        ))
        
        return(NULL)
      }
      
      sheets <- excel_sheets(file$datapath)
      
      if (!"catch data" %in% sheets) {
        
        showModal(modalDialog(
          title = i18n("MODAL_NO_SHEET"),
          paste0(
            i18n("MODAL_NO_SHEET_MESSAGE"),
            paste(sheets, collapse = ", ")
          ),
          easyClose = TRUE
        ))
        
        return(NULL)
      }
      
      
      df_header <- read_excel(file$datapath, sheet = "catch data",n_max=0)
      
      missing_cols <- setdiff(required_cols, names(df_header))
      
      if (length(missing_cols) > 0) {
        
        showModal(modalDialog(
          title = i18n("MODAL_NO_COLUMN"),
          tagList(
            i18n("MODAL_NO_COLUMN_MESSAGE"),
            tags$ul(lapply(missing_cols, tags$li))
          ),
          easyClose = TRUE
        ))
        
        return(NULL)
      }

       outt<-shiny::withProgress(
         value = 0,
         min=0,
         max=1,
         message = i18n("VALIDATION_PROGRESS_MESSAGE"),
         detail = "" , 
         validateArtisanalFile(file,pool,monitor=shinyMonitor)
       )
      
      if(!is.null(outt$valid)){
        out$data<-outt$data
        out$result<-outt$result
        out$errors<-outt$errors
        out$referentials<-outt$referentials
        out$valid<-outt$valid
        out$message<-outt$message
      }
      go_visualisation(TRUE)
      shinyjs::enable(ns("check_validity"))
    })
      
    #Display validity test results
    observeEvent(go_visualisation(),{
      req(isTRUE(go_visualisation()))
      req(!is.null(out$valid))
        #Display errors, warning and referential to add
        output$validity_result<-renderUI({
          req(!is.null(out$errors)&!is.null(out$referentials))
          fluidRow(
            bs4Dash::box(width=6,title=paste0(i18n("REFERENTIAL_UPDATE_LABEL")," :"),
                if(nrow(out$referentials)>0){
                  tagList(
                    p(sprintf(paste0(i18n("NUMBER_REFERENTIAL_INSPECT")," : %s"),nrow(subset(out$referentials,type=="duplicate"))),style = "color:orange"),
                    p(sprintf(paste0(i18n("NUMBER_REFERENTIAL_UPDATE")," : %s"),nrow(subset(out$referentials,type=="missing"))),style = "color:red"),
                    DTOutput(ns('referentials'))
                  )
                }else{
                  p(i18n("NUMBER_REFERENTIAL_UPTODATE"),style = "color:green")
                }
            ),
            bs4Dash::box(width=6,title=paste0(i18n("ERROR_IN_DATA_TITLE")," :"),
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
                  list(extend = 'csv', filename =  paste0(i18n("REFERENTIAL_TABLE_DATA_FILENAME"),strsplit(input$file_to_validate$name,".xlsx")[[1]]), title = NULL, header = TRUE)
                ),
                exportOptions = list(
                  modifiers = list(page = "all",selected=TRUE)
                ),
                language = list(url = i18n("TABLE_LANGUAGE"))
              )
            )%>%
              formatStyle("type",target = 'row',backgroundColor = styleEqual(c("duplicate","missing"), c("#FDEBD0","#F2D7D5")))
          }
        })

        out$errors$type<-as.factor(out$errors$type)
        out$errors$category<-as.factor(out$errors$category)

        output$errors<-DT::renderDT(server = FALSE, {
          if(nrow(out$errors)>0){
            DT::datatable(
              subset(out$errors,type!="VALID"),
              colnames = c(i18n("ERROR_TABLE_COLNAME_1"),i18n("ERROR_TABLE_COLNAME_2"),i18n("ERROR_TABLE_COLNAME_3"),i18n("ERROR_TABLE_COLNAME_4"),i18n("ERROR_TABLE_COLNAME_5")),
              extensions = c("Buttons"),
              escape = FALSE,
              filter = list(position = 'top',clear =FALSE),
              options = list(
                dom = 'Bfrtip',
                scrollX=TRUE,
                pageLength=5,
                buttons = list(
                  list(extend = 'csv', filename =  paste0(i18n("ERROR_TABLE_DATA_FILENAME"),strsplit(input$file_to_validate$name,".xlsx")[[1]]), title = NULL, header = TRUE)
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
            paste0(strsplit(input$file_to_validate$name,".xlsx")[[1]],".sql")  },
          content = function(file){
            writeLines(out$result,file)
          }
        )
        
        #Generate error report
        output$generate_report_btn<-renderUI({
          req(isTRUE(go_visualisation()))
            downloadButton(ns("generate_report"),i18n("GENERATE_REPORT_FILE"))
        })
        
        #Download error report
        output$generate_report <- downloadHandler(
          filename = function(){
            paste0(strsplit(input$file_to_validate$name,".xlsx")[[1]],"_report.xlsx")  },
          content = function(file){
            waiter::waiter_show(
              html = waiter::spin_fading_circles(),
              color = "rgba(0, 0, 0, 0.4)"
            )
            reportArtisanalFile(out$data,out$errors,out$referentials,file)
            waiter::waiter_hide()
          }
        )
        
        #Dispay SQL content summary
        showModal(
          modalDialog(
            title = i18n("DATA_STATUS"),
            easyClose = TRUE,
            footer = modalButton("OK"),
            tagList(
              shiny::HTML(markdown::markdownToHTML(text = out$message, fragment.only = TRUE))
            )
          )
        )

    })


    observeEvent(input$file_to_validate, {
      output$validity_result<-renderUI(NULL)
      output$generate_SQL_btn<-renderUI(NULL)
      output$generate_reprt_btn<-renderUI(NULL)
      go_visualisation(FALSE)
    })
    
    
  })
  
  
}

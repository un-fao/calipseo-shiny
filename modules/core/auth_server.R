#authLoginServer
authLoginServer <- function (id, config, log_out = shiny::reactiveVal(), reload_on_logout = FALSE) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    credentials <- shiny::reactiveValues(
      user_auth = FALSE, 
      user_info = NULL
    )
    shiny::observeEvent(log_out(), {
      if (reload_on_logout) {
        session$reload()
      }else{
        shiny::updateTextInput(session, "auth_password", value = "")
        credentials$user_auth <- FALSE
        credentials$user_info <- NULL
      }
    })
    shiny::observe({
      shinyjs::toggle(id = "panel", condition = !credentials$user_auth)
    })
    shiny::observeEvent(input$auth_button_login,{
      
       shinyjs::disable("auth_button_login")
       auth_endpoint = sprintf("%s/jwt-auth", config$auth_endpoint)
       INFO("Using authentication endpoint '%s'", auth_endpoint)
       print(input$auth_username)
       print(input$auth_password)
       auth_req <- httr::POST(
         url = auth_endpoint, 
         body = list(
           username = input$auth_username, 
           password = input$auth_password
         ),
         encode = "json"
       )
       if(httr::status_code(auth_req)==200) {
         auth_content <- httr::content(auth_req) 
         credentials$user_auth <- TRUE
         credentials$auth_info <- list(
           user = input$auth_username,
           pwd = input$auth_password,
           token = auth_content$jwttoken,
           roles = auth_content$givenRoles
         )
       }else{
         credentials$user_auth <- FALSE
         credentials$auth_info <- list(
           user = character(0),
           pwd = character(0),
           token = character(0),
           roles = list()
         )
       }
       shinyjs::enable("auth_button_login")
      
       if(!credentials$user_auth){
        shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
        shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
       }
    })
    shiny::reactive({
      shiny::reactiveValuesToList(credentials)
    })
  })
}
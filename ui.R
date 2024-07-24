# Define UI for application
#==========================================================================================
ui <- shiny::tagList(
  dashboardPage(
    dashboardHeader(
      title = tags$div(
        tags$a(
          href=appConfig$country_profile$website,
          HTML(createBase64Image(src = appConfig$country_profile$logo, width = '50px', alt = 'Logo'))
        ),
        tags$span("Calipseo Dashboard", style = "font-size:80%;")
      ),
      tags$li(
        class = "dropdown",
        style = "padding: 8px;",
        shinyauthr::logoutUI("logout", icon = icon("sign-out", lib = "font-awesome"))
      )
    ),
    dashboardSidebar(
      collapsed = FALSE,
      sidebarMenuFromModules(appConfig)
      #withSpinner(uiOutput("side_ui"))
    ),
    dashboardBody(
      tags$head(
        #tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.rtlcss.com/bootstrap/v4.5.3/css/bootstrap.min.css"),
        #tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.rtl.min.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "shiny-calipseo.css"),
        tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/css/select2.min.css"),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/js/select2.min.js")
      ),
      if(appConfig$auth) authLoginUI(
        id = "login",
        config = appConfig,
        cookie_expiry = if(!is.null(appConfig$auth_cookie_expiry)){appConfig$auth_cookie_expiry}else{7}, 
        additional_ui = tags$div(
          HTML(appConfig$auth_footer)
        )
      ),
      loadModuleUIs(appConfig),
      #withSpinner(uiOutput("main_ui")),
      useShinyjs()
    )
  ),
  tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
)
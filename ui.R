# Define UI for application
#==========================================================================================
ui <- shiny::tagList(
  bs4Dash::dashboardPage(
    bs4Dash::dashboardHeader(
      title = tags$div(
        tags$a(
          href=appConfig$country_profile$website,
          HTML(createBase64Image(src = appConfig$country_profile$logo, width = '60px', alt = 'Logo')),
          style = "margin: 8px !important;"
        ),
        tags$span("Calipseo", style = "font-weight:bold;")
      )
    ),
    bs4Dash::dashboardSidebar(
      collapsed = FALSE,
      sidebarMenuFromModules(appConfig)
    ),
    bs4Dash::dashboardBody(
      tags$head(
        #tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.rtlcss.com/bootstrap/v4.5.3/css/bootstrap.min.css"),
        #tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.rtl.min.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "shiny-calipseo.css"),
        tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/css/select2.min.css"),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/js/select2.min.js")
      ),
      loadModuleUIs(appConfig),
      shinyjs::useShinyjs()
    )
  ),
  tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
)
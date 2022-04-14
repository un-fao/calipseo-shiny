# Define UI for application
#==========================================================================================
ui <- shiny::tagList(
  dashboardPage(
    dashboardHeader(title = dbHeader),
    dashboardSidebar(
      sidebarMenuFromModules(appConfig)
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "shiny-calipseo.css"),
        tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/css/select2.min.css"),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.13/js/select2.min.js")
      ),
      loadModuleUIs(appConfig),
      useShinyjs()
    )
  ),
  tags$footer(footer(getAppId(), getAppVersion(), getAppDate()), align = "center")
)
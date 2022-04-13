# Define UI for application that draws a histogram
#==========================================================================================
ui <- shiny::tagList(
  dashboardPage(
    dashboardHeader(
      title = tags$div(
        tags$a(
          href=appConfig$country_profile$website,
          HTML(createBase64Image(src = appConfig$country_profile$logo, width = '100', height = '50', alt = 'Logo'))
        ),
        tags$span("Calipseo Dashboard", style = "font-size:80%;")
      )
    ),
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
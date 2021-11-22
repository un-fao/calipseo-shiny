# Define UI for application that draws a histogram
#==========================================================================================
ui <- dashboardPage(
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
      tags$link(rel = "stylesheet", type = "text/css", href = "shiny-calipseo.css")
    ),
    loadModuleUIs(appConfig),
    useShinyjs()
  )
  
)
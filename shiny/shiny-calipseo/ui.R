# Define UI for application that draws a histogram
#==========================================================================================
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$a(
        href=appConfig$country_profile$website,
        tags$img(src=imgToBase64(appConfig$country_profile$logo),height='50',width='100')
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
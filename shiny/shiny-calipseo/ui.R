# Define UI for application that draws a histogram
#==========================================================================================
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$a(
        href=appConfig$country$website,
        tags$img(src=imgToBase64(appConfig$country$logo),height='50',width='100')
      ),
      tags$span("Calipseo Dashboard", style = "font-size:80%;")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "calipseo-tabs",
      #Home
      menuItem(text = "Home", tabName = "home"),
      #Vessels
      menuItem(
        text = "Vessels", tabName = "vessels",
        menuSubItem("List of vessels", tabName = "vessel_list", icon = icon("table")),
        menuSubItem("Vessel details", tabName = "vessel_info")
        #menuSubItem("Repartition of Vessels", tabName = "vessel_repartition", icon = icon("map-marker")),
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "shiny-calipseo.css")
    ),
    tabItems(
      homeUI("home"),
      vesselListUI("vessel_list"),
      vesselInfoUI("vessel_info")
    ),
    useShinyjs()
  )
  
)
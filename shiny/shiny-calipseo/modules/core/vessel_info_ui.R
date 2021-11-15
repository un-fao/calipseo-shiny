vessel_info_ui <- function(id){
  
  
  ns <- NS(id)
  
  tabItem(tabName = "vessel_info",
          
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "vertical_tab.css")
          ),
          
          
          tags$head(HTML('<script type="text/javascript">
  function openvrttab(evt, VrttabName) {
  
  var i, vrttabcontent, tablinks;

  
  vrttabcontent = document.getElementsByClassName("vrttabcontent");
  for (i = 0; i < vrttabcontent.length; i++) {
  vrttabcontent[i].style.display = "none";
  }

  
  tablinks = document.getElementsByClassName("tablinks");
  for (i = 0; i < tablinks.length; i++) {
    tablinks[i].className = tablinks[i].className.replace(" active", "");
  }
  
  document.getElementById(VrttabName).style.display = "block";
  evt.currentTarget.className += " active";
  
  
}
</script>')),
          
 
          
          fluidRow(width=12,
                   
                   htmlOutput(ns("vessel_header")),
                   column(width = 5,
                          
                          fluidRow(
                            
                            column(width = 4, style='padding-left: 15px;',
                                   
                                   
                                   tags$img(src='placeholder_pic.png',width='160px') #TODO
                                   
                            ),
                            
                            column(width = 6,offset = 2,
                                   
                                   fluidRow(
                                     tabsetPanel(
                                       
                                       tabPanel('Info',
                                                uiOutput(ns("vessel_description"),style = "font-weight:bold;")),
                                       tabPanel('Registration',uiOutput(ns("vessel_registration"),style = "font-weight:bold;"))
                                     )
                                     
                                   ))
                            
                            
                          )
                          
                   ),
                   
                   column(width = 6, offset = 1,
                          
                          '#TODO'
                   )
                   
                   
          ),br(),
          
          fluidRow(width=12,
                   
                   tags$div(class='vrt-tab',
                            tags$button(class='tablinks', onclick='openvrttab(event, "Ownership" )',("Ownership")),
                            tags$button(class='tablinks', onclick='openvrttab(event, "Licenses" )',("Licenses")),
                            tags$button(class='tablinks', onclick='openvrttab(event, "Catches" )',("Catches"))),
                   
                   
                   tags$div(id="Ownership", class="vrttabcontent", DT::dataTableOutput(ns("vessel_owners"))),
                   tags$div(id="Licenses", class="vrttabcontent",style='display:none;','#TODO'),
                   tags$div(id="Catches", class="vrttabcontent",style='display:none;',
                            tabsetPanel(selected = 'Summary',
                              tabPanel('Summary',DT::dataTableOutput(ns("vessel_catch_summary"))),
                              tabPanel('History', DT::dataTableOutput(ns("vessel_catch_history")),htmlOutput(ns("vessel_catch_datasource")))
                            ))
                  
                   
            
          )
          
          
  )
}
#observor_biological_server
observor_biological_server <- function(id, pool, reloader) {
  
  moduleServer(id, function(input, output, session){  
    
    ns <- session$ns
    
    output$species_selector<-renderUI({
      
      species<-NULL
      
      selectizeInput(ns("species"),paste0(i18n("SELECT_TITLE_SPECIES")," :"),choices=species,multiple = F,selected=NULL,
                     options = list(
                       placeholder = i18n("SELECT_SPECIES_PLACEHOLDER"),
                       onInitialize = I('function() { this.setValue(""); }')
                     )
      )
    })
    
    
    output$correlationPlot <- renderPlotly({
      print("BUG1")
    set.seed(123)
    n <- 50
    length_cm <- runif(n, 20, 100)   
    weight_g <- 0.01 * length_cm^3 + rnorm(n, sd = 200) 
    
    df <- tibble(
      Length = length_cm,
      Weight = pmax(weight_g, 0)  
    )
    
    df<-df%>% filter(!is.na(Weight), Weight > 0, Length > 0)
    
    fit <- lm(log(Weight) ~ log(Length), data = df)
    
    grid <- tibble(Length = seq(min(df$Length), max(df$Length), length.out = 100))
    grid$Weight_pred <- exp(predict(fit, newdata = grid))
    
    plot_ly(df, x = ~Length, y = ~Weight,
                   type = "scatter", mode = "markers",
                   name = "Sample", marker = list(color = "blue")) %>%
      add_trace(x = grid$Length, y = grid$Weight_pred,
                type = "scatter", mode = "lines",
                name = "Regression log-log",
                line = list(color = "red")) %>%
      layout(
        title = "Relation Length-Weight",
        xaxis = list(title = "Length (cm)"),
        yaxis = list(title = "Weight (g)", type = "log"),  
        legend = list(x = 0.1, y = 0.9)
      )
    
    })
    
  })
}

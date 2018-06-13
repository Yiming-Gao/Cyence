#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# Note that global.R will be loaded before either ui.R or server.R

setwd("/Users/ygao/Desktop/Dashboard/Code/try")
options(scipen = 999)

# Fetch the data
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3.score where run_date >= '", "2018-06-01", "' limit 10")
scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
scores$cyence_id <- as.character(scores$cyence_id)
# scores <- scores[order(cyence_id, run_date)]
dbDisconnect(con_postgresql)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$plot <- renderPlot({

    # Render plots based on input$variable from ui.R
    if (input$var == "cy") {
      ggplot(scores, aes(x = cy)) + geom_histogram(bins = 50, fill = I("blue"), col = I("red"), alpha = I(.2)) + scale_x_log10() + theme_minimal()
    }
    if (input$var == "sus") {
      ggplot(scores, aes(x = sus)) + geom_histogram(bins = 50, fill = I("blue"), col = I("red"), alpha = I(.2)) + scale_x_log10() + theme_minimal()
    }
    if (input$var == "mo") {
      ggplot(scores, aes(x = mo)) + geom_histogram(bins = 50, fill = I("blue"), col = I("red"), alpha = I(.2)) + scale_x_log10() + theme_minimal()
    }
  })
  
})

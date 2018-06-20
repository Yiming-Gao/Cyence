#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Left join scores& Company Info by cyence_id
ex_company <- ex_company[ex_company$cyence_sector == input$sector, ]
ex_scores <- ex_scores[ex_scores$cyence_sector == input$sector, ]
ex_company_scores <- inner_join(ex_company, ex_scores, by = c("cyence_id" = "cyence_id"))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Renter plot 
  output$top15 <- renderPlotly({
    
    top_unique = ex_company_scores %>%
      group_by(cyence_id) %>%
      mutate(cy_change = cy - lag(cy)) %>%
      filter(run_date == rundate) %>%
      arrange(desc(cy_change)) %>%
      distinct(cyence_id)
    
    
    ex_company_scores = ex_company_scores[ex_company_scores$cyence_id %in% as.character(top_unique[1:15,]$cyence_id), ]
    
    # basic time series
    ex_company_scores_plot <- ex_company_scores[ex_company_scores$cyence_id == as.character(top_unique[1:15,]$cyence_id)[1], ]
    ex_company_scores_plot <- melt(ex_company_scores_plot[, c("run_date", "cy", "sus", "mo")], id = "run_date")
    p <- ggplot(ex_company_scores_plot, aes(x = run_date, y = value, colour = variable)) + 
      geom_line() +
      (scale_x_date(breaks = date_breaks("1 month"),
                    labels = date_format("%b %y"))) +
      xlab("Run Date") + ylab("Score") + 
      labs(title = paste0("Cyence Scores for ", unique(ex_company_scores[ex_company_scores$cyence_id == as.character(top_unique[1:15,]$cyence_id)[1], ]$company_name)),
           color = "Type") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        text = element_text(size = 12)
      ) +
      theme_minimal() 
    
    ggplotly(p)
    
  })
  
})

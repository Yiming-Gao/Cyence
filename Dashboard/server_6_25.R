#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
# Note that global.R will be loaded before either ui.R or server.R

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Output 1
  output$map <- renderLeaflet({
    
    # read data
    country_scores_avg = read.csv("country_score_avg.csv")
    
    # Map by country
    countries@data <- right_join(country_scores_avg, countries@data, by = c("country" = "NAME"))
    # summary(countries$NAME)
    
    map <- leaflet(countries) %>% addTiles() %>% setView(lat = 46, lng = -41, zoom = 3)
    pal <- colorNumeric(
      palette = "YlOrBr",
      domain = countries$country_cy_avg,
      na.color = "#f2f0ea"
    )
    
    # Prepare the text for the popup message
    mytext <- 
      paste("<b>Country: ", countries$country,"</b><br/>", 
            "Number of Companies: ", countries$n_companies, "<br/>",
            "<b>Cyence Score: ", round(countries$country_cy_avg, 2), "</b><br/>", 
            "Susceptibility: ", round(countries$country_sus_avg, 2), "<br/>",
            "Motivation: ", round(countries$country_mo_avg,2)) %>%
      lapply(htmltools::HTML)
    
    # Display
    map %>%
      addPolygons(data = countries, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.9, color = "white", weight = 0.3,
                  fillColor = ~pal(countries$country_cy_avg),
                  label = mytext,
                  labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>%
      addLegend("bottomright", pal = pal, values = countries$country_cy_avg,
                title = "Average Cyence Score",
                opacity = 1
      )
  })
  
  # Output 2
  output$heatmap <- renderPlotly({
    
    # read data
    sector_scores_avg = read.csv("sector_scores_avg.csv")
    
    # basic heatmap
    p <- ggplot(sector_scores_avg, aes(revenue_bins, cyence_sector,
                                       # customize the tooltip
                                       text = paste("Sector: ", cyence_sector, "\n",
                                                    "Number of Companies: ", n_companies, "\n",
                                                    "Average score: ", round(sector_cy_avg, 2), "\n",
                                                    "Change in score: ", round(cy_change, 3)))) + 
      geom_tile(aes(fill = cy_change), colour = "white") + 
      scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 0, limits = range(sector_scores_avg$cy_change), name = "Change in Score") +
      labs(title = paste0("Change in Cyence Score from \n", month.name[month(rundate)-1], " to ", month.name[month(rundate)])) +
      xlab("Revenue Bins") + ylab("Cyence Sector") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        text = element_text(size = 12)
      ) 
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  
  # Output 3
  output$text_revenue_bin <- renderPrint({
    cat("Ten companies with greatest score change in ", input$sector, " with revenue ", input$revenue_bin, ".")
  })
  
  
  
  # Output 4
  output$top10 <- renderPlotly({
    
    ex_company_scores <- read.csv(paste(input$sector, ".csv", sep = ""))
    ex_company_scores <- ex_company_scores[ex_company_scores$revenue_bins == input$revenue_bin, ]
    top_unique <- unique(ex_company_scores$cyence_id)
    
    myplots <- list()
    
    # for loop
    for (i in 1:10) {
      ex_company_scores_plot <- ex_company_scores[ex_company_scores$cyence_id == top_unique[i], ]
      ex_company_scores_plot <- melt(ex_company_scores_plot[, c("run_date", "cy", "sus", "mo")], id = "run_date")
      # ex_company_scores_plot <- merge(ex_company_scores_plot, data.frame(run_date = seq(rundate-months(5), by = "month", length.out = 6), cy = rep(NA, 6), mo = rep(NA, 6), sus = rep(NA, 6)), all = TRUE) %>% 
      #   group_by(run_date) %>% 
      #   filter(!duplicated(run_date) & !is.na(run_date))
      if (i == 1) {
        p <- plot_ly(ex_company_scores_plot, x = ~run_date, y = ~round(value, 2), type = 'scatter', mode = 'lines+markers', color = ~variable) %>%
          layout(xaxis = list(title = "Run Date", tick0 = rundate-months(5), dtick = "M1"), yaxis = list(title = "Scores"),
                 title = unique(ex_company_scores[ex_company_scores$cyence_id == top_unique[i], ]$company_name))
      }
      
      else {
        p <- plot_ly(ex_company_scores_plot, x = ~run_date, y = ~round(value, 2), type = 'scatter', mode = 'lines+markers', color = ~variable) %>%
          layout(xaxis = list(title = "Run Date", tick0 = rundate-months(5), dtick = "M1"), yaxis = list(title = "Scores"),
                 title = unique(ex_company_scores[ex_company_scores$cyence_id == top_unique[i], ]$company_name),
                 showlegend = F)
      }
        myplots[[i]] <- p
    }
    
    subplot(myplots[[1]], myplots[[2]], myplots[[3]], myplots[[4]], myplots[[5]],
            myplots[[6]], myplots[[7]], myplots[[8]], myplots[[9]], myplots[[10]],
            nrows = 5)
  })
  
})

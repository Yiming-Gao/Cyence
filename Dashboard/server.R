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
    # Map by country
    countries <- readOGR(dsn = getwd() , layer = "TM_WORLD_BORDERS_SIMPL-0.3") # https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet/
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
  output$top10 <- renderPlotly({
    
    # Left join scores& Company Info by cyence_id
    ex_company <- ex_company[ex_company$cyence_sector == input$sector, ]
    ex_company$cyence_id <- as.character(ex_company$cyence_id)
    ex_scores <- ex_scores[ex_scores$cyence_id %in% ex_company$cyence_id, ]
    ex_company_scores <- inner_join(ex_company, ex_scores, by = c("cyence_id" = "cyence_id"))
    ex_company_scores$run_date <- as.Date(ex_company_scores$run_date)
    
    top_unique = ex_company_scores %>%
      group_by(cyence_id) %>%
      mutate(cy_change = cy - lag(cy)) %>%
      filter(run_date == rundate) %>%
      arrange(desc(cy_change)) %>%
      distinct(cyence_id)
    
    
    ex_company_scores = ex_company_scores[ex_company_scores$cyence_id %in% as.character(top_unique[1:10,]$cyence_id), ]
    
    myplots <- list()
    
    # for loop
    for (i in 1:10) {
      ex_company_scores_plot <- ex_company_scores[ex_company_scores$cyence_id == as.character(top_unique[1:10,]$cyence_id)[i], ] 
      ex_company_scores_plot <- melt(ex_company_scores_plot[, c("run_date", "cy", "sus", "mo")], id = "run_date")
      ex_company_scores_plot$main1 =  paste0("Cyence Scores for ", unique(ex_company_scores[ex_company_scores$cyence_id == as.character(top_unique[1:10,]$cyence_id)[i], ]$company_name))
      
      p <- ggplot(ex_company_scores_plot, aes(x = run_date, y = value, colour = variable)) + 
        geom_line() +
        (scale_x_date(breaks = date_breaks("1 month"),
                      labels = date_format("%b %y"),
                      limits = as.Date(c(rundate-months(5), rundate-months(0))))) +
        xlab("Run Date") + ylab("Score") + 
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          text = element_text(size = 12),
          legend.position="none"
        ) +
        facet_wrap(~main1) + 
        theme_minimal() +
        geom_point(show.legend = FALSE) +
        guides(colour = FALSE)
      
      myplots[[i]] <- p
    }
    
    subplot(ggplotly(myplots[[1]]) %>% layout(legend = list(x = 0.1, y = 0.9)), 
            ggplotly(myplots[[2]]) %>% layout(legend = list(x = 0.1, y = 0.9)), 
            ggplotly(myplots[[3]]) %>% layout(legend = list(x = 0.1, y = 0.9)), 
            ggplotly(myplots[[4]]) %>% layout(legend = list(x = 0.1, y = 0.9)), 
            ggplotly(myplots[[5]]) %>% layout(legend = list(x = 0.1, y = 0.9)),
            ggplotly(myplots[[6]]) %>% layout(legend = list(x = 0.1, y = 0.9)), 
            ggplotly(myplots[[7]]) %>% layout(legend = list(x = 0.1, y = 0.9)), 
            ggplotly(myplots[[8]]) %>% layout(legend = list(x = 0.1, y = 0.9)), 
            ggplotly(myplots[[9]]) %>% layout(legend = list(x = 0.1, y = 0.9)), 
            ggplotly(myplots[[10]]) %>% layout(legend = list(x = 0.1, y = 0.9)),
            nrows = 5) %>% layout(title = "Companies with most changes in this month", showlegend = F)
    
  })
  
})

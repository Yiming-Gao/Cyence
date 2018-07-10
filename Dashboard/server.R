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
  
  # US Output 1
  output$map <- renderLeaflet({
    
    # read data
    country_scores_avg = read.csv("country_scores_avg.csv")
    
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
  
  # US Output 2
  output$heatmap <- renderPlotly({
    
    # read data
    sector_scores_avg = read.csv("sector_scores_avg.csv")
    var = input$score_type
    
    # basic heatmap
    p <- ggplot(sector_scores_avg, aes(revenue_bins, cyence_sector,
                                       # customize the tooltip
                                       text = paste("Sector: ", cyence_sector, "\n",
                                                    "Number of Companies: ", n_companies, "\n",
                                                    "Average score: ", round(sector_scores_avg[, names(sector_scores_avg) == as.name(paste0("sector_", var, "_avg"))], 2), "\n",
                                                    "Change in score: ", round(sector_scores_avg[, names(sector_scores_avg) == as.name(paste0(var, "_change"))], 2)))) + 
      geom_tile(aes(fill = sector_scores_avg[, names(sector_scores_avg) == as.name(paste0(var, "_change"))]), colour = "white") + 
      scale_y_discrete(limits = rev(unique(sector_scores_avg$cyence_sector))) +
      scale_x_discrete(limits = c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M", "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up")) +
      scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 0, limits = range(sector_scores_avg[, names(sector_scores_avg) == as.name(paste0(var, "_change"))]), name = "Change in Score") +
      labs(title = paste0("Score Changes from \n", month.name[month(rundate)-1], " to ", month.name[month(rundate)])) +
      xlab("Revenue Bins") + ylab("Cyence Sector") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        text = element_text(size = 12)
      ) 
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  
  # US Output 3: need update every month
  output$text_revenue_bin <- renderPrint({
    cat("Ten companies with greatest Cyence rating change in ", input$sector, " with revenue ", input$revenue_bin, " from May to June.")
  })
  
  
  
  # US Output 4
  output$top10 <- renderPlotly({
    
    ex_company_scores <- read.csv(paste(input$sector, ".csv", sep = ""))
    ex_company_scores <- ex_company_scores[ex_company_scores$revenue_bins == input$revenue_bin, ]
    ex_company_scores_temp = ex_company_scores %>% group_by(cyence_id) %>% summarise(n_months = n())
    
    if (input$gap_or_not == FALSE) {
      top_unique <- unique(ex_company_scores_temp[ex_company_scores_temp$n_months == 6, ]$cyence_id)
    }
    
    else top_unique <- unique(ex_company_scores$cyence_id)
    
    myplots <- list()
    
    # for loop
    for (i in 1:10) {
      ex_company_scores_plot <- ex_company_scores[ex_company_scores$cyence_id == top_unique[i], ]
      ex_company_scores_plot <- melt(ex_company_scores_plot[, c("run_date", "cy", "sus", "mo")], id = "run_date")
      ex_company_scores_plot$main1 =  paste0("Cyence Scores for ", unique(ex_company_scores[ex_company_scores$cyence_id == top_unique[i], ]$company_name))
      ex_company_scores_plot$run_date = as.Date(ex_company_scores_plot$run_date)
      
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
      
      # Force y-axis to be the same
      if (input$same_y_scale_or_not == FALSE) {
        p <- p + ylim(range(ex_company_scores_plot$value)[1], range(ex_company_scores_plot$value)[2])
      }
      
      else p <- p + ylim(min(range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$cy)[1], range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$mo)[1],range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$sus)[1]),
                         max(range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$cy)[2], range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$mo)[2],range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$sus)[2]))
      
      myplots[[i]] <- p
    }
    
    subplot(ggplotly(myplots[[1]]), ggplotly(myplots[[2]]), 
            ggplotly(myplots[[3]]), ggplotly(myplots[[4]]), 
            ggplotly(myplots[[5]]), ggplotly(myplots[[6]]), 
            ggplotly(myplots[[7]]), ggplotly(myplots[[8]]), 
            ggplotly(myplots[[9]]), ggplotly(myplots[[10]]),
            nrows = 5) %>% layout(showlegend = F)
    
  })
  
  
  # US Output 5
  output$diverging <- renderPlotly({
    
    ex_company_scores1 <- read.csv(paste(input$sector, ".csv", sep = ""))
    ex_company_scores1 <- ex_company_scores1[ex_company_scores1$revenue_bins == input$revenue_bin, ]
    
    # tell gap or not
    ex_company_scores1_temp = ex_company_scores1 %>% group_by(cyence_id) %>% summarise(n_months = n())
    
    
    if (input$gap_or_not == FALSE) {
      ex_company_scores1 <- ex_company_scores1[ex_company_scores1$cyence_id %in% (ex_company_scores1_temp[ex_company_scores1_temp$n_months == 6, ]$cyence_id), ]
    }
    
    else ex_company_scores1 <- ex_company_scores1
    
    # data preparation
    ex_company_scores1 <- ex_company_scores1 %>% group_by(company_name) %>% summarise(cy_avg = mean(cy))
    ex_company_scores1$cy_avg_z <- round((ex_company_scores1$cy_avg - mean(ex_company_scores1$cy_avg))/ sd(ex_company_scores1$cy_avg), 2)
    ex_company_scores1$cy_avg_type <- ifelse(ex_company_scores1$cy_avg_z < 0, "below average (safe)", "above average (risky)")
    ex_company_scores1 <- ex_company_scores1[order(ex_company_scores1$cy_avg_z), ]
    ex_company_scores1 <- rbind(head(ex_company_scores1, 20), tail(ex_company_scores1, 20))
    ex_company_scores1$company_name <- factor(ex_company_scores1$company_name, levels = unique(ex_company_scores1$company_name)) # convert to factor to retain sorted order in plot
    
    # Diverging Barcharts
    theme_set(theme_bw())
    
    p <- ggplot(ex_company_scores1, aes(x = company_name, y = cy_avg_z, label = cy_avg_z,
                                        # customize the tooltip
                                        text = paste("Normalized score: ", cy_avg_z))) +
      geom_bar(stat = "identity", aes(fill = cy_avg_type), width = 0.4) +
      scale_fill_manual(name = "Risk",
                        labels = c("Above Average", "Below Average"),
                        values = c("above average (risky)" = "#f8766d", "below average (safe)" = "#00ba38")) +
      labs(subtitle = "Normalized cyence scores",
           title = "Diverging Bars") +
      ylab("Normalized Score") + xlab("Company Name") +
      coord_flip()
    
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  
  # US Some text output
  output$legend_explanation <- renderPrint({
    cat("Red cells have their scores increased (riskier), and blue cells have their scores decreased.")
  })
  
  output$text_diverging <- renderPrint({
    cat("Cyence ratings have been normalized as z score. Those 20 companies with score above zero are marked red and those 20 below are marked green. 
Any z-score greater than 3 or less than -3 could be considered as an outlier. You can check them manually in our app.")
  })

  
  
  
  
  
  
  
  
  ######################## EU #########################
  # EU Output 1
  output$map_eu <- renderLeaflet({
    
    # read data
    country_scores_avg = read.csv("country_scores_avg_eu.csv")
    
    # Map by country
    countries@data <- right_join(country_scores_avg, countries@data, by = c("country" = "NAME"))
    # summary(countries$NAME)
    
    map <- leaflet(countries) %>% addTiles() %>% setView(lat = 46, lng = 12, zoom = 3.5)
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
  
  # EU Output 2
  output$heatmap_eu <- renderPlotly({
    
    # read data
    sector_scores_avg = read.csv("sector_scores_avg_eu.csv")
    var = input$score_type_eu
    
    # basic heatmap
    p <- ggplot(sector_scores_avg, aes(revenue_bins, cyence_sector,
                                       # customize the tooltip
                                       text = paste("Sector: ", cyence_sector, "\n",
                                                    "Number of Companies: ", n_companies, "\n",
                                                    "Average score: ", round(sector_scores_avg[, names(sector_scores_avg) == as.name(paste0("sector_", var, "_avg"))], 2), "\n",
                                                    "Change in score: ", round(sector_scores_avg[, names(sector_scores_avg) == as.name(paste0(var, "_change"))], 2)))) + 
      geom_tile(aes(fill = sector_scores_avg[, names(sector_scores_avg) == as.name(paste0(var, "_change"))]), colour = "white") + 
      scale_y_discrete(limits = rev(unique(sector_scores_avg$cyence_sector))) +
      scale_x_discrete(limits = c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M", "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up")) +
      scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 0, limits = range(sector_scores_avg[, names(sector_scores_avg) == as.name(paste0(var, "_change"))]), name = "Change in Score") +
      labs(title = paste0("Score Changes from \n", month.name[month(rundate)-1], " to ", month.name[month(rundate)])) +
      xlab("Revenue Bins") + ylab("Cyence Sector") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        text = element_text(size = 12)
      ) 
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  
  # EU Output 3: need update every month
  output$text_revenue_bin_eu <- renderPrint({
    cat("Ten companies with greatest Cyence rating change in ", input$sector_eu, " with revenue ", input$revenue_bin_eu, " from May to June.")
  })
  
  
  
  # EU Output 4
  output$top10_eu <- renderPlotly({
    
    ex_company_scores <- read.csv(paste(input$sector_eu, "_eu.csv", sep = ""))
    ex_company_scores <- ex_company_scores[ex_company_scores$revenue_bins == input$revenue_bin_eu, ]
    ex_company_scores_temp = ex_company_scores %>% group_by(cyence_id) %>% summarise(n_months = n())
    
    if (input$gap_or_not_eu == FALSE) {
      top_unique <- unique(ex_company_scores_temp[ex_company_scores_temp$n_months == 6, ]$cyence_id)
    }
    
    else top_unique <- unique(ex_company_scores$cyence_id)
    
    myplots <- list()
    
    # for loop
    for (i in 1:10) {
      
      # require certain condition
      req(length(top_unique) >= 10)
      
      ex_company_scores_plot <- ex_company_scores[ex_company_scores$cyence_id == top_unique[i], ]
      ex_company_scores_plot <- melt(ex_company_scores_plot[, c("run_date", "cy", "sus", "mo")], id = "run_date")
      ex_company_scores_plot$main1 =  paste0("Cyence Scores for ", unique(ex_company_scores[ex_company_scores$cyence_id == top_unique[i], ]$company_name))
      ex_company_scores_plot$run_date = as.Date(ex_company_scores_plot$run_date)
      
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
      
      # Force y-axis to be the same
      if (input$same_y_scale_or_not_eu == FALSE) {
        p <- p + ylim(range(ex_company_scores_plot$value)[1], range(ex_company_scores_plot$value)[2])
      }
      
      else p <- p + ylim(min(range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$cy)[1], range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$mo)[1],range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$sus)[1]),
                         max(range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$cy)[2], range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$mo)[2],range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$sus)[2]))
      
      myplots[[i]] <- p
    }
    
    subplot(ggplotly(myplots[[1]]), ggplotly(myplots[[2]]), 
            ggplotly(myplots[[3]]), ggplotly(myplots[[4]]), 
            ggplotly(myplots[[5]]), ggplotly(myplots[[6]]),
            ggplotly(myplots[[7]]), ggplotly(myplots[[8]]), 
            ggplotly(myplots[[9]]), ggplotly(myplots[[10]]), 
            nrows = 5) %>% layout(showlegend = F)
    
  })
  
  
  # EU Output 5
  output$diverging_eu <- renderPlotly({
    
    ex_company_scores1 <- read.csv(paste(input$sector_eu, "_eu.csv", sep = ""))
    ex_company_scores1 <- ex_company_scores1[ex_company_scores1$revenue_bins == input$revenue_bin_eu, ]
    
    # tell gap or not
    ex_company_scores1_temp = ex_company_scores1 %>% group_by(cyence_id) %>% summarise(n_months = n())
    
    
    if (input$gap_or_not_eu == FALSE) {
      ex_company_scores1 <- ex_company_scores1[ex_company_scores1$cyence_id %in% (ex_company_scores1_temp[ex_company_scores1_temp$n_months == 6, ]$cyence_id), ]
    }
    
    else ex_company_scores1 <- ex_company_scores1
    
    # data preparation
    ex_company_scores1 <- ex_company_scores1 %>% group_by(company_name) %>% summarise(cy_avg = mean(cy))
    ex_company_scores1$cy_avg_z <- round((ex_company_scores1$cy_avg - mean(ex_company_scores1$cy_avg))/ sd(ex_company_scores1$cy_avg), 2)
    ex_company_scores1$cy_avg_type <- ifelse(ex_company_scores1$cy_avg_z < 0, "below average (safe)", "above average (risky)")
    ex_company_scores1 <- ex_company_scores1[order(ex_company_scores1$cy_avg_z), ]
    ex_company_scores1 <- rbind(head(ex_company_scores1, 20), tail(ex_company_scores1, 20))
    ex_company_scores1$company_name <- factor(ex_company_scores1$company_name, levels = unique(ex_company_scores1$company_name)) # convert to factor to retain sorted order in plot
    
    # Require certain condition
    req(nrow(ex_company_scores1) > 0)
    
    # Diverging Barcharts
    theme_set(theme_bw())
    
    p <- ggplot(ex_company_scores1, aes(x = company_name, y = cy_avg_z, label = cy_avg_z,
                                        # customize the tooltip
                                        text = paste("Normalized score: ", cy_avg_z))) +
      geom_bar(stat = "identity", aes(fill = cy_avg_type), width = 0.4) +
      scale_fill_manual(name = "Risk",
                        labels = c("Above Average", "Below Average"),
                        values = c("above average (risky)" = "#f8766d", "below average (safe)" = "#00ba38")) +
      labs(subtitle = "Normalized cyence scores",
           title = "Diverging Bars") +
      ylab("Normalized Score") + xlab("Company Name") +
      coord_flip()
    
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  
  # EU Some text output
  output$legend_explanation_eu <- renderPrint({
    cat("Red cells have their scores increased (riskier), and blue cells have their scores decreased.")
  })
  
  output$text_diverging_eu <- renderPrint({
    cat("Cyence ratings have been normalized as z score. Those 20 companies with score above zero are marked red and those 20 below are marked green. 
        Any z-score greater than 3 or less than -3 could be considered as an outlier. You can check them manually in our app.")
  })
  
  
  
  
  
  
  ######################## JP #########################
  # JP Output 1
  output$heatmap_jp <- renderPlotly({
    
    # read data
    sector_scores_avg = read.csv("sector_scores_avg_jp.csv")
    var = input$score_type_jp
    
    # basic heatmap
    p <- ggplot(sector_scores_avg, aes(revenue_bins, cyence_sector,
                                       # customize the tooltip
                                       text = paste("Sector: ", cyence_sector, "\n",
                                                    "Number of Companies: ", n_companies, "\n",
                                                    "Average score: ", round(sector_scores_avg[, names(sector_scores_avg) == as.name(paste0("sector_", var, "_avg"))], 2), "\n",
                                                    "Change in score: ", round(sector_scores_avg[, names(sector_scores_avg) == as.name(paste0(var, "_change"))], 2)))) + 
      geom_tile(aes(fill = sector_scores_avg[, names(sector_scores_avg) == as.name(paste0(var, "_change"))]), colour = "white") + 
      scale_y_discrete(limits = rev(unique(sector_scores_avg$cyence_sector))) +
      scale_x_discrete(limits = c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M", "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up")) +
      scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 0, limits = range(sector_scores_avg[, names(sector_scores_avg) == as.name(paste0(var, "_change"))]), name = "Change in Score") +
      labs(title = paste0("Score Changes from \n", month.name[month(rundate)-1], " to ", month.name[month(rundate)])) +
      xlab("Revenue Bins") + ylab("Cyence Sector") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        text = element_text(size = 12)
      ) 
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  
  # JP Output 2: need update every month
  output$text_revenue_bin_jp <- renderPrint({
    cat("Ten companies with greatest Cyence rating change in ", input$sector_jp, " with revenue ", input$revenue_bin_jp, " from June to July.")
  })
  
  
  
  # JP Output 3
  output$top10_jp <- renderPlotly({
    
    ex_company_scores <- read.csv(paste(input$sector_jp, "_jp.csv", sep = ""))
    ex_company_scores <- ex_company_scores[ex_company_scores$revenue_bins == input$revenue_bin_jp, ]
    ex_company_scores_temp = ex_company_scores %>% group_by(cyence_id, run_date) %>% 
      summarise(cy = mean(cy), mo = mean(mo), sus = mean(sus))
    ex_company_scores_temp = ex_company_scores_temp %>% group_by(cyence_id) %>% summarise(n_months = n())
    
    if (input$gap_or_not_jp == FALSE) {
      top_unique <- unique(ex_company_scores_temp[ex_company_scores_temp$n_months == 6, ]$cyence_id)
    }
    
    else top_unique <- unique(ex_company_scores$cyence_id)
    
    myplots <- list()
    
    # for loop
    for (i in 1:10) {
      
      # require certain condition
      req(length(top_unique) >= 10)
      
      ex_company_scores_plot <- ex_company_scores[ex_company_scores$cyence_id == top_unique[i], ]
      ex_company_scores_plot <- melt(ex_company_scores_plot[, c("run_date", "cy", "sus", "mo")], id = "run_date")
      ex_company_scores_plot$main1 =  paste0("Cyence Scores for ", unique(ex_company_scores[ex_company_scores$cyence_id == top_unique[i], ]$company_name))
      ex_company_scores_plot$run_date = as.Date(ex_company_scores_plot$run_date)
      
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
      
      # Force y-axis to be the same
      if (input$same_y_scale_or_not_jp == FALSE) {
        p <- p + ylim(range(ex_company_scores_plot$value)[1], range(ex_company_scores_plot$value)[2])
      }
      
      else p <- p + ylim(min(range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$cy)[1], range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$mo)[1],range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$sus)[1]),
                         max(range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$cy)[2], range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$mo)[2],range(ex_company_scores[ex_company_scores$cyence_id %in% top_unique, ]$sus)[2]))
      
      myplots[[i]] <- p
    }
    
    subplot(ggplotly(myplots[[1]]), ggplotly(myplots[[2]]), 
            ggplotly(myplots[[3]]), ggplotly(myplots[[4]]), 
            ggplotly(myplots[[5]]), ggplotly(myplots[[6]]),
            ggplotly(myplots[[7]]), ggplotly(myplots[[8]]),
            ggplotly(myplots[[9]]), ggplotly(myplots[[10]]),
            nrows = 5) %>% layout(showlegend = F)
    
  })
  
  
  # JP Output 4
  output$diverging_jp <- renderPlotly({
    
    ex_company_scores1 <- read.csv(paste(input$sector_jp, "_jp.csv", sep = ""))
    ex_company_scores1 <- ex_company_scores1[ex_company_scores1$revenue_bins == input$revenue_bin_jp, ]
    
    # tell gap or not
    ex_company_scores1_temp = ex_company_scores1 %>% group_by(cyence_id) %>% summarise(n_months = n())
    
    
    if (input$gap_or_not_jp == FALSE) {
      ex_company_scores1 <- ex_company_scores1[ex_company_scores1$cyence_id %in% (ex_company_scores1_temp[ex_company_scores1_temp$n_months == 6, ]$cyence_id), ]
    }
    
    else ex_company_scores1 <- ex_company_scores1
    
    # data preparation
    ex_company_scores1 <- ex_company_scores1 %>% group_by(company_name) %>% summarise(cy_avg = mean(cy))
    ex_company_scores1$cy_avg_z <- round((ex_company_scores1$cy_avg - mean(ex_company_scores1$cy_avg))/ sd(ex_company_scores1$cy_avg), 2)
    ex_company_scores1$cy_avg_type <- ifelse(ex_company_scores1$cy_avg_z < 0, "below average (safe)", "above average (risky)")
    ex_company_scores1 <- ex_company_scores1[order(ex_company_scores1$cy_avg_z), ]
    ex_company_scores1 <- rbind(head(ex_company_scores1, 20), tail(ex_company_scores1, 20))
    ex_company_scores1$company_name <- factor(ex_company_scores1$company_name, levels = unique(ex_company_scores1$company_name)) # convert to factor to retain sorted order in plot
    
    # Require certain condition
    req(nrow(ex_company_scores1) > 0)
    
    # Diverging Barcharts
    theme_set(theme_bw())
    
    p <- ggplot(ex_company_scores1, aes(x = company_name, y = cy_avg_z, label = cy_avg_z,
                                        # customize the tooltip
                                        text = paste("Normalized score: ", cy_avg_z))) +
      geom_bar(stat = "identity", aes(fill = cy_avg_type), width = 0.4) +
      scale_fill_manual(name = "Risk",
                        labels = c("Above Average", "Below Average"),
                        values = c("above average (risky)" = "#f8766d", "below average (safe)" = "#00ba38")) +
      labs(subtitle = "Normalized cyence scores",
           title = "Diverging Bars") +
      ylab("Normalized Score") + xlab("Company Name") +
      coord_flip()
    
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  
  # JP Some text output
  output$legend_explanation_jp <- renderPrint({
    cat("Red cells have their scores increased (riskier), and blue cells have their scores decreased.")
  })
  
  output$text_diverging_jp <- renderPrint({
    cat("Cyence ratings have been normalized as z score. Those 20 companies with score above zero are marked red and those 20 below are marked green. 
        Any z-score greater than 3 or less than -3 could be considered as an outlier. You can check them manually in our app.")
  })
  
  
})

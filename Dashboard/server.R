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
  
  output$map <- renderLeaflet({
    # Map by country
    countries <- readOGR(dsn = getwd() , layer = "TM_WORLD_BORDERS_SIMPL-0.3") # https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet/
    # summary(countries$NAME)
    
    map <- leaflet(countries) %>% addTiles() %>% setView(lat = 40, lng = -20, zoom = 2)
    pal <- colorNumeric(
      palette = "YlOrBr",
      domain = country_scores_avg$country_cy_avg
    )
    
    # Prepare the text for the popup message
    mytext <- 
      paste("<b>Country: ", countries$NAME,"</b><br/>", 
            "Number of Companies: ", country_scores_avg$n_companies, "<br/>",
            "<b>Cyence Score: ", round(country_scores_avg$country_cy_avg, 2), "</b><br/>", 
            "Susceptibility: ", round(country_scores_avg$country_sus_avg, 2), "<br/>",
            "Motivation: ", round(country_scores_avg$country_mo_avg,2)) %>%
      lapply(htmltools::HTML)
    
    # Display
    map %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.9, color = "white", weight = 0.3,
                  fillColor = ~pal(country_scores_avg$country_cy_avg),
                  label = mytext,
                  labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>%
      addLegend("bottomright", pal = pal, values = country_scores_avg$country_cy_avg,
                title = "Average Cyence Score",
                opacity = 1
      )
  })
  
})

#-------------------------------------------------------------------------------------------------------------------------
### Install packages
#-------------------------------------------------------------------------------------------------------------------------
library(RPostgreSQL) 
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(shiny)
library(pool)
library(readr)
library(plotly)


setwd("/Users/ygao/Desktop/Dashboard/Code/try")
options(scipen = 999)

#-------------------------------------------------------------------------------------------------------------------------
### Supporting functions
#-------------------------------------------------------------------------------------------------------------------------
# Connect to databases
drv <- dbDriver("PostgreSQL")

# postgres
aws <- Sys.getenv(c("DB_NAME", "DB_HOST", "DB_PORT", "DB_USER", "DB_PASS"))
con_postgresql <- dbConnect(drv, 
                            dbname = aws["DB_NAME"], 
                            host = aws["DB_HOST"], 
                            port = aws["DB_PORT"], 
                            user = aws["DB_USER"], 
                            password = aws["DB_PASS"])

# Fetch the data
# Cyence score
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3.score where run_date >= '", rundate-months(12), "'")
scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
scores$cyence_id <- as.character(scores$cyence_id)
scores <- scores[order(cyence_id, run_date)]

# US company Info
query <- paste0("select distinct run_date, cyence_id, company_name, website, city, state, country, cyence_sector, sic4, revenue, income, employees from er.company_info_union where run_date >= '", rundate-months(1), "' 
                and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
companies <- dbGetQuery(con_postgresql, query) %>% as.data.table()
companies$cyence_id <- as.character(companies$cyence_id)
companies <- companies[order(cyence_id, run_date)]

# One example company (Apple Inc., cyence_id = 60704780) info across months
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3.score where cyence_id = 60704780 and run_date >='", rundate-months(12), "'")
apple_scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
apple_scores$cyence_id <- as.character(apple_scores$cyence_id)
apple_scores <- apple_scores[order(run_date)]
dbDisconnect(con_postgresql)

# Left join scores& Company Info by cyence_id
companies_scores <- left_join(scores, companies, by = "cyence_id") %>% 
  filter(!is.na(company_name)) %>% # filter NAs in company_name column
  # filter(.$country == "United States") %>%
  arrange(desc(cyence_id)) 

# Group by country
country_scores_avg <- companies_scores %>% # order by descending cyence_id
  group_by(country) %>%
  summarise(country_cy_avg = mean(cy),
            country_sus_avg = mean(sus),
            country_mo_avg = mean(mo))

# Map by country
library(rgdal)
countries <- readOGR(dsn= getwd() , layer = "TM_WORLD_BORDERS_SIMPL-0.3") # https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet/
# summary(countries$NAME)
map <- leaflet(countries) %>% addTiles()
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = country_scores_avg$country_cy_avg
)

# Prepare the text for the popup message
mytext <- 
  paste("Country: ", countries$NAME,"<br/>", 
        "Cyence Score: ", round(country_scores_avg$country_cy_avg, 2), "<br/>", 
        "Susceptibility: ", round(country_scores_avg$country_sus_avg, 2), "<br/>",
        "Motivation: ", round(country_scores_avg$country_mo_avg,2)) %>%
  lapply(htmltools::HTML)

# Display
map %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(country_scores_avg$country_cy_avg),
              label = mytext,
              labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend("bottomright", pal = pal, values = country_scores_avg$country_cy_avg,
            title = "Average Cyence Score",
            opacity = 1
  )



# Time series plot of Apple Inc.
plot_ly(x = apple_scores$run_date, y = apple_scores$cy, mode = "lines")


# ------------------------------------------------------------ Map ------------------------------------------------------#
# Fetch the data
rundate = as.Date('2018-06-01')

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

# Cyence score
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3.score where run_date >= '", rundate-months(1), "'")
scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
scores$cyence_id <- as.character(scores$cyence_id)
scores <- scores[order(cyence_id, run_date)]

# US Company Info
query <- paste0("select distinct run_date, cyence_id, company_name, website, city, state, country, cyence_sector, sic4, revenue, income, employees from er.company_info where run_date >= '", rundate-months(1), "' 
                and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
us_companies <- dbGetQuery(con_postgresql, query) %>% as.data.table()
us_companies$cyence_id <- as.character(us_companies$cyence_id)
us_companies <- us_companies[order(cyence_id, run_date)]

# # EU Company Info
# query <- paste0("select distinct run_date, cyence_id, company_name, website, city, state, country, cyence_sector, sic4, revenue, income, employees from er.company_info_eu where run_date >= '", rundate-months(1), "' 
#                 and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
# eu_companies <- dbGetQuery(con_postgresql, query) %>% as.data.table()
# eu_companies$cyence_id <- as.character(eu_companies$cyence_id)
# eu_companies <- eu_companies[order(cyence_id, run_date)]
# 
# # JP Company Info
# query <- paste0("select distinct run_date, cyence_id, company_name, website, city, state, country, cyence_sector, sic4, revenue, income, employees from er.company_info_jp where run_date >= '", rundate-months(1), "' 
#                 and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
# jp_companies <- dbGetQuery(con_postgresql, query) %>% as.data.table()
# jp_companies$cyence_id <- as.character(jp_companies$cyence_id)
# jp_companies <- jp_companies[order(cyence_id, run_date)]
dbDisconnect(con_postgresql)

# Left join scores& Company Info by cyence_id
companies_scores <- inner_join(us_companies, scores, by = "cyence_id") %>% 
  filter(!is.na(company_name)) %>% # filter NAs in company_name column
  # filter(.$country == "United States") %>%
  arrange(desc(cyence_id)) 

# Group by country
country_scores_avg <- companies_scores %>% # order by descending cyence_id
  group_by(country) %>%
  summarise(country_cy_avg = mean(cy),
            country_sus_avg = mean(sus),
            country_mo_avg = mean(mo),
            n_companies = n())



# Map by country
countries <- readOGR(dsn = getwd() , layer = "TM_WORLD_BORDERS_SIMPL-0.3") # https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet/
countries@data <- right_join(country_scores_avg, countries@data, by = c("country" = "NAME"))
# summary(countries$NAME)

map <- leaflet(countries) %>% addTiles() %>% setView(lat = 40, lng = -20, zoom = 2)
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


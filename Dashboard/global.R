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
library(rgdal)
library(leaflet)


setwd("/Users/ygao/Desktop/Dashboard/Code/try")
options(scipen = 999)

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
query <- paste0("select distinct run_date, cyence_id, company_name, website, city, state, country, cyence_sector, sic4, revenue, income, employees from er.company_info_union where run_date >= '", rundate-months(1), "' 
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

# Data Manipulation for Maps
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


# # Data Manipulation for heatmaps
# # Group by cyence_sector (industry)
# sector_scores_avg <- companies_scores %>% # order by descending cyence_id
#   group_by(cyence_sector, run_date.x) %>%
#   summarise(sector_cy_avg = mean(cy),
#             sector_sus_avg = mean(sus),
#             sector_mo_avg = mean(mo))



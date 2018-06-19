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



# Data Manipulation for heatmaps
# Left join scores& Company Info by cyence_id
companies_scores <- inner_join(scores, companies, by = c("cyence_id" = "cyence_id", "run_date" = "run_date")) %>% 
  filter(!is.na(company_name)) %>% # filter NAs in company_name column
  # filter(.$country == "United States") %>%
  arrange(desc(cyence_id)) 

# Create a new column containing revenue bins
# https://cyence.atlassian.net/wiki/spaces/DS/pages/99323219/Data+sources+Documentations
companies_scores$revenue_bins <- ifelse(companies_scores$revenue < 5, "0-5M",
                                        ifelse((companies_scores$revenue >= 5) & (companies_scores$revenue < 10), "5-10M",
                                               ifelse((companies_scores$revenue >= 10) & (companies_scores$revenue < 25), "10-25M",
                                                      ifelse((companies_scores$revenue >= 25) & (companies_scores$revenue < 50), "25-50M", 
                                                             ifelse((companies_scores$revenue >= 50) & (companies_scores$revenue < 100), "50-100M",
                                                                    ifelse((companies_scores$revenue >= 100) & (companies_scores$revenue < 500), "100-500M",
                                                                           ifelse((companies_scores$revenue >= 500) & (companies_scores$revenue < 1000), "500M-1B",
                                                                                  ifelse((companies_scores$revenue >= 1000) & (companies_scores$revenue < 5000), "1-5B",
                                                                                         ifelse((companies_scores$revenue >= 5000) & (companies_scores$revenue < 10000), "5-10B","10B& up")))))))))

# Change this to a specific order
companies_scores$revenue_bins <- factor(companies_scores$revenue_bins, levels = c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
                                                                                  "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up"))

# Group by cyence_sector (industry)
sector_scores_avg <- companies_scores %>% # order by descending cyence_id
  group_by(cyence_sector, revenue_bins, run_date) %>%
  summarise(n_companies = n(),
            sector_cy_avg = mean(cy),
            sector_sus_avg = mean(sus),
            sector_mo_avg = mean(mo)) %>%
  group_by(cyence_sector, revenue_bins) %>%
  mutate(cy_change = sector_cy_avg - lag(sector_cy_avg),
         sus_change = sector_sus_avg - lag(sector_sus_avg),
         mo_change = sector_mo_avg - lag(sector_mo_avg)) %>%
  na.omit() 




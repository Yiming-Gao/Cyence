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
library(shinythemes)
library(pool)
library(readr)
library(plotly)
library(rgdal)
library(leaflet)
library(scales)


#setwd("/Users/ygao/Desktop/Dashboard/Code/try")
options(scipen = 999)
rundate = as.Date('2018-06-01')
# ------------------------------------------------------------ Map ------------------------------------------------------#
# Fetch the data

# # Connect to databases
# drv <- dbDriver("PostgreSQL")
# 
# # postgres
# aws <- Sys.getenv(c("DB_NAME", "DB_HOST", "DB_PORT", "DB_USER", "DB_PASS"))
# con_postgresql <- dbConnect(drv,
#                             dbname = aws["DB_NAME"],
#                             host = aws["DB_HOST"],
#                             port = aws["DB_PORT"],
#                             user = aws["DB_USER"],
#                             password = aws["DB_PASS"])
# 
# # Cyence score
# query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3.score where run_date >= '", rundate-months(1), "'")
# scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
# scores$cyence_id <- as.character(scores$cyence_id)
# scores <- scores[order(cyence_id, run_date)]
# 
# # US Company Info
# query <- paste0("select distinct run_date, cyence_id, company_name, website, city, state, country, cyence_sector, sic4, revenue, income, employees from er.company_info where run_date >= '", rundate-months(1), "'
#                 and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
# us_companies <- dbGetQuery(con_postgresql, query) %>% as.data.table()
# us_companies$cyence_id <- as.character(us_companies$cyence_id)
# us_companies <- us_companies[order(cyence_id, run_date)]
# 
# 
# # Data Preparation for top10
# query <- paste0("select distinct cyence_id, company_name, cyence_sector, revenue from er.company_info where run_date >= '", rundate-months(5), "'
#                 and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
# ex_company <- dbGetQuery(con_postgresql, query) %>% as.data.table()
# ex_company$cyence_id <- as.character(ex_company$cyence_id)
# # ex_company <- ex_company[order(run_date)]
# 
# # Cyence score
# query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3.score where run_date >= '", rundate-months(5), "'
#                 and cyence_id in (select cyence_id from er.company_info)")
# ex_scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
# ex_scores$cyence_id <- as.character(ex_scores$cyence_id)
# ex_scores <- ex_scores[order(run_date)]
# dbDisconnect(con_postgresql)
# 
# 
# # write to csv
# write.csv(us_companies, "us_companies.csv")
# write.csv(scores, "scores.csv")
# write.csv(ex_company, "ex_company.csv")
# write.csv(ex_scores, "ex_scores.csv")


# read csv
# us_companies = read.csv("us_companies.csv")
# scores = read.csv("scores.csv")
# ex_company = read.csv("ex_company.csv")
# ex_company$cyence_id = as.character(ex_company$cyence_id)
# ex_scores = read.csv("ex_scores.csv")
# ex_scores$cyence_id = as.character(ex_scores$cyence_id)

countries <- readOGR(dsn = getwd() , layer = "TM_WORLD_BORDERS_SIMPL-0.3") # https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet/

####### Data Manipulation for Maps
# companies_scores <- inner_join(us_companies, scores, by = "cyence_id") %>% 
#   filter(!is.na(company_name)) %>% # filter NAs in company_name column
#   # filter(.$country == "United States") %>%
#   arrange(desc(cyence_id)) 
# 
# # Group by country
# country_scores_avg <- companies_scores %>% # order by descending cyence_id
#   group_by(country) %>%
#   summarise(country_cy_avg = mean(cy),
#             country_sus_avg = mean(sus),
#             country_mo_avg = mean(mo),
#             n_companies = n()/2)
# 
# 
# 
# # Data Manipulation for heatmaps
# # Left join scores& Company Info by cyence_id
# companies_scores <- inner_join(scores, us_companies, by = c("cyence_id" = "cyence_id", "run_date" = "run_date")) %>% 
#   filter(!is.na(company_name)) %>% # filter NAs in company_name column
#   # filter(.$country == "United States") %>%
#   arrange(desc(cyence_id)) 
# 
# # Create a new column containing revenue bins
# # https://cyence.atlassian.net/wiki/spaces/DS/pages/99323219/Data+sources+Documentations
# companies_scores$revenue_bins <- ifelse(companies_scores$revenue < 5, "0-5M",
#                                         ifelse((companies_scores$revenue >= 5) & (companies_scores$revenue < 10), "5-10M",
#                                                ifelse((companies_scores$revenue >= 10) & (companies_scores$revenue < 25), "10-25M",
#                                                       ifelse((companies_scores$revenue >= 25) & (companies_scores$revenue < 50), "25-50M", 
#                                                              ifelse((companies_scores$revenue >= 50) & (companies_scores$revenue < 100), "50-100M",
#                                                                     ifelse((companies_scores$revenue >= 100) & (companies_scores$revenue < 500), "100-500M",
#                                                                            ifelse((companies_scores$revenue >= 500) & (companies_scores$revenue < 1000), "500M-1B",
#                                                                                   ifelse((companies_scores$revenue >= 1000) & (companies_scores$revenue < 5000), "1-5B",
#                                                                                          ifelse((companies_scores$revenue >= 5000) & (companies_scores$revenue < 10000), "5-10B","10B& up")))))))))
# 
# # Change this to a specific order
# companies_scores$revenue_bins <- factor(companies_scores$revenue_bins, levels = c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
#                                                                                   "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up"))
# 
# # Group by cyence_sector (industry)
# sector_scores_avg <- companies_scores %>% # order by descending cyence_id
#   group_by(cyence_sector, revenue_bins, run_date) %>%
#   summarise(n_companies = n(),
#             sector_cy_avg = mean(cy),
#             sector_sus_avg = mean(sus),
#             sector_mo_avg = mean(mo)) %>%
#   group_by(cyence_sector, revenue_bins) %>%
#   mutate(cy_change = sector_cy_avg - lag(sector_cy_avg),
#          sus_change = sector_sus_avg - lag(sector_sus_avg),
#          mo_change = sector_mo_avg - lag(sector_mo_avg)) %>%
#   na.omit() 














# ##### Data Manipulation for score changes
# # Left join scores& Company Info by cyence_id
# ex_company$revenue_bins <- ifelse(ex_company$revenue < 5, "0-5M",
#                                   ifelse((ex_company$revenue >= 5) & (ex_company$revenue < 10), "5-10M",
#                                          ifelse((ex_company$revenue >= 10) & (ex_company$revenue < 25), "10-25M",
#                                                 ifelse((ex_company$revenue >= 25) & (ex_company$revenue < 50), "25-50M", 
#                                                        ifelse((ex_company$revenue >= 50) & (ex_company$revenue < 100), "50-100M",
#                                                               ifelse((ex_company$revenue >= 100) & (ex_company$revenue < 500), "100-500M",
#                                                                      ifelse((ex_company$revenue >= 500) & (ex_company$revenue < 1000), "500M-1B",
#                                                                             ifelse((ex_company$revenue >= 1000) & (ex_company$revenue < 5000), "1-5B",
#                                                                                    ifelse((ex_company$revenue >= 5000) & (ex_company$revenue < 10000), "5-10B","10B& up")))))))))
# 
# 
# ex_scores <- ex_scores[ex_scores$cyence_id %in% ex_company$cyence_id, ]
# ex_company_scores <- inner_join(ex_company, ex_scores, by = c("cyence_id" = "cyence_id"))
# ex_company_scores$run_date <- as.Date(ex_company_scores$run_date)
# 
# 
# # get top 10 companies in every sector
# for (sector in c("Education & Research", "Licensed Professional Services", "Financial Services", "Membership Organizations", "Healthcare",
#                  "Consumer Services", "Wholesale Trade", "Manufacturing", "Hospitality", "Software and Technology Services",
#                  "Non-Profit Organizations", "Business Services", "Publishing", "Retail Trade", "Utilities",
#                  "Transportation Services", "Agriculture & Mining")) {
#   
#   # Initialize an empty data frame
#   temp_ex_company_scores = data.frame(X.x = integer(),
#                                       cyence_id = character(),
#                                       company_name = factor(),
#                                       cyence_sector = factor(),
#                                       revenue = numeric(),
#                                       income = numeric(),
#                                       employees = integer(),
#                                       revenue_bins = character(),
#                                       X.y = integer(),
#                                       run_date = as.Date(character()),
#                                       cy = numeric(),
#                                       mo = numeric(),
#                                       sus = numeric())
#   
#   for (revenue in c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
#                     "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up")) {
#     temp = ex_company_scores[(ex_company_scores$cyence_sector == sector) & (ex_company_scores$revenue_bins == revenue), ]
#     
#     temp_top_unique = temp %>%
#       group_by(cyence_id) %>%
#       mutate(cy_change = cy - lag(cy)) %>%
#       filter(run_date == rundate) %>%
#       arrange(desc(cy_change)) %>%
#       distinct(cyence_id)
#     
#     temp_ex_company_scores = rbind(temp_ex_company_scores, temp[temp$cyence_id %in% as.character(temp_top_unique[1:10,]$cyence_id), ])
#   }
#   # write into csv  
#   write.csv(temp_ex_company_scores, paste(sector, ".csv", sep = ""))
# }




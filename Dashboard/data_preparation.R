# Run the following codes every time before running the app
# Change the rundate to latest date
library(magrittr)
library(RPostgreSQL)
library(lubridate)
library(iptools)
library(dplyr)
library(ggrepel)
library(stringr)
library(tidyr)
library(lubridate)
library(data.table)
library(plyr)
library(scales)
library(zoo)
library(odbc)
library(tibble)
library(gridExtra)
setwd("/Users/ygao/Desktop/Dashboard/Code/try")

## 1. US TAB STARTS
# Connect to database
rundate = as.Date('2018-10-01')

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

# Two months' cyence score for heatmaps
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


# Data Preparation for top10
query <- paste0("select distinct cyence_id, company_name, cyence_sector, revenue from er.company_info where run_date >= '", rundate-months(5), "'
                and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
ex_company <- dbGetQuery(con_postgresql, query) %>% as.data.table()
ex_company$cyence_id <- as.character(ex_company$cyence_id)
# ex_company <- ex_company[order(run_date)]

# Cyence score
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3.score where run_date >= '", rundate-months(5), "'
                and cyence_id in (select cyence_id from er.company_info)")
ex_scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
ex_scores$cyence_id <- as.character(ex_scores$cyence_id)
ex_scores <- ex_scores[order(run_date)]
dbDisconnect(con_postgresql)




# Step 1: Data Manipulation for Maps (2 months' data)
companies_scores <- inner_join(us_companies, scores, by = c("cyence_id", "run_date")) %>%
  filter(!is.na(company_name)) %>% # filter NAs in company_name column
  # filter(.$country == "United States") %>%
  arrange(desc(cyence_id))

# Group by country
country_scores_avg <- companies_scores[companies_scores$run_date == rundate-months(0), ] %>% # order by descending cyence_id
  group_by(country) %>%
  summarise(country_cy_avg = mean(cy),
            country_sus_avg = mean(sus),
            country_mo_avg = mean(mo),
            n_companies = n())

# write as csv
write.csv(country_scores_avg, "country_scores_avg.csv")


# Step 2: Data Manipulation for heatmaps (2 months' data)
# Left join scores& Company Info by cyence_id
companies_scores <- inner_join(scores, us_companies, by = c("cyence_id" = "cyence_id", "run_date" = "run_date")) %>%
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

# write as csv
write.csv(sector_scores_avg, "sector_scores_avg.csv")



# Step 3: Data Manipulation for score changes by sector
# Left join scores& Company Info by cyence_id
ex_company$revenue_bins <- ifelse(ex_company$revenue < 5, "0-5M",
                                  ifelse((ex_company$revenue >= 5) & (ex_company$revenue < 10), "5-10M",
                                         ifelse((ex_company$revenue >= 10) & (ex_company$revenue < 25), "10-25M",
                                                ifelse((ex_company$revenue >= 25) & (ex_company$revenue < 50), "25-50M",
                                                       ifelse((ex_company$revenue >= 50) & (ex_company$revenue < 100), "50-100M",
                                                              ifelse((ex_company$revenue >= 100) & (ex_company$revenue < 500), "100-500M",
                                                                     ifelse((ex_company$revenue >= 500) & (ex_company$revenue < 1000), "500M-1B",
                                                                            ifelse((ex_company$revenue >= 1000) & (ex_company$revenue < 5000), "1-5B",
                                                                                   ifelse((ex_company$revenue >= 5000) & (ex_company$revenue < 10000), "5-10B","10B& up")))))))))


ex_scores <- ex_scores[ex_scores$cyence_id %in% ex_company$cyence_id, ]
ex_company_scores <- inner_join(ex_company, ex_scores, by = c("cyence_id" = "cyence_id"))
ex_company_scores$run_date <- as.Date(ex_company_scores$run_date)


# get top companies across all sectors/ revenue bins
temp = ex_company_scores
temp_top_unique = temp %>%
  group_by(cyence_id) %>%
  mutate(cy_change = cy - lag(cy)) %>%
  filter(run_date == rundate) %>%
  arrange(desc(cy_change)) %>%
  distinct(cyence_id)

temp_ex_company_scores = data.frame(X.x = integer(),
                                    cyence_id = character(),
                                    company_name = factor(),
                                    cyence_sector = factor(),
                                    revenue = numeric(),
                                    income = numeric(),
                                    employees = integer(),
                                    revenue_bins = character(),
                                    X.y = integer(),
                                    run_date = as.Date(character()),
                                    cy = numeric(),
                                    mo = numeric(),
                                    sus = numeric())

temp_ex_company_scores = rbind(temp_ex_company_scores, temp[temp$cyence_id %in% as.character(temp_top_unique[1:200,]$cyence_id), ])
write.csv(temp_ex_company_scores, paste("All", ".csv", sep = ""))


# get top companies in every sector (200 to get enough valid obs)
for (sector in c("Education & Research", "Licensed Professional Services", "Financial Services", "Membership Organizations", "Healthcare",
                 "Consumer Services", "Wholesale Trade", "Manufacturing", "Hospitality", "Software and Technology Services",
                 "Non-Profit Organizations", "Business Services", "Publishing", "Retail Trade", "Utilities",
                 "Transportation Services", "Agriculture & Mining")) {

  # Initialize an empty data frame
  temp_ex_company_scores = data.frame(X.x = integer(),
                                      cyence_id = character(),
                                      company_name = factor(),
                                      cyence_sector = factor(),
                                      revenue = numeric(),
                                      income = numeric(),
                                      employees = integer(),
                                      revenue_bins = character(),
                                      X.y = integer(),
                                      run_date = as.Date(character()),
                                      cy = numeric(),
                                      mo = numeric(),
                                      sus = numeric())

  for (revenue in c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
                    "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up")) {
    
    temp = ex_company_scores[(ex_company_scores$cyence_sector == sector) & (ex_company_scores$revenue_bins == revenue), ]

    temp_top_unique = temp %>%
      group_by(cyence_id) %>%
      mutate(cy_change = cy - lag(cy)) %>%
      filter(run_date == rundate) %>%
      arrange(desc(cy_change)) %>%
      distinct(cyence_id)

    temp_ex_company_scores = rbind(temp_ex_company_scores, temp[temp$cyence_id %in% as.character(temp_top_unique[1:200,]$cyence_id), ])
  }
  # write into csv
  write.csv(temp_ex_company_scores, paste(sector, ".csv", sep = ""))
}

## US TAB ENDS













## 2. EU TAB STARTS
# Connect to database
rundate = as.Date('2018-10-01')

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

# Two months' cyence score for heatmaps
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3_eu.score where run_date >= '", rundate-months(1), "'")
scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
scores$cyence_id <- as.character(scores$cyence_id)
scores <- scores[order(cyence_id, run_date)]

# EU Company Info
query <- paste0("select distinct run_date, cyence_id, company_name, website, city, state, country, cyence_sector, sic4, revenue, income, employees from er.company_info_eu where run_date >= '", rundate-months(1), "'
                and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
eu_companies <- dbGetQuery(con_postgresql, query) %>% as.data.table()
eu_companies$cyence_id <- as.character(eu_companies$cyence_id)
eu_companies <- eu_companies[order(cyence_id, run_date)]


# Data Preparation for top10
query <- paste0("select distinct cyence_id, company_name, cyence_sector, revenue from er.company_info_eu where run_date >= '", rundate-months(5), "'
                and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
ex_company <- dbGetQuery(con_postgresql, query) %>% as.data.table()
ex_company$cyence_id <- as.character(ex_company$cyence_id)
# ex_company <- ex_company[order(run_date)]

# Cyence score
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3_eu.score where run_date >= '", rundate-months(5), "'
                and cyence_id in (select cyence_id from er.company_info_eu)")
ex_scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
ex_scores$cyence_id <- as.character(ex_scores$cyence_id)
ex_scores <- ex_scores[order(run_date)]
dbDisconnect(con_postgresql)




# Step 1: Data Manipulation for Maps (2 months' data)
companies_scores <- inner_join(eu_companies, scores, by = c("cyence_id", "run_date")) %>%
  filter(!is.na(company_name)) %>% # filter NAs in company_name column
  # filter(.$country == "United States") %>%
  arrange(desc(cyence_id))

# Group by country
country_scores_avg <- companies_scores[companies_scores$run_date == rundate-months(0), ] %>% # order by descending cyence_id
  group_by(country) %>%
  summarise(country_cy_avg = mean(cy),
            country_sus_avg = mean(sus),
            country_mo_avg = mean(mo),
            n_companies = n())

# write as csv
write.csv(country_scores_avg, "country_scores_avg_eu.csv")


# Step 2: Data Manipulation for heatmaps (2 months' data)
# Left join scores& Company Info by cyence_id
companies_scores <- inner_join(scores, eu_companies, by = c("cyence_id" = "cyence_id", "run_date" = "run_date")) %>%
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

# write as csv
write.csv(sector_scores_avg, "sector_scores_avg_eu.csv")



# Step 3: Data Manipulation for score changes by sector
# Left join scores& Company Info by cyence_id
ex_company$revenue_bins <- ifelse(ex_company$revenue < 5, "0-5M",
                                  ifelse((ex_company$revenue >= 5) & (ex_company$revenue < 10), "5-10M",
                                         ifelse((ex_company$revenue >= 10) & (ex_company$revenue < 25), "10-25M",
                                                ifelse((ex_company$revenue >= 25) & (ex_company$revenue < 50), "25-50M",
                                                       ifelse((ex_company$revenue >= 50) & (ex_company$revenue < 100), "50-100M",
                                                              ifelse((ex_company$revenue >= 100) & (ex_company$revenue < 500), "100-500M",
                                                                     ifelse((ex_company$revenue >= 500) & (ex_company$revenue < 1000), "500M-1B",
                                                                            ifelse((ex_company$revenue >= 1000) & (ex_company$revenue < 5000), "1-5B",
                                                                                   ifelse((ex_company$revenue >= 5000) & (ex_company$revenue < 10000), "5-10B","10B& up")))))))))


ex_scores <- ex_scores[ex_scores$cyence_id %in% ex_company$cyence_id, ]
ex_company_scores <- inner_join(ex_company, ex_scores, by = c("cyence_id" = "cyence_id"))
ex_company_scores$run_date <- as.Date(ex_company_scores$run_date)


# get top companies across all sectors/ revenue bins
temp = ex_company_scores
temp_top_unique = temp %>%
  group_by(cyence_id) %>%
  mutate(cy_change = cy - lag(cy)) %>%
  filter(run_date == rundate) %>%
  arrange(desc(cy_change)) %>%
  distinct(cyence_id)

temp_ex_company_scores = data.frame(X.x = integer(),
                                    cyence_id = character(),
                                    company_name = factor(),
                                    cyence_sector = factor(),
                                    revenue = numeric(),
                                    income = numeric(),
                                    employees = integer(),
                                    revenue_bins = character(),
                                    X.y = integer(),
                                    run_date = as.Date(character()),
                                    cy = numeric(),
                                    mo = numeric(),
                                    sus = numeric())

temp_ex_company_scores = rbind(temp_ex_company_scores, temp[temp$cyence_id %in% as.character(temp_top_unique[1:200,]$cyence_id), ])
write.csv(temp_ex_company_scores, paste("All_eu", ".csv", sep = ""))


# get top companies in every sector (200 to get enough valid obs)
for (sector in c("Education & Research", "Licensed Professional Services", "Financial Services", "Membership Organizations", "Healthcare",
                 "Consumer Services", "Wholesale Trade", "Manufacturing", "Hospitality", "Software and Technology Services",
                 "Non-Profit Organizations", "Business Services", "Publishing", "Retail Trade", "Utilities",
                 "Transportation Services", "Agriculture & Mining")) {
  
  # Initialize an empty data frame
  temp_ex_company_scores = data.frame(X.x = integer(),
                                      cyence_id = character(),
                                      company_name = factor(),
                                      cyence_sector = factor(),
                                      revenue = numeric(),
                                      income = numeric(),
                                      employees = integer(),
                                      revenue_bins = character(),
                                      X.y = integer(),
                                      run_date = as.Date(character()),
                                      cy = numeric(),
                                      mo = numeric(),
                                      sus = numeric())
  
  for (revenue in c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
                    "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up")) {
    
    temp = ex_company_scores[(ex_company_scores$cyence_sector == sector) & (ex_company_scores$revenue_bins == revenue), ]
    
    temp_top_unique = temp %>%
      group_by(cyence_id) %>%
      mutate(cy_change = cy - lag(cy)) %>%
      filter(run_date == rundate) %>%
      arrange(desc(cy_change)) %>%
      distinct(cyence_id)
    
    temp_ex_company_scores = rbind(temp_ex_company_scores, temp[temp$cyence_id %in% as.character(temp_top_unique[1:200,]$cyence_id), ])
  }
  # write into csv
  write.csv(temp_ex_company_scores, paste(sector, "_eu.csv", sep = ""))
}

## EU TAB ENDS


















## 3. JP TAB STARTS
# Connect to database
rundate = as.Date('2018-10-01')

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

# Two months' cyence score for heatmaps
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3_jp.score where run_date >= '", rundate-months(1), "'")
scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
scores$cyence_id <- as.character(scores$cyence_id)
scores <- scores[order(cyence_id, run_date)]

# EU Company Info
query <- paste0("select distinct run_date, cyence_id, company_name, website,cyence_sector, sic4, revenue, income, employees from er.company_info_jp where run_date >= '", rundate-months(1), "'
                and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
jp_companies <- dbGetQuery(con_postgresql, query) %>% as.data.table()
jp_companies$cyence_id <- as.character(jp_companies$cyence_id)
jp_companies <- jp_companies[order(cyence_id, run_date)]


# Data Preparation for top10
query <- paste0("select distinct cyence_id, company_name, cyence_sector, revenue from er.company_info_jp where run_date >= '", rundate-months(5), "'
                and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
ex_company <- dbGetQuery(con_postgresql, query) %>% as.data.table()
ex_company$cyence_id <- as.character(ex_company$cyence_id)
# ex_company <- ex_company[order(run_date)]

# Cyence score
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3_jp.score where run_date >= '", rundate-months(5), "'
                and cyence_id in (select cyence_id from er.company_info_jp)")
ex_scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
ex_scores$cyence_id <- as.character(ex_scores$cyence_id)
ex_scores <- ex_scores[order(run_date)]
dbDisconnect(con_postgresql)




# Step 1: Data Manipulation for Maps (2 months' data)
# Step 2: Data Manipulation for heatmaps (2 months' data)
# Left join scores& Company Info by cyence_id
companies_scores <- inner_join(scores, jp_companies, by = c("cyence_id" = "cyence_id", "run_date" = "run_date")) %>%
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

# write as csv
write.csv(sector_scores_avg, "sector_scores_avg_jp.csv")



# Step 3: Data Manipulation for score changes by sector
# Left join scores& Company Info by cyence_id
ex_company$revenue_bins <- ifelse(ex_company$revenue < 5, "0-5M",
                                  ifelse((ex_company$revenue >= 5) & (ex_company$revenue < 10), "5-10M",
                                         ifelse((ex_company$revenue >= 10) & (ex_company$revenue < 25), "10-25M",
                                                ifelse((ex_company$revenue >= 25) & (ex_company$revenue < 50), "25-50M",
                                                       ifelse((ex_company$revenue >= 50) & (ex_company$revenue < 100), "50-100M",
                                                              ifelse((ex_company$revenue >= 100) & (ex_company$revenue < 500), "100-500M",
                                                                     ifelse((ex_company$revenue >= 500) & (ex_company$revenue < 1000), "500M-1B",
                                                                            ifelse((ex_company$revenue >= 1000) & (ex_company$revenue < 5000), "1-5B",
                                                                                   ifelse((ex_company$revenue >= 5000) & (ex_company$revenue < 10000), "5-10B","10B& up")))))))))


ex_scores <- ex_scores[ex_scores$cyence_id %in% ex_company$cyence_id, ]
ex_company_scores <- inner_join(ex_company, ex_scores, by = c("cyence_id" = "cyence_id"))
ex_company_scores$run_date <- as.Date(ex_company_scores$run_date)


# get top companies across all sectors/ revenue bins
temp = ex_company_scores
temp_top_unique = temp %>%
  group_by(cyence_id) %>%
  mutate(cy_change = cy - lag(cy)) %>%
  filter(run_date == rundate) %>%
  arrange(desc(cy_change)) %>%
  distinct(cyence_id)

temp_ex_company_scores = data.frame(X.x = integer(),
                                    cyence_id = character(),
                                    company_name = factor(),
                                    cyence_sector = factor(),
                                    revenue = numeric(),
                                    income = numeric(),
                                    employees = integer(),
                                    revenue_bins = character(),
                                    X.y = integer(),
                                    run_date = as.Date(character()),
                                    cy = numeric(),
                                    mo = numeric(),
                                    sus = numeric())

temp_ex_company_scores = rbind(temp_ex_company_scores, temp[temp$cyence_id %in% as.character(temp_top_unique[1:200,]$cyence_id), ])
write.csv(temp_ex_company_scores, paste("All_jp", ".csv", sep = ""))



# get top companies in every sector (200 to get enough valid obs)
for (sector in c("Education & Research", "Licensed Professional Services", "Financial Services", "Membership Organizations", "Healthcare",
                 "Consumer Services", "Wholesale Trade", "Manufacturing", "Hospitality", "Software and Technology Services",
                 "Non-Profit Organizations", "Business Services", "Publishing", "Retail Trade", "Utilities",
                 "Transportation Services", "Agriculture & Mining")) {
  
  # Initialize an empty data frame
  temp_ex_company_scores = data.frame(cyence_id = character(),
                                      company_name = factor(),
                                      cyence_sector = factor(),
                                      revenue = numeric(),
                                      revenue_bins = character(),
                                      run_date = as.Date(character()),
                                      cy = numeric(),
                                      mo = numeric(),
                                      sus = numeric())
  
  for (revenue in c("0-5M", "5-10M", "10-25M", "25-50M", "50-100M",
                    "100-500M", "500M-1B", "1-5B", "5-10B", "10B& up")) {
    
    temp = ex_company_scores[(ex_company_scores$cyence_sector == sector) & (ex_company_scores$revenue_bins == revenue), ]
    
    temp_top_unique = temp %>%
      group_by(cyence_id) %>%
      mutate(cy_change = cy - lag(cy)) %>%
      filter(run_date == rundate) %>%
      arrange(desc(cy_change)) %>%
      distinct(cyence_id)
    
    temp_ex_company_scores = rbind(temp_ex_company_scores, temp[temp$cyence_id %in% as.character(temp_top_unique[1:100,]$cyence_id), ])
  }
  # write into csv
  write.csv(temp_ex_company_scores, paste(sector, "_jp.csv", sep = ""))
}

## JP TAB ENDS















## Share table review starts
# Pull all agg_ids
connect_to_redshift <- function(){
  drv <- dbDriver("PostgreSQL")
  var_names <- c("RED_DB_NAME", "RED_DB_HOST", "RED_DB_PORT", "RED_DB_USER", "RED_DB_PASS")
  aws <- Sys.getenv(var_names)
  con <- dbConnect(drv, dbname=aws[1],host=aws[2],port=aws[3],user=aws[4],password=aws[5])
  return(con)
}

con <- connect_to_redshift()
query <- paste0("select a.sw_agg_id, b.label from (select sw_agg_id, cnt from (select sw_agg_id, count(distinct cyence_id) as cnt from dev_aggregation_monthly_v3.software_share_all_1 group by sw_agg_id order by cnt desc)) a left join aggregation_monthly_v3.software_agg b on a.sw_agg_id=b.agg_id order by a.sw_agg_id")
all_sw <- dbGetQuery(con, query) %>% as.data.table()
all_sw$sw_agg_id = as.character(all_sw$sw_agg_id)
dbDisconnect(con)


### read csvs
raw_all = list()
smoothing_all = list()
for (i in 1:9) {
  filename_raw = paste0("raw_all_", i, ".csv")
  filename_smoothing = paste0("smoothing_all_", i, ".csv")
  raw_all[[i]] = fread(filename_raw) %>% mutate(cyence_id = as.character(cyence_id), run_date = as.Date(run_date)) %>% select(cyence_id, run_date, sw_agg_id, stable_flag)
  smoothing_all[[i]] = fread(filename_smoothing) %>% mutate(cyence_id = as.character(cyence_id), run_date = as.Date(run_date)) %>% select(cyence_id, run_date, sw_agg_id, expiration_flag)
}

plot_coverage_comparison <- function(input_sw_agg_id = "100005") {
  # data to use
  data1 = raw_all
  data2= smoothing_all
  
  # empty dataframe for plotting
  plotdata = data.frame(run_date = rep(unique(data1[[i]]$run_date), length(data1)),
                        coverage1 = rep(0, length(data1)),
                        coverage2 = rep(0, length(data2)))
  
  # calculate data for each overage
  for (i in 1:length(data1)) {
    plotdata$run_date[i] = unique(data1[[i]]$run_date)
    plotdata$coverage1[i] = nrow(unique(data1[[i]] %>% filter(sw_agg_id == input_sw_agg_id) %>% select(cyence_id) %>% distinct()))
    plotdata$coverage2[i] = nrow(unique(data2[[i]] %>% filter(sw_agg_id == input_sw_agg_id) %>% select(cyence_id) %>% distinct()))
  }
  
  # plot
  plotdata = melt(plotdata, id = "run_date")
  plotdata$labels = as.vector(plotdata$value)
  cy_change = (((plotdata %>% filter((run_date == "2018-09-01") & (variable == "coverage2")))$value - 
    (plotdata %>% filter((run_date == "2018-09-01") & (variable == "coverage1")))$value))/ 
    (plotdata %>% filter((run_date == "2018-09-01") & (variable == "coverage1")))$value
  
  ggplot(plotdata, aes(run_date, y = value, color = variable)) + 
    geom_line() + geom_point(color = "black") + 
    (scale_x_date(breaks = date_breaks("1 month"),
                  labels = date_format("%b %y"))) + 
    xlab("Run Date") + ylab("Number of associated companies") + 
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "grey", linetype = "dotted"),  
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      text = element_text(size = 14),
      legend.position = "top"
    ) + scale_y_continuous(limits = c(0, ceiling(max(plotdata$value)/ 10000) * 10000), 
                           breaks = seq(0, ceiling(max(plotdata$value)/ 10000) * 10000, by = 20000)) +
    geom_label_repel(aes(label = labels, color = factor(variable)), size = 3.5, fill = "gray95", show.legend = FALSE) + 
    scale_colour_manual(labels = c("Production", "Smoothing"), values = c('dodgerblue2', 'firebrick2')) + 
    labs(caption = "Run date: 2018-09-01", color = "Type of coverage") + 
    ggtitle(paste0("Product: ", all_sw[all_sw$sw_agg_id == input_sw_agg_id, ]$label, 
                   ", coverage change in last month: ", paste0(formatC(100 * cy_change, format = "f", digits = 2), "%")))
  
  
  # save plot to corresponding directory
  ggsave(sprintf("share_table_coverage_comparison/%s.png", str_replace_all(all_sw[all_sw$sw_agg_id == input_sw_agg_id, ]$label, " ", "_")), width = 8, height = 8)
}

for (sw_agg_id0 in as.character(all_sw$sw_agg_id)) {
  plot_coverage_comparison(input_sw_agg_id = sw_agg_id0)
}


## Share table review ends

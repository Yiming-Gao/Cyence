# Reference: https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/

setwd("/Users/ygao/Desktop/Dashboard/Code/ttt")
options(scipen = 999)
library(reshape2)
library(scales)
library(grid)
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

#-------------------------------------------------------------------------------------------------------------------------
### Supporting functions
#-------------------------------------------------------------------------------------------------------------------------
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


# Data Preparation
# The data is re-pulled every time the we provide a new cyence_id
query <- paste0("select distinct cyence_id, company_name, cyence_sector, revenue, income, employees from er.company_info where run_date >= '", rundate-months(5), "' 
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


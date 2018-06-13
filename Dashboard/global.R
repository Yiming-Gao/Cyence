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
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3.score where run_date >= '", "2018-06-01", "' limit 10")
scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
scores$cyence_id <- as.character(scores$cyence_id)
scores <- scores[order(cyence_id, run_date)]
dbDisconnect(con_postgresql)



setwd("/Users/ygao/Desktop/Dashboard/Code/try")
options(scipen = 999)

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

# The data is re-pulled every time the we provide a new cyence_id
# Example: cyence_id = 79942718
query <- paste0("select distinct run_date, cyence_id, company_name, website, city, state, country, cyence_sector, sic4, revenue, income, employees from er.company_info_union where run_date >= '", rundate-months(11), "' 
                and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration' and cyence_id = 79942718")
ex_company <- dbGetQuery(con_postgresql, query) %>% as.data.table()
ex_company$cyence_id <- as.character(ex_company$cyence_id)
ex_company <- ex_company[order(run_date)]

# Cyence score
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3.score where run_date >= '", rundate-months(11), "'and cyence_id = 79942718")
ex_scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
ex_scores$cyence_id <- as.character(ex_scores$cyence_id)
ex_scores <- ex_scores[order(run_date)]
dbDisconnect(con_postgresql)

# Left join scores& Company Info by cyence_id
companies_scores <- inner_join(scores, companies, by = c("cyence_id" = "cyence_id", "run_date" = "run_date")) %>% 
  filter(!is.na(company_name)) %>% # filter NAs in company_name column
  # filter(.$country == "United States") %>%
  arrange(desc(cyence_id)) 

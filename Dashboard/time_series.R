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
ex_company_scores <- inner_join(ex_company, ex_scores, by = c("run_date" = "run_date"))

# basic time series
library(reshape2)
library(scales)

ex_company_scores_plot <- melt(ex_company_scores[, c("run_date", "cy", "sus", "mo")], id = "run_date")
p <- ggplot(ex_company_scores_plot, aes(x = run_date, y = value, colour = variable)) + 
  geom_line() +
  (scale_x_date(breaks = date_breaks("1 month"),
                labels = date_format("%b %y"))) +
  xlab("Run Date") + ylab("Score") + 
  labs(title = paste0("Cyence Scores for ", names(sort(table(ex_company_scores$company_name), decreasing = TRUE)[1])),
       color = "Type") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    text = element_text(size = 12)
  ) +
  theme_minimal()

ggplotly(p)

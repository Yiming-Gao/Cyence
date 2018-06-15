# Reference: https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/

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
# Fetch the data
# Company Info
query <- paste0("select distinct run_date, cyence_id, company_name, website, city, state, country, cyence_sector, sic4, revenue, income, employees from er.company_info_union where run_date >= '", rundate-months(1), "' 
                and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
companies <- dbGetQuery(con_postgresql, query) %>% as.data.table()
companies$cyence_id <- as.character(companies$cyence_id)
companies <- companies[order(cyence_id, run_date)]

# Cyence score
query <- paste0("select distinct run_date, cyence_id, cy, mo, sus from model_monthly_v3.score where run_date >= '", rundate-months(1), "'")
scores <- dbGetQuery(con_postgresql, query) %>% as.data.table()
scores$cyence_id <- as.character(scores$cyence_id)
scores <- scores[order(cyence_id, run_date)]
dbDisconnect(con_postgresql)

# Left join scores& Company Info by cyence_id
companies_scores <- left_join(scores, companies, by = c("cyence_id" = "cyence_id", "run_date" = "run_date")) %>% 
  filter(!is.na(company_name)) %>% # filter NAs in company_name column
  # filter(.$country == "United States") %>%
  arrange(desc(cyence_id)) 

# Create a new column containing revenue bins
companies_scores$revenue_bins <- ifelse(companies_scores$revenue < 20, "<20",
                                        ifelse((companies_scores$revenue >= 20) & (companies_scores$revenue < 50), "20-50",
                                        ifelse((companies_scores$revenue >= 50) & (companies_scores$revenue < 100), "50-100",
                                        ifelse((companies_scores$revenue >= 100) & (companies_scores$revenue < 500), "100-500", ">500"))))

# Change this to a specific order
companies_scores$revenue_bins <- factor(companies_scores$revenue_bins, levels = c("<20", "20-50", "50-100", "100-500", ">500"))

# Group by cyence_sector (industry)
sector_scores_avg <- companies_scores %>% # order by descending cyence_id
  group_by(cyence_sector, revenue_bins, run_date) %>%
  summarise(n_companies = n(),
            sector_cy_avg = mean(cy),
            sector_sus_avg = mean(sus),
            sector_mo_avg = mean(mo)) %>%
  group_by(cyence_sector, revenue_bins) %>%
  mutate(cy_pct_change = (sector_cy_avg - lag(sector_cy_avg))/ sector_cy_avg,
         sus_pct_change = (sector_sus_avg - lag(sector_sus_avg))/ sector_sus_avg,
         mo_pct_change = (sector_mo_avg - lag(sector_mo_avg))/ sector_mo_avg) %>%
  na.omit() 
# %>%
#   mutate(cy_pct_change = sprintf("%.3f %%", 100 * cy_pct_change),
#          sus_pct_change = sprintf("%.3f %%", 100 * sus_pct_change),
#          mo_pct_change = sprintf("%.3f %%", 100 * mo_pct_change))


# basic heatmap
p <- ggplot(sector_scores_avg, aes(revenue_bins, cyence_sector,
                                   # customize the tooltip
                                   text = paste("Sector: ", cyence_sector, "\n",
                                                "Number of Companies: ", n_companies, "\n",
                                                "Change in score: ", sprintf("%.3f %%", 100 * cy_pct_change)))) + 
  geom_tile(aes(fill = cy_pct_change), colour = "white") + 
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 0, limits = range(sector_scores_avg$cy_pct_change), name = "Change (%)") +
  labs(title = paste0("Change (%) in Cyence Score from \n", month.abb[month(rundate)-1], " to ", month.abb[month(rundate)])) +
  xlab("Revenue Bins") + ylab("Cyence Sector") +
  theme(
    plot.title = element_text(size = 16, face = "bold.italic"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    text = element_text(size = 12)
  ) 

ggplotly(p, tooltip = c("text"))

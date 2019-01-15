rm(list = ls())
library(magrittr)
library(RPostgreSQL)
library(tidyr)
library(lubridate)
library(iptools)
library(data.table)
library(dplyr)
library(plyr)
library(scales)
library(ggrepel)
library(zoo)
library(stringr)
library(odbc)
library(tibble)
library(openxlsx)
library(Rgb)
options(scipen = 999)

# read in for_dashboard.csv
outliers = fread("for_dashboard.csv") # %>% mutate(agg_id = as.character(agg_id))
outlier_agg_ids = outliers[outliers$type == "software", ]$agg_id
rm(outliers)



connect_to_redshift <- function(){
  drv <- dbDriver("PostgreSQL")
  var_names <- c("RED_DB_NAME", "RED_DB_HOST", "RED_DB_PORT", "RED_DB_USER", "RED_DB_PASS")
  aws <- Sys.getenv(var_names)
  con <- dbConnect(drv, dbname = aws[1], host = aws[2], port = aws[3], user = aws[4], password = aws[5])
  return(con)
}


rundate = as.Date('2019-01-01')


#----------------------------------------------- Helpful Reference Tables -----------------------------------------------#
con <- connect_to_redshift()
query <- "select * from aggregation_monthly_v3.software_agg"
software_agg <- dbGetQuery(con, query) %>% as.data.table() 
dbDisconnect(con)





#----------------------------------------------- Coverage History -----------------------------------------------#
# get coverage history for all agg IDs
con <- connect_to_redshift()
query <- paste0("select sw_agg_id, run_date, count(distinct cyence_id) as num_cyid from aggregation_monthly_v3.software_share_all where run_date >= '", rundate-months(12),
                "' group by run_date, sw_agg_id order by sw_agg_id, run_date")
coverage_history_all <- dbGetQuery(con, query) %>% as.data.table() 
write.csv(coverage_history_all, 'software/coverage_history_all.csv', row.names = F) 
dbDisconnect(con)

# rm(coverage_history)






#----------------------------------------------- IP Coverage Analysis for Outliers -----------------------------------------------#
# initialize data frames
ip_info_change_new <- data.frame("ip_dec" = numeric(),
                                 "n_cyid" = integer(),
                                 "outlier_agg_id" = numeric())

ip_info_change_disappeared <- data.frame("ip_dec" = numeric(),
                                         "n_cyid" = integer(),
                                         "outlier_agg_id" = numeric())
  

# start analysis
for (outlier in outlier_agg_ids) {
  
  print(paste0("Outlier Agg ID: ", outlier))
  
  # outlier's parent agg id
  outlier_info = paste0("Vendor: ", software_agg[software_agg$agg_id == outlier, ]$vendor,
                        ", Product: ", software_agg[software_agg$agg_id == outlier, ]$product,
                        ", Label: ", software_agg[software_agg$agg_id == outlier, ]$label)
  print(paste0("Outlier's information: ", outlier_info))
  
  
  #================= Part 1 -- Data Pull & Analysis -- cyence_id level =================#
  con <- connect_to_redshift()
  
  # What this query does: 
  # gets all records that were connected to the outlier_agg_id either last month or this month
  query <- paste0("select distinct run_date, cyence_id, ip_dec, vuln_agg_id from aggregation_monthly_v3.software_share_all where run_date >= '", 
                  rundate-months(1), "' and sw_agg_id = ", outlier, "")
  all_records <- dbGetQuery(con, query) %>% as.data.table() 
  dbDisconnect(con)
  
  
  # get cyence IDs for two months
  cyid_last_month <- unique((all_records %>% filter(run_date == rundate-months(1)))$cyence_id)
  cyid_this_month <- unique((all_records %>% filter(run_date == rundate))$cyence_id)

  # new cyence IDs
  new_cyids <- setdiff(cyid_this_month,  cyid_last_month)
  
  # disappeared IPs
  disappeared_cyids <- setdiff(cyid_last_month,  cyid_this_month)
  
  
  #================= Part 2 -- Analysis -- IP level =================#
  # get IP information for two months that associated with this agg_id
  ip_last_month <- unique((all_records %>% filter(run_date == rundate-months(1)))$ip_dec)
  ip_this_month <- unique((all_records %>% filter(run_date == rundate))$ip_dec)
  
  # new cyence IDs
  new_ips <- setdiff(ip_this_month,  ip_last_month)
  
  # find top IPs that are connecting to > 1k companies and brought most new companies in this month
  new_ips_most_connected <- all_records %>% 
    filter((run_date == rundate) & (ip_dec %in% new_ips)) %>% 
    select(run_date, cyence_id, ip_dec) %>% 
    distinct() %>% 
    group_by(ip_dec) %>% 
    dplyr::summarise(n = n()) %>% 
    filter(n >= 500) %>%
    arrange(desc(n))
  
  # handle empty data frame
  if (nrow(new_ips_most_connected) == 0) {
    new_ips_most_connected <-ip_info_change_new[0, ]
  } else {
    colnames(new_ips_most_connected)[colnames(new_ips_most_connected) == "n"] <- "n_cyid"
    new_ips_most_connected$outlier_agg_id = outlier
  }
  
  
  # disappeared IPs
  disappeared_ips <- setdiff(ip_last_month,  ip_this_month)
  
  # find top 50 IPs that dropped most companies from last month
  disappeared_ips_most_connected <- all_records %>% 
    filter((run_date == rundate-months(1)) & (ip_dec %in% disappeared_ips)) %>% 
    select(run_date, cyence_id, ip_dec) %>% 
    distinct() %>% 
    group_by(ip_dec) %>% 
    dplyr::summarise(n = n()) %>% 
    filter(n >= 500) %>%
    arrange(desc(n))
  
  # handle empty data frame
  if (nrow(disappeared_ips_most_connected) == 0) {
    disappeared_ips_most_connected <-ip_info_change_disappeared[0, ]
  } else {
    colnames(disappeared_ips_most_connected)[colnames(disappeared_ips_most_connected) == "n"] <- "n_cyid"
    disappeared_ips_most_connected$outlier_agg_id = outlier
      }
  
  
  
  #================= Part 3 -- Combine agg id change + parent agg id change =================#
  
  # if (nrow(ip_info %>% filter(agg_id0 != agg_id)) == 0) {IPs_changed_location <- ip_info[0, ]} else {IPs_changed_location <- ip_info %>% filter(agg_id0 != agg_id) %>% mutate(type = "switch location")}
  ip_info_change_new <- rbind(ip_info_change_new, data.frame(new_ips_most_connected)) 
  ip_info_change_disappeared<- rbind(ip_info_change_disappeared, data.frame(disappeared_ips_most_connected)) 
  
  # clean up things 
  rm(cyid_last_month, cyid_this_month, ip_last_month, ip_this_month, disappeared_cyids, disappeared_ips, disappeared_ips_most_connected, new_cyids, new_ips, new_ips_most_connected)
  
}




# ip_info_change = ip_info_change %>% arrange(desc(n_cyid))
# ip_info_change_parent = ip_info_change_parent %>% arrange(desc(n_cyid))




# ----------------------------------------------- Output changed-IP information as xlsx with formatting -----------------------------------------------#

write_spreadsheet <- function(input_outlier = outlier) {
  
  change_new = ip_info_change_new %>% filter(outlier_agg_id == input_outlier) 
  change_disappeared = ip_info_change_disappeared %>% filter(outlier_agg_id == input_outlier) %>% mutate(n_cyid_last_month = n_cyid) %>% select(ip_dec, n_cyid_last_month, outlier_agg_id)
  
  # create a workbook
  wb <- createWorkbook()
  
  # add tabs
  addWorksheet(wb, "new IPs")
  addWorksheet(wb, "disappeared IPs")

  
  # write data
  writeData(wb, "new IPs", change_new %>% select(ip_dec, n_cyid))
  writeData(wb, "disappeared IPs", change_disappeared %>% select(ip_dec, n_cyid_last_month))

  
  # create styles
  row_border_style = createStyle(border = "bottom", borderStyle = "medium")
  col_border_style = createStyle(border = "right", borderStyle = "medium", halign = "center")
  
  # add styles - sheet1
  if (nrow(change_new) > 0) {
    headerStyle <- createStyle(fontSize = 14, halign = "center", border = "TopBottom", borderStyle = "medium", textDecoration = c("bold"))
    addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = seq(1, 2, 1), gridExpand = TRUE)
    addStyle(wb, sheet = 1, createStyle(halign = "center"), seq(2, nrow(change_new) + 1, 1), cols = seq(1, 2, 1), gridExpand = TRUE)
    setColWidths(wb, 1, cols = seq(1, 2, 1), widths = 16) ## set column width for row names column
  }
  
  # add styles - sheet2
  if (nrow(change_disappeared) > 0) {
    headerStyle <- createStyle(fontSize = 14, halign = "center", border = "TopBottom", borderStyle = "medium", textDecoration = c("bold"))
    addStyle(wb, sheet = 2, headerStyle, rows = 1, cols = seq(1, 2, 1), gridExpand = TRUE)
    addStyle(wb, sheet = 2, createStyle(halign = "center"), seq(2, nrow(change_disappeared) + 1, 1), cols = seq(1, 2, 1), gridExpand = TRUE)
    setColWidths(wb, 2, cols = seq(1, 2, 1), widths = 16) ## set column width for row names column
  }
  
  
  # save workbook
  saveWorkbook(wb, paste0("software/outlier_ip_info/outlier_", input_outlier, ".xlsx"), overwrite = TRUE)
  
}

# write to xlsx file
for (outlier in outlier_agg_ids) {
  write_spreadsheet(input_outlier = outlier)
}


########## ----------------------------------------------- Stability Metrics ----------------------------------------------- ##########
con <- connect_to_redshift()
query <- paste0("Select sw_agg_id, month_cnt, count(distinct cyence_id) as cyid_cnt from 

                (select sw_agg_id, cyence_id, count(distinct run_date) as month_cnt from aggregation_monthly_v3.software_share_all where cyence_id in (select distinct cyence_id from aggregation_monthly_v3.software_share_all where run_date = '", rundate-months(4),
                "') and run_date >= '", rundate-months(4),
                "' and run_date <= '", rundate,
                "' group by sw_agg_id, cyence_id)
                
                Group by sw_agg_id, month_cnt order by sw_agg_id, month_cnt")
stability_agg <- dbGetQuery(con, query) %>% as.data.table() 
dbDisconnect(con)

# calculation
stability_agg_all <- stability_agg %>% group_by(sw_agg_id) %>% 
  dplyr::summarise(total_cyids = sum(cyid_cnt)) %>%
  left_join(stability_agg %>% filter(month_cnt == 5)) %>%
  select(-c(month_cnt)) %>%
  mutate(stable_cyid_cnt = ifelse(is.na(cyid_cnt), 0, cyid_cnt)) %>% 
  select(-c(cyid_cnt)) %>%
  mutate(stability = stable_cyid_cnt/ total_cyids) %>%
  left_join(software_agg %>% select(agg_id, label), by = c("sw_agg_id" = "agg_id")) %>% distinct() 

write.csv(stability_agg_all %>% select(sw_agg_id, label, total_cyids, stable_cyid_cnt, stability), 'software/stability_agg.csv')


# weighed average stability
print(paste0("stability: ", round(sum((stability_agg_all$total_cyids/ sum(stability_agg_all$total_cyids)) * stability_agg_all$stability), 4))) # 71.48%


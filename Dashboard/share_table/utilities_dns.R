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
outlier_agg_ids = outliers[outliers$type == "dns", ]$agg_id
rm(outliers)
# outlier_agg_ids = c(300238, 300234, 308428, 300246, 300641)


connect_to_redshift <- function(){
  drv <- dbDriver("PostgreSQL")
  var_names <- c("RED_DB_NAME", "RED_DB_HOST", "RED_DB_PORT", "RED_DB_USER", "RED_DB_PASS")
  aws <- Sys.getenv(var_names)
  con <- dbConnect(drv, dbname = aws[1], host = aws[2], port = aws[3], user = aws[4], password = aws[5])
  return(con)
}


rundate = as.Date('2019-01-01')

#----------------------------------------------- Helpful Reference Tables -----------------------------------------------#
# only keep type = 'ns'
con <- connect_to_redshift()
query <- "select parent_agg_id, parent, agg_id, region, type from aggregation_monthly_v3.dns_mx_agg join aggregation_monthly_v3.dns_mx_parent_agg using (parent_agg_id) where type = 'ns'"
dns_agg <- dbGetQuery(con, query) %>% as.data.table() 
dbDisconnect(con)




#----------------------------------------------- Coverage History -----------------------------------------------#
# get coverage history for all agg IDs
con <- connect_to_redshift()
query <- paste0("select agg_id, run_date, count(distinct cyence_id) as num_cyid from aggregation_monthly_v3.dns_mx_providers_all where run_date >= '", rundate-months(6),
                "' and agg_id in (",
                paste0(dns_agg$agg_id, collapse = ","),
                ") group by run_date, agg_id order by agg_id, run_date")
coverage_history_all <- dbGetQuery(con, query) %>% as.data.table() 
write.csv(coverage_history_all, 'dns/coverage_history_ns.csv', row.names = F) 


# get coverage history for outliers
query <- paste0("select parent_agg_id, run_date, count(distinct cyence_id) as num_cyid from aggregation_monthly_v3.dns_mx_providers_all join aggregation_monthly_v3.dns_mx_agg using (agg_id) join aggregation_monthly_v3.dns_mx_parent_agg using (parent_agg_id) where parent_agg_id in (",
                paste0(unique(dns_agg$parent_agg_id), collapse = ","),
                ") and run_date >= '", rundate-months(6),
                "' group by run_date, parent_agg_id order by parent_agg_id, run_date")
coverage_history_parent <- dbGetQuery(con, query) %>% as.data.table() 
write.csv(coverage_history_parent, 'dns/coverage_history_parent.csv', row.names = F) 
dbDisconnect(con)






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
  outlier_parent_agg_id = dns_agg[agg_id == outlier, ]$parent_agg_id
  print(paste0("Outlier's Parent Agg ID: ", outlier_parent_agg_id))
  
  # ================= Part 1 -- Data Pull & Analysis -- Agg ID level ================= #
  con <- connect_to_redshift()
  
  # What this query does: 
  # gets all records associated with IP that were connected to the outlier either last month or this month, and are connected to the outlier's parent
  # This will help us detect IPs that are switching agg ids, but won't detect IPs that are switching parents
  query <- paste0("select distinct cyence_id, agg_id, parent_agg_id, region, type, run_date from aggregation_monthly_v3.dns_mx_providers_all inner join aggregation_monthly_v3.dns_mx_agg using (agg_id) inner join aggregation_monthly_v3.dns_mx_parent_agg using (parent_agg_id) where run_date >= '", 
                  rundate-months(1), 
                  "' and agg_id = ", outlier, " and parent_agg_id = ", outlier_parent_agg_id, "")
  
  dns_table <- dbGetQuery(con, query) %>% as.data.table()
  
  query <- paste0("select run_date, cyence_id, ip_dec from (select * from ip_us.cyence_to_ip_union union select * from ip_eu.cyence_to_ip_union union select * from ip_jp.cyence_to_ip_union) where cyence_id in (",
                  paste0(unique(dns_table$cyence_id), collapse = ","), ") and run_date >= '",
                  rundate-months(2),
                  "'")
  # merge two tables
  matching_ip_entries_same_parent <- dbGetQuery(con, query) %>% as.data.table() %>% mutate(run_date = run_date-months(-1)) %>% left_join(dns_table %>% select(cyence_id, agg_id, parent_agg_id, region), by = "cyence_id")
  dbDisconnect(con)
  
  
  # get IP information for two months that associated with this agg_id
  ip_last_month <- unique((matching_ip_entries_same_parent %>% filter(run_date == rundate-months(1)))$ip_dec)
  ip_this_month <- unique((matching_ip_entries_same_parent %>% filter(run_date == rundate))$ip_dec)
  
  # new cyence IDs
  new_ips <- setdiff(ip_this_month,  ip_last_month)
  
  # find top IPs that are connecting to > 1k companies and brought most new companies in this month
  new_ips_most_connected <- matching_ip_entries_same_parent %>% 
    filter((run_date == rundate) & (ip_dec %in% new_ips)) %>% 
    select(run_date, cyence_id, ip_dec) %>% 
    distinct() %>% 
    group_by(ip_dec) %>% 
    dplyr::summarise(n = n()) %>% 
    filter(n >= 50) %>%
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
  disappeared_ips_most_connected <- matching_ip_entries_same_parent %>% 
    filter((run_date == rundate-months(1)) & (ip_dec %in% disappeared_ips)) %>% 
    select(run_date, cyence_id, ip_dec) %>% 
    distinct() %>% 
    group_by(ip_dec) %>% 
    dplyr::summarise(n = n()) %>% 
    filter(n >= 50) %>%
    arrange(desc(n))
  
  # handle empty data frame
  if (nrow(disappeared_ips_most_connected) == 0) {
    disappeared_ips_most_connected <-ip_info_change_disappeared[0, ]
  } else {
    colnames(disappeared_ips_most_connected)[colnames(disappeared_ips_most_connected) == "n"] <- "n_cyid"
    disappeared_ips_most_connected$outlier_agg_id = outlier
  }
  
  
  ip_info_change_new <- rbind(ip_info_change_new, data.frame(new_ips_most_connected)) 
  ip_info_change_disappeared<- rbind(ip_info_change_disappeared, data.frame(disappeared_ips_most_connected)) 
  
  # clean up things 
  rm(disappeared_ips, disappeared_ips_most_connected, new_ips, new_ips_most_connected)
  
}
rm(matching_ip_entries_same_parent)



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
  saveWorkbook(wb, paste0("dns/outlier_ip_info/outlier_", input_outlier, ".xlsx"), overwrite = TRUE)
  
}

# write to xlsx file
for (outlier in outlier_agg_ids) {
  write_spreadsheet(input_outlier = outlier)
}














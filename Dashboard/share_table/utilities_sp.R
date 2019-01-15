# this script is good for service providers review
# as of 1/15/2019

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
outlier_agg_ids = outliers[outliers$type == "service_provider", ]$agg_id
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
con <- connect_to_redshift()
query <- "select * from aggregation_monthly_v3.service_providers_agg"
service_providers_agg <- dbGetQuery(con, query) %>% as.data.table() 

query <- "select * from aggregation_monthly_v3.sp_parent_agg"
sp_parent_agg <- dbGetQuery(con, query) %>% as.data.table() 
dbDisconnect(con)





#----------------------------------------------- Coverage History -----------------------------------------------#
# get coverage history for all agg IDs
con <- connect_to_redshift()
query <- paste0("select agg_id, run_date, count(distinct cyence_id) as num_cyid from aggregation_monthly_v3.service_providers_all where run_date >= '", rundate-months(6),
                "' group by run_date, agg_id order by agg_id, run_date")
coverage_history_all <- dbGetQuery(con, query) %>% as.data.table() 
write.csv(coverage_history_all, 'service_providers/coverage_history_all.csv', row.names = F) 

# parent level
query <- paste0("select parent_agg_id, run_date, count(distinct cyence_id) as num_cyid from aggregation_monthly_v3.service_providers_all join aggregation_monthly_v3.service_providers_agg using (agg_id) join aggregation_monthly_v3.sp_parent_agg using (parent_agg_id) where run_date >= '", rundate-months(6),
                "' group by run_date, parent_agg_id order by parent_agg_id, run_date")
coverage_history_parent <- dbGetQuery(con, query) %>% as.data.table() 
write.csv(coverage_history_parent, 'service_providers/coverage_history_parent.csv', row.names = F) 
dbDisconnect(con)
# may need to clean them up to reduce memory usage when necessary
# rm(coverage_history)






#----------------------------------------------- IP Coverage Analysis for Outliers -----------------------------------------------#
# initialize data frames
ip_info_change_outliers <- data.frame("outlier_agg_id" = numeric(),
                                      "type" = character(),
                                      "ip_dec" = numeric(),
                                      "country_code0" = character(),
                                      "stateprov0" = character(),
                                      "agg_id0" = integer(),
                                      "n_cyid0" = integer(),
                                      "country_code" = character(),
                                      "stateprov" = character(),
                                      "agg_id" = integer(),
                                      "n_cyid" = integer())

ip_info_change_parent_outliers <- data.frame("outlier_agg_id" = numeric(),
                                             "ip_dec" = numeric(),
                                             "parent_agg_id0" = list(),
                                             "n_cyid0" = integer(),
                                             "parent_agg_id" = list(),
                                             "n_cyid" = integer())


# start analysis
for (outlier in outlier_agg_ids) {
  
  print(paste0("Outlier Agg ID: ", outlier))
  
  # outlier's parent agg id
  outlier_parent_agg_id = service_providers_agg[agg_id == outlier, ]$parent_agg_id
  print(paste0("Outlier's Parent Agg ID: ", outlier_parent_agg_id))
  
  #================= Part 1 -- Data Pull & Analysis -- Agg ID level =================#
  con <- connect_to_redshift()
  
  # What this query does: 
  # gets all records associated with IP that were connected to the outlier either last month or this month, and are connected to the outlier's parent
  # This will help us detect IPs that are switching locations, but won't detect IPs that are switching parents
  query <- paste0("select ip_dec, cyence_id, agg_id, country_code, stateprov, run_date from aggregation_monthly_v3.service_providers_all inner join aggregation_monthly_v3.service_providers_agg using (agg_id) inner join aggregation_monthly_v3.sp_parent_agg using (parent_agg_id) where run_date >= '", 
                  rundate-months(1), "' and ip_dec in (select distinct ip_dec from aggregation_monthly_v3.service_providers_all where run_date >= '", rundate-months(1),
                  "' and agg_id = ", outlier, ") and parent_agg_id = ", outlier_parent_agg_id, "")
  matching_ip_entries_same_parent <- dbGetQuery(con, query) %>% as.data.table() 
  dbDisconnect(con)
  
  
  # get coverage by this outlier's IP for last month
  ip_info_last_month <- matching_ip_entries_same_parent %>% filter(run_date == rundate-months(1)) %>%
    select(ip_dec, country_code, stateprov, agg_id, cyence_id) %>% distinct() %>%
    dplyr::group_by(ip_dec, country_code, stateprov, agg_id) %>%
    dplyr::summarise(n_cyid = n())
  
  
  # get coverage by this outlier's IP for this month
  ip_info_this_month <- matching_ip_entries_same_parent %>% filter(run_date == rundate) %>%
    select(ip_dec, cyence_id, country_code, stateprov, agg_id, cyence_id) %>% distinct() %>%
    dplyr::group_by(ip_dec, country_code, stateprov, agg_id) %>%
    dplyr::summarise(n_cyid = n()) 
  
  
  # new IPs
  new_IPs <- setdiff(unique(ip_info_this_month$ip_dec),  unique(ip_info_last_month$ip_dec))
  
  # disappeared IPs
  disappeared_IPs <- setdiff(unique(ip_info_last_month$ip_dec),  unique(ip_info_this_month$ip_dec))
  
  # rename columns
  ip_info_last_month <- rename(ip_info_last_month, c("country_code" = "country_code0", "stateprov" = "stateprov0", "agg_id" = "agg_id0", "n_cyid" = "n_cyid0"))
  
  
  # full join two tables & remove old tables
  ip_info <- full_join(ip_info_last_month, ip_info_this_month, by = c("ip_dec" = "ip_dec"))
  ip_info$outlier_agg_id <- outlier
  
  
  # Find IPs that switch agg ids
  if (nrow(ip_info %>% filter(agg_id0 != agg_id)) == 0) {IPs_changed_location <- ip_info[0, ]} else {IPs_changed_location <- ip_info %>% filter(agg_id0 != agg_id) %>% mutate(type = "switch location")}
  if (nrow(ip_info %>% filter(ip_dec %in% new_IPs)) == 0) {IPs_new <- ip_info[0, ]} else {IPs_new <- ip_info %>% filter(ip_dec %in% new_IPs) %>% mutate(type = "new IPs")}
  if (nrow(ip_info %>% filter(ip_dec %in% disappeared_IPs)) == 0) {IPs_disappeared <- ip_info[0, ]} else {IPs_disappeared <- ip_info %>% filter(ip_dec %in% disappeared_IPs) %>% mutate(type = "disappeared IPs")}
  ip_info_change <- rbind(IPs_changed_location, IPs_new, IPs_disappeared)
  ip_info_change <- ip_info_change[, c(10, 11, seq(1, 9, 1))]
  
  
  
  
  
  
  #================= Part 2 -- Data Pull & Analysis -- Parent Agg ID level =================#
  con <- connect_to_redshift()
  query <- paste0("select distinct ip_dec, parent_agg_id, run_date from aggregation_monthly_v3.service_providers_all inner join aggregation_monthly_v3.service_providers_agg using (agg_id) inner join aggregation_monthly_v3.sp_parent_agg using (parent_agg_id) where run_date >= '", 
                  rundate-months(1), "' and ip_dec in (",
                  paste0(unique(ip_info$ip_dec), collapse = ","), ") order by ip_dec, run_date")
  matching_ip_entries <- dbGetQuery(con, query) %>% as.data.table() %>% mutate(identifier = paste0(ip_dec, parent_agg_id)) %>% select(ip_dec, parent_agg_id, run_date, identifier) %>% distinct()
  dbDisconnect(con)
  
  
  # different identifiers: find IPs that switching (or)
  diff = matching_ip_entries %>% 
    filter(identifier %in% c(setdiff((matching_ip_entries %>% filter(run_date == rundate-months(1)))$identifier, (matching_ip_entries %>% filter(run_date == rundate))$identifier),
                             setdiff((matching_ip_entries %>% filter(run_date == rundate))$identifier, (matching_ip_entries %>% filter(run_date == rundate-months(1)))$identifier)))
  
  # last month & this month
  ip_info_last_month <- matching_ip_entries %>% filter((run_date == rundate-months(1)) & (ip_dec %in% diff$ip_dec)) %>% select(ip_dec, parent_agg_id) %>% group_by(ip_dec) %>% dplyr::summarise(parent_agg_id = list(parent_agg_id))
  ip_info_this_month <- matching_ip_entries %>% filter((run_date == rundate) & (ip_dec %in% diff$ip_dec)) %>% select(ip_dec, parent_agg_id) %>% group_by(ip_dec) %>% dplyr::summarise(parent_agg_id = list(parent_agg_id))
  ip_info_change_parent <- full_join(ip_info_last_month, ip_info_this_month, by = c("ip_dec" = "ip_dec"))
  ip_info_change_parent <- rename(ip_info_change_parent, c("parent_agg_id.x" = "parent_agg_id0", "parent_agg_id.y" = "parent_agg_id"))
  ip_info_change_parent$outlier_agg_id <- outlier
  ip_info_change_parent <- ip_info_change_parent %>% left_join(ip_info %>% select(ip_dec, n_cyid0, n_cyid), by = c("ip_dec" = "ip_dec"))
  ip_info_change_parent <- ip_info_change_parent %>% select(outlier_agg_id, ip_dec, parent_agg_id0, n_cyid0, parent_agg_id, n_cyid)
  
  
  # clean up things 
  rm(ip_info_last_month, ip_info_this_month, IPs_changed_location, IPs_new, IPs_disappeared, new_IPs, disappeared_IPs, diff)

  
  
  #================= Part 3 -- Combine agg id change + parent agg id change =================#
  ip_info_change_outliers <- rbind(ip_info_change_outliers, data.frame(ip_info_change)) # agg id level
  ip_info_change_parent_outliers <- rbind(ip_info_change_parent_outliers, data.frame(ip_info_change_parent)) # parent agg id level
  
} 
rm(matching_ip_entries, matching_ip_entries_same_parent)



# ip_info_change = ip_info_change %>% arrange(desc(n_cyid))
# ip_info_change_parent = ip_info_change_parent %>% arrange(desc(n_cyid))




# ----------------------------------------------- Output changed-IP information as xlsx with formatting -----------------------------------------------#

write_spreadsheet <- function(input_outlier = outlier) {
  
  ip_info_change = ip_info_change_outliers %>% filter(outlier_agg_id == input_outlier) %>% arrange(desc(n_cyid))
  ip_info_change_parent = ip_info_change_parent_outliers %>% filter(outlier_agg_id == input_outlier) %>% arrange(desc(n_cyid))
  
  # create a workbook
  wb <- createWorkbook()
  
  # add tabs
  addWorksheet(wb, "switch location (same parent)")
  addWorksheet(wb, "new IPs")
  addWorksheet(wb, "disappeared IPs")
  addWorksheet(wb, "switch parent")
  
  
  # write data
  writeData(wb, "switch location (same parent)", ip_info_change[ip_info_change$type == "switch location", ])
  writeData(wb, "new IPs", ip_info_change[ip_info_change$type == "new IPs", ])
  writeData(wb, "disappeared IPs", ip_info_change[ip_info_change$type == "disappeared IPs", ])
  writeData(wb, "switch parent", ip_info_change_parent)
  
  # create styles
  row_border_style = createStyle(border = "bottom", borderStyle = "medium")
  col_border_style = createStyle(border = "right", borderStyle = "medium", halign = "center")
  
  # add styles - sheet1
  if (nrow(ip_info_change[ip_info_change$type == "switch location", ]) > 0) {
    headerStyle <- createStyle(fontSize = 14, halign = "center", border = "TopBottom", borderStyle = "medium", textDecoration = c("bold"))
    addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = seq(1, 11, 1), gridExpand = TRUE)
    addStyle(wb, sheet = 1, createStyle(halign = "center"),seq(2, nrow(ip_info_change[ip_info_change$type == "switch location", ]) + 1, 1), cols = seq(1, 11, 1), gridExpand = TRUE)
    setColWidths(wb, 1, cols = seq(1, 11, 1), widths = 16) ## set column width for row names column
    addStyle(wb, 1, col_border_style, rows = seq(2, nrow(ip_info_change[ip_info_change$type == "switch location", ]) + 1, 1), cols = 7)
    addStyle(wb, 1, createStyle(fgFill = rgb(252/ 255, 233/ 255, 218/ 255), halign = "center"), rows = seq(2, nrow(ip_info_change[ip_info_change$type == "switch location", ]) + 1, 1), cols = 2)
    addStyle(wb, 1, createStyle(fgFill = rgb(252/ 255, 233/ 255, 218/ 255), halign = "center", border = "right", borderStyle = "medium"), rows = seq(2, nrow(ip_info_change[ip_info_change$type == "switch location", ]) + 1, 1), cols = 3)
  }
    
  # add styles - sheet2
  if (nrow(ip_info_change[ip_info_change$type == "new IPs", ]) > 0) {
    headerStyle <- createStyle(fontSize = 14, halign = "center", border = "TopBottom", borderStyle = "medium", textDecoration = c("bold"))
    addStyle(wb, sheet = 2, headerStyle, rows = 1, cols = seq(1, 11, 1), gridExpand = TRUE)
    addStyle(wb, sheet = 2, createStyle(halign = "center"),seq(2, nrow(ip_info_change[ip_info_change$type == "new IPs", ]) + 1, 1), cols = seq(1, 11, 1), gridExpand = TRUE)
    setColWidths(wb, 2, cols = seq(1, 11, 1), widths = 16) ## set column width for row names column
    addStyle(wb, 2, col_border_style, rows = seq(2, nrow(ip_info_change[ip_info_change$type == "new IPs", ]) + 1, 1), cols = 7)
    addStyle(wb, 2, createStyle(fgFill = rgb(235/ 255, 241/ 255, 223/ 255), halign = "center"), rows = seq(2, nrow(ip_info_change[ip_info_change$type == "new IPs", ]) + 1, 1), cols = 2)
    addStyle(wb, 2, createStyle(fgFill = rgb(235/ 255, 241/ 255, 223/ 255), halign = "center", border = "right", borderStyle = "medium"), rows = seq(2, nrow(ip_info_change[ip_info_change$type == "new IPs", ]) + 1, 1), cols = 3)
  }
    
  # add styles - sheet3
  if (nrow(ip_info_change[ip_info_change$type == "disappeared IPs", ]) > 0) {
    headerStyle <- createStyle(fontSize = 14, halign = "center", border = "TopBottom", borderStyle = "medium", textDecoration = c("bold"))
    addStyle(wb, sheet = 3, headerStyle, rows = 1, cols = seq(1, 11, 1), gridExpand = TRUE)
    addStyle(wb, sheet = 3, createStyle(halign = "center"),seq(2, nrow(ip_info_change[ip_info_change$type == "disappeared IPs", ]) + 1, 1), cols = seq(1, 11, 1), gridExpand = TRUE)
    setColWidths(wb, 3, cols = seq(1, 11, 1), widths = 16) ## set column width for row names column
    addStyle(wb, 3, col_border_style, rows = seq(2, nrow(ip_info_change[ip_info_change$type == "disappeared IPs", ]) + 1, 1), cols = 7)
    addStyle(wb, 3, createStyle(fgFill = rgb(241/ 255, 220/ 255, 219/ 255), halign = "center"), rows = seq(2, nrow(ip_info_change[ip_info_change$type == "disappeared IPs", ]) + 1, 1), cols = 2)
    addStyle(wb, 3, createStyle(fgFill = rgb(241/ 255, 220/ 255, 219/ 255), halign = "center", border = "right", borderStyle = "medium"), rows = seq(2, nrow(ip_info_change[ip_info_change$type == "disappeared IPs", ]) + 1, 1), cols = 3)
  }
    
  # add styles - sheet4
  if (nrow(ip_info_change_parent) > 0) {
    headerStyle <- createStyle(fontSize = 12, halign = "center", border = "TopBottom", borderStyle = "medium", textDecoration = c("bold"))
    addStyle(wb, sheet = 4, headerStyle, rows = 1, cols = seq(1, 6, 1), gridExpand = TRUE) 
    setColWidths(wb, 4, cols = seq(1, 6, 1), widths = 14)
    setColWidths(wb, 4, cols = 2, widths = 18)
    setColWidths(wb, 4, cols = 3, widths = 18)
    setColWidths(wb, 4, cols = 5, widths = 18)
    addStyle(wb, sheet = 4, createStyle(halign = "center"), seq(2, nrow(ip_info_change_parent) + 1, 1), cols = seq(1, 6, 1), gridExpand = TRUE)
    addStyle(wb, 4, createStyle(border = "right", borderStyle = "medium", halign = "center"), rows = seq(2, nrow(ip_info_change_parent) + 1, 1), cols = 2)
    addStyle(wb, 4, createStyle(border = "right", borderStyle = "medium", halign = "center"), rows = seq(2, nrow(ip_info_change_parent) + 1, 1), cols = 4)
  }
  
  
  # save workbook
  saveWorkbook(wb, paste0("service_providers/outlier_ip_info/outlier_", input_outlier, ".xlsx"), overwrite = TRUE)
  
}

# write IP info to xlsx file for each agg_id
for (outlier in outlier_agg_ids) {
  write_spreadsheet(input_outlier = outlier)
}



# ----------------------------------------------- Coverage Plots -----------------------------------------------#
roundUp <- function(x) {
  digit = ceiling(log10(x)) - 2
  ceiling(x/ (10^digit)) * (10^digit) + (10^digit)
}

plot_coverage_agg_id <- function(input_agg_id = 300029) {
  
  # dataframe for plotting
  plotdata = coverage_history_all[coverage_history_all$agg_id == input_agg_id, ]
  info = paste0("Parent: ", sp_parent_agg[sp_parent_agg$parent_agg_id == service_providers_agg[service_providers_agg$agg_id == input_agg_id, ]$parent_agg_id, ]$parent,
                ", Type: ", sp_parent_agg[sp_parent_agg$parent_agg_id == service_providers_agg[service_providers_agg$agg_id == input_agg_id, ]$parent_agg_id, ]$type,
                ", Location: ", service_providers_agg[service_providers_agg$agg_id == input_agg_id, ]$region, 
                ", ", service_providers_agg[service_providers_agg$agg_id == input_agg_id, ]$country_code,
                " ", service_providers_agg[service_providers_agg$agg_id == input_agg_id, ]$stateprov)
  
  # plot
  plotdata$run_date = as.Date(plotdata$run_date)
  plotdata$labels = as.vector(plotdata$num_cyid)
  ggplot(plotdata, aes(x = run_date, y = num_cyid)) + 
    geom_line() + geom_point(color = "navy", size = 2) +
    (scale_x_date(breaks = date_breaks("1 month"),
                  labels = date_format("%b %y"))) + 
    ylim(0, roundUp(max(plotdata$num_cyid))) +
    xlab("Run Date") + ylab("Coverage") + 
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "grey", linetype = "dotted"),  
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(face = "italic", color = "black"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 11),
      text = element_text(size = 12)
    ) +
    geom_label_repel(aes(label = labels), size = 3.5, color = "dodgerblue2", fill = "gray95") + 
    labs(title = paste0("Agg ID: ", input_agg_id),
         subtitle = info)
  
  
  # save plot to corresponding directory
  if (input_agg_id %in% outlier_agg_ids) {
    ggsave(sprintf("service_providers/coverage_plots/%s.png", paste0("outlier_", input_agg_id)), width = 12, height = 6)
  }  else {
    ggsave(sprintf("service_providers/coverage_plots/%s.png", input_agg_id), width = 12, height = 6)
  }
}

for (agg_id in unique(coverage_history_all$agg_id)[1:10]) {
  plot_coverage_agg_id(input_agg_id = agg_id)
} # plot 1:10 for testing purpose

plot_coverage_parent <- function(input_parent_agg_id = 30001) {
  
  # dataframe for plotting
  plotdata = coverage_history_parent[coverage_history_parent$parent_agg_id == input_parent_agg_id, ]
  info = paste0("Parent: ", sp_parent_agg[sp_parent_agg$parent_agg_id == input_parent_agg_id, ]$parent,
                ", Type: ", sp_parent_agg[sp_parent_agg$parent_agg_id == input_parent_agg_id, ]$type)
  
  # plot
  plotdata$run_date = as.Date(plotdata$run_date)
  plotdata$labels = as.vector(plotdata$num_cyid)
  ggplot(plotdata, aes(x = run_date, y = num_cyid)) + 
    geom_line() + geom_point(color = "navy", size = 2) +
    (scale_x_date(breaks = date_breaks("1 month"),
                  labels = date_format("%b %y"))) + 
    ylim(0, roundUp(max(plotdata$num_cyid))) +
    xlab("Run Date") + ylab("Coverage") + 
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "grey", linetype = "dotted"),  
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(face = "italic", color = "black"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 11),
      text = element_text(size = 12)
    ) +
    geom_label_repel(aes(label = labels), size = 3.5, color = "dodgerblue2", fill = "gray95") + 
    labs(title = paste0("Parent Agg ID: ", input_parent_agg_id),
         subtitle = info)
  
  
  # save plot to corresponding directory
  ggsave(sprintf("service_providers/coverage_plots_parent/%s.png", input_parent_agg_id), width = 12, height = 6)
  
}

for (parent_agg_id in unique(coverage_history_parent$parent_agg_id)[1:10]) {
  plot_coverage_parent(input_parent_agg_id = parent_agg_id)
} # plot 1:10 for testing purpose



########## ----------------------------------------------- Stability Metrics ----------------------------------------------- ##########
# part 1 - data preparation
con <- connect_to_redshift()
query <- paste0("Select agg_id, month_cnt, count(distinct cyence_id) as cyid_cnt from 
                
                (select agg_id, cyence_id, count(distinct run_date) as month_cnt from aggregation_monthly_v3.service_providers_all where cyence_id in (select distinct cyence_id from aggregation_monthly_v3.service_providers_all where run_date = '", rundate-months(4),
                "') and run_date >= '", rundate-months(4),
                "' and run_date <= '", rundate,
                "' group by agg_id, cyence_id)
                
                Group by agg_id, month_cnt order by agg_id, month_cnt")
stability_agg <- dbGetQuery(con, query) %>% as.data.table() 

query <- paste0("Select parent_agg_id, month_cnt, count(distinct cyence_id) as cyid_cnt from 
                
                (select parent_agg_id, cyence_id, count(distinct run_date) as month_cnt from aggregation_monthly_v3.service_providers_all join aggregation_monthly_v3.service_providers_agg using (agg_id) join aggregation_monthly_v3.sp_parent_agg using (parent_agg_id) where cyence_id in (select distinct cyence_id from aggregation_monthly_v3.service_providers_all where run_date = '", rundate-months(4),
                "') and run_date >= '", rundate-months(4),
                "' and run_date <= '", rundate,
                "' group by parent_agg_id, cyence_id)
                
                Group by parent_agg_id, month_cnt order by parent_agg_id, month_cnt")
stability_parent_agg <- dbGetQuery(con, query) %>% as.data.table() 
dbDisconnect(con)

# read data
stability_agg = stability_agg %>% 
  left_join(service_providers_agg %>% select(parent_agg_id, agg_id), by = "agg_id") %>% 
  left_join(sp_parent_agg %>% select(parent_agg_id, type), by = "parent_agg_id") %>% distinct() 

stability_parent_agg = stability_parent_agg %>% left_join(sp_parent_agg %>% select(parent_agg_id, type), by = "parent_agg_id") %>% distinct() 


# by agg_id
stability_agg_all <- stability_agg %>% group_by(agg_id) %>% 
  dplyr::summarise(total_cyids = sum(cyid_cnt)) %>%
  left_join(stability_agg %>% filter(month_cnt == 5)) %>%
  select(-c(month_cnt)) %>%
  mutate(stable_cyid_cnt = ifelse(is.na(cyid_cnt), 0, cyid_cnt)) %>% 
  select(-c(cyid_cnt)) %>%
  mutate(stability = stable_cyid_cnt/ total_cyids)

# write to csv
write.csv(stability_agg_all %>% select(agg_id, parent_agg_id, type, total_cyids, stable_cyid_cnt, stability), 'service_providers/stability_agg.csv')

# by parent_agg_id
stability_parent_agg_all <- stability_parent_agg %>% group_by(parent_agg_id) %>% 
  dplyr::summarise(total_cyids = sum(cyid_cnt)) %>%
  left_join(stability_parent_agg %>% filter(month_cnt == 5)) %>%
  select(-c(month_cnt)) %>%
  mutate(stable_cyid_cnt = ifelse(is.na(cyid_cnt), 0, cyid_cnt)) %>% 
  select(-c(cyid_cnt)) %>%
  mutate(stability = stable_cyid_cnt/ total_cyids)

# write to csv
write.csv(stability_parent_agg_all%>% select(parent_agg_id, type, total_cyids, stable_cyid_cnt, stability), 'service_providers/stability_parent_agg.csv')


# part 2 - stability calculation
# cloud - weighed average stability
cloud_temp = stability_agg_all %>% filter(type == "cloud") 
print(paste0("type: cloud, agg_id stability: ", round(sum((cloud_temp$total_cyids/ sum(cloud_temp$total_cyids)) * cloud_temp$stability), 4))) # agg_id level: 90.86%
cloud_temp = stability_parent_agg_all %>% filter(type == "cloud") 
print(paste0("type: cloud, parent_agg_id stability: ", round(sum((cloud_temp$total_cyids/ sum(cloud_temp$total_cyids)) * cloud_temp$stability), 4))) # parent_agg_id level: 93.87%
rm(cloud_temp)


# isp - weighed average stability
isp_temp = stability_agg_all %>% filter(type == "isp") 
print(paste0("type: isp, agg_id stability: ", round(sum((isp_temp$total_cyids/ sum(isp_temp$total_cyids)) * isp_temp$stability), 4))) # agg_id level: 91.03%
isp_temp = stability_parent_agg_all %>% filter(type == "isp") 
print(paste0("type: isp, parent_agg_id stability: ", round(sum((isp_temp$total_cyids/ sum(isp_temp$total_cyids)) * isp_temp$stability), 4))) # parent_agg_id level: 93.18%
rm(isp_temp)


# cdn - weighed average stability
cdn_temp = stability_agg_all %>% filter(type == "cdn") 
print(paste0("type: cdn, agg_id stability: ", round(sum((cdn_temp$total_cyids/ sum(cdn_temp$total_cyids)) * cdn_temp$stability), 4))) # agg_id level: 35.48%
cdn_temp = stability_parent_agg_all %>% filter(type == "cdn") 
print(paste0("type: cdn, parent_agg_id stability: ", round(sum((cdn_temp$total_cyids/ sum(cdn_temp$total_cyids)) * cdn_temp$stability), 4))) # parent_agg_id level: 79.59%
rm(cdn_temp)


# hosting - weighed average stability
hosting_temp = stability_agg_all %>% filter(type == "hosting") 
print(paste0("type: hosting, agg_id stability: ", round(sum((hosting_temp$total_cyids/ sum(hosting_temp$total_cyids)) * hosting_temp$stability), 4))) # agg_id level: 93%
hosting_temp = stability_parent_agg_all %>% filter(type == "hosting") 
print(paste0("type: hosting, parent_agg_id stability: ", round(sum((hosting_temp$total_cyids/ sum(hosting_temp$total_cyids)) * hosting_temp$stability), 4))) # parent_agg_id level: 95.30%
rm(hosting_temp)


# all - weighed average stability
all_temp = stability_agg_all
print(paste0("type: all, agg_id stability: ", round(sum((all_temp$total_cyids/ sum(all_temp$total_cyids)) * all_temp$stability), 4))) # agg_id level: 82.21%
all_temp = stability_parent_agg_all
print(paste0("type: all, parent_agg_id stability: ", round(sum((all_temp$total_cyids/ sum(all_temp$total_cyids)) * all_temp$stability), 4))) # parent_agg_id level: 92.93%
rm(all_temp)

 












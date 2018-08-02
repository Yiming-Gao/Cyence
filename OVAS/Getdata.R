# 201801
query1 = "SELECT b.cyence_id, a.nvt_oid, MAX(a.severity) AS severity, MAX(a.cvss) AS cvss FROM scan.ovas_results_201801 a INNER JOIN (SELECT cyence_id, ip_dec FROM er_staging.cyence_to_ip WHERE run_date = '2018-01-01' AND ip_dec NOT IN (3273005816,1208576286,2992224938,874077184,1760548103,3328835083,3438076914,1538856250,3423287344,609632182,3328832523,3359274967,3422290141,910273539,1559400458,3328835146,3325687748,1986159033,3325929113,3328832586)) b ON a.ip = b.ip_dec AND a.severity > 0 AND (port IS NULL or port != 53) GROUP BY b.cyence_id, a.nvt_oid"
ovas_results_201801 <- dbGetQuery(con, query1) %>% as.data.table()
write.csv(ovas_results_201801, "ovas_results_patch_201801.csv")

# 201802
query2 = "SELECT b.cyence_id, a.nvt_oid, MAX(a.severity) AS severity, MAX(a.cvss) AS cvss FROM scan.ovas_results_201802 a INNER JOIN (SELECT cyence_id, ip_dec FROM er_staging.cyence_to_ip WHERE run_date = '2018-02-01' AND ip_dec NOT IN (3273005816,1208576286,2992224938,874077184,1760548103,3328835083,3438076914,1538856250,3423287344,609632182,3328832523,3359274967,3422290141,910273539,1559400458,3328835146,3325687748,1986159033,3325929113,3328832586)) b ON a.ip = b.ip_dec AND a.severity > 0 AND (port IS NULL or port != 53) GROUP BY b.cyence_id, a.nvt_oid"
ovas_results_201802 <- dbGetQuery(con, query2) %>% as.data.table()
write.csv(ovas_results_201802, "ovas_results_patch_201802.csv")

# 201803
query3 = "SELECT b.cyence_id, a.nvt_oid, MAX(a.severity) AS severity, MAX(a.cvss) AS cvss FROM scan.ovas_results_201803 a INNER JOIN (SELECT cyence_id, ip_dec FROM er_staging.cyence_to_ip WHERE run_date = '2018-03-01' AND ip_dec NOT IN (3273005816,1208576286,2992224938,874077184,1760548103,3328835083,3438076914,1538856250,3423287344,609632182,3328832523,3359274967,3422290141,910273539,1559400458,3328835146,3325687748,1986159033,3325929113,3328832586)) b ON a.ip = b.ip_dec AND a.severity > 0 AND (port IS NULL or port != 53) GROUP BY b.cyence_id, a.nvt_oid"
ovas_results_201803 <- dbGetQuery(con, query3) %>% as.data.table()
write.csv(ovas_results_201803, "ovas_results_patch_201803.csv")

# 201804
query4 = "SELECT b.cyence_id, a.nvt_oid, MAX(a.severity) AS severity, MAX(a.cvss) AS cvss FROM scan.ovas_results_201804 a INNER JOIN (SELECT cyence_id, ip_dec FROM er_staging.cyence_to_ip WHERE run_date = '2018-04-01' AND ip_dec NOT IN (3273005816,1208576286,2992224938,874077184,1760548103,3328835083,3438076914,1538856250,3423287344,609632182,3328832523,3359274967,3422290141,910273539,1559400458,3328835146,3325687748,1986159033,3325929113,3328832586)) b ON a.ip = b.ip_dec AND a.severity > 0 AND (port IS NULL or port != 53) GROUP BY b.cyence_id, a.nvt_oid"
ovas_results_201804 <- dbGetQuery(con, query4) %>% as.data.table()
write.csv(ovas_results_201804, "ovas_results_patch_201804.csv")

# 201805
query5 = "SELECT b.cyence_id, a.nvt_oid, MAX(a.severity) AS severity, MAX(a.cvss) AS cvss FROM scan.ovas_results_201805 a INNER JOIN (SELECT cyence_id, ip_dec FROM er_staging.cyence_to_ip WHERE run_date = '2018-05-01' AND ip_dec NOT IN (3273005816,1208576286,2992224938,874077184,1760548103,3328835083,3438076914,1538856250,3423287344,609632182,3328832523,3359274967,3422290141,910273539,1559400458,3328835146,3325687748,1986159033,3325929113,3328832586)) b ON a.ip = b.ip_dec AND a.severity > 0 AND (port IS NULL or port != 53) GROUP BY b.cyence_id, a.nvt_oid"
ovas_results_201805 <- dbGetQuery(con, query5) %>% as.data.table()
write.csv(ovas_results_201805, "ovas_results_patch_201805.csv")

# 201806
query6 = "SELECT b.cyence_id, a.nvt_oid, MAX(a.severity) AS severity, MAX(a.cvss) AS cvss FROM scan.ovas_results_201806 a INNER JOIN (SELECT cyence_id, ip_dec FROM er_staging.cyence_to_ip WHERE run_date = '2018-06-01' AND ip_dec NOT IN (3273005816,1208576286,2992224938,874077184,1760548103,3328835083,3438076914,1538856250,3423287344,609632182,3328832523,3359274967,3422290141,910273539,1559400458,3328835146,3325687748,1986159033,3325929113,3328832586)) b ON a.ip = b.ip_dec AND a.severity > 0 AND (port IS NULL or port != 53) GROUP BY b.cyence_id, a.nvt_oid"
ovas_results_201806 <- dbGetQuery(con, query6) %>% as.data.table()
write.csv(ovas_results_201806, "ovas_results_patch_201806.csv")

# 201807 (select one more: a.ip, group by a.ip)
query7 = "SELECT b.cyence_id, a.ip, a.nvt_oid, MAX(a.severity) AS severity, MAX(a.cvss) AS cvss FROM scan.ovas_results_201807 a INNER JOIN (SELECT cyence_id, ip_dec FROM er_staging.cyence_to_ip WHERE run_date = '2018-07-01' AND ip_dec NOT IN (3273005816,1208576286,2992224938,874077184,1760548103,3328835083,3438076914,1538856250,3423287344,609632182,3328832523,3359274967,3422290141,910273539,1559400458,3328835146,3325687748,1986159033,3325929113,3328832586)) b ON a.ip = b.ip_dec AND a.severity > 0 AND (port IS NULL or port != 53) GROUP BY b.cyence_id, a.ip, a.nvt_oid"
ovas_results_201807 <- dbGetQuery(con, query7) %>% as.data.table()
write.csv(ovas_results_201807, "ovas_results_patch_201807.csv")








###########################################################################################
##### Get a sample of representative companies by industry/ sector to scan frequently #####
###########################################################################################

### Step 1:  Pull the data
connect_to_db = function() {
  drv = dbDriver("PostgreSQL")
  aws_confid = Sys.getenv(c("DB_NAME", "DB_HOST", "DB_PORT", "DB_USER", "DB_PASS"))
  con =
    dbConnect(
      drv,
      dbname = aws_confid["DB_NAME"],
      host = aws_confid["DB_HOST"],
      port = aws_confid["DB_PORT"],
      user = aws_confid["DB_USER"],
      password = aws_confid["DB_PASS"]
    )
  return(con)
}

connect_to_redshift <- function(){
  drv <- dbDriver("PostgreSQL")
  var_names <- c("RED_DB_NAME", "RED_DB_HOST", "RED_DB_PORT", "RED_DB_USER", "RED_DB_PASS")
  aws <- Sys.getenv(var_names)
  con <- dbConnect(drv, dbname=aws[1],host=aws[2],port=aws[3],user=aws[4],password=aws[5])
  return(con)
}

con <- connect_to_db()
con_redshift <- connect_to_redshift()
rundate = as.Date('2018-07-01')

## 1. Company Information
# US company info
query = paste0("select distinct run_date, cyence_id, company_name, city, state, country, cyence_sector, revenue, employees from er.company_info_union where run_date = '", rundate-months(0), "' 
                and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0")
company_info_us = dbGetQuery(con, query) %>% as.data.table()
company_info_us[, cyence_id := as.character(cyence_id)]

# JP company info
query = paste0("select distinct run_date, cyence_id, company_name, city, state, country, cyence_sector, revenue, employees from er.company_info_jp where run_date = '", rundate-months(0), "' 
                and (gap_flag = 1 or revenue >= 20) and gov_flag = 0")
company_info_jp = dbGetQuery(con, query) %>% as.data.table()
company_info_jp[, cyence_id := as.character(cyence_id)]

# Merge US& JP company info
company_info <- rbind(company_info_us, company_info_jp)


## 2. Description of vulntype
query = paste0("select * from scan.ref_vulntype_lookup")
vulntype = dbGetQuery(con, query) %>% as.data.table()

## 3. ref_oid_lookup
query = paste0("select nvt_oid, nvt_name, cvss, vulntype, cvss_complexity, cvss_authentication, cvss_network, cvss_confidentiality, cvss_availability, cvss_integrity from scan.ref_oid_lookup")
ref_oid_lookup = dbGetQuery(con, query) %>% as.data.table()


# merge ref_oid with OVAS scans
ovas_ref_oid_201806 = ovas_results_201806 %>% merge(ref_oid_lookup_201806, by = c("nvt_oid"))
# filter_id1 = unique(ovas_ref_oid_201806[ovas_ref_oid_201806$vulntype %in% c("00000010", "00001000"), ]$cyence_id) # DoS and Execution


## 4. Cyence to ip
query = paste0("select * from er_staging.cyence_to_ip where run_date = '", rundate-months(0), "'")
cyence_to_ip = dbGetQuery(con, query) %>% as.data.table()

# Self-owned IPs
cyence_to_ip_selfowned = cyence_to_ip[ip_label == "Self-owned", ] 

# Self-owned IPs that are not shared by other companies
cyence_to_ip_selfowned_unique = cyence_to_ip_selfowned[ip_dec %in% names(table(cyence_to_ip_selfowned$ip_dec))[table(cyence_to_ip_selfowned$ip_dec) == 1], ]


## 5. OpenVAS data
# 5.a 201807 (select one more: a.ip, group by a.ip)
query7 = "SELECT b.cyence_id, a.ip, a.nvt_oid, MAX(a.severity) AS severity, MAX(a.cvss) AS cvss FROM scan.ovas_results_201807 a INNER JOIN (SELECT cyence_id, ip_dec FROM er_staging.cyence_to_ip WHERE run_date = '2018-07-01' AND ip_dec NOT IN (3273005816,1208576286,2992224938,874077184,1760548103,3328835083,3438076914,1538856250,3423287344,609632182,3328832523,3359274967,3422290141,910273539,1559400458,3328835146,3325687748,1986159033,3325929113,3328832586)) b ON a.ip = b.ip_dec AND a.severity > 0 AND (port IS NULL or port != 53) GROUP BY b.cyence_id, a.ip, a.nvt_oid"
ovas_results_201807 <- dbGetQuery(con, query7) %>% as.data.table()
ovas_results_201807_temp <- ovas_results_201807[ip %in% cyence_to_ip_selfowned_unique$ip_dec, ]
filter_id1 = unique(ovas_results_201807_temp$cyence_id)


### Step 2: Data Preparation
# function for revenue bins (optional)
bin_revenue <- function(revenue) {
  if (revenue < 20) {
    return("0-20M")
  } else if (revenue < 100) {
    return("20-100M")
  } else if (revenue < 500) {
    return("100-500M")
  } else {
    return("500M+")
  }
}

# Apply functions to data 
company_info$rev_bin <- sapply(company_info$revenue, bin_revenue)
company_info$rev_bin <- factor(company_info$rev_bin, levels = c("0-20M", "20-100M", "100-500M", "500M+"))

# Merge company info with patching cadence by cyence_id
merged_data = company_info %>% 
  merge(result, by = c("cyence_id")) 

# Filter out NAs & companies with 6 months' data
merged_data <- merged_data[!is.na(revenue) & !is.na(cyence_sector)]
merged_data <- merged_data[!(is.na(Low_patched) | is.na(Medium_patched) | is.na(High_patched))]
merged_data <- merged_data[N_months == 6,]

filter_id2 = merged_data[merged_data$cyence_id %in% filter_id1, ]$cyence_id
# head(merged_data[merged_data$cyence_id %in% filter_id2, ])

company_info_temp = merged_data[merged_data$cyence_id %in% filter_id2, ]

# basic information
sectors = sort(unique(company_info$cyence_sector))



# Function: get top n cyence_ids with highest revenue values for a given sector and rev_bin
sample_companies <- function(top_n) {
  # an empty data frame
  data_final = data.frame(run_date = as.Date(character()),
                          cyence_id = character(),
                          employees = integer(),
                          revenue = numeric(),
                          cyence_sector = character(),
                          country = character())
  
  for (sector0 in sectors) {
    data = company_info[(company_info$cyence_sector == sector0), ]
    data = data[!duplicated(data[, c("employees", "revenue")])] # filter data by non-duplicated columns (employees, revenue) 
    data_temp = data.frame(head(data[order(-data$revenue), ], top_n)) # Sort the data by descending revenue 
    data_final = rbind(data_final, data_temp)
  }
  return(data_final)
}




###########################################################################################
################# Calculate patching cadence for sample of companies& IPs #################
###########################################################################################






###########################################################################################
##################### Exploration of Sample companies (Alena's script) ####################
###########################################################################################
# Box plot
ggplot(company_info_temp, aes_string(x = cyence_sector, y = Total_patched)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12)) +
  expand_limits(y = 0)


# heatmap

# Calculate mean of "total_patched" by revenue/ sector
grouped <- company_info_temp[, .(mean_total_patched = mean(Total_patched), 
                                 mean_low_patched = mean(Low_patched), 
                                 mean_medium_patched = mean(Medium_patched), 
                                 mean_high_patched = mean(High_patched), .N), 
                             by = c("rev_bin", "cyence_sector")]


ggplot(grouped[N >= 10], aes_string(x = "rev_bin", y = "cyence_sector", fill = "mean_total_patched")) +
  scale_fill_distiller(palette = "Spectral", limits = c(0, 0.75)) +
  geom_tile() +
  geom_text(aes(label = round(get("mean_total_patched"), 3))) +
  theme(text = element_text(size = 14))

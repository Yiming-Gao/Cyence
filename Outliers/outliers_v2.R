### OUTLIER DETECTION v2
# Input data: er.company_info_union -> companies that are shown in the app, according to the following logic:
# country name is in table appcache.vw_country and (gap_flag is true or rev>=20) and gov_flag is not 1 and sector is not public administration

# Steps:
# 1 - Basic heuristics
#   (i)   Values out of expected range
#   (ii)  Income > Revenue
#   (iii) Big changes compared to previous month:
#         * cyence_sector: different sector
#         * Revenue: top changes in revenue for companies with revenue > 250
#         * Income: top changes in income for companies with income > 100
#         * Employees: top changes in employees for companies with employees > 2000

# 2 - Autoencoder
#   (i)   Find anomalies in data using (revenue, sector, employees, income) for current month
#   (ii)  Keep a list of companies that are anomalies because of the huge size but are correct

# 3 - Combine results and prioritize review
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls())
setwd("/Users/ygao/Desktop/Outliers")
source('utilities.R')
options(scipen = 999)

# =========================================================================================================================
# 1 - Load and process data
# =========================================================================================================================
rundate = as.Date('2018-06-01')

## Pull companies from DB for last 2 months
con <- connect_to_db()
query <- paste0("select distinct run_date, cyence_id, company_name, website, city, state, country, cyence_sector, sic4, revenue, income, employees from er.company_info_union where run_date >= '", rundate-months(1), "' 
                and country in (select name from appcache.vw_country where included) and (gap_flag = 1 or revenue >= 20) and gov_flag = 0 and cyence_sector <> 'Public Administration'")
us_companies <- dbGetQuery(con, query) %>% as.data.table()
us_companies$cyence_id <- as.character(us_companies$cyence_id)
us_companies <- us_companies[order(cyence_id, run_date)]
dbDisconnect(con)

## Basic distribution plots
ggplot(us_companies[run_date == rundate], aes(x = revenue)) + geom_histogram(bins = 100) + scale_x_log10()
ggplot(us_companies[run_date == rundate], aes(x = income)) + geom_histogram(bins = 100) + scale_x_log10()
ggplot(us_companies[run_date == rundate], aes(x = employees)) + geom_histogram(bins = 100) + scale_x_log10()


# =========================================================================================================================
# 2 - Heuristics
# =========================================================================================================================
# Filter 1 (|income| > |revenue|): companies with (revenue > 250 or |income| > 250) and |income|/revenue > 2
aux_filter_1 <- us_companies[run_date == rundate & abs(income) > abs(revenue),]
aux_filter_1$ratio <- abs(aux_filter_1$income)/abs(aux_filter_1$revenue)
aux_filter_1 <- aux_filter_1[(revenue > 250 | abs(income) > 250) & abs(income)/revenue > 2,]
aux_filter_1 <- aux_filter_1[income > 0,]     # Temporary condition
filter_1 <- us_companies[cyence_id %in% aux_filter_1$cyence_id & run_date == rundate,]
rm(aux_filter_1)

# Filter 2: changes in sector
aux_filter_2 <- spread(us_companies[, .(run_date, cyence_id, cyence_sector)], run_date, cyence_sector)
names(aux_filter_2) <- c('cyence_id', 'prev_value', 'new_value')
aux_filter_2$dif <- (aux_filter_2$prev_value != aux_filter_2$new_value)*1
aux_filter_2 <- aux_filter_2[dif == 1,]
filter_2 <- us_companies[cyence_id %in% aux_filter_2$cyence_id & run_date == rundate,]
rm(aux_filter_2)

# Filter 3: changes in revenue [companies with revenue > 100 and (new_rev/old_rev > 3x or new_rev/old_rev < 0.5x)]
aux_filter_3 <- spread(us_companies[, .(run_date, cyence_id, revenue)], run_date, revenue)
names(aux_filter_3) <- c('cyence_id', 'prev_value', 'new_value')
aux_filter_3$increase <- (abs(aux_filter_3$new_value) + 0.000001)/(abs(aux_filter_3$prev_value) + 0.000001)
aux_filter_3 <- aux_filter_3[(increase > 3 | increase < 0.5) & (prev_value > 100 | new_value > 100)]
filter_3 <- us_companies[cyence_id %in% aux_filter_3$cyence_id & run_date == rundate,]
rm(aux_filter_3)

# Filter 4: changes in income [companies with income > 50 and (new_inc/old_inc > 1.3x or new_inc/old_inc < 0.5x)]
aux_filter_4 <- spread(us_companies[, .(run_date, cyence_id, income)], run_date, income)
names(aux_filter_4) <- c('cyence_id', 'prev_value', 'new_value')
aux_filter_4$increase <- (abs(aux_filter_4$new_value) + 0.000001)/(abs(aux_filter_4$prev_value) + 0.000001)
aux_filter_4 <- aux_filter_4[(increase > 1.3 | increase < 0.5) & (prev_value > 50 | new_value > 50)]
filter_4 <- us_companies[cyence_id %in% aux_filter_4$cyence_id & run_date == rundate,]
rm(aux_filter_4)


# =========================================================================================================================
# 3 - Autoencoder
# =========================================================================================================================
# Notes: after training with multiple combinations of activation functions, layer sizes and iterations, these are the conclusions:
#       1 - 'Tanh' is the activation function that works better, given the nature of our features (normalized between 0 and 1)
#       2 - Above a certain threshold (around 10), the number of iterations through the entire dataset barely affects the final result
#       3 - The optimal number of hidden layers is 1, and the greater the size of the layer, the lower the overall error. A good value for our use case is 1

# Part 1. Using Revenue & Employees only
#.....................................................................................
us_filter <- us_companies[run_date == rundate & revenue > 0 & employees > 0 & !is.na(cyence_sector),][order(cyence_id)]
print(paste0("Total # of companies with positive revenue & employees: ", nrow(us_filter)))

# Normalize features
us_filter$revenue_norm <- normalize(log10(us_filter$revenue + 1), 0, 1)
us_filter$employees_norm <- normalize(log10(us_filter$employees), 0, 1)

features <- c("revenue_norm", "employees_norm")

# Define optimal parameters
iterations <- 30
activation_function <- "Tanh"
hidden_layers <- c(1)

h2o.init(nthreads = -1)

for (sector in unique(us_filter$cyence_sector)){
  print(paste0("Training model for sector: ", sector))
  
  # Convert to h2o dataframe and split in training and test
  us_hf <- as.h2o(us_filter[cyence_sector == sector,])
  splits <- h2o.splitFrame(us_hf, 
                           ratios = c(0.6), 
                           seed = 1717)
  train  <- splits[[1]]
  test <- splits[[2]]
  
  # Train model and get MSE for training and test
  model_name <- paste0("model_", gsub(" ", "", sector))
  model_outlier <- autoencoder(train, features, hidden_layers, iterations, activation_function)
  assign(model_name, value = model_outlier)     # To save model
  
  anomaly_train <- h2o.anomaly(model_outlier, train) %>% as.data.frame() %>% tibble::rownames_to_column()
  anomaly_test <- h2o.anomaly(model_outlier, test) %>% as.data.frame() %>% tibble::rownames_to_column()
  print(paste0("Error train: ", mean(anomaly_train$Reconstruction.MSE)))
  print(paste0("Error test: ", mean(anomaly_test$Reconstruction.MSE)))
  
  # Save reconstructed values
  predictions <- cbind(as.data.table(us_hf$cyence_id), as.data.table(h2o.predict(model_outlier, us_hf)))[order(cyence_id)]
  us_filter[cyence_sector == sector, reconstr_revenue_norm := predictions$reconstr_revenue_norm]
  us_filter[cyence_sector == sector, reconstr_employees_norm := predictions$reconstr_employees_norm]
  
  # Flag outliers (top 25 per sector)
  errors <- as.data.table(h2o.anomaly(model_outlier, us_hf))
  errors <- cbind(as.data.table(us_hf$cyence_id), errors)
  us_filter[cyence_sector == sector, outlier_score := errors$Reconstruction.MSE]
  threshold <- quantile(errors$Reconstruction.MSE, 1-25/nrow(errors))
  assign(paste0("threshold_", gsub(" ", "", sector)), threshold)
  errors$outlier <- (errors$Reconstruction.MSE > threshold)*1
  us_filter[cyence_sector == sector, outlier := errors$outlier]
}

# Save table with reconstructed values and outlier scores
outliers_rev_emp <- us_filter

# Take top 'n' outliers with revenue >= 1B or employees >= 500 
n <- 180
filter_5 <- outliers_rev_emp[(revenue >= 1000 | employees >= 500), ]
filter_5 <- filter_5[outlier_score > quantile(filter_5$outlier_score, 1-n/nrow(filter_5)), 1:12]

# Part 2. Using Revenue, Employees & Income (for the companies that have it)
#.....................................................................................
us_filter <- us_companies[run_date == rundate & revenue > 0 & employees > 0 & income > 0 & !is.na(cyence_sector),][order(cyence_id)]
print(paste0("Total # of companies with positive revenue, employees and income: ", nrow(us_filter)))

# Normalize features
us_filter$revenue_norm <- normalize(log10(us_filter$revenue + 1), 0, 1)
us_filter$employees_norm <- normalize(log10(us_filter$employees), 0, 1)
us_filter$income_norm <- normalize(log10(us_filter$income + 1), 0, 1)

features <- c("revenue_norm", "employees_norm", "income_norm")

# Define optimal parameters
iterations <- 30
activation_function <- "Tanh"
hidden_layers <- c(2)

h2o.init(nthreads = -1)

for (sector in unique(us_filter$cyence_sector)){
  print(paste0("Training model for sector: ", sector))
  
  # Convert to h2o dataframe and split in training and test
  us_hf <- as.h2o(us_filter[cyence_sector == sector,])
  splits <- h2o.splitFrame(us_hf, 
                           ratios = c(0.6), 
                           seed = 1717)
  train  <- splits[[1]]
  test <- splits[[2]]
  
  # Train model and get MSE for training and test
  model_name <- paste0("model_", gsub(" ", "", sector))
  model_outlier <- autoencoder(train, features, hidden_layers, iterations, activation_function)
  assign(model_name, value = model_outlier)     # To save model
  
  anomaly_train <- h2o.anomaly(model_outlier, train) %>% as.data.frame() %>% tibble::rownames_to_column()
  anomaly_test <- h2o.anomaly(model_outlier, test) %>% as.data.frame() %>% tibble::rownames_to_column()
  print(paste0("Error train: ", mean(anomaly_train$Reconstruction.MSE)))
  print(paste0("Error test: ", mean(anomaly_test$Reconstruction.MSE)))
  
  # Save reconstructed values
  predictions <- cbind(as.data.table(us_hf$cyence_id), as.data.table(h2o.predict(model_outlier, us_hf)))[order(cyence_id)]
  us_filter[cyence_sector == sector, reconstr_revenue_norm := predictions$reconstr_revenue_norm]
  us_filter[cyence_sector == sector, reconstr_employees_norm := predictions$reconstr_employees_norm]
  us_filter[cyence_sector == sector, reconstr_income_norm := predictions$reconstr_income_norm]
  
  # Flag outliers
  errors <- as.data.table(h2o.anomaly(model_outlier, us_hf))
  errors <- cbind(as.data.table(us_hf$cyence_id), errors)
  us_filter[cyence_sector == sector, outlier_score := errors$Reconstruction.MSE]
  threshold <- quantile(errors$Reconstruction.MSE, 1-25/nrow(errors))
  assign(paste0("threshold_", gsub(" ", "", sector)), threshold)
  errors$outlier <- (errors$Reconstruction.MSE > threshold)*1
  us_filter[cyence_sector == sector, outlier := errors$outlier]
}

# Save table with reconstructed values and outlier scores
outliers_rev_emp_inc <- us_filter

# Take top 'n' outliers with revenue >= 1B or income >= 50M or employees >= 500 
n <- 180
filter_6 <- outliers_rev_emp_inc[(revenue >= 1000 | abs(income) >= 50 | employees >= 500), ]
filter_6 <- filter_6[outlier_score > quantile(filter_6$outlier_score, 1-n/nrow(filter_6)), 1:12]


# =========================================================================================================================
# 4 - Plots
# =========================================================================================================================
# Part 1. Using Revenue & Employees only
#.....................................................................................
# Distribution of outlier scores
ggplot(outliers_rev_emp, aes(x = outlier_score)) + geom_histogram(bins = 100) + xlim(0, 0.04)
# Distribution of outlier scores by sector
ggplot(outliers_rev_emp, aes(x = outlier_score)) + geom_histogram(bins = 100) + facet_wrap(~cyence_sector) + coord_cartesian(xlim = c(0,0.03))
# Rev - Employees scatter plot flagging outliers
ggplot(outliers_rev_emp, aes(x = revenue_norm, y = employees_norm, col = outlier)) + geom_point(alpha = 0.4) + geom_smooth(method = 'lm') + facet_wrap(~cyence_sector)
# Plot with outliers and their reconstructed values
ggplot(outliers_rev_emp[outlier==1,], aes(x = revenue_norm, y = employees_norm)) + geom_point(col = 'blue', alpha = 0.4) + facet_wrap(~cyence_sector) + 
  geom_point(aes(x = reconstr_revenue_norm, y = reconstr_employees_norm), col = 'red')

# Part 2. Using Revenue, Employees & Income (for the companies that have it)
#.....................................................................................
# Distribution of outlier scores
ggplot(outliers_rev_emp_inc, aes(x = outlier_score)) + geom_histogram(bins = 100) + xlim(0, 0.04)
# Distribution of outlier scores by sector
ggplot(outliers_rev_emp_inc, aes(x = outlier_score)) + geom_histogram(bins = 100) + facet_wrap(~cyence_sector) + coord_cartesian(xlim = c(0,0.03))
# Rev - Employees scatter plot flagging outliers
ggplot(outliers_rev_emp_inc, aes(x = revenue_norm, y = employees_norm, col = outlier)) + geom_point(alpha = 0.4) + geom_smooth(method = 'lm') + facet_wrap(~cyence_sector)
# Rev - Income scatter plot flagging outliers
ggplot(outliers_rev_emp_inc[income != 0,], aes(x = revenue_norm, y = income_norm, col = outlier)) + geom_point(alpha = 0.4) + geom_smooth(method = 'lm') + facet_wrap(~cyence_sector)
# Plot with outliers and their reconstructed values (in rev-emp axis)
ggplot(outliers_rev_emp_inc[outlier==1,], aes(x = revenue_norm, y = employees_norm)) + geom_point(col = 'blue', alpha = 0.4) + facet_wrap(~cyence_sector) + 
  geom_point(aes(x = reconstr_revenue_norm, y = reconstr_employees_norm), col = 'red')
# Plot with outliers and their reconstructed values (in rev-income axis)
ggplot(outliers_rev_emp_inc[outlier==1,], aes(x = revenue_norm, y = income_norm)) + geom_point(col = 'blue', alpha = 0.4) + facet_wrap(~cyence_sector) + 
  geom_point(aes(x = reconstr_revenue_norm, y = reconstr_income_norm), col = 'red')


# =========================================================================================================================
# 5 - Combine and output companies to review
# =========================================================================================================================
review <- rbind(filter_1, filter_5, filter_6) %>% unique(by = NULL)
review_changes <- rbind(filter_2, filter_3, filter_4) %>% unique(by = NULL)

# Load companies that were reviewed in previous months to avoid repetition
prev <- c(as.character(fread('October 2017/outliers_2017_10.csv', select = "cyence_id")$cyence_id), 
          as.character(fread('November 2017/outliers_2017_11.csv', select = "cyence_id")$cyence_id),
          as.character(fread('December 2017/outliers_2017_12.csv', select = "cyence_id")$cyence_id),
          as.character(fread('January 2018/outliers_2018_01.csv', select = "cyence_id")$cyence_id),
          as.character(fread('February 2018/outliers_2018_02.csv', select = "cyence_id")$cyence_id),
          as.character(fread('March 2018/outliers_2018_03.csv', select = "cyence_id")$cyence_id),
          as.character(fread('April 2018/outliers_2018_04.csv', select = "cyence_id")$cyence_id),
          as.character(fread('May 2018/outliers_2018_05.csv', select = "cyence_id")$cyence_id))

good_ids <- as.character(c(79942718, 81466849, 1213206, 144709193, 51957769, 196337864, 79834910))
review <- review[!(cyence_id %in% prev) & !(cyence_id %in% good_ids),]
review_changes <- review_changes[!(cyence_id %in% prev) & !(cyence_id %in% review$cyence_id) & !(cyence_id %in% good_ids),]

# Format and output
review <- review[, run_date := NULL]
review[is.na(review)] <- ""
review <- review[order(cyence_id)]

name_csv <- paste0("outliers_", format(rundate, '%Y_%m'), ".csv")
write.csv(review, name_csv, fileEncoding = 'UTF-8', row.names = F)

# (for review_changes, add extra columns to identify the change)
review_changes <- review_changes[, run_date := NULL]
review_changes$change_sector <- (review_changes$cyence_id %in% filter_2$cyence_id)*1
review_changes$change_revenue <- (review_changes$cyence_id %in% filter_3$cyence_id)*1
review_changes$change_income <- (review_changes$cyence_id %in% filter_4$cyence_id)*1
# only show changes in sector if revenue > 500M
review_changes <- review_changes[change_revenue == 1 | change_income == 1 | revenue > 500,]
review_changes[is.na(review_changes)] <- ""
review_changes <- review_changes[order(-change_sector, -change_revenue, -change_income)]

name_csv <- paste0("outliers_changes_", format(rundate, '%Y_%m'), ".csv")
write.csv(review_changes, name_csv, fileEncoding = 'UTF-8', row.names = F)


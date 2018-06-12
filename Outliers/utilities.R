#-------------------------------------------------------------------------------------------------------------------------
### Install packages
#-------------------------------------------------------------------------------------------------------------------------
list.of.packages = c("RPostgreSQL", "data.table", "ggplot2", "h2o", "dplyr", "lubridate", "tidyr")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {
  install.packages(new.packages)
}
library(RPostgreSQL) 
library(data.table)
library(ggplot2)
library(h2o)
library(dplyr)
library(lubridate)
library(tidyr)

#-------------------------------------------------------------------------------------------------------------------------
### Supporting functions
#-------------------------------------------------------------------------------------------------------------------------

connect_to_db = function(){
  drv =dbDriver("PostgreSQL")
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

normalize <- function(x, a, b){
  # Input: a vector of numeric values (x) and a range of values (a, b)
  # Output: vector 'x' normalized between 'a' and 'b'
  if (!(typeof(x) %in% c("double", "integer"))){
    stop("Non-numeric vector provided for normalization!")
  }
  return(a + (x-min(x))/(max(x)-min(x))*(b-a))
}

autoencoder <- function(train, features, hidden_layers, iter, activ){
  # Input:  -'train': H2O Dataframe with the training samples
  #         -'features': vector with the names of columns to use as input features (eg: c("revenue", "employees"))
  #         -'hidden_layers': vector containing the sizes of the hidden layers (eg: c(5,3,5))
  #         -'iter': number of iterations through the entire dataset
  #         -'activ': Activation function. Must be in ["Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout"]
  # Output: -'model_nn': the trained autoencoder model
  model_name <- paste0("model_", paste0(hidden_layers, collapse = '_'), "_", activ)
  model_nn <- h2o.deeplearning(x = features,
                               training_frame = train,
                               model_id = model_name,
                               autoencoder = TRUE,
                               reproducible = TRUE, #slow - turn off for real problems
                               ignore_const_cols = FALSE,
                               seed = 1717,
                               hidden = hidden_layers, 
                               epochs = iter,
                               activation = activ)
  
  return(model_nn)
}

library(data.table)
library(ranger)
library(pROC)
library(rgdal)
library(maptools)
library(raster)
library(rgeos)
library(plyr)
library(pdp)
library(ggplot2)
library(tidyr)
library(mlr)

set.seed(1)

#Set up possible combinations of covariates
possible_combinations <- expand.grid(agro_cols = 0:1, host_cols = 0:1, agro_seas = 0:1, climate_cols = 0:1, stringsAsFactors = FALSE)
#Remove row with no data
possible_combinations <- possible_combinations[-which(rowSums(possible_combinations) == 0), ]

#Load in data
model_run_data <- read.csv("data/model_run_data.csv", stringsAsFactors = FALSE)
training_data <- read.csv("data/training_oos_data.csv", stringsAsFactors = FALSE)
covariate_categories <- read.csv("data/covariate_categories.csv", stringsAsFactors = FALSE)

for(i in names(model_run_data)[grepl("plant|harvest", names(model_run_data))]){
  model_run_data[, i]<-as.factor(model_run_data[, i])
}

#Run for all rows
all_rows_run <- sapply(1:nrow(possible_combinations), function(row){
  
  print(row)
  
  hyper_grid <- expand.grid(
    mtry       = 1:4,
    node_size  = 1:4,
    num.trees = seq(100, 500, 100),
    OOB_RMSE   = 0
  )
  
  #Select covariates
  updated_row_use <- possible_combinations[row, ]
  all_covariates_use <- as.character(na.omit(unlist(sapply(1:ncol(covariate_categories), function(x) covariate_categories[, x], simplify = FALSE)[which(updated_row_use %in% 1)])))
  
  for(i in 1:nrow(hyper_grid)) {
    print(i)
    rf <- ranger(as.formula("report_classify ~."), 
                 data = model_run_data[, c("report_classify", gsub(" |-", ".", all_covariates_use))], 
                 num.trees      = hyper_grid$num.trees[i],
                 mtry           = hyper_grid$mtry[i],
                 min.node.size  = hyper_grid$node_size[i],
                 importance = "permutation", 
                 classification = T, 
                 probability = T)
    hyper_grid$OOB_RMSE[i] <- sqrt(rf$prediction.error)
  }
  
  position = which.min(hyper_grid$OOB_RMSE)
  
  write.csv(data.frame(row = row, hyper_grid),
            paste0("output/rf_tune/row_", row, "_results"))
  
}, simplify = FALSE)

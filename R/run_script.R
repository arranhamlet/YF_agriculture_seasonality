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
  
  #Select covariates
  updated_row_use <- possible_combinations[row, ]
  all_covariates_use <- as.character(na.omit(unlist(sapply(1:ncol(covariate_categories), function(x) covariate_categories[, x], simplify = FALSE)[which(updated_row_use %in% 1)])))
   
  #Run model
  #In report_classify, 1 = human report, 2 = NHP report,  3 = both
  model_run <- ranger(as.formula("report_classify ~."), data = model_run_data[, c("report_classify", gsub(" |-", ".", all_covariates_use))], 
          num.trees = 800, importance = "permutation", classification = T, probability = T)
  
  #Extract predictions
  all_predictions <- as.data.frame(model_run$predictions)
  colnames(all_predictions) <- c("none", "human", "both", "NHP")
  
  all_data_classify <- matrix(rep(model_run_data$report_classify, 3), ncol = 3)
  
  all_data_classify[which(all_data_classify[, 1] != 1), 1] <- 0
  all_data_classify[which(all_data_classify[, 1] == 1), 1] <- 1
  
  all_data_classify[which(all_data_classify[, 2] != 2), 2] <- 0
  all_data_classify[which(all_data_classify[, 2] == 2), 2] <- 1
  
  all_data_classify[which(all_data_classify[, 3] != 3), 3] <- 0
  all_data_classify[which(all_data_classify[, 3] == 3), 3] <- 1
  
  colnames(all_data_classify) <- c("human", "NHP", "both")
  
  all_in_sample_auc <- do.call(rbind, sapply(colnames(all_data_classify), function(x){
    
    as.numeric(ci.auc(all_data_classify[, x], all_predictions[, x]))
    
  }, simplify = FALSE))
  
  colnames(all_in_sample_auc) <- c("low_auc", "mid_auc", "high_auc")
  
  list(df = data.frame(row = row, updated_row_use, data.frame(type = row.names(all_in_sample_auc), all_in_sample_auc,
                                                              brier_score = model_run$prediction.error,
                                                              stringsAsFactors = FALSE), stringsAsFactors = FALSE),
       predictions = data.frame(row = row, all_predictions, stringsAsFactors = FALSE),
       variable_importance = data.frame(row = row, updated_row_use, t(data.frame(model_run$variable.importance)), stringsAsFactors = FALSE))
  
}, simplify = FALSE)

dataframe_all <- do.call(rbind, sapply(1:length(all_rows_run), function(x) all_rows_run[[x]][[1]], simplify = FALSE))
predictions_all <- do.call(rbind, sapply(1:length(all_rows_run), function(x) all_rows_run[[x]][[2]], simplify = FALSE))
variable_importance_all <- do.call(rbind.fill, sapply(1:length(all_rows_run), function(x) all_rows_run[[x]][[3]], simplify = FALSE))

variable_importance_all_long <- gather(variable_importance_all, covariate, importance, Number.of.peanut.farms:EVI.delay.by.2.month, factor_key = T)

predictions_all$month <- model_run_data$month
predictions_all_long <- gather(predictions_all, type, prediction, none:NHP, factor_key = T)


fwrite(dataframe_all, "output/in_sample_dataframe_all.csv", row.names = FALSE)
fwrite(predictions_all_long[which(predictions_all_long$type != "none"), ], "output/predictions_all.csv", row.names = FALSE)
fwrite(variable_importance_all_long, "output/variable_importance_all.csv", row.names = FALSE)




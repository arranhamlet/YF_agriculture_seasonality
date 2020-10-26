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
  model_run <- ranger(as.formula("report_classify ~."), data = model_run_data[, c("report_classify", all_covariates_use)], 
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
  
  data.frame(row = row, updated_row_use, data.frame(type = row.names(all_in_sample_auc), all_in_sample_auc,
                          stringsAsFactors = FALSE),
             stringsAsFactors = FALSE)
  
}, simplify = FALSE)

all_done <- do.call(rbind, all_rows_run)

ggplot(data = all_done, aes(x = row, y = mid_auc, color = type)) + geom_point() +
  theme_minimal() + labs(x = "Row", y = "auc", color = "Type") +
  geom_errorbar(aes(ymin = low_auc, ymax = high_auc))




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
library(dismo)
library(gbm)

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

#All tuned values
model_run_data$report_classify <- factor(model_run_data$report_classify)
model_run_data$both_report <- rowSums(model_run_data[, c("human_report", "NHP_report")])
model_run_data$both_report[which(model_run_data$both_report != 2)] <- 0
model_run_data$both_report[which(model_run_data$both_report == 2)] <- 1

#Run for all rows
all_rows_run <- sapply(1:nrow(possible_combinations), function(row){
  
  print(row)
  
  #Select covariates
  updated_row_use <- possible_combinations[row, ]
  all_covariates_use <- as.character(na.omit(unlist(sapply(1:ncol(covariate_categories), function(x) covariate_categories[, x], simplify = FALSE)[which(updated_row_use %in% 1)])))
  
  #Run model
  #In report_classify, 1 = human report, 2 = NHP report,  3 = both
  
  model_run_human <- gbm::gbm(data = model_run_data[, c("human_report", gsub(" |-", ".", all_covariates_use))], 
                         formula = as.formula(paste0("human_report ~ ", paste(gsub(" |-", ".", all_covariates_use), collapse = " + "))),
                         distribution = "bernoulli", 
                         bag.fraction = 0.65, 
                         n.trees = 500,
                         keep.data = T)
  
  model_run_NHP <- gbm::gbm(data = model_run_data, 
                            formula = as.formula(paste0("NHP_report ~ ", paste(gsub(" |-", ".", all_covariates_use), collapse = " + "))),
                            distribution = "bernoulli", 
                            bag.fraction = 0.65, 
                            n.trees = 500)
  
  model_run_both <- gbm::gbm(data = model_run_data, 
                             formula = as.formula(paste0("both_report ~ ", paste(gsub(" |-", ".", all_covariates_use), collapse = " + "))),
                             distribution = "bernoulli", 
                             bag.fraction = 0.65, 
                             n.trees = 500)
  
    #Extract predictions
  all_predictions <- data.frame(human = predict(model_run_human, type = "response"),
                                NHP = predict(model_run_NHP, type = "response"),
                                both = predict(model_run_both, type = "response"),
                                stringsAsFactors = FALSE)
  
  all_data_classify <- data.frame(human = model_run_data$human_report,
                                  NHP = model_run_data$NHP_report,
                                  both = model_run_data$both_report,
                                  stringsAsFactors = FALSE)
  
  all_in_sample_auc <- do.call(rbind, sapply(colnames(all_data_classify), function(x){
    
    as.numeric(ci.auc(all_data_classify[, x], all_predictions[, x]))
    
  }, simplify = FALSE))
  
  colnames(all_in_sample_auc) <- c("low_auc", "mid_auc", "high_auc")
  
  list(df = data.frame(row = row, updated_row_use, data.frame(type = row.names(all_in_sample_auc), 
                                                              all_in_sample_auc,
                                                              stringsAsFactors = FALSE), stringsAsFactors = FALSE),
       predictions = data.frame(row = row, all_predictions, stringsAsFactors = FALSE),
       variable_importance = data.frame(row = row, updated_row_use, rbind(data.frame(type = "human", summary(model_run_human)),
                                                                          data.frame(type = "NHP", summary(model_run_NHP)),
                                                                          data.frame(type = "both", summary(model_run_both))), stringsAsFactors = FALSE))
  
}, simplify = FALSE)

dataframe_all <- do.call(rbind, sapply(1:length(all_rows_run), function(x) all_rows_run[[x]][[1]], simplify = FALSE))
predictions_all <- do.call(rbind, sapply(1:length(all_rows_run), function(x) all_rows_run[[x]][[2]], simplify = FALSE))
variable_importance_all <- do.call(rbind.fill, sapply(1:length(all_rows_run), function(x) all_rows_run[[x]][[3]], simplify = FALSE))

predictions_all$month <- model_run_data$month


fwrite(dataframe_all, "output/brt_in_sample_dataframe_all.csv", row.names = FALSE)
fwrite(predictions_all, "output/brt_predictions_all.csv", row.names = FALSE)
fwrite(variable_importance_all, "output/brt_variable_importance_all.csv", row.names = FALSE)


ggplot(data = dataframe_all) + 
  geom_bar(aes(x = factor(row), y = mid_auc, fill = type, group = type), stat = "identity", position = "dodge") +
  theme_minimal() + 
  coord_cartesian(ylim = c(0.75, 1)) + 
  labs(x = "Row", y = "AUC", fill = "Type") + 
  geom_errorbar(aes(x = factor(row), ymin = low_auc, ymax = high_auc, group = type), position = "dodge")





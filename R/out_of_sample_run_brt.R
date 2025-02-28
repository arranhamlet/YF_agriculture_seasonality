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

n_runs <- 200

#Run for all rows
all_rows_run <- sapply(1:nrow(possible_combinations), function(row){
  
  print(row)
  
  all_runs_done <- sapply(1:n_runs, function(x){
    
    #Select covariates
    updated_row_use <- possible_combinations[row, ]
    all_covariates_use <- as.character(na.omit(unlist(sapply(1:ncol(covariate_categories), function(x) covariate_categories[, x], simplify = FALSE)[which(updated_row_use %in% 1)])))
    
    #Run model
    #In report_classify, 1 = human report, 2 = NHP report,  3 = both
    these_take_out <- matrix(training_data[, x], ncol = 12)
    
    #Build training set
    train_pred_df_build <- sapply(1:12, function(q){
      
      this_month_data <- model_run_data[model_run_data$month == q, ]
      
      predict_df_month <- this_month_data[which(this_month_data$codigo_ibg %in% these_take_out[, q]), ]
      train_df_month <- this_month_data[-which(this_month_data$codigo_ibg %in% these_take_out[, q]), ]
      
      predict_df_month <- predict_df_month[, -which(colnames(predict_df_month) == "codigo_ibg")]
      train_df_month <- train_df_month[, -which(colnames(train_df_month) == "codigo_ibg")]
      
      list(predict_df_month, train_df_month)
      
    }, simplify = FALSE)
    
    train_df<-do.call(rbind, sapply(1:length(train_pred_df_build), function(a) train_pred_df_build[[a]][[2]], simplify = FALSE))
    predict_df<-do.call(rbind, sapply(1:length(train_pred_df_build), function(a) train_pred_df_build[[a]][[1]], simplify = FALSE))
    
    model_run_human <- gbm::gbm(data = train_df[, c("human_report", gsub(" |-", ".", all_covariates_use))], 
                                formula = as.formula(paste0("human_report ~ ", paste(gsub(" |-", ".", all_covariates_use), collapse = " + "))),
                                distribution = "bernoulli", 
                                bag.fraction = 0.65, 
                                n.trees = 500,
                                keep.data = T)
    
    model_run_NHP <- gbm::gbm(data = train_df[, c("NHP_report", gsub(" |-", ".", all_covariates_use))], 
                              formula = as.formula(paste0("NHP_report ~ ", paste(gsub(" |-", ".", all_covariates_use), collapse = " + "))),
                              distribution = "bernoulli", 
                              bag.fraction = 0.65, 
                              n.trees = 500)
    
    model_run_both <- gbm::gbm(data = train_df[, c("both_report", gsub(" |-", ".", all_covariates_use))], 
                               formula = as.formula(paste0("both_report ~ ", paste(gsub(" |-", ".", all_covariates_use), collapse = " + "))),
                               distribution = "bernoulli", 
                               bag.fraction = 0.65, 
                               n.trees = 500)
    
    #Extract predictions
    all_predictions <- data.frame(human = predict(model_run_human, newdata = predict_df, type = "response", n.trees = 500),
                                  NHP = predict(model_run_NHP, newdata = predict_df, type = "response", n.trees = 500),
                                  both = predict(model_run_both, newdata = predict_df, type = "response", n.trees = 500),
                                  stringsAsFactors = FALSE)
    
    all_data_classify <- data.frame(human = predict_df$human_report,
                                    NHP = predict_df$NHP_report,
                                    both = predict_df$both_report,
                                    stringsAsFactors = FALSE)
    
    all_in_sample_auc <- do.call(rbind, sapply(colnames(all_data_classify), function(x){
      as.numeric(suppressWarnings(ci.auc(all_data_classify[, x], all_predictions[, x], quiet = T)))
    }, simplify = FALSE))
    
    colnames(all_in_sample_auc) <- c("low_auc", "mid_auc", "high_auc")
    
    brier_scores <- do.call(rbind, sapply(colnames(all_data_classify), function(x){
      mean((all_predictions[, x] - all_data_classify[, x])^2)
    }, simplify = FALSE))
    
    list(df = suppressWarnings(data.frame(row = row, run = x, updated_row_use, data.frame(type = row.names(all_in_sample_auc),
                                                                                          brier_score = sum(brier_scores),
                                                                                          total_brier_score = sum(brier_scores),
                                                                                          all_in_sample_auc,
                                                                                          stringsAsFactors = FALSE), stringsAsFactors = FALSE)),
                          predictions = data.frame(row = row, run = x, all_predictions, stringsAsFactors = FALSE),
                          variable_importance = suppressWarnings(data.frame(row = row, run = x, updated_row_use, rbind(data.frame(type = "human", summary(model_run_human)),
                                                                                                                data.frame(type = "NHP", summary(model_run_NHP)),
                                                                                                                data.frame(type = "both", summary(model_run_both))), stringsAsFactors = FALSE)))
    
  }, simplify = FALSE)
  
  df_outcome <- do.call(rbind, sapply(1:length(all_runs_done), function(t) all_runs_done[[t]][[1]], simplify = FALSE))
  
  agg_df_outcome <- aggregate(x = df_outcome[, c("low_auc", "mid_auc", "high_auc", "brier_score", "total_brier_score")],
                             by = list(row = df_outcome$row,
                                       agro_cols = df_outcome$agro_cols,
                                       host_cols = df_outcome$host_cols,
                                       agro_seas = df_outcome$agro_seas,
                                       climate_cols = df_outcome$climate_cols,
                                       type = df_outcome$type),
                             FUN = mean)
  
  agg_df_outcome$runs <- n_runs
  
  fwrite(agg_df_outcome, paste0("output/out_of_sample/row_", row, "_brt_out_sample_dataframe_all.csv"), row.names = FALSE)
  
}, simplify = FALSE)




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

#Run model
n_rows <- nrow(possible_combinations)
n_runs <- 200

#Run for all rows
all_rows_run <- sapply(1:n_rows, function(row){
  
  print(row)
  
  all_runs <- do.call(rbind, sapply(1:n_runs, function(x){
    
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
    
    train_model <- ranger(as.formula("report_classify ~."), data = train_df[, c("report_classify", all_covariates_use)], 
                          num.trees = 800, importance = "permutation", classification = T, probability = T)
    
    predictions <- predict(train_model, predict_df)
    
    no_yes <- predict_df$report_classify
    no_yes[no_yes != 0] <- 5
    no_yes[no_yes == 0] <- 1
    no_yes[no_yes == 5] <- 0
    
    human_yes <- predict_df$report_classify
    human_yes[human_yes != 1] <- 0
    
    NHP_yes <- predict_df$report_classify
    NHP_yes[NHP_yes != 2] <- 0
    NHP_yes[NHP_yes == 2] <- 1
    
    both_yes <- predict_df$report_classify
    both_yes[both_yes != 3] <- 0
    both_yes[both_yes == 3] <- 1
    
    no_auc <- as.numeric(ci.auc(no_yes, predictions$predictions[, 1]))
    human_auc <- as.numeric(ci.auc(human_yes, predictions$predictions[, 2]))
    NHP_auc <- as.numeric(ci.auc(NHP_yes, predictions$predictions[, 4]))
    any_auc <- as.numeric(ci.auc(both_yes, predictions$predictions[, 3]))
    
    auc_df <- matrix(c(no_auc, human_auc, NHP_auc, any_auc), ncol = 3, byrow = T)
    
    output_df <- data.frame(classification = c("none", "human", "NHP", "both"),
                            run = x,
                            auc_df, stringsAsFactors = F)
    colnames(output_df) <- c("classification", "run", "auc_lo", "auc_mid", "auc_hi")
    
    output_df
    
  }, simplify = FALSE))
  
  all_runs$row <- row
  
  this_row_results <- aggregate(x = list(auc_lo = all_runs$auc_lo,
                     auc_mid = all_runs$auc_mid,
                     auc_hi = all_runs$auc_hi),
            by = list(classification = all_runs$classification,
                      row = all_runs$row),
            FUN = mean)
  
  write.csv(this_row_results, paste("output/out_of_sample/oos_values_row", row, n_runs, "runs.csv", sep = "_"), row.names = FALSE)
  
  
}, simplify = FALSE)






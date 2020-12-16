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
library(car)

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
  
  model_run_human <- glm(data = model_run_data, 
                              formula = paste0("human_report ~ ", paste(gsub(" |-", ".", all_covariates_use), collapse = " + ")),
                              family = "binomial")
  
  model_run_NHP <- glm(data = model_run_data, 
                       formula = paste0("NHP_report ~ ", paste(gsub(" |-", ".", all_covariates_use), collapse = " + ")),
                       family = "binomial")
  
  model_run_both <- glm(data = model_run_data, 
                        formula = paste0("both_report ~ ", paste(gsub(" |-", ".", all_covariates_use), collapse = " + ")),
                        family = "binomial")
  
  
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
  
  vif_df <- rbind(data.frame(row = row,
                             type = "human",
                             covariate = names(vif(model_run_human)),
                             value = as.numeric(vif(model_run_human))),
                  data.frame(row = row,
                             type = "NHP",
                             covariate = names(vif(model_run_NHP)),
                             value = as.numeric(vif(model_run_NHP))),
                  data.frame(row = row,
                             type = "both",
                             covariate = names(vif(model_run_both)),
                             value = as.numeric(vif(model_run_both))))
  
  list(all_in_sample_auc,
       vif_df)
  
}, simplify = FALSE)

VIF_df <- do.call(rbind, sapply(1:length(all_rows_run), function(x) all_rows_run[[x]][[2]], simplify = FALSE))
auc_df <- do.call(rbind, sapply(1:length(all_rows_run), function(x) all_rows_run[[x]][[1]], simplify = FALSE))

name_covariate <- c("Agricultural output",
                    "Host demographics",
                    "Agricultural seasonality",
                    "Climate/vegetation seasonality")

VIF_df$category <- NA
for(i in unique(VIF_df$covariate)){
  this_name <- which(sapply(1:ncol(covariate_categories), function(y) any(gsub("-", " ", covariate_categories[, y]) == gsub("\\.", " ", i), na.rm = T)))
  VIF_df[which(VIF_df$covariate == i), ]$category <- name_covariate[this_name]
}

VIF_df$row_name <- paste0("Row ", VIF_df$row)
VIF_df$row_name <- factor(VIF_df$row_name, levels = unique(VIF_df$row_name))

VIF_plot <- ggplot(data = subset(VIF_df, type == "human")) + geom_bar(aes(x = covariate, y = value, fill = category), stat = "identity") +
  theme_minimal() + labs(x = "Covariate", y = "Variance Inflation Factor", fill = "Category") +
  coord_cartesian(ylim = c(0, 12)) + facet_wrap(~row_name) + theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(yintercept = 10, linetype = "dashed")

ggsave(filename = "figures/VIF_plot.png", VIF_plot, height = 14, width = 20)



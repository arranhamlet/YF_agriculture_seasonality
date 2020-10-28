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
library(tidyverse)
library(Hmisc)
library(ggpubr)

#Set up possible combinations of covariates
possible_combinations <- expand.grid(agro_cols = 0:1, host_cols = 0:1, agro_seas = 0:1, climate_cols = 0:1, stringsAsFactors = FALSE)
#Remove row with no data
possible_combinations <- possible_combinations[-which(rowSums(possible_combinations) == 0), ]
colnames(possible_combinations) <- c("O", "H", "A", "C")
all_row_ID <- sapply(1:nrow(possible_combinations), function(x) paste(names(possible_combinations)[which(possible_combinations[x, ] != 0)], collapse = ""))

#Load in all data
model_run_data <- read.csv("data/model_run_data.csv", stringsAsFactors = FALSE)
model_run_data$both_report <- model_run_data$human_report + model_run_data$NHP_report
model_run_data$both_report[which(model_run_data$both_report < 2)] <- 0
model_run_data$both_report[which(model_run_data$both_report == 2)] <- 1

out_of_sample_results <- do.call(rbind, sapply(list.files("output/out_of_sample", full.names = T), function(x) read.csv(x, stringsAsFactors = FALSE), simplify = FALSE))
row.names(out_of_sample_results) <- NULL
colnames(out_of_sample_results)[3:5] <- paste0("oos_", colnames(out_of_sample_results)[3:5])
out_of_sample_results <- out_of_sample_results[order(out_of_sample_results$row), ]
out_of_sample_results <- out_of_sample_results[-which(out_of_sample_results$classification == "none"), ]

in_sample_results <- read.csv("output/in_sample_dataframe_all.csv", stringsAsFactors = FALSE)
in_sample_results$row <- factor(in_sample_results$row)

predictions <- read.csv("output/predictions_all.csv", stringsAsFactors = FALSE)
predictions_monthly <- aggregate(x = list(prediction = predictions$prediction),
                                 by = list(row = predictions$row,
                                           month = predictions$month,
                                           type = predictions$type),
                                 FUN = sum)

predictions_monthly$month <- factor(predictions_monthly$month, levels = c(7:12, 1:6))
predictions_monthly$type <- factor(predictions_monthly$type, levels = c("human", "NHP", "both"))

data_monthly <- data.frame(row = "data",
                           month = 1:12,
                           type = rep(c("human", "NHP", "both"), each = 12),
                           prediction = c(colSums(matrix(model_run_data$human_report, ncol = 12, byrow = T)),
                                          colSums(matrix(model_run_data$NHP_report, ncol = 12, byrow = T)),
                                          colSums(matrix(model_run_data$both_report, ncol = 12, byrow = T))))


variable_importance <- read.csv("output/variable_importance_all.csv", stringsAsFactors = FALSE)


#Plot insample auc
best_both_seasonal <- "OHAC"
best_agri_seasonal <- "OHA"
best_climate_seasonal <- "OHC"


ggplot(data = in_sample_results, aes(x = row, y = mid_auc, fill = type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_errorbar(aes(x = row, ymin = low_auc, ymax = high_auc), position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Row", y = "AUC", fill = "Type") +
  coord_cartesian(ylim = c(min(in_sample_results$low_auc), 1))


#Plot predictions
data_monthly$type <- factor(as.character(data_monthly$type), levels = levels(predictions_monthly$type))
data_monthly <- data_monthly[order(data_monthly$type), ]


data_prediction_combined <- rbind(predictions_monthly[order(predictions_monthly$type), ], data_monthly)

for(i in 1:15){
  data_prediction_combined[which(data_prediction_combined$row == i), ]$row <- gsub(i, all_row_ID[as.numeric(i)], data_prediction_combined[which(data_prediction_combined$row == i), ]$row)
}

data_prediction_combined$row <- factor(data_prediction_combined$row, levels = unique(data_prediction_combined$row))
data_prediction_combined$pred_difference <- NA

for(i in all_row_ID){
  data_prediction_combined[which(data_prediction_combined$row == i), ]$pred_difference <- data_prediction_combined[which(data_prediction_combined$row == i), ]$prediction - subset(data_prediction_combined, row == "data")$prediction
}


data_prediction_combined$facet_wrap_1 <- data_prediction_combined$type
data_prediction_combined$facet_wrap_2 <- data_prediction_combined$type


data_prediction_combined$facet_wrap_1 <- gsub("human", "A",
                                              gsub("NHP", "B",
                                                   gsub("both", "C", data_prediction_combined$facet_wrap_1)))
data_prediction_combined$facet_wrap_2 <- gsub("human", "D",
                                              gsub("NHP", "E",
                                                   gsub("both", "F", data_prediction_combined$facet_wrap_2)))


raw_data <- ggplot(data = subset(data_prediction_combined, row %in% c(best_both_seasonal, best_agri_seasonal, best_climate_seasonal))) + 
  geom_bar(aes(x = month, y = prediction, fill = row), stat = "identity", position = position_dodge(width = 0.8), alpha = 1) +
  facet_wrap(~facet_wrap_1) + theme_minimal() +
  geom_bar(data = subset(data_prediction_combined, row == "data"), aes(x = month, y = prediction, group = type), color = "black",
           stat = "identity", alpha = 1, fill = NA, size = 1) +
  theme(strip.text.x = element_text(hjust = -0.01, size = 10)) + labs(x = "", y = "Sum of predictions\nin a month", fill = "Row")
  
diff_data <- ggplot(data = subset(data_prediction_combined, row %in% c(best_both_seasonal, best_agri_seasonal, best_climate_seasonal))) + 
  geom_bar(aes(x = month, y = pred_difference, fill = row, group = row), stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~facet_wrap_2) + theme_minimal() +
  theme(strip.text.x = element_text(hjust = -0.01, size = 10)) + labs(x = "Month", y = "Difference between\npredictions and data", fill = "Row")

diff_predictions <- ggarrange(raw_data, diff_data, ncol = 1, common.legend = T, legend = "bottom", align = "v")

ggsave("figures/Figure_3.png", diff_predictions, width = 10, height = 6, dpi = 400)


data_prediction_combined$pred_difference_abs <- abs(data_prediction_combined$pred_difference)

data_difference <- aggregate(x = list(difference = data_prediction_combined$pred_difference_abs),
          by = list(row = data_prediction_combined$row,
                    type = data_prediction_combined$type),
          FUN = sum)

total_diff_agg <- subset(data_difference, row %in% c(best_both_seasonal, best_agri_seasonal, best_climate_seasonal))

write.csv(data.frame(covariate_groupings = c("OHA", "OHC", "OHAC"),
                     Human = subset(total_diff_agg, type == "human")$difference,
                     NHP = subset(total_diff_agg, type == "NHP")$difference,
                     Both = subset(total_diff_agg, type == "both")$difference,
                     stringsAsFactors = FALSE),
          file = "output/Table_3.csv", row.names = FALSE)



monthly_correlation <- do.call(rbind, sapply(c(best_agri_seasonal, best_climate_seasonal, best_both_seasonal), function(x){
  
  data.frame(
    row = x,
  human_cor = cor(subset(data_prediction_combined, row == "data" & type == "human")$prediction, 
                  subset(data_prediction_combined, row == x & type == "human")$prediction),
  NHP_cor = cor(subset(data_prediction_combined, row == "data" & type == "NHP")$prediction, 
                subset(data_prediction_combined, row == x & type == "NHP")$prediction),
  both_cor = cor(subset(data_prediction_combined, row == "data" & type == "both")$prediction, 
                 subset(data_prediction_combined, row == x & type == "both")$prediction)
  )
  
}, simplify = FALSE))

write.csv(monthly_correlation, file = "output/Table_4.csv", row.names = FALSE)





#Plot AUC
in_out_results <- cbind(in_sample_results, out_of_sample_results)
in_out_results$type <- factor(capitalize(in_out_results$type),
                              levels = c("Human", "NHP", "Both"))
in_out_results$row <- as.character(in_out_results$row)

for(i in unique(in_out_results$row)){
  in_out_results[which(in_out_results$row == i), ]$row <- gsub(i, all_row_ID[as.numeric(i)], in_out_results[which(in_out_results$row == i), ]$row)
}

in_out_results$row <- factor(in_out_results$row,
                             all_row_ID)

in_out_results$facet_name <- in_out_results$type
in_out_results$facet_name <- gsub("Human", "A",
                                  gsub("NHP", "B",
                                       gsub("Both", "C", in_out_results$facet_name)))

#Plot in vs out sample auc values
in_out_sample_auc <- ggplot(data = in_out_results[, !duplicated(colnames(in_out_results))]) + 
  geom_point(position = position_dodge(width = 0.8), aes(x = row, y = oos_auc_mid), color = "skyblue") +
  geom_errorbar(position = position_dodge(width = 0.8), aes(x = row, ymin = oos_auc_lo, ymax = oos_auc_hi), color = "skyblue") +
  geom_point(position = position_dodge(width = 0.8), aes(x = row, y = mid_auc), color = "tomato") +
  geom_errorbar(position = position_dodge(width = 0.8), aes(x = row, ymin = low_auc, ymax = high_auc), color = "tomato") +
  facet_wrap(~facet_name, ncol = 1) + theme_minimal() + labs(x = "Row", y = "AUC value", color = "Type") + 
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_text(hjust = -0.01, size = 10)) + 
  coord_cartesian(ylim = c(0.6, 1))

ggsave("figures/Figure_1.png", in_out_sample_auc, height = 6, width = 4, dpi = 300)



#Create dataframe for results
table_2_paper_results <- do.call(rbind, sapply(unique(in_out_results$row), function(x){
  
  this_row <- in_out_results[which(in_out_results$row == x), ]
  for(i in which(grepl("auc", colnames(this_row)))){
    this_row[, i] <- formatC(this_row[, i], format = "f", digits = 2, flag = 0)
  }
  
  data.frame("Agriculture output (O)" = this_row$agro_cols[1],
             "Host covariates (H)" = this_row$host_cols[1],
             "Agriculture seasonality (A)" = this_row$agro_seas[1],
             "Climate/vegetation seasonality (C)" = this_row$climate_cols[1],
             "Human report AUC" = paste0(this_row[which(this_row$type == "Human"), ]$oos_auc_mid,
                                         " (",
                                         this_row[which(this_row$type == "Human"), ]$oos_auc_lo,
                                         " - ",
                                         this_row[which(this_row$type == "Human"), ]$oos_auc_hi, ")"),
             "NHP report AUC" = paste0(this_row[which(this_row$type == "NHP"), ]$oos_auc_mid,
                                       " (",
                                       this_row[which(this_row$type == "NHP"), ]$oos_auc_lo,
                                       " - ",
                                       this_row[which(this_row$type == "NHP"), ]$oos_auc_hi, ")"),
             "Both report AUC" = paste0(this_row[which(this_row$type == "Both"), ]$oos_auc_mid,
                                        " (",
                                        this_row[which(this_row$type == "Both"), ]$oos_auc_lo,
                                        " - ",
                                        this_row[which(this_row$type == "Both"), ]$oos_auc_hi, ")"),
             "Brier score" = this_row$brier_score[1],
             "ID" = x,
             stringsAsFactors = FALSE)
  
  
  
  
}, simplify = FALSE))

table_2_paper_results$rank <- rank(table_2_paper_results$Brier.score)

all_names <- c("Agriculture output (O)", "Host covariates (H)", "Agriculture seasonality (A)", "Climate/vegetation seasonality (C)",
               "Human report AUC", "NHP report AUC", "Both report AUC", "Brier score", "ID", "Overall rank")

colnames(table_2_paper_results) <- all_names

write.csv(table_2_paper_results, "tables/table_2_paper_results.csv", row.names = FALSE)


#Variable importance
for(i in unique(variable_importance$row)){
  variable_importance[which(variable_importance$row == i), ]$row <- gsub(i, all_row_ID[as.numeric(i)], variable_importance[which(variable_importance$row == i), ]$row)
}

best_both_seasonal <- "OHAC"
best_agri_seasonal <- "OHA"
best_climate_seasonal <- "OHC"

variable_importance$type <- NA

colnames(covariate_categories) <- c("Agriculture output (O)", 
                                    "Host covariates (H)", 
                                    "Agriculture seasonality (A)", 
                                    "Climate/vegetation seasonality (C)")

for(i in unique(variable_importance$covariate)){
  variable_importance$type[which(variable_importance$covariate == i)] <- colnames(covariate_categories)[which(sapply(1:ncol(covariate_categories), function(j){any(gsub(" |-", ".", covariate_categories[, j]) == i, na.rm = T)}))]
}

variable_importance$row <- factor(variable_importance$row, levels = unique(variable_importance$row))

variable_importance$facet_legend <- NA
variable_importance$facet_legend[which(variable_importance$row == "OHA")] <- "A"
variable_importance$facet_legend[which(variable_importance$row == "OHC")] <- "B"
variable_importance$facet_legend[which(variable_importance$row == "OHAC")] <- "C"


variable_importance_graph <- ggplot(data = subset(variable_importance, row %in% c(best_both_seasonal, best_agri_seasonal, best_climate_seasonal))) +
  geom_bar(aes(x = reorder(gsub("\\.", " ", covariate), importance, na.rm = T), y = importance, fill = type), stat = "identity") + 
  theme_minimal() + 
  labs(x = "Variable", y = "Importance", fill = "Covariate grouping") +
  facet_wrap(~facet_legend, ncol = 1) + theme(axis.text.x = element_text(angle = 90), legend.position = "top",
                                              strip.text.x = element_text(hjust = -0.01, size = 10)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top"))

ggsave("figures/Figure_5.png", variable_importance_graph, height = 8, width = 8, dpi = 400)


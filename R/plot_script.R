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

#Load in all data
model_run_data <- read.csv("data/model_run_data.csv", stringsAsFactors = FALSE)

out_of_sample_results <- do.call(rbind, sapply(list.files("output/out_of_sample", full.names = T), function(x) read.csv(x, stringsAsFactors = FALSE), simplify = FALSE))
row.names(out_of_sample_results) <- NULL

in_sample_results <- read.csv("output/in_sample_dataframe_all.csv", stringsAsFactors = FALSE)

predictions <- read.csv("output/predictions_all.csv", stringsAsFactors = FALSE)
predictions_monthly <- aggregate(x = list(prediction = predictions$prediction),
                                 by = list(row = predictions$row,
                                           month = predictions$month,
                                           type = predictions$type),
                                 FUN = sum)

predictions_monthly$month <- factor(predictions_monthly$month, levels = c(7:12, 1:6))
predictions_monthly$type <- factor(predictions_monthly$type, levels = c("human", "NHP", "both"))

variable_importance <- read.csv("output/variable_importance_all.csv", stringsAsFactors = FALSE)

#Plot predictions
ggplot() + geom_bar(data = subset(predictions_monthly, row == 15), aes(x = month, y = prediction,
                                                               group = type, fill = type), stat = "identity", position = 'dodge') +
  theme_minimal() + labs(x = "Month", y = "Prediction", fill = "Type") + facet_wrap(~type)








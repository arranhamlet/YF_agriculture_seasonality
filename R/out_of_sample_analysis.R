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
library(tidyverse)
library(Hmisc)
library(ggpubr)

all_brt_oos <- list.files("output/out_of_sample", "brt_out_sample", full.names = T)
all_brt_load <- do.call(rbind, sapply(all_brt_oos, function(x) read.csv(x, stringsAsFactors = FALSE), simplify = FALSE))

row.names(all_brt_load) <- NULL
all_brt_load$row <- factor(all_brt_load$row, levels = 1:15)

all_rf_oos <- list.files("output/out_of_sample", "rf_out_sample", full.names = T)
all_rf_load <- do.call(rbind, sapply(all_rf_oos, function(x) read.csv(x, stringsAsFactors = FALSE), simplify = FALSE))

row.names(all_rf_load) <- NULL
all_rf_load$row <- factor(all_rf_load$row, levels = 1:15)

all_rf_load$method <- "Random forest"
all_brt_load$method <- "Boosted regression tree"

all_type <- rbind(all_rf_load, all_brt_load)

all_type$agro_or_climate <- sapply(1:nrow(all_type), function(x){
  if(all_type$climate_cols[x] == 0 & all_type$agro_seas[x] == 1){
    "Agricultural seasonality"
  } else if(all_type$climate_cols[x] == 1 & all_type$agro_seas[x] == 0){
    "Climate seasonality"
  } else if(all_type$climate_cols[x] == 1 & all_type$agro_seas[x] == 1){
    "Both"
  } else if(all_type$climate_cols[x] == 0 & all_type$agro_seas[x] == 0){
    "Neither"
  }
})


all_type$agro_or_climate_method <- paste0(all_type$agro_or_climate, "_", all_type$method)
all_type$type <- factor(capitalize(all_type$type), levels = c("Human", "NHP", "Both"))
all_type$agro_or_climate <- factor(all_type$agro_or_climate, levels = c("Agricultural seasonality", "Climate seasonality", "Both", "Neither"))
if(any(duplicated(all_type))) all_type <- all_type[!duplicated(all_type), ]

all_type$rank_method_type <- NA
for(x in unique(all_type$method)){
  for(y in unique(all_type$type)){
    all_type[which(all_type$method == x & all_type$type == y), ]$rank_method_type <- rank(all_type[which(all_type$method == x & all_type$type == y), ]$mid_auc)
  }
}

all_type$agro_type <- paste0(all_type$type, all_type$agro_or_climate)


ggplot(data = all_type) + 
  geom_point(aes(x = row, y = mid_auc, col = method)) + 
  geom_errorbar(aes(x = row, ymin = low_auc, ymax = high_auc, col = method)) + 
  theme_minimal() + 
  labs(x = "Row", y = "AUC") +
  facet_wrap(~type)

auc_oos <- ggplot(data = all_type) +
  geom_boxplot(aes(x = agro_or_climate, y = mid_auc, group = agro_or_climate_method, fill = method)) +
  facet_wrap(~type) +
  theme_minimal() +
  labs(x = "", y = "AUC", fill = "Method") +
  theme(axis.text.x = element_blank())
  
brier_oos <- ggplot(data = subset(all_type)) +
  geom_boxplot(aes(x = agro_or_climate, y = brier_score, group = agro_or_climate_method, fill = method)) +
  theme_minimal() +
  facet_wrap(~ type, scales = "free_y") +
  labs(x = "", y = "Brier score", fill = "Method") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90),
        strip.text.x = element_blank())

combo_plot <- ggarrange(auc_oos, brier_oos, ncol = 1, heights = c(1.5, 2), common.legend = T, legend = "top")


ggsave("figures/auc_brier_out_of_sample_brt_vs_rf.png", combo_plot, height = 5, width = 9.5)





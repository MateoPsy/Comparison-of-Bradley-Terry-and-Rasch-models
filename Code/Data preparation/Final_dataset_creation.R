############################################
#### Pairwise comparisons analysis 2025 ####
####       Final dataset creation       ####
############################################

rm(list=ls())
setwd("~/Desktop/Pairwise comparisons_study")

### packages ####
library(tidyverse)
library(ggplot2)
library(dplyr)

### loading datasets ####

# Rasch model data
aut_ready    <-   read.csv("Data/RM/aut_ready.csv")
bmpn_ready   <-   read.csv("Data/RM/bmpn_ready.csv")
dass_ready   <-   read.csv("Data/RM/dass_ready.csv")
dem_ready    <-   read.csv("Data/RM/dem_ready.csv")
ders_ready   <-   read.csv("Data/RM/ders_ready.csv")
hi_ready     <-   read.csv("Data/RM/hi_ready.csv")

# Bradley-Terry model data
bt_aut    <-   read.csv("Data/BTM/bt_aut.csv")
bt_bmpn   <-   read.csv("Data/BTM/bt_bmpn.csv")
bt_dass   <-   read.csv("Data/BTM/bt_dass.csv")
bt_dem    <-   read.csv("Data/BTM/bt_dem.csv")
bt_ders   <-   read.csv("Data/BTM/bt_ders.csv")
bt_hi     <-   read.csv("Data/BTM/bt_hi.csv")


#### combining BT and LS data ####

analysis_aut        <- rbind(bt_aut, aut_ready)
analysis_bmpn       <- rbind(bt_bmpn, bmpn_ready)
analysis_dass       <- rbind(bt_dass, dass_ready)
analysis_democr     <- rbind(bt_dem, dem_ready)
analysis_ders       <- rbind(bt_ders, ders_ready)
analysis_height_inv <- rbind(bt_hi, hi_ready)

#### SUPER DATASET CREATION ####

# Define dataset list and labels
datasets <- list(
  AUT = analysis_aut,
  BMPN = analysis_bmpn,
  DASS = analysis_dass,
  DEM = analysis_democr,
  DERS = analysis_ders,
  HI = analysis_height_inv
)

codebook_datasets <- data.frame(
  codes = c("AUT","BMPN","DASS","DEM","DERS","HI"),
  datasets = c("AUT - Autonomy",
               "BMPN - Basic psychological needs",
               "DASS - Depression, Anxiety, and Stress",
               "DEMOCR - Democracy",
               "DERS - Difficulties in emotion regulation",
               "HEIGHT INV - Height Inventory"
  )
)

# Define all required column names
required_cols <- c("r", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "id_type", "id", "sex", "item", "collection", "dataset_id")

# Harmonize and tag datasets
datasets_harmonized <- lapply(names(datasets), function(label) {
  d <- datasets[[label]]
  
  # Add missing columns with NA
  missing_cols <- setdiff(required_cols, names(d))
  for (col in missing_cols) {
    d[[col]] <- NA
  }
  
  # Reorder columns to match required structure
  d <- d[, required_cols]
  
  # Add dataset_id
  d$dataset_id <- label
  
  return(d)
})

# Combine all datasets row-wise
analysis_total <- do.call(rbind, datasets_harmonized)

analysis_total$item_type <- analysis_total$id_type

# write.csv(analysis_total, "Data/analysis_total.csv")


### Creating dataset for RM-BTM global model analysis - experimental, do not use ####

# adding variable item_type with reversing reversed variable for RM-BTM global model analysis
#     the correct reversion should be as follows:
#                         - in columns V1:V7 change 1 to -1 for reversed items
#                               - changing easiness to difficulty
#                         - for random grouping variable item with respect to item_type (0+item_type|item) change 1 to -1 for reversed items
#                               - accounting for easiness/difficulty change in random effects

analysis_total_GM <- analysis_total
analysis_total_GM$item_type <- analysis_total_GM$id_type

cond <- with(analysis_total_GM, id_type == 1 &
               ((dataset_id == "AUT" & item %in% c("V2", "V4", "V7")) |
                  (dataset_id == "BMPN" & item %in% c("V2", "V5"))))

analysis_total_GM[cond, paste0("V", 1:7)] <- -analysis_total_GM[cond, paste0("V", 1:7)]
analysis_total_GM$item_type[cond] <- -analysis_total_GM$item_type[cond]

# write.csv(analysis_total_GM, "Data/analysis_total_GM.csv")

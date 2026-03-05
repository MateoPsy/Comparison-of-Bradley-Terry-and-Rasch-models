############################################
#### Pairwise comparisons analysis 2025 ####
####    LS  dataset preparation         ####
############################################

rm(list=ls())
setwd("~/Desktop/Pairwise comparisons_study")

### packages ####
library(tidyverse)
library(ggplot2)
library(dplyr)

### loading datasets ####


# data collection 1 - in house collection
aut          <-     read.csv("Data/snippets/aut-snip.csv")
bmpn         <-     read.csv("Data/snippets/bmpn-snip.csv")
dass         <-     read.csv("Data/snippets/dass-snip.csv")
democr       <-     read.csv("Data/snippets/democr-snip.csv")
ders         <-     read.csv("Data/snippets/ders-snip.csv")
height_inv   <-     read.csv("Data/snippets/height_inv-snip.csv")

# data collection 2 - outsourced collection

height_orig <- read_rds("Data/Outsourced data/iga.rds")
czg_orig <- read.csv("Data/Outsourced data/CzG.csv")
qz_orig <- read.csv("Data/Outsourced data/QZ.csv")
aut_orig <- read.delim("Data/Outsourced data/AS_1PL.csv", sep = ";")
dem_orig <- read.csv("Data/Outsourced data/dem.csv")

### REVERSING VARIABLES AND PREPARING FOR 2PL - collection 1 ####

# it does what it says :)) it converts df into numeric
convert_columns_to_numeric <- function(df) {
  df[] <- lapply(df, function(x) as.numeric(as.character(x)))
  return(df)
}

# ID creation - so we have it ready in case of discrepancy
id_codebook <- data.frame()
id_codebook <- aut$id
id_codebook <- data.frame(cbind(id_codebook,seq(1:60)))
names(id_codebook) <- c("original", "id")

# write.csv(id_codebook, "Documentation/id_codebook.csv")
id <- c()
id <- as.numeric(unlist(id_codebook$id))

ls_aut        <- cbind(convert_columns_to_numeric(aut[,44:50]), aut$gender,id)
ls_bmpn       <- cbind(convert_columns_to_numeric(bmpn[,22:26]), bmpn$gender,id)
ls_dass       <- cbind(convert_columns_to_numeric(dass[,44:50]), dass$gender,id)
ls_democr     <- cbind(convert_columns_to_numeric(democr[,22:26]), democr$gender,id)
ls_ders       <- cbind(convert_columns_to_numeric(ders[,44:50]), ders$gender,id)
ls_height_inv <- cbind(convert_columns_to_numeric(height_inv[,58:65]), height_inv$gender,id)

# adding col names
names(ls_aut) <- c("V1", "V2", "V3","V4", "V5", "V6","V7", "sex", "id")
names(ls_bmpn) <- c("V1", "V2", "V3","V4", "V5", "sex", "id")
names(ls_dass) <- c("V1", "V2", "V3","V4", "V5", "V6","V7", "sex", "id")
names(ls_democr) <- c("V1", "V2", "V3","V4", "V5", "sex", "id")
names(ls_ders) <- c("V1", "V2", "V3","V4", "V5", "V6","V7", "sex", "id")
names(ls_height_inv) <- c("V1", "V2", "V3","V4", "V5", "V6", "V7", "V8", "sex", "id")

# adding dataset_id

ls_aut$dataset_id        <- "AUT"
ls_bmpn$dataset_id       <- "BMPN"
ls_dass$dataset_id       <- "DASS"
ls_democr$dataset_id     <- "DEM"
ls_ders$dataset_id       <- "DERS"
ls_height_inv$dataset_id <- "HI"

# adding collection 1 = in house collection

ls_aut$collection        <- 1
ls_bmpn$collection       <- 1
ls_dass$collection       <- 1
ls_democr$collection     <- 1
ls_ders$collection       <- 1
ls_height_inv$collection <- 1




# let´s check reversed items
# psych::fa(ls_aut[-c(8,9)]) # reversed V2,V4,V7
# psych::fa(ls_bmpn[-c(6,7)]) # reversed V1,V3, which is kinda BS, as reversed items are V2 & V5
# psych::fa(ls_dass[-c(8,9)])
# psych::fa(ls_democr[-c(6,7)])# weird it says V5 has loading -0.02, fa.parallel says 0 factors and 1 component
# psych::fa(ls_ders[-c(8,9)])
# psych::fa(ls_height_inv[-c(9,10)])

# Let´s reverse items in AUT & BMPN

ls_aut <- ls_aut %>% 
  mutate(
    V2 = case_when(
      V2 %in% 1 ~ 4,
      V2 %in% 2 ~ 3,
      V2 %in% 3 ~ 2,
      V2 %in% 4 ~ 1,
    ),
    V4 = case_when(
      V4 %in% 1 ~ 4,
      V4 %in% 2 ~ 3,
      V4 %in% 3 ~ 2,
      V4 %in% 4 ~ 1,
    ),
    V7 = case_when(
      V7 %in% 1 ~ 4,
      V7 %in% 2 ~ 3,
      V7 %in% 3 ~ 2,
      V7 %in% 4 ~ 1,
    ))

#psych::fa(ls_aut[-c(8,9)]) #looks good now

ls_bmpn <- ls_bmpn %>% 
  mutate(
    V2 = case_when(
      V2 %in% 1 ~ 4,
      V2 %in% 2 ~ 3,
      V2 %in% 3 ~ 2,
      V2 %in% 4 ~ 1,
    ),
    V4 = case_when(
      V4 %in% 1 ~ 4,
      V4 %in% 2 ~ 3,
      V4 %in% 3 ~ 2,
      V4 %in% 4 ~ 1,
    ),
    V5 = case_when(
      V5 %in% 1 ~ 4,
      V5 %in% 2 ~ 3,
      V5 %in% 3 ~ 2,
      V5 %in% 4 ~ 1,
    ))

# psych::fa.parallel(ls_bmpn[1:5])
# psych::fa(ls_bmpn[1:5])

# fa looks very wrong here, at least all factor loadings are positive

# list of items and how they are supposedly coded:
# 1 K lidem, se kterými trávím čas, zažívám silný pocit blízkosti. - relatedness (5) - satisfaction
# 2 *Udělal/a jsem nějakou hloupost, po které jsem se cítil/a neschopně.  - competence (10) - dissatisfaction
# 3 Dařilo se mi i v náročných/obtížných věcech. - competence (11) - satisfaction
# 4 *Někteří lidé mi radili, co bych měl/a dělat. - autonomy (16) - dissatisfaction
# 5 *Musel/a jsem dělat věci proti své vůli. - autonomy (18) - dissatisfaction


### REVERSING VARIABLES AND PREPARING FOR 2PL - collection 2 ####

# Autonomy items from original dataset AS_1PL: all
# BMPN items from original dataset CzG: bmpn05, bmpn10, bmpn11, bmpn16, bmpn18
# DASS items from original dataset QZ: 1, 6, 8, 11, 12, 14, 18 (stress facet)
# DEM items from original dataset 
# DERS items from original dataset CzG: ders07, ders08, ders09, ders10, ders11, ders12, ders15
# HI items from original dataset IGA.RDS: 1, 2, 3, 4, 5, 6, 7, 8


# AUT 
# already binary
aut_orig <- aut_orig %>%
  filter(rowSums(is.na(across(c(a1_2, a2_2, a3_2, a4_2, a5_2, a6_2, a7_2)))) <= 2) %>% 
  mutate(id = 10000 + X)

aut_orig <- aut_orig[,-1]
names(aut_orig) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "id")
# psych::fa.parallel(aut_orig[-8]) # looks like it is already reversed

# BMPN
bmpn_orig <- czg_orig %>%
  select(bmpn_5, bmpn_10, bmpn_11, bmpn_16, bmpn_18) %>%
  filter(rowSums(is.na(across(c(bmpn_5, bmpn_10, bmpn_11, bmpn_16, bmpn_18)))) <= 2) %>%
  mutate(id = 20000 + seq(1:nrow(.)))
names(bmpn_orig) <- c("V1", "V2", "V3", "V4", "V5", "id")
psych::fa.parallel(bmpn_orig[-6]) # 1 factor, 2 components
psych::fa(bmpn_orig[-6]) # 1 factor, 2 components

# DASS 
dass_orig <- qz_orig %>%
  select(DASS_1s, DASS_6s, DASS_8s, DASS_11s, DASS_12s, DASS_14s, DASS_18s) %>%
  filter(rowSums(is.na(across(c(DASS_1s, DASS_6s, DASS_8s, DASS_11s, DASS_12s, DASS_14s, DASS_18s)))) <= 2) %>%
  mutate(id = 30000 + seq(1:nrow(.)))
names(dass_orig) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "id")

# DEM
dem_orig <- dem_orig %>%
  select(dem4_s, dem5_s, dem6_s, dem7_s, dem8_s, pohlavi) %>%
  filter(rowSums(is.na(across(c(dem4_s, dem5_s, dem6_s, dem7_s, dem8_s)))) <= 2) %>%
  mutate(id = 40000 + seq(1:nrow(.)))
names(dem_orig) <- c("V1", "V2", "V3", "V4", "V5","sex", "id")

# DERS 
ders_orig <- czg_orig %>%
  select(ders07, ders08, ders09, ders10, ders11, ders12, ders15) %>%
  filter(rowSums(is.na(across(c(ders07, ders08, ders09, ders10, ders11, ders12, ders15)))) <= 2) %>%
  mutate(id = 50000 + seq(1:nrow(.)))
names(ders_orig) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "id")

# HEIGHT INVENTORY
hi_orig <- height_orig %>%
  select(q1_h01, q1_h02, q1_h03, q1_h04, q1_h05, q1_h06, q1_h07, q1_h08, sex) %>%
  filter(rowSums(is.na(across(c(q1_h01, q1_h02, q1_h03, q1_h04, q1_h05, q1_h06, q1_h07, q1_h08)))) <= 2) %>% 
  filter(!is.na(sex)) %>%
  mutate(id = 60000 + seq(1:nrow(.)))
names(hi_orig) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "sex","id")

# variable sex
aut_orig$sex         <- NA
bmpn_orig$sex        <- NA
dass_orig$sex        <- NA
ders_orig$sex        <- NA

# and for these 2 we have sex available
dem_orig$sex <- factor(dem_orig$sex,
                       levels = c(1, 2, 3),
                       labels = c("female", "male", "other")) # beware of the changed order
hi_orig$sex <- factor(hi_orig$sex,
                      levels = c("m", "f", "o"),
                      labels = c("male", "female", "other")) # beware of the changed order

# adding dataset_id
aut_orig$dataset_id     <- "AUT"
bmpn_orig$dataset_id    <- "BMPN"
dass_orig$dataset_id    <- "DASS"
dem_orig$dataset_id     <- "DEM"
ders_orig$dataset_id    <- "DERS"
hi_orig$dataset_id      <- "HI"

# adding collection 1 = outsourced collection

aut_orig$collection     <- 2
bmpn_orig$collection    <- 2
dass_orig$collection    <- 2
dem_orig$collection     <- 2
ders_orig$collection    <- 2
hi_orig$collection      <- 2

### BINDING DATASETS TOGETHER - 2PL (non-essential) ####

aut_combined    <-   rbind(ls_aut, aut_orig) # beware, collection = 2 is already binarized // 1=agree, 2=disagree
bmpn_combined   <-   rbind(ls_bmpn, bmpn_orig)
dass_combined   <-   rbind(ls_dass, dass_orig)
dem_combined    <-   rbind(ls_democr, dem_orig)
ders_combined   <-   rbind(ls_ders, ders_orig)
hi_combined     <-   rbind(ls_height_inv, hi_orig)


# just to have available for analysis
write.csv(aut_combined, "Data/2PL/aut_combined.csv") 
write.csv(bmpn_combined, "Data/2PL/bmpn_combined.csv") 
write.csv(dass_combined, "Data/2PL/dass_combined.csv")
write.csv(dem_combined, "Data/2PL/dem_combined.csv")
write.csv(ders_combined, "Data/2PL/ders_combined.csv")
write.csv(hi_combined, "Data/2PL/hi_combined.csv")

# creating one dataset for analysis non-binarized

datasets <- list(
  AUT = aut_combined,
  BMPN = bmpn_combined,
  DASS = dass_combined,
  DEM = dem_combined,
  DERS = ders_combined,
  HI = hi_combined
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

names(aut_combined)

# Define all required column names
required_cols <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "id", "sex", "collection", "dataset_id")

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
all_combined <- do.call(rbind, datasets_harmonized)

write.csv(all_combined, "Data/2PL/all_combined.csv", row.names = FALSE)



### BINARIZATION OF DATASETS FOR RM - collection 1 ####
# recoding for 4-point ls

ls_dass_bi <- ls_dass %>%
  mutate(across(V1:V7, ~ case_when( # beware to put relevant number of items
    . %in% c(1, 2) ~ -1,
    . %in% c(3, 4) ~ 1
  )))

ls_ders_bi <- ls_ders %>%
  mutate(across(V1:V7, ~ case_when( # beware to put relevant number of items
    . %in% c(1, 2) ~ -1,
    . %in% c(3, 4) ~ 1
  )))

ls_aut_bi <- ls_aut %>%
  mutate(across(V1:V7, ~ case_when( # beware to put relevant number of items
    . %in% c(1, 2) ~ -1,
    . %in% c(3, 4) ~ 1
  )))


ls_democr_bi <- ls_democr %>%
  mutate(across(V1:V5, ~ case_when( # beware to put relevant number of items
    . %in% c(1, 2) ~ -1,
    . %in% c(3, 4) ~ 1
  )))

ls_bmpn_bi <- ls_bmpn %>%
  mutate(across(V1:V5, ~ case_when( # beware to put relevant number of items
    . %in% c(1, 2) ~ -1,
    . %in% c(3, 4) ~ 1
  )))


ls_height_inv_bi <- ls_height_inv %>%
  mutate(across(V1:V8, ~ case_when( # beware to put relevant number of items
    . %in% c(1, 2) ~ -1,
    . %in% c(3, 4) ~ 1
  )))




## melting dfs and transforming them for analysis
ls_aut_bi        <- reshape2::melt(ls_aut_bi, id.vars = c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:7)
ls_bmpn_ov_bi    <- reshape2::melt(ls_bmpn_bi, id.vars = c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:5)
ls_dass_bi       <- reshape2::melt(ls_dass_bi, id.vars = c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:7)
ls_democr_bi     <- reshape2::melt(ls_democr_bi, id.vars = c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:5)
ls_ders_bi       <- reshape2::melt(ls_ders_bi, id.vars = c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:7)
ls_height_inv_bi <- reshape2::melt(ls_height_inv_bi, id.vars = c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:8)



transformed_df <- function(df, items) {
  # Create V1 to Vx columns, all zeros
  vars <- paste0("V", 1:items)
  zero_df <- as.data.frame(matrix(0, nrow = nrow(df), ncol = length(vars)))
  colnames(zero_df) <- vars
  
  # For each row, put -1 only in the matching variable column
  for (i in seq_len(nrow(df))) {
    col <- as.character(df$variable[i])
    if (col %in% vars) {
      zero_df[i, col] <- 1
    }
  }
  
  # Combine with id and value columns
  result <- cbind(id = df[["id"]], sex = df$sex, dataset_id = df$dataset_id, collection = df$collection, value = df$value, zero_df)
  names(result)[names(result) == "value"] <- "r"
  result$r[result$r == -1] <- 0
  result <- result[order(result$id), ]
  return(result)
}

# Transform the data frame
ls_aut_bi        <- transformed_df(ls_aut_bi, 7)
ls_bmpn_ov_bi    <- transformed_df(ls_bmpn_ov_bi, 5)
ls_dass_bi       <- transformed_df(ls_dass_bi, 7)
ls_democr_bi     <- transformed_df(ls_democr_bi, 5)
ls_ders_bi       <- transformed_df(ls_ders_bi, 7)
ls_height_inv_bi <- transformed_df(ls_height_inv_bi, 8)

# adding type column (1 = likert scale)

ls_aut_bi$id_type          <- 1
ls_bmpn_ov_bi$id_type      <- 1
ls_dass_bi$id_type         <- 1
ls_democr_bi$id_type       <- 1
ls_ders_bi$id_type         <- 1
ls_height_inv_bi$id_type   <- 1


### BINARIZATION OF DATASETS FOR RM - collection 2 ####

# making items binary
aut_orig_bi <- aut_orig %>%
  mutate(across(
    starts_with("V"),
    ~ case_when(
      .x == 1 ~ -1,
      .x == 2 ~ 1
    )
  ))

bmpn_orig_bi <- bmpn_orig %>%
  mutate(across(
    starts_with("V"),
    ~ case_when(
      .x %in% c(1, 2) ~ -1,
      .x == 3 ~ NA_real_,
      .x %in% c(4, 5) ~ 1
    )
  ))

dass_orig_bi <- dass_orig %>%
  mutate(across(
    starts_with("V"),
    ~ case_when(
      .x %in% c(0,1) ~ -1,
      .x %in% c(2,3) ~ 1
    )
  ))

dem_orig_bi <- dem_orig %>%
  mutate(across(
    starts_with("V"),
    ~ case_when(
      .x %in% c(1,2,3) ~ -1,
      .x %in% c(5,6,7) ~ 1  ### data from Vendy are coded as follows: 1,2,3 -> disagreement, 5,6,7 -> agreement
    )
  ))


ders_orig_bi <- ders_orig %>%
  mutate(across(
    starts_with("V"),
    ~ case_when(
      .x %in% c(1, 2) ~ -1,
      .x == 3 ~ NA_real_,
      .x %in% c(4, 5) ~ 1
    )
  ))

hi_orig_bi <- hi_orig %>% 
  mutate(across(
    starts_with("V"),
    ~ case_when(
      .x %in% c(1,2) ~ -1,
      .x %in% c(3,4) ~ 1
    )
  ))

hi_orig_bi <- as.data.frame(hi_orig_bi)


# melting down dataset
aut_orig_bi         <- reshape2::melt(aut_orig_bi, id.vars =  c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:7)
bmpn_orig_bi        <- reshape2::melt(bmpn_orig_bi, id.vars =  c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:5)
dass_orig_bi        <- reshape2::melt(dass_orig_bi, id.vars =  c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:7)
dem_orig_bi         <- reshape2::melt(dem_orig_bi, id.vars = c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:5)
ders_orig_bi        <- reshape2::melt(ders_orig_bi, id.vars =  c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:7)
hi_orig_bi          <- reshape2::melt(hi_orig_bi, id.vars = c("id", "sex", "dataset_id", "collection"), fun.aggregate = 1:8)



transformed_df <- function(df, items) {
  # Create V1 to Vx columns, all zeros
  vars <- paste0("V", 1:items)
  zero_df <- as.data.frame(matrix(0, nrow = nrow(df), ncol = length(vars)))
  colnames(zero_df) <- vars
  
  # For each row, put -1 only in the matching variable column
  for (i in seq_len(nrow(df))) {
    col <- as.character(df$variable[i])
    if (col %in% vars) {
      zero_df[i, col] <- 1
    }
  }
  
  # Combine with id and value columns
  result <- cbind(id = df[["id"]], sex = df$sex, dataset_id = df$dataset_id, collection = df$collection, value = df$value, zero_df)
  names(result)[names(result) == "value"] <- "r"
  result$r[result$r == -1] <- 0
  result <- result[order(result$id), ]
  return(result)
}


aut_orig_bi        <- transformed_df(aut_orig_bi, 7)
bmpn_orig_bi       <- transformed_df(bmpn_orig_bi, 5)
dass_orig_bi       <- transformed_df(dass_orig_bi, 7)
dem_orig_bi        <- transformed_df(dem_orig_bi, 5)
ders_orig_bi       <- transformed_df(ders_orig_bi, 7)
hi_orig_bi         <- transformed_df(hi_orig_bi, 8)




# adding type column (1 = likert scale)
aut_orig_bi$id_type        <- 1
bmpn_orig_bi$id_type       <- 1
dass_orig_bi$id_type       <- 1
dem_orig_bi$id_type        <- 1
ders_orig_bi$id_type       <- 1
hi_orig_bi$id_type         <- 1



### BINDING DATASETS TOGETHER - RM ####

# connecting datasets across collections

aut_ready    <-   rbind(ls_aut_bi, aut_orig_bi)
bmpn_ready   <-   rbind(ls_bmpn_ov_bi, bmpn_orig_bi)
dass_ready   <-   rbind(ls_dass_bi, dass_orig_bi)
dem_ready    <-   rbind(ls_democr_bi, dem_orig_bi)
ders_ready   <-   rbind(ls_ders_bi, ders_orig_bi)
hi_ready     <-   rbind(ls_height_inv_bi, hi_orig_bi)

# adding variable names
add_var_name <- function(data, var1, varx) {
  # Determine which columns are in the range
  start_col <- which(names(data) == var1)
  end_col <- which(names(data) == varx)
  col_names <- names(data)[start_col:end_col]
  
  # Create empty column if it doesn't exist
  if (!"item" %in% names(data)) {
    data$item <- "V1"
  }
  
  # Loop over rows
  for (i in 1:nrow(data)) {
    if (data$id_type[i] == 1) {
      row_vals <- data[i, col_names]
      selected_name <- names(row_vals)[which(row_vals != 0)]
      if (length(selected_name) > 0) {
        data$item[i] <- selected_name[1]  # If more than one, take first
      }
    }
  }
  
  return(data)
}



aut_ready        <- add_var_name(aut_ready, "V1", "V7")
bmpn_ready       <- add_var_name(bmpn_ready, "V1", "V5")
dass_ready       <- add_var_name(dass_ready, "V1", "V7")
dem_ready        <- add_var_name(dem_ready, "V1", "V5")
ders_ready       <- add_var_name(ders_ready, "V1", "V7")
hi_ready         <- add_var_name(hi_ready, "V1", "V8")

# omitting rows, where r = NA
aut_ready        <- aut_ready %>% filter(!is.na(r))
bmpn_ready       <- bmpn_ready %>% filter(!is.na(r))
dass_ready       <- dass_ready %>% filter(!is.na(r))
dem_ready        <- dem_ready %>% filter(!is.na(r))
ders_ready       <- ders_ready %>% filter(!is.na(r))
hi_ready         <- hi_ready %>% filter(!is.na(r))


# saving datasets

write.csv(aut_ready, "Data/RM/aut_ready.csv", row.names = FALSE)
write.csv(bmpn_ready, "Data/RM/bmpn_ready.csv", row.names = FALSE)
write.csv(dass_ready, "Data/RM/dass_ready.csv", row.names = FALSE)
write.csv(dem_ready, "Data/RM/dem_ready.csv", row.names = FALSE)
write.csv(ders_ready, "Data/RM/ders_ready.csv", row.names = FALSE)
write.csv(hi_ready, "Data/RM/hi_ready.csv", row.names = FALSE)

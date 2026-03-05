

# ---- Pairwise comparisons analysis 2025 - Factor analysis ---- ####



rm(list=ls())
setwd("~/Desktop/Pairwise comparisons_study")

# Packages ####
library(tidyverse)
library(dplyr)
library(psych)

# Loading datasets ####


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

all_combined_bi <- read_csv("Data/2PL/all_combined_bi.csv")


# INHOUSE DATASETS ####
## Preparing datasets for factor analysis - in house collection ####


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



## Factor analysis ####

# --- AUT ---
cat("\nAUT factor analysis\n")
fa.parallel(ls_aut %>% select(V1:V7)) # suggests 2 factors, looks weird
aut_fa <- fa(ls_aut %>% select(V1:V7),2) # might actually be 2 factors
print(aut_fa)

# --- BMPN ---
cat("\nBMPN factor analysis\n")
fa.parallel(ls_bmpn %>% select(V1:V5)) # suggests 0 factors, looks obnoxious
bmpn_fa <- fa(ls_bmpn %>% select(V1:V5)) # V1 and V3 are off... prolly something to do with them being positivitely worded?
print(bmpn_fa)

# --- DASS ---
cat("\nDASS factor analysis\n")
fa.parallel(ls_dass %>% select(V1:V7)) # suggests 1 factor
dass_fa <- fa(ls_dass %>% select(V1:V7)) # might as well be
print(dass_fa)

# --- DEMOCR ---
cat("\nDEMOCR factor analysis\n")
fa.parallel(ls_democr %>% select(V1:V5)) # suggests 0 factors, looks obnoxious, I would believe there is no factor...
democr_fa <- fa(ls_democr %>% select(V1:V5)) # V4 and V5 don´t load at all on factor
print(democr_fa)

# --- DERS ---
cat("\nDERS factor analysis\n")
fa.parallel(ls_ders %>% select(V1:V7)) # suggests 2 factors, but looks like 1 factor only
ders_fa <- fa(ls_ders %>% select(V1:V7)) # 2nd factor created by V2 completely, must be influential case (When I’m upset, I cannot stay focused)
print(ders_fa)

# --- HEIGHT INVENTORY: split by sex ---

# Male
hi_male_ls <- ls_height_inv %>% filter(sex == "male")
cat("\nHEIGHT INVENTORY - Males factor analysis\n")
fa.parallel(hi_male_ls %>% select(V1:V8)) # suggests 1 factor
hi_male_fa <- fa(hi_male %>% select(V1:V8))
print(hi_male_fa)

# Female
hi_female_ls <- ls_height_inv %>% filter(sex == "female")
cat("\nHEIGHT INVENTORY - Females factor analysis\n")
fa.parallel(hi_female %>% select(V1:V8)) # suggests 1 factor
hi_female_fa <- fa(hi_female %>% select(V1:V8))
print(hi_female_fa)


## Descriptive statistics ####

# AUT
summary(ls_aut %>% select(V1:V7) %>% rowSums(na.rm = TRUE))
summary(ls_aut %>% select(V1:V7)) # no NAs
psych::alpha(ls_aut %>% select(V1:V7))
psych::omega(ls_aut %>% select(V1:V7),1) # McDonald´s omega 0.77, Cronbach alpha 0.76 # item 2 doesn´t add to internal consistency

# BMPN
summary(ls_bmpn %>% select(V1:V5) %>% rowSums(na.rm = TRUE))
summary(ls_bmpn %>% select(V1:V5)) # no NAs
psych::alpha(ls_bmpn %>% select(V1:V5))
psych::omega(ls_bmpn %>% select(V1:V5),1) # McDonald´s omega 0.51, Cronbach alpha 0.48 # item 1 and 2 don´t add to internal consistency, item V5 supports most of it 

# DASS
summary(ls_dass %>% select(V1:V7) %>% rowSums(na.rm = TRUE))
summary(ls_dass %>% select(V1:V7)) # no NAs
psych::alpha(ls_dass %>% select(V1:V7))
psych::omega(ls_dass %>% select(V1:V7),1) # McDonald´s omega 0.79, Cronbach alpha 0.79 # item 6 don´t add to internal consistency

# DEM
summary(ls_democr %>% select(V1:V5) %>% rowSums(na.rm = TRUE))
summary(ls_democr %>% select(V1:V5)) # no NAs
psych::alpha(ls_democr %>% select(V1:V5))
psych::omega(ls_democr %>% select(V1:V5),) # McDonald´s omega 0.4, Cronbach alpha 0.33, no factor showing

# DERS
summary(ls_ders %>% select(V1:V7) %>% rowSums(na.rm = TRUE))
summary(ls_ders %>% select(V1:V7)) # no NAs
psych::alpha(ls_ders %>% select(V1:V7))
psych::omega(ls_ders %>% select(V1:V7),1) # McDonald´s omega 0.85, Cronbach alpha 0.85

# HEIGHT INVENTORY
# male
summary(hi_male_ls %>% select(V1:V7) %>% rowSums(na.rm = TRUE))
summary(hi_male_ls %>% select(V1:V7)) # NA´s around 1%, but V2 over 3%
psych::alpha(hi_male_ls %>% select(V1:V7))
psych::omega(hi_male_ls %>% select(V1:V7),1) # McDonald´s omega 0.89, Cronbach alpha 0.89, item V2 doesn´t add to internal consistency

# female
summary(hi_female_ls %>% select(V1:V7) %>% rowSums(na.rm = TRUE))
summary(hi_female_ls %>% select(V1:V7)) # NA´s generally around 1%, but V1 at almost 6%
psych::alpha(hi_female_ls %>% select(V1:V7))
psych::omega(hi_female_ls %>% select(V1:V7),1) # McDonald´s omega 0.91, Cronbach alpha 0.91


# OUTSOURCED DATASETS ####
## Preparing datasets for factor analysis - outsourced collection ####

# AUT 
# already binary
aut_orig <- aut_orig %>%
  filter(rowSums(is.na(across(c(a1_2, a2_2, a3_2, a4_2, a5_2, a6_2, a7_2)))) <= 2) %>% 
  mutate(id = 10000 + X)

aut_orig <- aut_orig[,-1]
names(aut_orig) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "id")


# BMPN
bmpn_orig <- czg_orig %>%
  select(bmpn_5, bmpn_10, bmpn_11, bmpn_16, bmpn_18) %>%
  filter(rowSums(is.na(across(c(bmpn_5, bmpn_10, bmpn_11, bmpn_16, bmpn_18)))) <= 2) %>%
  mutate(id = 20000 + seq(1:nrow(.)))
names(bmpn_orig) <- c("V1", "V2", "V3", "V4", "V5", "id")

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

## Factor analysis ####

# AUT
cat("\nAUT factor analysis\n")
fa.parallel(aut_orig %>% select(V1:V7)) # suggests 2 factors, but only item that binds there is V5, also looks like 1 factor only
aut_fa <- fa(aut_orig %>% select(V1:V7))
print(aut_fa)
fa.diagram(aut_fa)

# BMPN
cat("\nBMPN factor analysis\n")
fa.parallel(bmpn_orig %>% select(V1:V5)) # suggests 2 factors, but looks like 1 factor only
bmpn_fa <- fa(bmpn_orig %>% select(V1:V5))
print(bmpn_fa)

# DASS
cat("\nDASS factor analysis\n")
fa.parallel(dass_orig %>% select(V1:V7)) # suggests 3 factors, but looks like 1 factor only
dass_fa <- fa(dass_orig %>% select(V1:V7)) # looks like V2 alone creates 2nd factor, and V6 alone creates 3rd factor
print(dass_fa)

# DEM
cat("\nDEM factor analysis\n")
fa.parallel(dem_orig %>% select(V1:V5)) # suggests 2 factors and 2 PC, but to be honest, looks really wild... as if it was missing any factor structure at all :(
dem_fa <- fa(dem_orig %>% select(V1:V5),1) # possibly 2 factor
print(dem_fa)

# DERS
cat("\nDERS factor analysis\n")
fa.parallel(ders_orig %>% select(V1:V7)) # suggests 3 factors, but looks like 1 factor only
ders_fa <- fa(ders_orig %>% select(V1:V7))
print(ders_fa)

# HEIGHT INVENTORY
  # Male
  hi_male <- hi_orig %>% filter(sex == "male")
  cat("\nHEIGHT INVENTORY - Males factor analysis\n")
  fa.parallel(hi_male %>% select(V1:V8)) # suggests 2 factors, but looks like 1 factor only
  hi_male_fa <- fa(hi_male %>% select(V1:V8)) # only 1 factor
  print(hi_male_fa)

  # Female
  hi_female <- hi_orig %>% filter(sex == "female")
  cat("\nHEIGHT INVENTORY - Females factor analysis\n")
  fa.parallel(hi_female %>% select(V1:V8)) # suggests 2 factors, but looks like 1 factor only
  hi_female_fa <- fa(hi_female %>% select(V1:V8)) # much better factor loading on V2 than in males
print(hi_female_fa)
  
## Descriptive statistics ####

# AUT
summary(aut_orig %>% select(V1:V7) %>% rowSums(na.rm = TRUE))
summary(aut_orig %>% select(V1:V7)) # singular cases of NAs
psych::alpha(aut_orig %>% select(V1:V7))
psych::omega(aut_orig %>% select(V1:V7),1) # McDonald´s omega 0.72, Cronbach alpha 0.69 # item 6 doesn´t add to internal consistency

# BMPN
summary(bmpn_orig %>% select(V1:V5) %>% rowSums(na.rm = TRUE))
summary(bmpn_orig %>% select(V1:V5))# singular cases of NAs
psych::alpha(bmpn_orig %>% select(V1:V5))
psych::omega(bmpn_orig %>% select(V1:V5),1) # McDonald´s omega 0.7, Cronbach alpha 0.69 

# DASS
summary(dass_orig %>% select(V1:V7) %>% rowSums(na.rm = TRUE))
summary(dass_orig %>% select(V1:V7)) # NAs around 1%
psych::alpha(dass_orig %>% select(V1:V7))
psych::omega(dass_orig %>% select(V1:V7),1) # McDonald´s omega 0.88, Cronbach alpha 0.87 # item 6 don´t add to internal consistency

# DEM
summary(dem_orig %>% select(V1:V5) %>% rowSums(na.rm = TRUE))
summary(dem_orig %>% select(V1:V5)) # no NAs
psych::alpha(dem_orig %>% select(V1:V5))
psych::omega(dem_orig %>% select(V1:V5),2) # McDonald´s omega 0.84, Cronbach alpha 0.75, two factor config. V1,V2,V3 and separately V4,V5

# DERS
summary(ders_orig %>% select(V1:V7) %>% rowSums(na.rm = TRUE))
summary(ders_orig %>% select(V1:V7)) # singular cases of NAs
psych::alpha(ders_orig %>% select(V1:V7))
psych::omega(ders_orig %>% select(V1:V7),2) # McDonald´s omega 0.85, Cronbach alpha 0.84

# HEIGHT INVENTORY
  # male
  summary(hi_male %>% select(V1:V7) %>% rowSums(na.rm = TRUE))
  summary(hi_male %>% select(V1:V7)) # NA´s around 1%, but V2 over 3%
  psych::alpha(hi_male %>% select(V1:V7))
  psych::omega(hi_male %>% select(V1:V7),1) # McDonald´s omega 0.89, Cronbach alpha 0.89, item V2 doesn´t add to internal consistency
  
  # female
  summary(hi_female %>% select(V1:V7) %>% rowSums(na.rm = TRUE))
  summary(hi_female %>% select(V1:V7)) # NA´s generally around 1%, but V1 at almost 6%
  psych::alpha(hi_female %>% select(V1:V7))
  psych::omega(hi_female %>% select(V1:V7),1) # McDonald´s omega 0.91, Cronbach alpha 0.91
ls()


fa.parallel(all_combined_bi[all_combined_bi$collection == 2 & all_combined_bi$dataset_id == "HI" & all_combined_bi$sex == "female",] %>% select(V1:V8))

# CONNECTED DATASETS ####
## Factor analysis ####

# AUT
cat("\nAUT factor analysis\n")
fa.parallel(all_combined_bi %>% 
              filter(dataset_id == "AUT") %>% 
              select(V1:V7))
aut_fa <- fa(all_combined_bi %>% 
               filter(dataset_id == "AUT") %>% 
               select(V1:V7))
print(aut_fa)
fa.diagram(aut_fa) # keeping it simple, I think we can consider it unidimensional scale with some noise

# BMPN
cat("\nBMPN factor analysis\n")
fa.parallel(all_combined_bi %>% 
              filter(dataset_id == "BMPN") %>% 
              select(V1:V5))
bmpn_fa <- fa(all_combined_bi %>% 
                filter(dataset_id == "BMPN") %>% 
                select(V1:V5),2)
print(bmpn_fa)
fa.diagram(bmpn_fa) # 2 factor scale with specific factor created by reversed variables

# DASS
cat("\nDASS factor analysis\n")
fa.parallel(all_combined_bi %>% 
              filter(dataset_id == "DASS") %>% 
              select(V1:V7))
dass_fa <- fa(all_combined_bi %>% 
                filter(dataset_id == "DASS") %>% 
                select(V1:V7))
print(dass_fa)
fa.diagram(dass_fa) # unidimensional scale, fa.parallel suggests 3 factors but that i think is not needed

# DEM
cat("\nDEM factor analysis\n")
fa.parallel(all_combined_bi %>% 
              filter(dataset_id == "DEM") %>% 
              select(V1:V5))
dem_fa <- fa(all_combined_bi %>% 
               filter(dataset_id == "DEM") %>% 
               select(V1:V5), 2, rotate = "oblimin")
print(dem_fa)
fa.diagram(dem_fa) # its ambivalent, shows that it should be 2 factor, but V4 doesn´t bind.... prolly like no factor at all

# DERS
cat("\nDERS factor analysis\n")
fa.parallel(all_combined_bi %>% 
              filter(dataset_id == "DERS") %>% 
              select(V1:V7))
ders_fa <- fa(all_combined_bi %>% 
                filter(dataset_id == "DERS") %>% 
                select(V1:V7),2)
print(ders_fa)
fa.diagram(ders_fa) # 2 factors, one created by 3 items aimed at self-control

# HEIGHT INVENTORY
# Sex Invariance testing

library(lavaan)
library(semTools)


df <- all_combined_bi %>% 
        filter(dataset_id == "HI", sex == "male" | sex == "female") %>% 
  mutate(sex = as.factor(sex))

model <- 'F1 =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8'

# standard CFA
cfanorm <- cfa(model, data = df, estimator = "ml") # problems with fit
summary(cfanorm, fit.measures=TRUE, standardized=TRUE)

###check fits

cfa.config <- cfa(model, data = df, estimator = "ml", group = "sex") # problems with fit
summary(cfa.config, fit.measures=TRUE, standardized=TRUE)

###metric invariance

cfa.metric <- cfa(model, data = df, estimator = "ml", group = "sex", group.equal = "loadings")
summary(cfa.metric, fit.measures=TRUE, standardized=TRUE)

compfit <- compareFit(cfa.config, cfa.metric) # we don´t have metric invariance
summary(compfit)

###scalar invariance

cfa.scalar <- cfa(model, data = df, estimator = "ml", group = "sex",
                  group.equal = c("loadings", "intercepts"))

compfit2 <- compareFit(cfa.metric, cfa.scalar) # we don´t have scalar invariance
summary(compfit2)

# Male
cat("\nHEIGHT INVENTORY - Males factor analysis\n")
fa.parallel(all_combined_bi %>% 
              filter(dataset_id == "HI", sex == "male") %>% 
              select(V1:V8))
hi_male_fa <- fa(all_combined_bi %>% 
                   filter(dataset_id == "HI", sex == "male") %>% 
                   select(V1:V8))
print(hi_male_fa)
fa.diagram(hi_male_fa) # unidimensional scale

# Female
cat("\nHEIGHT INVENTORY - Females factor analysis\n")
fa.parallel(all_combined_bi %>% 
              filter(dataset_id == "HI", sex == "female") %>% 
              select(V1:V8))
hi_female_fa <- fa(all_combined_bi %>% 
                     filter(dataset_id == "HI", sex == "female") %>% 
                     select(V1:V8))
print(hi_female_fa)
fa.diagram(hi_female_fa) # one dimensional scale


## Collection invariance testing ####

library(lavaan)
library(semTools)

### AUT  ####

df <- all_combined_bi %>% 
  filter(dataset_id == "AUT")

model <- 'F1 =~ V1 + V2 + V3 + V4 + V5 + V6 + V7'

# standard CFA
cfanorm <- cfa(model, data = df, estimator = "ml")
summary(cfanorm, fit.measures=TRUE, standardized=TRUE)

###check fits

cfa.config <- cfa(model, data = df, estimator = "ml", group = "collection") # worse fit
summary(cfa.config, fit.measures=TRUE, standardized=TRUE)

###metric invariance

cfa.metric <- cfa(model, data = df, estimator = "ml", group = "collection", group.equal = "loadings")
summary(cfa.metric, fit.measures=TRUE, standardized=TRUE)

compfit <- compareFit(cfa.config, cfa.metric) # bit greater difference in CFI, but still acceptable
summary(compfit)

###scalar invariance

cfa.scalar <- cfa(model, data = df, estimator = "ml", group = "collection",
                  group.equal = c("loadings", "intercepts"))

compfit2 <- compareFit(cfa.metric, cfa.scalar) # we have scalar invariance for collection
summary(compfit2)

### BMPN  ####

df <- all_combined_bi %>% 
  filter(dataset_id == "BMPN")

model <- 'F1 =~ V1 + V3
          F2 =~ V2 + V4 + V5' # trying two-factor model

# standard CFA
cfanorm <- cfa(model, data = df, estimator = "ml")
summary(cfanorm, fit.measures=TRUE, standardized=TRUE) # good 2 factor fit

###check fits

cfa.config <- cfa(model, data = df, estimator = "ml", group = "collection") # problems with fit 
summary(cfa.config, fit.measures=TRUE, standardized=TRUE) # almost perfect fit

###metric invariance

cfa.metric <- cfa(model, data = df, estimator = "ml", group = "collection", group.equal = "loadings")
summary(cfa.metric, fit.measures=TRUE, standardized=TRUE)

compfit <- compareFit(cfa.config, cfa.metric) 
summary(compfit) # we have metric invariance

###scalar invariance

cfa.scalar <- cfa(model, data = df, estimator = "ml", group = "collection",
                  group.equal = c("loadings", "intercepts"))

compfit2 <- compareFit(cfa.metric, cfa.scalar)
summary(compfit2) # we have scalar invariance for collection

### DASS  ####

df <- all_combined_bi %>% 
  filter(dataset_id == "DASS")

model <- 'F1 =~ V1 + V2 + V3 + V4 + V5 + V6 + V7'

# standard CFA
cfanorm <- cfa(model, data = df, estimator = "ml")
summary(cfanorm, fit.measures=TRUE, standardized=TRUE) # good fit

###check fits

cfa.config <- cfa(model, data = df, estimator = "ml", group = "collection") 
summary(cfa.config, fit.measures=TRUE, standardized=TRUE) # good fit

###metric invariance

cfa.metric <- cfa(model, data = df, estimator = "ml", group = "collection", group.equal = "loadings")
summary(cfa.metric, fit.measures=TRUE, standardized=TRUE)

compfit <- compareFit(cfa.config, cfa.metric) 
summary(compfit) # we have metric invariance

###scalar invariance

cfa.scalar <- cfa(model, data = df, estimator = "ml", group = "collection",
                  group.equal = c("loadings", "intercepts"))

compfit2 <- compareFit(cfa.metric, cfa.scalar)
summary(compfit2) # we have scalar invariance for collection

### DEM  ####

df <- all_combined_bi %>% 
  filter(dataset_id == "DEM")

model <- 'F1 =~ V1 + V2 + V3
          F2 =~ V4 + V5' # trying two-factor model

# standard CFA
cfanorm <- cfa(model, data = df, estimator = "ml")
summary(cfanorm, fit.measures=TRUE, standardized=TRUE) # good fit

###check fits

cfa.config <- cfa(model, data = df, estimator = "gls", group = "collection") 
summary(cfa.config, fit.measures=TRUE, standardized=TRUE) # beware of the estimator

###metric invariance

cfa.metric <- cfa(model, data = df, estimator = "gls", group = "collection", group.equal = "loadings")
summary(cfa.metric, fit.measures=TRUE, standardized=TRUE)

compfit <- compareFit(cfa.config, cfa.metric) 
summary(compfit) # we have metric invariance

###scalar invariance

cfa.scalar <- cfa(model, data = df, estimator = "gls", group = "collection",
                  group.equal = c("loadings", "intercepts"))

compfit2 <- compareFit(cfa.metric, cfa.scalar)
summary(compfit2) # we have scalar invariance for collection

### DERS  ####

df <- all_combined_bi %>% 
  filter(dataset_id == "DERS")

model <- 'F1 =~ V1 + V2 + V3 + V6 + V7
          F2 =~ V3 + V4 + V5' # items aimed at self-control separated

# standard CFA
cfanorm <- cfa(model, data = df, estimator = "ml")
summary(cfanorm, fit.measures=TRUE, standardized=TRUE) # good fit

###check fits

cfa.config <- cfa(model, data = df, estimator = "ml", group = "collection") 
summary(cfa.config, fit.measures=TRUE, standardized=TRUE) # good fit

###metric invariance

cfa.metric <- cfa(model, data = df, estimator = "ml", group = "collection", group.equal = "loadings")
summary(cfa.metric, fit.measures=TRUE, standardized=TRUE)

compfit <- compareFit(cfa.config, cfa.metric) 
summary(compfit) # we have metric invariance

###scalar invariance

cfa.scalar <- cfa(model, data = df, estimator = "ml", group = "collection",
                  group.equal = c("loadings", "intercepts"))

compfit2 <- compareFit(cfa.metric, cfa.scalar)
summary(compfit2) # we have scalar invariance for collection

### HI males  ####

df <- all_combined_bi %>% 
  filter(dataset_id == "HI", sex == "male")

model <- 'F1 =~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8' 

# standard CFA
cfanorm <- cfa(model, data = df, estimator = "ml")
summary(cfanorm, fit.measures=TRUE, standardized=TRUE) # good fit

###check fits

cfa.config <- cfa(model, data = df, estimator = "ml", group = "collection") 
summary(cfa.config, fit.measures=TRUE, standardized=TRUE) # good fit

###metric invariance

cfa.metric <- cfa(model, data = df, estimator = "ml", group = "collection", group.equal = "loadings")
summary(cfa.metric, fit.measures=TRUE, standardized=TRUE)

compfit <- compareFit(cfa.config, cfa.metric) 
summary(compfit) # we have metric invariance

###scalar invariance

cfa.scalar <- cfa(model, data = df, estimator = "ml", group = "collection",
                  group.equal = c("loadings", "intercepts"))

compfit2 <- compareFit(cfa.metric, cfa.scalar)
summary(compfit2) # we have scalar invariance for collection

### HI females  ####

df <- all_combined_bi %>% 
  filter(dataset_id == "HI", sex == "female")

model <- 'F1 =~ V1 + V2 + V3 + V4 + V5 + V6 + V7' # we have to remove V8 due to no positive response in in-house collection

# standard CFA
cfanorm <- cfa(model, data = df, estimator = "ml")
summary(cfanorm, fit.measures=TRUE, standardized=TRUE) # good fit

###check fits

cfa.config <- cfa(model, data = df, estimator = "ml", group = "collection") 
summary(cfa.config, fit.measures=TRUE, standardized=TRUE) # good fit

###metric invariance

cfa.metric <- cfa(model, data = df, estimator = "ml", group = "collection", group.equal = "loadings")
summary(cfa.metric, fit.measures=TRUE, standardized=TRUE)

compfit <- compareFit(cfa.config, cfa.metric) 
summary(compfit) # we have metric invariance

###scalar invariance

cfa.scalar <- cfa(model, data = df, estimator = "ml", group = "collection",
                  group.equal = c("loadings", "intercepts"))

compfit2 <- compareFit(cfa.metric, cfa.scalar)
summary(compfit2) # we have scalar invariance for collection

## Descriptive statistics ####

# Function for convenience

desc_stats <- function(df, vars) {
  cat("\nRow sums summary:\n")
  print(summary(rowSums(df %>% select(all_of(vars)), na.rm = TRUE)))
  cat("\nIndividual item summary:\n")
  print(summary(df %>% select(all_of(vars))))
  cat("\nCronbach's alpha:\n")
  print(psych::alpha(df %>% select(all_of(vars))))
  cat("\nMcDonald's omega:\n")
  print(psych::omega(df %>% select(all_of(vars)), nfactors = 1))
}

# AUT
desc_stats(all_combined_bi %>% filter(dataset_id == "AUT"), vars = paste0("V", 1:7)) # Alpha = 0.69, omega 0.71

# BMPN
cat("\nBMPN descriptive statistics\n")
bmpn_df <- all_combined_bi %>% filter(dataset_id == "BMPN")
print(summary(rowSums(bmpn_df %>% select(V1:V5), na.rm = TRUE)))
print(summary(bmpn_df %>% select(V1:V5)))
print(psych::alpha(bmpn_df %>% select(V1:V5))) 
print(psych::omega(bmpn_df %>% select(V1:V5), nfactors = 2)) # alpha 0.7, omega 0.75, hierarchical 0.54


# DASS
desc_stats(all_combined_bi %>% filter(dataset_id == "DASS"), vars = paste0("V", 1:7)) # people tend to disagree more alpha 0.82, omega 0.83

# DEM (two-factor)
cat("\nDEM descriptive statistics\n")
dem_df <- all_combined_bi %>% filter(dataset_id == "DEM")
print(summary(rowSums(dem_df %>% select(V1:V5), na.rm = TRUE)))
print(summary(dem_df %>% select(V1:V5)))
print(psych::alpha(dem_df %>% select(V1:V5))) # alpha 0.67, omega 0.78, hierarchical 0.22
print(psych::omega(dem_df %>% select(V1:V5), nfactors = 2))

# DERS (two-factor)
cat("\nDERS descriptive statistics\n")
ders_df <- all_combined_bi %>% filter(dataset_id == "DERS")
print(summary(rowSums(ders_df %>% select(V1:V7), na.rm = TRUE)))
print(summary(ders_df %>% select(V1:V7)))
print(psych::alpha(ders_df %>% select(V1:V7)))  # lots of disagreement, alpha 0.81, omega 0.87
print(psych::omega(ders_df %>% select(V1:V7), nfactors = 2))


# HEIGHT INVENTORY
# Male
cat("\nHEIGHT INVENTORY - Males descriptive statistics\n")
desc_stats(all_combined_bi %>% filter(dataset_id == "HI", sex == "male"), vars = paste0("V", 1:8)) # alpha 0.86, omega 0.86

# Female
cat("\nHEIGHT INVENTORY - Females descriptive statistics\n")
desc_stats(all_combined_bi %>% filter(dataset_id == "HI", sex == "female"), vars = paste0("V", 1:8)) # alpha 0.86, omega 0.86

install.packages("subscore")
subscore::Yen.Q3(subscore::scored.data,  "Rasch")
?subscore::Yen.Q3
subscore::scored.data

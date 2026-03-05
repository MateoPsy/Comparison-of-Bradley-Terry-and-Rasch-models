############################################
#### Pairwise comparisons analysis 2025 ####
####           Descriptives             ####
############################################

rm(list=ls())
setwd("~/Desktop/Pairwise comparisons_study")

### packages ####
library(tidyverse)
library(ggplot2)
library(dplyr)

### loading datasets ####

# data collection 2 - outsourced collection

height_orig <- read_rds("Data/Outsourced data/iga.rds")
czg_orig <- read.csv("Data/Outsourced data/CzG.csv")
qz_orig <- read.csv("Data/Outsourced data/QZ.csv")
aut_orig <- read.delim("Data/Outsourced data/AS_1PL.csv", sep = ";")
dem_orig <- read.csv("Data/Outsourced data/dem.csv")

### Descriptives ####
view(height_orig)
table(height_orig$sex[is.na(height_orig$q1_h02)])
table(height_orig$sex)
HI <- height_orig %>%
  select(q1_h01, q1_h02, q1_h03, q1_h04, q1_h05, q1_h06, q1_h07, q1_h08, sex, sr_age) %>%
  filter(rowSums(is.na(across(c(q1_h01, q1_h02, q1_h03, q1_h04, q1_h05, q1_h06, q1_h07, q1_h08)))) <= 2) %>% 
  filter(!is.na(sex))

DERS <- czg_orig %>%
  select(ders07, ders08, ders09, ders10, ders11, ders12, ders15) %>%
  filter(rowSums(is.na(across(c(ders07, ders08, ders09, ders10, ders11, ders12, ders15)))) <= 2)

DASS <- qz_orig %>%
  select(DASS_1s, DASS_6s, DASS_8s, DASS_11s, DASS_12s, DASS_14s, DASS_18s) %>%
  filter(rowSums(is.na(across(c(DASS_1s, DASS_6s, DASS_8s, DASS_11s, DASS_12s, DASS_14s, DASS_18s)))) <= 2)
  
AUT <- aut_orig %>%
  filter(rowSums(is.na(across(c(a1_2, a2_2, a3_2, a4_2, a5_2, a6_2, a7_2)))) <= 2)

BMPN <- czg_orig %>%
  select(bmpn_5, bmpn_10, bmpn_11, bmpn_16, bmpn_18) %>%
  filter(rowSums(is.na(across(c(bmpn_5, bmpn_10, bmpn_11, bmpn_16, bmpn_18)))) <= 2)

DEM <- dem_orig %>%
  select(dem4_s, dem5_s, dem6_s, dem7_s, dem8_s, pohlavi, vek) %>%
  filter(rowSums(is.na(across(c(dem4_s, dem5_s, dem6_s, dem7_s, dem8_s)))) <= 2)

NROW(DEM)
summary(DEM)
sd(DEM$vek, na.rm = T)
stats::sd(HI$sr_age, na.rm = T)
DEM$pohlavi <- factor(DEM$pohlavi)


# PURPOSE -------------------------------------------------------------------------------------------------------------------
# 1) To automate processing trawl data specimen tables and catch summaries from Collaborative Pot 
#    Sampling II (CPS2) 2023 for BBRKC
# 2) To run error checks on processed specimen and catch summaries
# 3) To calculate and map cpue by haul and mat/sex category for BBRKC

# Author: Shannon Hennessey and Emily Ryznar, NOAA-AFSC

# INSTALL PACKAGES ----------------------------------------------------------------------------------------------------------
#install.packages(c("tidyverse", "gsubfn", "terra", "rgdal", "colorRamps", "sf", "viridis", "grid", "shadowtext",
#                     "ggnewscale"))

# LOAD PACKAGES -------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(gsubfn)
library(terra)
library(rgdal)
library(colorRamps)
library(sf)
library(viridis)
library(grid)
library(shadowtext)
library(gstat)
library(ggnewscale)

# LOAD DATA -----------------------------------------------------------------------------------------------------------------

# Load summary catch and specimen tables
catch <- list.files("./Data/Trawl Data/Catch - FTP/") %>%
  purrr::map_df(~read.csv(paste0("./Data/Trawl Data/Catch - FTP/", .x))) 

specimen <- list.files("./Data/Trawl Data/Specimen - FTP/") %>%
  purrr::map_df(~read.csv(paste0("./Data/Trawl Data/Specimen - FTP/", .x))%>% mutate(STATION = paste0("X", STATION))) 

# Load raw data for processing below   
raw_sample <- list.files("./Data/Trawl Data/Raw Data - FTP/", pattern = "_SAMPLE_0") %>% # RECORDS of SAMPLE INFO
  purrr::map_df(~read.csv(paste0("./Data/Trawl Data/Raw Data - FTP/", .x))) #E.G. SEX, SPECIES

raw_sample_values <- list.files("./Data/Trawl Data/Raw Data - FTP/", pattern = "_SAMPLE_VALUES") %>% #RECORDS OF # TOSSED
  purrr::map_df(~read.csv(paste0("./Data/Trawl Data/Raw Data - FTP/", .x))) 
  # mutate(TOSSED = ifelse(is.na(COUNT) == FALSE, COUNT,0)) %>%
  # group_by(HAUL_ID, CATCH_SAMPLE_ID) %>%
  # dplyr::reframe(TOSSED = sum(TOSSED)) 

raw_specimen <- list.files("./Data/Trawl Data/Raw Data - FTP/", pattern = "_SPECIMEN_0") %>% 
  purrr::map_df(~read.csv(paste0("./Data/Trawl Data/Raw Data - FTP/", .x))) %>%
  dplyr::select(HAUL_ID, SPECIMEN_ID, CATCH_SAMPLE_ID, SPECIES_CODE)

raw_specimen_bio<- list.files("./Data/Trawl Data/Raw Data - FTP/", pattern = "_SPECIMEN_BIOMETRICS") %>% 
  purrr::map_df(~read.csv(paste0("./Data/Trawl Data/Raw Data - FTP/", .x))) %>%
  right_join(., catch %>% dplyr::select(HAUL, HAUL_ID, RECORDING_DEVICE)) %>%
  distinct()

# Read in haul table

################################################################################
## loop functions to generate indicator datasets by indicator
## NCEE EPA 
## Last edited: 11/28/2023
################################################################################

# Load packages

# Data cleaning packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  modelsummary, # regression table generation
  stringr, # string manipulation
  MASS , #For regressions and modeling
  dplyr, #data manipulation
  readxl, #read excel files
  janitor #cleaning names
)

################################################################################
##Set directories
################################################################################

#Tina
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

#WA
my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()

################################################################################
## Create indicator datasets for different boundaries by indicator
################################################################################

# Call different boundary types

boundary <- c("epic", "usgs", "zc", "county")

# Load base indicator violations data

HB_vio <- read.csv("Data/health_violations2015.csv") %>%
  clean_names()

# Create loop function to call each boundary and combine with indicator

for (i in boundary) {
  
  pwsid_dem <- read.csv(paste0("Data/", i, "_boundaries/", i, "_dems.csv")) %>%
    clean_names() %>%
    dplyr::select(-contains("p_")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "pop_sum" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem, HB_vio, by = "pwsid", relationship = "many-to-many") 
  
  cbg_vio <- cbg_vio %>%
    mutate(total_violations = coalesce(total_violations, 0),
           diff_days = coalesce(diff_days, 0)) 

  saveRDS(cbg_vio, file = paste0("Data/combined/HB_vio_", i, ".rds"))
  
}

rm(HB_vio)


################################################################################
## Lead violations
################################################################################

# Load lead violations

lcr_vio <- readRDS("Data/lcr_violations.rds") %>%
  clean_names

for (i in boundary) {
  
  pwsid_dem <- read.csv(paste0("Data/", i, "_boundaries/", i, "_dems.csv")) %>%
    clean_names() %>%
    dplyr::select(-contains("p_")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "pop_sum" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem, lcr_vio, by = "pwsid", relationship = "many-to-many") %>%
    mutate(pb_vio_count = coalesce(pb_vio_count, 0),
           cu_vio_count = coalesce(cu_vio_count, 0),
           avg_cu_level = coalesce(avg_cu_level, 0),
           avg_pb_level = coalesce(avg_pb_level, 0),
           max_cu_level = coalesce(max_cu_level, 0),
           max_pb_level = coalesce(max_pb_level, 0))

  
  saveRDS(cbg_vio, file = paste0("Data/combined/lcr_vio_", i, ".rds"))
  
}

rm(lcr_vio)

################################################################################
## PFAS concentrations
################################################################################

# Load pfas violations data 

pfas_vio <- read.csv("Data/indicators_pfas.csv") %>%
  clean_names()

for (i in boundary) {
  
  pwsid_dem <- read.csv(paste0("Data/", i, "_boundaries/", i, "_dems.csv")) %>%
    clean_names() %>%
    dplyr::select(-contains("p_")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "pop_sum" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem,pfas_vio, by = "pwsid", relationship = "many-to-many") %>%
    mutate(concentration_sum = coalesce(concentration_sum, 0),
           total_samples = coalesce(total_samples, 0),
           detection = coalesce(detection, 0),
           pfas_count = coalesce(pfas_count, 0),
           max = coalesce(max, 0)
    )
  
  saveRDS(cbg_vio, file = paste0("Data/combined/pfas_vio_", i, ".rds"))
  
}

rm(pfas_vio)

################################################################################
## Cumulative DBP
################################################################################

# Load DBP concentrations data

dbp_vio <- read.csv("Data/indicator_dbp.csv") %>%
  clean_names()

for (i in boundary) {
  
  pwsid_dem <- read.csv(paste0("Data/", i, "_boundaries/", i, "_dems.csv")) %>%
    clean_names() %>%
    dplyr::select(-contains("p_")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "pop_sum" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem, dbp_vio, by = "pwsid", relationship = "many-to-many") %>%
    mutate(combined_dbp = coalesce(combined_dbp, 0))
  
  saveRDS(cbg_vio, file = paste0("Data/combined/dbp_vio_", i, ".rds"))
  
}

rm(dbp_vio)

################################################################################
## Cumulative DBP
################################################################################

# Load DBP concentrations data

tcr_vio <- read.csv("Data/indicator_tcr.csv") %>%
  clean_names()

for (i in boundary) {
  
  pwsid_dem <- read.csv(paste0("Data/", i, "_boundaries/", i, "_dems.csv")) %>%
    clean_names() %>%
    dplyr::select(-contains("p_")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "pop_sum" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem, tcr_vio, by = "pwsid", relationship = "many-to-many") %>%
    mutate(detection_share = coalesce(detection_share, 0))
  
  saveRDS(cbg_vio, file = paste0("Data/combined/tcr_vio_", i, ".rds"))
  
}

rm(tcr_vio)

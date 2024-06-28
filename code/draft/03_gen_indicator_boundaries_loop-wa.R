################################################################################
## Loop functions to generate indicator datasets by indicator
## NCEE EPA 
## Last edited: 6/7/2024
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
#my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

#WA
my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()

################################################################################
## Create indicator datasets for different boundaries by indicator
################################################################################

# Call different boundary types

boundary <- c("epic", "hm")

# boundary <- "hm"

# Load base indicator violations data

HB_vio <- read.csv("ej_service_areas/data/indicators/health_violations2015.csv") %>%
  clean_names()


# Create loop function to call each boundary and combine with indicator

for (i in boundary) {
  
  # Load the demographic datasets by boundary
  pwsid_dem <- read.csv(paste0("data/demographics/", i, "_dems_area.csv")) %>%
    clean_names() %>%
    dplyr::select(-ends_with("_us"), -ends_with("state")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "population_served_count" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  
  # Left join the boundary to violations data to get cbg-level data
  cbg_vio <- left_join(pwsid_dem, HB_vio, by = "pwsid", relationship = "many-to-many") 
  
  #Perform indicator-specific operations: fill in zeroes for HB violations 
  cbg_vio <- cbg_vio %>%
    mutate(total_violations = coalesce(total_violations, 0))
  
  # Find the Duplicated Columns
  duplicated_columns <- duplicated(t(cbg_vio))
  
  # Show the Names of the Duplicated Columns
  colnames(cbg_vio[duplicated_columns])
  
  # Remove the Duplicated Columns
  cbg_vio <- cbg_vio[!duplicated_columns]

  #Save the dataset
  saveRDS(cbg_vio, file = paste0("data/combined/HB_vio_", i, "_area.rds"))
  
}

rm(HB_vio)


################################################################################
## Lead violations
################################################################################

# Load lead violations

lcr_vio <- readRDS("ej_service_areas/data/indicators/lcr_violations.rds") %>%
  clean_names

for (i in boundary) {
  
  pwsid_dem <- read.csv(paste0("data/demographics/", i, "_dems_area.csv")) %>%
    clean_names() %>%
    dplyr::select(-ends_with("_us"), -ends_with("state")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "population_served_count" ~ "pop_served",
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
  
  
  # Find the Duplicated Columns
  duplicated_columns <- duplicated(t(cbg_vio))
  
  # Show the Names of the Duplicated Columns
  colnames(cbg_vio[duplicated_columns])
  
  # Remove the Duplicated Columns
  cbg_vio <- cbg_vio[!duplicated_columns]
  
  #Save the dataset
  saveRDS(cbg_vio, file = paste0("data/combined/lcr_vio_", i, "_area.rds"))
  
}

rm(lcr_vio)

################################################################################
## PFAS concentrations
################################################################################

# Load pfas violations data 

pfas_vio <- read.csv("ej_service_areas/data/indicators/indicators_pfas.csv") %>%
  clean_names()

for (i in boundary) {
  
  #i = "county"
  
  pwsid_dem <- read.csv(paste0("Data/demographics/", i, "_dems_area.csv")) %>%
    clean_names() %>%
    dplyr::select(-ends_with("_us"), -ends_with("state")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "population_served_count" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem,pfas_vio, by = "pwsid", relationship = "many-to-many")
  
  
  # Find the Duplicated Columns
  duplicated_columns <- duplicated(t(cbg_vio))
  
  # Show the Names of the Duplicated Columns
  colnames(cbg_vio[duplicated_columns])
  
  # Remove the Duplicated Columns
  cbg_vio <- cbg_vio[!duplicated_columns]
  
  #Save the dataset
  saveRDS(cbg_vio, file = paste0("data/combined/pfas_vio_", i, "_area.rds"))
  
}

rm(pfas_vio)

################################################################################
## Cumulative DBP
################################################################################

# Load DBP concentrations data

dbp_vio <- read.csv("ej_service_areas/data/indicators/indicator_dbp_v2.csv") %>%
  clean_names()

for (i in boundary) {
  
  pwsid_dem <- read.csv(paste0("Data/demographics/", i, "_dems_area.csv")) %>%
    clean_names() %>%
    dplyr::select(-ends_with("_us"), -ends_with("state")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "population_served_count" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem, dbp_vio, by = "pwsid", relationship = "many-to-many") 
  
  # Find the Duplicated Columns
  duplicated_columns <- duplicated(t(cbg_vio))
  
  # Show the Names of the Duplicated Columns
  colnames(cbg_vio[duplicated_columns])
  
  # Remove the Duplicated Columns
  cbg_vio <- cbg_vio[!duplicated_columns]
  
  #Save the dataset
  saveRDS(cbg_vio, file = paste0("data/combined/dbp_vio_", i, "_area_v2.rds"))
  
}

rm(dbp_vio)

################################################################################
## TCR detection shares
################################################################################

# Load tcr concentrations data

tcr_vio <- read.csv("ej_service_areas/data/indicators/indicator_tcr.csv") %>%
  clean_names()

for (i in boundary) {
  
  pwsid_dem <- read.csv(paste0("Data/demographics/", i, "_dems_area.csv")) %>%
    clean_names() %>%
    dplyr::select(-ends_with("_us"), -ends_with("state")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "population_served_count" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem, tcr_vio, by = "pwsid", relationship = "many-to-many") 
  
  
  # Find the Duplicated Columns
  duplicated_columns <- duplicated(t(cbg_vio))
  
  # Show the Names of the Duplicated Columns
  colnames(cbg_vio[duplicated_columns])
  
  # Remove the Duplicated Columns
  cbg_vio <- cbg_vio[!duplicated_columns]
  
  #Save the dataset
  saveRDS(cbg_vio, file = paste0("data/combined/tcr_vio_", i, "_area.rds"))
  
}

rm(tcr_vio)

################################################################################
## Arsenic concentrations
################################################################################

# Load ars concentrations data

ars_vio <- read_csv("ej_service_areas/data/indicators/indicator_arsenic.csv")%>%
  clean_names()

for (i in boundary) {  
  pwsid_dem <- read.csv(paste0("data/demographics/", i, "_dems_area.csv")) %>%
    clean_names() %>%
    dplyr::select(-ends_with("_us"), -ends_with("state")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "population_served_count" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem, ars_vio, by = "pwsid", relationship = "many-to-many") 
  
  
  # Find the Duplicated Columns
  duplicated_columns <- duplicated(t(cbg_vio))
  
  # Show the Names of the Duplicated Columns
  colnames(cbg_vio[duplicated_columns])
  
  # Remove the Duplicated Columns
  cbg_vio <- cbg_vio[!duplicated_columns]
  
  #Save the dataset
  saveRDS(cbg_vio, file = paste0("data/combined/arsenic_vio_", i, "_area.rds"))
  
}

rm(ars_vio)

################################################################################
## Nitrate concentrations
################################################################################

# Load nitrate concentrations data

nitrate_vio <- read.csv("ej_service_areas/data/indicators/indicator_nitrate.csv") %>%
  clean_names()

for (i in boundary) {    
  pwsid_dem <- read.csv(paste0("data/demographics/", i, "_dems_area.csv")) %>%
    clean_names() %>%
    dplyr::select(-ends_with("_us"), -ends_with("state")) %>%
    rename_with(
      ~ case_when(
        . == "lowincpct" ~ "lowinc",
        . == "population_served_count" ~ "pop_served",
        . == "id" ~ "ID",
        TRUE ~ . 
      )
    )
  
  cbg_vio <- left_join(pwsid_dem, nitrate_vio, by = "pwsid", relationship = "many-to-many") 
  
  
  # Find the Duplicated Columns
  duplicated_columns <- duplicated(t(cbg_vio))
  
  # Show the Names of the Duplicated Columns
  colnames(cbg_vio[duplicated_columns])
  
  # Remove the Duplicated Columns
  cbg_vio <- cbg_vio[!duplicated_columns]
  
  #Save the dataset
  saveRDS(cbg_vio, file = paste0("data/combined/nitrate_vio_", i, "_area.rds"))

}

rm(nitrate_vio)



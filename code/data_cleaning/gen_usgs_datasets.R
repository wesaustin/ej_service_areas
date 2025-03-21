################################################################################
##Generate PWSID level datasets
##NCEE EPA 
## Last edited: 9/8/2023
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
## Load data
################################################################################

# USGS boundary data 

pwsid_dem <- read_excel("Data/usgs_boundaries/usgs_dems.xlsx") %>%
  clean_names()

pwsid_dem <- pwsid_dem %>% 
  rename(lowinc = lowincpct) %>%
  rename(pop_served = pop_sum) %>%
  dplyr::select(-starts_with("p_")) 

################################################################################
##Health violations
################################################################################

HB_vio <- read.csv("Data/health_violations2015.csv")

HB_vio <- HB_vio %>%
  rename(pwsid = PWSID)

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

HB_vio <- left_join(pwsid_dem, HB_vio, by = "pwsid", relationship = "many-to-many")

HB_vio$total_violations[is.na(HB_vio$total_violations)] <- 0
HB_vio$diff_days[is.na(HB_vio$diff_days)] <- 0

write_rds(HB_vio, "Data/combined/HB_vio_usgs.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(HB_vio) #remove files if unnecessary

################################################################################
##LCR
################################################################################
# Load lcr violations data and sb_dems_v3 data (containing demographic / shapefile info)

lcr_vio <- readRDS("Data/lcr_violations.rds")

lcr_vio <- lcr_vio %>%
  rename(pwsid = PWSID)

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

lcr_vio <- left_join(pwsid_dem, lcr_vio, by = "pwsid", relationship = "many-to-many")

lcr_vio$pb_vio_count[is.na(lcr_vio$pb_vio_count)] <- 0
lcr_vio$cu_vio_count[is.na(lcr_vio$cu_vio_count)] <- 0
lcr_vio$avg_cu_level[is.na(lcr_vio$avg_cu_level)] <- 0
lcr_vio$avg_pb_level[is.na(lcr_vio$avg_pb_level)] <- 0
lcr_vio$max_cu_level[is.na(lcr_vio$max_cu_level)] <- 0
lcr_vio$max_pb_level[is.na(lcr_vio$max_pb_level)] <- 0

write_rds(lcr_vio, "Data/combined/lcr_vio_usgs.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(lcr_vio) #remove files if unnecessary

################################################################################
##PFAS
################################################################################
# Load pfas violations data and sb_dems_v3 data (containing demographic / shapefile info)

pfas_vio <- read.csv("Data/indicators_pfas.csv")

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

pfas_vio <- left_join(pwsid_dem, pfas_vio, by = "pwsid", relationship = "many-to-many")
pfas_vio$detection[is.na(pfas_vio$detection)] <- 0

write_rds(pfas_vio, "Data/combined/pfas_vio_usgs.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(pfas_vio) #remove files if unnecessary


################################################################################
##DBP
################################################################################

# Load dbp violations data and sb_dems_v3 data (containing demographic / shapefile info)

dbp_vio <- read.csv("Data/indicator_dbp.csv")

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

dbp_vio <- left_join(pwsid_dem, dbp_vio, by = "pwsid", relationship = "many-to-many")

write_rds(dbp_vio, "Data/combined/dbp_vio_usgs.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(dbp_vio) #remove files if unnecessary

################################################################################
##TCR 
################################################################################
tcr_vio <- read.csv("Data/indicator_tcr.csv")

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

tcr_vio <- left_join(pwsid_dem, tcr_vio, by = "pwsid", relationship = "many-to-many")

write_rds(tcr_vio, "Data/combined/tcr_vio_usgs.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(tcr_vio, pwsid_dem)



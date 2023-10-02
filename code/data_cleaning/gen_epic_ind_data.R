
################################################################################
# Generate EPIC indicator boundary datasets
# National Center for Environmental Economics
# Latest update: 10/2/23
################################################################################

################################################################################
## Load packages: 
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  sf, # vector data operations
  raster, # raster data operations
  exactextractr, # fast raster data extraction for polygons
  maps, # to get county boundary data
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  tmap, # for map creation
  modelsummary, # regression table generation
  future.apply, # parallel computation
  cowplot, # for bivariate mapping
  rgdal, # required for cdlTools
  prism, # download PRISM data
  stringr, # string manipulation
  magrittr,
  tidycensus,
  mapview,
  patchwork #for combining plots
)

################################################################################
##Set directories
################################################################################

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

getwd()
setwd(paste0(my_path))
getwd()

################################################################################
## Load Data 
################################################################################

#Load epic boundaries: sb_dems_area_v3 data (containing demographic / shapefile info)

pwsid_cbg <- read.csv("Data/sb_dems_area_v3.csv")

################################################################################
## Health-based violations
################################################################################

HB_vio <- read.csv("Data/health_violations2015.csv")

# Rename and convert CBG ID to string with leading 0s if necessary  
pwsid_cbg <- pwsid_cbg %>%
  rename(PWSID = pwsid)

# Left join the full PWSID dataset with the health violations data

cbg_HB_vio <- left_join(pwsid_cbg, HB_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

rm(HB_vio, pwsid_cbg) #remove files if unnecessary

# For tracts with PWSIDs, and null values for the number of violations, replace nulls 
# with zeros

cbg_HB_vio$total_violations[is.na(cbg_HB_vio$total_violations)] <- 0
cbg_HB_vio$diff_days[is.na(cbg_HB_vio$diff_days)] <- 0

write_rds(cbg_HB_vio, "Data/cbg_HB_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

# For tract with more than one PWSID, find average number of violations at the CBG level
# then collapse to include only 1 observation per census tract level (12-digit "ID") 

rm(HB_vio, cbg_HB_vio)

################################################################################
## Lead Copper Rule violations
################################################################################

##Load health violations data and sb_dems_v3 data (containing demographic / shapefile info)

lcr_vio <- readRDS("Data/lcr_violations.rds")

##Left join the full PWSID dataset with the lead content violations data

cbg_lcr_vio <- left_join(pwsid_cbg, lcr_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

##For tracts with PWSIDs, and null values for the number of violations, replace nulls 
##with zeros

cbg_lcr_vio$pb_vio_count[is.na(cbg_lcr_vio$pb_vio_count)] <- 0
cbg_lcr_vio$cu_vio_count[is.na(cbg_lcr_vio$cu_vio_count)] <- 0
cbg_lcr_vio$avg_cu_level[is.na(cbg_lcr_vio$avg_cu_level)] <- 0
cbg_lcr_vio$avg_pb_level[is.na(cbg_lcr_vio$avg_pb_level)] <- 0
cbg_lcr_vio$max_cu_level[is.na(cbg_lcr_vio$max_cu_level)] <- 0
cbg_lcr_vio$max_pb_level[is.na(cbg_lcr_vio$max_pb_level)] <- 0

write_rds(cbg_lcr_vio, "Data/combined/cbg_lcr_vio_combined.rds")

rm(lcr_vio, cbg_lcr_vio)

################################################################################
## Load Data 
################################################################################

# Load pfas violations data and sb_dems_v3 data (containing demographic / shapefile info)

pfas_vio <- read.csv("Data/indicators_pfas.csv")

# Match case for either dataset (if inconsistent)
pfas_vio <- pfas_vio %>%
  rename(PWSID = pwsid)

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

cbg_pfas_vio <- left_join(pwsid_cbg, pfas_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

rm(pfas_vio, pwsid_cbg) #remove files if unnecessary

# For tracts with PWSIDs, replace null values with zeroes

cbg_pfas_vio$concentration_sum[is.na(cbg_pfas_vio$concentration_sum)] <- 0
cbg_pfas_vio$total_samples[is.na(cbg_pfas_vio$total_samples)] <- 0
cbg_pfas_vio$detection[is.na(cbg_pfas_vio$detection)] <- 0
cbg_pfas_vio$pfas_count[is.na(cbg_pfas_vio$pfas_count)] <- 0
cbg_pfas_vio$max[is.na(cbg_pfas_vio$max)] <- 0

write_rds(cbg_pfas_vio, "Data/cbg_pfas_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(cbg_pfas_vio, pfas_vio)

################################################################################
## DBP violations Data 
################################################################################

# Load dbp violations data and sb_dems_v3 data (containing demographic / shapefile info)

dbp_vio <- read.csv("Data/indicator_dbp.csv")

pwsid_cbg <- read.csv("Data/sb_dems_area_v3.csv")

# Match case for either dataset (if inconsistent)
dbp_vio <- dbp_vio %>%
  rename(PWSID = pwsid)

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

cbg_dbp_vio <- left_join(pwsid_cbg, dbp_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

rm(dbp_vio, pwsid_cbg) #remove files if unnecessary

# For tracts with PWSIDs, replace null values with zeroes

cbg_dbp_vio$combined_dbp[is.na(cbg_dbp_vio$combined_dbp)] <- 0

write_rds(cbg_dbp_vio, "Data/cbg_dbp_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(cbg_dbp_vio, dbp_vio)

################################################################################
## TCR violations data
################################################################################

# Load tcr violations data and sb_dems_v3 data (containing demographic / shapefile info)

tcr_vio <- read.csv("Data/indicator_tcr.csv")

pwsid_cbg <- read.csv("Data/sb_dems_area_v3.csv")

# Match case for either dataset (if inconsistent)
tcr_vio <- tcr_vio %>%
  rename(PWSID = pwsid)

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

cbg_tcr_vio <- left_join(pwsid_cbg, tcr_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

rm(tcr_vio, pwsid_cbg) #remove files if unnecessary

# For tracts with PWSIDs, replace null values with zeroes

cbg_tcr_vio$detection_share[is.na(cbg_tcr_vio$detection_share)] <- 0

write_rds(cbg_tcr_vio, "Data/cbg_tcr_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(cbg_tcr_vio, tcr_vio)


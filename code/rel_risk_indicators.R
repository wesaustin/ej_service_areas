

################################################################################
# Demographic Indices for SDWA indicators
# National Center for Environmental Economics
# Last edited: 8/8/25
################################################################################


# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  modelsummary, # regression table generation
  stringr, # string manipulation
  magrittr,
  tidycensus, #Census data
  MASS #For regressions and modeling
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

# Dataset on health-based violations (since 2015, national) 

HB_vio <- read.csv("Data/health_violations2015.csv")

pwsid_cbg <- read.csv("Data/sb_dems_area_v3.csv")

# Rename and convert CBG ID to string with leading 0s if necessary  
pwsid_cbg <- pwsid_cbg %>%
  rename(PWSID = pwsid)

# Left join the full PWSID dataset with the health violations data

HB_vio <- left_join(pwsid_cbg, HB_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

rm(pwsid_cbg) #remove files if unnecessary

# For tracts with PWSIDs, and null values for the number of violations, replace nulls 
# with zeros

HB_vio$total_violations[is.na(HB_vio$total_violations)] <- 0
HB_vio$diff_days[is.na(HB_vio$diff_days)] <- 0


################################################################################
## Data manipulation : Race indicator
################################################################################

## Relative risk of POC and non-hispanic whites

HB_vio <- HB_vio %>%
  mutate(POC_vio = sum(MINORPCT*population_served_count*total_violations))

POC_risk <- (HB_vio$POC_vio/sum(HB_vio$population_served_count))

POC_risk

# 1.215875 

HB_vio <- HB_vio %>%
  mutate(WHITEPCT = (1-MINORPCT)) %>%
  mutate(WHI_vio = sum(WHITEPCT*population_served_count*total_violations))

WHI_risk <- (HB_vio$WHI_vio/sum(HB_vio$population_served_count))

WHI_risk

# 0.7718056 

rel_risk_race <- POC_risk / WHI_risk

rel_risk_race

# 1.575364


################################################################################
## Data manipulation: Income indicator
################################################################################

## Relative risk of individuals above or below 2X the poverty limit

HB_vio <- HB_vio %>%
  mutate(POV_vio = sum(LOWINCPCT*population_served_count*total_violations))

POV_risk <- (HB_vio$POV_vio/sum(HB_vio$population_served_count))

POV_risk

# 0.7162929 

HB_vio <- HB_vio %>%
  mutate(HIINC = (1-LOWINCPCT)) %>%
  mutate(HIINC_vio = sum(HIINC*population_served_count*total_violations))

HIINC_risk <- (HB_vio$HIINC_vio/sum(HB_vio$population_served_count))

HIINC_risk

# 1.271388

rel_risk_inc <- POV_risk / HIINC_risk

rel_risk_inc

# 0.5633946


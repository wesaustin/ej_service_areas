

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
## Health violations
################################################################################

# Dataset on health-based violations (since 2015, national) 

HB_vio <- read_rds("Data/cbg_HB_vio_combined.rds")

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

# Clear memory
rm(cbg_HB_vio)

################################################################################
## Load Data 
## LCR violations 
################################################################################

LCR_vio <- read_rds("Data/cbg_LCR_vio_comb.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

## Relative risk of POC and non-hispanic whites

LCR_vio <- LCR_vio %>%
  mutate(POC_vio = sum(MINORPCT*population_served_count*total_violations))

POC_risk <- (LCR_vio$POC_vio/sum(LCR_vio$population_served_count))

POC_risk

# 1.130441  

LCR_vio <- LCR_vio %>%
  mutate(WHITEPCT = (1-MINORPCT)) %>%
  mutate(WHI_vio = sum(WHITEPCT*population_served_count*total_violations))

WHI_risk <- (LCR_vio$WHI_vio/sum(LCR_vio$population_served_count))

WHI_risk

# 0.7051423  

rel_risk_race <- POC_risk / WHI_risk

rel_risk_race

# 1.603139 


################################################################################
## Data manipulation: Income indicator
################################################################################

## Relative risk of individuals above or below 2X the poverty limit

LCR_vio <- LCR_vio %>%
  mutate(POV_vio = sum(LOWINCPCT*population_served_count*total_violations))

POV_risk <- (LCR_vio$POV_vio/sum(LCR_vio$population_served_count))

POV_risk

# 0.6262008  

LCR_vio <- LCR_vio %>%
  mutate(HIINC = (1-LOWINCPCT)) %>%
  mutate(HIINC_vio = sum(HIINC*population_served_count*total_violations))

HIINC_risk <- (LCR_vio$HIINC_vio/sum(LCR_vio$population_served_count))

HIINC_risk

# 1.209383 

rel_risk_inc <- POV_risk / HIINC_risk

rel_risk_inc

# 0.5177855 

# Clear memory
rm(cbg_LCR_vio)

################################################################################
## Load Data 
## PFAS violations
################################################################################

pfas_vio <- read_rds("Data/cbg_pfas_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

pfas_vio <- pfas_vio %>%
  mutate(POC_vio = sum(MINORPCT*population_served_count*concentration_sum))

POC_risk <- (pfas_vio$POC_vio/sum(pfas_vio$population_served_count))

POC_risk

# 6.276714  

pfas_vio <- pfas_vio %>%
  mutate(WHITEPCT = (1-MINORPCT)) %>%
  mutate(WHI_vio = sum(WHITEPCT*population_served_count*concentration_sum))

WHI_risk <- (pfas_vio$WHI_vio/sum(pfas_vio$population_served_count))

WHI_risk

# 4.738303 

rel_risk_race <- POC_risk / WHI_risk

rel_risk_race

# 1.324676


################################################################################
## Data manipulation: Income indicator
################################################################################

## Relative risk of individuals above or below 2X the poverty limit

pfas_vio <- pfas_vio %>%
  mutate(POV_vio = sum(LOWINCPCT*population_served_count*concentration_sum))

POV_risk <- (pfas_vio$POV_vio/sum(pfas_vio$population_served_count))

POV_risk

# 3.355327 

pfas_vio <- pfas_vio %>%
  mutate(HIINC = (1-LOWINCPCT)) %>%
  mutate(HIINC_vio = sum(HIINC*population_served_count*concentration_sum))

HIINC_risk <- (pfas_vio$HIINC_vio/sum(pfas_vio$population_served_count))

HIINC_risk

# 7.65969

rel_risk_inc <- POV_risk / HIINC_risk

rel_risk_inc

# 0.4380499

# Clear memory
rm(pfas_vio)



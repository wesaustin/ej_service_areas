

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

cbg_HB_vio <- read.csv("Data/cbg_HB_vio_combined.csv")


################################################################################
## Data manipulation : Race indicator
################################################################################

## Relative risk of POC and non-hispanic whites

cbg_HB_vio <- cbg_HB_vio %>%
  mutate(POC_vio = sum(MINORPCT*population_served_count*total_violations))

POC_risk <- (cbg_HB_vio$POC_vio/sum(cbg_HB_vio$population_served_count))

POC_risk

# 1.18685

cbg_HB_vio <- cbg_HB_vio %>%
  mutate(WHITEPCT = (1-MINORPCT)) %>%
  mutate(WHI_vio = sum(WHITEPCT*population_served_count*total_violations))

WHI_risk <- (cbg_HB_vio$WHI_vio/sum(cbg_HB_vio$population_served_count))

WHI_risk

# 0.7123336

rel_risk_race <- POC_risk / WHI_risk

rel_risk_race

# 1.666143

## Using avg number of violations per CBG 

cbg_HB_vio <- cbg_HB_vio %>%
  mutate(POC_vio_av = sum(MINORPCT*population_served_count*avg_vio_CBG))

POC_risk2 <- (cbg_HB_vio$POC_vio_av/sum(cbg_HB_vio$population_served_count))

POC_risk2

# 1.146108 

cbg_HB_vio <- cbg_HB_vio %>%
   mutate(WHI_vio_av = sum(WHITEPCT*population_served_count*avg_vio_CBG))

WHI_risk2 <- (cbg_HB_vio$WHI_vio_av/sum(cbg_HB_vio$population_served_count))

WHI_risk2

# 0.6954254

rel_risk_race2 <- POC_risk2 / WHI_risk2

rel_risk_race2

# 1.648068 

################################################################################
## Data manipulation: Income indicator
################################################################################

## Relative risk of individuals above or below 2X the poverty limit

cbg_HB_vio <- cbg_HB_vio %>%
  mutate(POV_vio = sum(LOWINCPCT*population_served_count*total_violations))

POV_risk <- (cbg_HB_vio$POV_vio/sum(cbg_HB_vio$population_served_count))

POV_risk

# 0.6955291 

cbg_HB_vio <- cbg_HB_vio %>%
  mutate(HIINC = (1-LOWINCPCT)) %>%
  mutate(HIINC_vio = sum(HIINC*population_served_count*total_violations))

HIINC_risk <- (cbg_HB_vio$HIINC_vio/sum(cbg_HB_vio$population_served_count))

HIINC_risk

rel_risk_inc <- POV_risk / HIINC_risk

rel_risk_inc

# 0.5778479

## Using average number of violations per CBG

cbg_HB_vio <- cbg_HB_vio %>%
  mutate(POV_vio_av = sum(LOWINCPCT*population_served_count*avg_vio_CBG))

POV_risk2 <- (cbg_HB_vio$POV_vio_av/sum(cbg_HB_vio$population_served_count))

POV_risk2

# 0.6786553  

cbg_HB_vio <- cbg_HB_vio %>%
  mutate(HIINC_vio_av = sum(HIINC*population_served_count*avg_vio_CBG))

HIINC_risk2 <- (cbg_HB_vio$HIINC_vio_av/sum(cbg_HB_vio$population_served_count))

HIINC_risk2

# 1.162878 

rel_risk_inc2 <- POV_risk2 / HIINC_risk2

rel_risk_inc2

# 0.5835996 




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

HB_vio <- read_rds("Data/HB_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

## Relative risk of POC and non-hispanic whites

HB_vio <- HB_vio %>%
  mutate(POC_vio = sum(MINORPCT*population_served_count*total_violations, na.rm = TRUE))

POC_risk <- (HB_vio$POC_vio/sum(HB_vio$population_served_count))

POC_risk

# 1.398261  

HB_vio <- HB_vio %>%
  mutate(WHITEPCT = (1-MINORPCT)) %>%
  mutate(WHI_vio = sum(WHITEPCT*population_served_count*total_violations, na.rm = TRUE))

WHI_risk <- (HB_vio$WHI_vio/sum(HB_vio$population_served_count))

WHI_risk

# 1.673041  

rel_risk_race <- POC_risk / WHI_risk

rel_risk_race

# 0.8357604 


################################################################################
## Data manipulation: Income indicator
################################################################################

## Relative risk of individuals above or below 2X the poverty limit

HB_vio <- HB_vio %>%
  mutate(POV_vio = sum(LOWINC*population_served_count*total_violations, na.rm = TRUE))

POV_risk <- (HB_vio$POV_vio/sum(HB_vio$population_served_count))

POV_risk

# 1.168507  

HB_vio <- HB_vio %>%
  mutate(HIINC = (1-LOWINC)) %>%
  mutate(HIINC_vio = sum(HIINC*population_served_count*total_violations, na.rm = TRUE))

HIINC_risk <- (HB_vio$HIINC_vio/sum(HB_vio$population_served_count))

HIINC_risk

# 1.902795 

rel_risk_inc <- POV_risk / HIINC_risk

rel_risk_inc

# 0.6141005 

# Clear memory
rm(HB_vio)

################################################################################
## Load Data 
## LCR violations 
################################################################################

LCR_vio <- read_rds("Data/lcr_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

## Relative risk of POC and non-hispanic whites

LCR_vio <- LCR_vio %>%
  mutate(POC_vio = sum(MINORPCT*population_served_count*total_violations, na.rm = TRUE))

POC_risk <- (LCR_vio$POC_vio/sum(LCR_vio$population_served_count))

POC_risk

# 0.4457843   

LCR_vio <- LCR_vio %>%
  mutate(WHITEPCT = (1-MINORPCT)) %>%
  mutate(WHI_vio = sum(WHITEPCT*population_served_count*total_violations, na.rm = TRUE))

WHI_risk <- (LCR_vio$WHI_vio/sum(LCR_vio$population_served_count))

WHI_risk

# 0.5751014   

rel_risk_race <- POC_risk / WHI_risk

rel_risk_race

# 0.7751404  


################################################################################
## Data manipulation: Income indicator
################################################################################

## Relative risk of individuals above or below 2X the poverty limit

LCR_vio <- LCR_vio %>%
  mutate(POV_vio = sum(LOWINC*population_served_count*total_violations, na.rm = TRUE))

POV_risk <- (LCR_vio$POV_vio/sum(LCR_vio$population_served_count))

POV_risk

# 0.3162561   

LCR_vio <- LCR_vio %>%
  mutate(HIINC = (1-LOWINC)) %>%
  mutate(HIINC_vio = sum(HIINC*population_served_count*total_violations, na.rm = TRUE))

HIINC_risk <- (LCR_vio$HIINC_vio/sum(LCR_vio$population_served_count))

HIINC_risk

# 0.7046295  

rel_risk_inc <- POV_risk / HIINC_risk

rel_risk_inc

# 0.4488261  

# Clear memory
rm(LCR_vio)

################################################################################
## Load Data 
## PFAS violations
################################################################################

pfas_vio <- read_rds("Data/pfas_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

pfas_vio <- pfas_vio %>%
  mutate(POC_vio = sum(MINORPCT*population_served_count*detection, na.rm = TRUE))

POC_risk <- (pfas_vio$POC_vio/sum(pfas_vio$population_served_count))

POC_risk

# 0.1421632    

pfas_vio <- pfas_vio %>%
  mutate(WHITEPCT = (1-MINORPCT)) %>%
  mutate(WHI_vio = sum(WHITEPCT*population_served_count*detection, na.rm = TRUE))

WHI_risk <- (pfas_vio$WHI_vio/sum(pfas_vio$population_served_count))

WHI_risk

# 0.1262117   

rel_risk_race <- POC_risk / WHI_risk

rel_risk_race

# 1.126386  


################################################################################
## Data manipulation: Income indicator
################################################################################

## Relative risk of individuals above or below 2X the poverty limit

pfas_vio <- pfas_vio %>%
  mutate(POV_vio = sum(LOWINC*population_served_count*detection, na.rm = TRUE))

POV_risk <- (pfas_vio$POV_vio/sum(pfas_vio$population_served_count))

POV_risk

# 0.08656677   

pfas_vio <- pfas_vio %>%
  mutate(HIINC = (1-LOWINC)) %>%
  mutate(HIINC_vio = sum(HIINC*population_served_count*detection, na.rm = TRUE))

HIINC_risk <- (pfas_vio$HIINC_vio/sum(pfas_vio$population_served_count))

HIINC_risk

# 0.1818081  

rel_risk_inc <- POV_risk / HIINC_risk

rel_risk_inc

# 0.4761436  

# Clear memory
rm(pfas_vio)


################################################################################
## Load Data 
## DBP violations
################################################################################

dbp_vio <- read_rds("Data/dbp_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

dbp_vio <- dbp_vio %>%
  mutate(POC_vio = sum(MINORPCT*population_served_count*combined_dbp, na.rm = TRUE))

POC_risk <- (dbp_vio$POC_vio/sum(dbp_vio$population_served_count))

POC_risk

# 17.84256    

dbp_vio <- dbp_vio %>%
  mutate(WHITEPCT = (1-MINORPCT)) %>%
  mutate(WHI_vio = sum(WHITEPCT*population_served_count*combined_dbp, na.rm = TRUE))

WHI_risk <- (dbp_vio$WHI_vio/sum(dbp_vio$population_served_count))

WHI_risk

# 22.13686   

rel_risk_race <- POC_risk / WHI_risk

rel_risk_race

# 0.8060115  


################################################################################
## Data manipulation: Income indicator
################################################################################

## Relative risk of individuals above or below 2X the poverty limit

dbp_vio <- dbp_vio %>%
  mutate(POV_vio = sum(LOWINC*population_served_count*combined_dbp, na.rm = TRUE))

POV_risk <- (dbp_vio$POV_vio/sum(dbp_vio$population_served_count))

POV_risk

# 13.15933   

dbp_vio <- dbp_vio %>%
  mutate(HIINC = (1-LOWINC)) %>%
  mutate(HIINC_vio = sum(HIINC*population_served_count*combined_dbp, na.rm = TRUE))

HIINC_risk <- (dbp_vio$HIINC_vio/sum(dbp_vio$population_served_count))

HIINC_risk

# 26.82008  

rel_risk_inc <- POV_risk / HIINC_risk

rel_risk_inc

# 0.4906522  

# Clear memory
rm(dbp_vio)


################################################################################
## Load Data 
## TCR violations
################################################################################

tcr_vio <- read_rds("Data/tcr_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

tcr_vio <- tcr_vio %>%
  mutate(POC_vio = sum(MINORPCT*population_served_count*detection_share, na.rm = TRUE))

POC_risk <- (tcr_vio$POC_vio/sum(tcr_vio$population_served_count))

POC_risk

# 0.01332716     

tcr_vio <- tcr_vio %>%
  mutate(WHITEPCT = (1-MINORPCT)) %>%
  mutate(WHI_vio = sum(WHITEPCT*population_served_count*detection_share, na.rm = TRUE))

WHI_risk <- (tcr_vio$WHI_vio/sum(tcr_vio$population_served_count))

WHI_risk

# 0.01640588    

rel_risk_race <- POC_risk / WHI_risk

rel_risk_race

# 0.8123408   


################################################################################
## Data manipulation: Income indicator
################################################################################

## Relative risk of individuals above or below 2X the poverty limit

tcr_vio <- tcr_vio %>%
  mutate(POV_vio = sum(LOWINC*population_served_count*detection_share, na.rm = TRUE))

POV_risk <- (tcr_vio$POV_vio/sum(tcr_vio$population_served_count))

POV_risk

# 0.01016267    

tcr_vio <- tcr_vio %>%
  mutate(HIINC = (1-LOWINC)) %>%
  mutate(HIINC_vio = sum(HIINC*population_served_count*detection_share, na.rm = TRUE))

HIINC_risk <- (tcr_vio$HIINC_vio/sum(tcr_vio$population_served_count))

HIINC_risk

# 0.01957037   

rel_risk_inc <- POV_risk / HIINC_risk

rel_risk_inc

# 0.5192888   

# Clear memory
rm(tcr_vio)


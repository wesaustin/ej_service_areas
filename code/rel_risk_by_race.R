

################################################################################
# Demographic Indices for SDWA indicators : by demographic group
# National Center for Environmental Economics
# Last edited: 9/16/25
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
  MASS , #For regressions and modeling
  dplyr,
  readxl
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
## Load Data 
## Health violations
################################################################################

# Dataset on health-based violations (since 2015, national) 

HB_vio <- readRDS("data/HB_vio_combined.rds")

################################################################################
## Data manipulation : Indicators by race
################################################################################

## Relative risk of POC and non-Hispanic whites (nhw)

summary(HB_vio$pop_served)

HB_vio <- HB_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) 

HB_vio <- HB_vio %>%
  mutate(black_served = black_pct*pop_served) %>% #black population served
  mutate(amer_ind_served = amer_ind_pct*pop_served) %>% #american indian served
  mutate(asian_served = asian_pct*pop_served) %>% #asian served
  mutate(pac_isl_served = pac_isl_pct*pop_served) %>% #pacific islander served
  mutate(hispanic_served = hispanic_pct*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

HB_vio <- HB_vio %>%
  mutate(black_risk = weighted.mean(total_violations, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(total_violations, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(total_violations, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(total_violations, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(total_violations, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(total_violations, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk 
rel_risk_black <- HB_vio$black_risk / HB_vio$nhw_risk
#0.99

rel_risk_am_in <- HB_vio$amer_ind_risk / HB_vio$nhw_risk
#1.79

rel_risk_asian <- HB_vio$asian_risk / HB_vio$nhw_risk
#0.47

rel_risk_pac_isl <- HB_vio$pac_isl_risk / HB_vio$nhw_risk
#0.64

rel_risk_hisp <- HB_vio$hispanic_risk / HB_vio$nhw_risk
#0.91


# Clear memory
rm(HB_vio)

################################################################################
## Load Data 
## LCR violations 
################################################################################

LCR_vio <- read_rds("data/lcr_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

## Relative risk of POC and non-hispanic whites

LCR_vio <- LCR_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) 

LCR_vio <- LCR_vio %>%
  mutate(black_served = black_pct*pop_served) %>% #black population served
  mutate(amer_ind_served = amer_ind_pct*pop_served) %>% #american indian served
  mutate(asian_served = asian_pct*pop_served) %>% #asian served
  mutate(pac_isl_served = pac_isl_pct*pop_served) %>% #pacific islander served
  mutate(hispanic_served = hispanic_pct*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

LCR_vio <- LCR_vio %>%
  mutate(black_risk = weighted.mean(total_violations, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(total_violations, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(total_violations, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(total_violations, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(total_violations, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(total_violations, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk 
rel_risk_black <- LCR_vio$black_risk / LCR_vio$nhw_risk
#0.95

rel_risk_am_in <- LCR_vio$amer_ind_risk / LCR_vio$nhw_risk
#0.92

rel_risk_asian <- LCR_vio$asian_risk / LCR_vio$nhw_risk
#1.20

rel_risk_pac_isl <- LCR_vio$pac_isl_risk / LCR_vio$nhw_risk
#1.12

rel_risk_hisp <- LCR_vio$hispanic_risk / LCR_vio$nhw_risk
#0.98

# Clear memory
rm(LCR_vio)

################################################################################
## Load Data 
## PFAS violations
################################################################################

pfas_vio <- read_rds("data/pfas_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

# Note switching to the binary detection variable instead of the total_violations one

pfas_vio <- pfas_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) 

pfas_vio <- pfas_vio %>%
  mutate(black_served = black_pct*pop_served) %>% #black population served
  mutate(amer_ind_served = amer_ind_pct*pop_served) %>% #american indian served
  mutate(asian_served = asian_pct*pop_served) %>% #asian served
  mutate(pac_isl_served = pac_isl_pct*pop_served) %>% #pacific islander served
  mutate(hispanic_served = hispanic_pct*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

pfas_vio <- pfas_vio %>%
  mutate(black_risk = weighted.mean(detection, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(detection, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(detection, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(detection, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(detection, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(detection, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk 
rel_risk_black <- pfas_vio$black_risk / pfas_vio$nhw_risk
#1.01

rel_risk_am_in <- pfas_vio$amer_ind_risk / pfas_vio$nhw_risk
#1.19

rel_risk_asian <- pfas_vio$asian_risk / pfas_vio$nhw_risk
#1.45

rel_risk_pac_isl <- pfas_vio$pac_isl_risk / pfas_vio$nhw_risk
#1.25

rel_risk_hisp <- pfas_vio$hispanic_risk / pfas_vio$nhw_risk
#1.43

# Clear memory
rm(pfas_vio)


################################################################################
## Load Data 
## DBP violations
################################################################################

dbp_vio <- read_rds("data/dbp_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

# Note switching to the continuous variable combined_dbp instead of the total_violations one

dbp_vio <- dbp_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) 

dbp_vio <- dbp_vio %>%
  mutate(black_served = black_pct*pop_served) %>% #black population served
  mutate(amer_ind_served = amer_ind_pct*pop_served) %>% #american indian served
  mutate(asian_served = asian_pct*pop_served) %>% #asian served
  mutate(pac_isl_served = pac_isl_pct*pop_served) %>% #pacific islander served
  mutate(hispanic_served = hispanic_pct*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

dbp_vio <- dbp_vio %>%
  mutate(black_risk = weighted.mean(combined_dbp, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(combined_dbp, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(combined_dbp, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(combined_dbp, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(combined_dbp, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(combined_dbp, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk 
rel_risk_black <- dbp_vio$black_risk / dbp_vio$nhw_risk
#1.08

rel_risk_am_in <- dbp_vio$amer_ind_risk / dbp_vio$nhw_risk
#0.95

rel_risk_asian <- dbp_vio$asian_risk / dbp_vio$nhw_risk
#0.99

rel_risk_pac_isl <- dbp_vio$pac_isl_risk / dbp_vio$nhw_risk
#0.91

rel_risk_hisp <- dbp_vio$hispanic_risk / dbp_vio$nhw_risk
#0.94


# Clear memory
rm(dbp_vio)


################################################################################
## Load Data 
## TCR violations
################################################################################

tcr_vio <- read_rds("data/tcr_vio_combined.rds")


################################################################################
## Data manipulation : Race indicator
################################################################################

# Note switching to the continuous variable detection_share instead of the total_violations one

tcr_vio <- tcr_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) 

tcr_vio <- tcr_vio %>%
  mutate(black_served = black_pct*pop_served) %>% #black population served
  mutate(amer_ind_served = amer_ind_pct*pop_served) %>% #american indian served
  mutate(asian_served = asian_pct*pop_served) %>% #asian served
  mutate(pac_isl_served = pac_isl_pct*pop_served) %>% #pacific islander served
  mutate(hispanic_served = hispanic_pct*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

tcr_vio <- tcr_vio %>%
  mutate(black_risk = weighted.mean(detection_share, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(detection_share, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(detection_share, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(detection_share, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(detection_share, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(detection_share, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk 
rel_risk_black <- tcr_vio$black_risk / tcr_vio$nhw_risk
#1.39

rel_risk_am_in <- tcr_vio$amer_ind_risk / tcr_vio$nhw_risk
#0.82

rel_risk_asian <- tcr_vio$asian_risk / tcr_vio$nhw_risk
#1.38

rel_risk_pac_isl <- tcr_vio$pac_isl_risk / tcr_vio$nhw_risk
#0.99

rel_risk_hisp <- tcr_vio$hispanic_risk / tcr_vio$nhw_risk
#0.93


# Clear memory
rm(tcr_vio)


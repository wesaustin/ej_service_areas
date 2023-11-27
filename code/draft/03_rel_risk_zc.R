################################################################################
# Demographic Indices for SDWA indicators : Zip Code level
# National Center for Environmental Economics
# Last edited: 9/19/25
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

HB_vio <- readRDS("data/combined/HB_vio_zc.rds")

HB_vio <- HB_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) 

HB_vio <- HB_vio %>%
  mutate(minor_served = minorpct*pop_served) %>%
  mutate(black_served = frac_black*pop_served) %>% #black population served
  mutate(amer_ind_served = frac_amerind*pop_served) %>% #american indian served
  mutate(asian_served = frac_asian*pop_served) %>% #asian served
  mutate(pac_isl_served = frac_pacisl*pop_served) %>% #pacific islander served
  mutate(hispanic_served = frac_hisp*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

HB_vio <- HB_vio %>%
  mutate(minor_risk = weighted.mean(total_violations, minor_served, na.rm= TRUE)) %>% #black population weight
  mutate(black_risk = weighted.mean(total_violations, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(total_violations, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(total_violations, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(total_violations, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(total_violations, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(total_violations, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk :Racial categories
rel_risk_minor <- HB_vio$minor_risk / HB_vio$nhw_risk

#1.07


rel_risk_black <- HB_vio$black_risk / HB_vio$nhw_risk

#0.995


rel_risk_am_in <- HB_vio$amer_ind_risk / HB_vio$nhw_risk

#2.86


rel_risk_asian <- HB_vio$asian_risk / HB_vio$nhw_risk

#0.92

rel_risk_pac_isl <- HB_vio$pac_isl_risk / HB_vio$nhw_risk

#0.72


rel_risk_hisp <- HB_vio$hispanic_risk / HB_vio$nhw_risk

#1.07

# Generate Relative Risk : Income category

HB_vio <- HB_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(total_violations, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(total_violations, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- HB_vio$lowinc_risk / HB_vio$highinc_risk

# 1.10

# Clear memory
rm(HB_vio)

################################################################################
## Load Data 
## LCR violations
################################################################################

# Dataset on LCR violations 

lcr_vio <- readRDS("data/combined/lcr_vio_zc.rds")

lcr_vio <- lcr_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) 

lcr_vio <- lcr_vio %>%
  mutate(minor_served = minorpct*pop_served) %>%
  mutate(black_served = frac_black*pop_served) %>% #black population served
  mutate(amer_ind_served = frac_amerind*pop_served) %>% #american indian served
  mutate(asian_served = frac_asian*pop_served) %>% #asian served
  mutate(pac_isl_served = frac_pacisl*pop_served) %>% #pacific islander served
  mutate(hispanic_served = frac_hisp*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

lcr_vio <- lcr_vio %>%
  mutate(minor_risk = weighted.mean(total_violations, minor_served, na.rm= TRUE)) %>% #black population weight
  mutate(black_risk = weighted.mean(total_violations, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(total_violations, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(total_violations, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(total_violations, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(total_violations, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(total_violations, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk :Racial categories
rel_risk_minor <- lcr_vio$minor_risk / lcr_vio$nhw_risk

#1.03


rel_risk_black <- lcr_vio$black_risk / lcr_vio$nhw_risk

#1.03


rel_risk_am_in <- lcr_vio$amer_ind_risk / lcr_vio$nhw_risk

#0.72


rel_risk_asian <- lcr_vio$asian_risk / lcr_vio$nhw_risk

#1.37

rel_risk_pac_isl <- lcr_vio$pac_isl_risk / lcr_vio$nhw_risk

#1.34


rel_risk_hisp <- lcr_vio$hispanic_risk / lcr_vio$nhw_risk

#0.91

# Generate Relative Risk : Income category

lcr_vio <- lcr_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(total_violations, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(total_violations, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- lcr_vio$lowinc_risk / lcr_vio$highinc_risk

# 0.96

# Clear memory
rm(lcr_vio)

################################################################################
## Load Data 
## PFAS violations
################################################################################

# Dataset on PFAS violations

pfas_vio <- readRDS("data/combined/pfas_vio_zc.rds")

pfas_vio <- pfas_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) 

pfas_vio <- pfas_vio %>%
  mutate(minor_served = minorpct*pop_served) %>%
  mutate(black_served = frac_black*pop_served) %>% #black population served
  mutate(amer_ind_served = frac_amerind*pop_served) %>% #american indian served
  mutate(asian_served = frac_asian*pop_served) %>% #asian served
  mutate(pac_isl_served = frac_pacisl*pop_served) %>% #pacific islander served
  mutate(hispanic_served = frac_hisp*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

pfas_vio <- pfas_vio %>%
  mutate(minor_risk = weighted.mean(detection, minor_served, na.rm= TRUE)) %>% #black population weight
  mutate(black_risk = weighted.mean(detection, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(detection, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(detection, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(detection, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(detection, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(detection, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk :Racial categories
rel_risk_minor <- pfas_vio$minor_risk / pfas_vio$nhw_risk

#1.40


rel_risk_black <- pfas_vio$black_risk / pfas_vio$nhw_risk

#1.24


rel_risk_am_in <- pfas_vio$amer_ind_risk / pfas_vio$nhw_risk

#0.73


rel_risk_asian <- pfas_vio$asian_risk / pfas_vio$nhw_risk

#1.52

rel_risk_pac_isl <- pfas_vio$pac_isl_risk / pfas_vio$nhw_risk

#1.36


rel_risk_hisp <- pfas_vio$hispanic_risk / pfas_vio$nhw_risk

#1.52

# Generate Relative Risk : Income category

pfas_vio <- pfas_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(detection, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(detection, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- pfas_vio$lowinc_risk / pfas_vio$highinc_risk

# 0.995

# Clear memory
rm(pfas_vio)

################################################################################
## Load Data 
## DBP violations
################################################################################

# Dataset on DBP violations

dbp_vio <- readRDS("data/combined/dbp_vio_zc.rds")

dbp_vio <- dbp_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) 

dbp_vio <- dbp_vio %>%
  mutate(minor_served = minorpct*pop_served) %>%
  mutate(black_served = frac_black*pop_served) %>% #black population served
  mutate(amer_ind_served = frac_amerind*pop_served) %>% #american indian served
  mutate(asian_served = frac_asian*pop_served) %>% #asian served
  mutate(pac_isl_served = frac_pacisl*pop_served) %>% #pacific islander served
  mutate(hispanic_served = frac_hisp*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

dbp_vio <- dbp_vio %>%
  mutate(minor_risk = weighted.mean(combined_dbp, minor_served, na.rm= TRUE)) %>% #black population weight
  mutate(black_risk = weighted.mean(combined_dbp, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(combined_dbp, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(combined_dbp, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(combined_dbp, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(combined_dbp, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(combined_dbp, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk :Racial categories
rel_risk_minor <- dbp_vio$minor_risk / dbp_vio$nhw_risk

#0.92


rel_risk_black <- dbp_vio$black_risk / dbp_vio$nhw_risk

#1.02


rel_risk_am_in <- dbp_vio$amer_ind_risk / dbp_vio$nhw_risk

#0.99


rel_risk_asian <- dbp_vio$asian_risk / dbp_vio$nhw_risk

#0.91

rel_risk_pac_isl <- dbp_vio$pac_isl_risk / dbp_vio$nhw_risk

#0.885


rel_risk_hisp <- dbp_vio$hispanic_risk / dbp_vio$nhw_risk

#0.86

# Generate Relative Risk : Income category

dbp_vio <- dbp_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(combined_dbp, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(combined_dbp, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- dbp_vio$lowinc_risk / dbp_vio$highinc_risk

# 1.02

rm(dbp_vio)

################################################################################
## Load Data 
## TCR violations
################################################################################

# Dataset on TCR violations

tcr_vio <- readRDS("data/combined/tcr_vio_zc.rds")

tcr_vio <- tcr_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) 

tcr_vio <- tcr_vio %>%
  mutate(minor_served = minorpct*pop_served) %>%
  mutate(black_served = frac_black*pop_served) %>% #black population served
  mutate(amer_ind_served = frac_amerind*pop_served) %>% #american indian served
  mutate(asian_served = frac_asian*pop_served) %>% #asian served
  mutate(pac_isl_served = frac_pacisl*pop_served) %>% #pacific islander served
  mutate(hispanic_served = frac_hisp*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

tcr_vio <- tcr_vio %>%
  mutate(minor_risk = weighted.mean(detection_share, minor_served, na.rm= TRUE)) %>% #black population weight
  mutate(black_risk = weighted.mean(detection_share, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(detection_share, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(detection_share, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(detection_share, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(detection_share, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(detection_share, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk :Racial categories
rel_risk_minor <- tcr_vio$minor_risk / tcr_vio$nhw_risk

#1.07


rel_risk_black <- tcr_vio$black_risk / tcr_vio$nhw_risk

#1.36


rel_risk_am_in <- tcr_vio$amer_ind_risk / tcr_vio$nhw_risk

#0.70


rel_risk_asian <- tcr_vio$asian_risk / tcr_vio$nhw_risk

#1.18

rel_risk_pac_isl <- tcr_vio$pac_isl_risk / tcr_vio$nhw_risk

#0.93


rel_risk_hisp <- tcr_vio$hispanic_risk / tcr_vio$nhw_risk

#0.84

# Generate Relative Risk : Income category

tcr_vio <- tcr_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(detection_share, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(detection_share, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- tcr_vio$lowinc_risk / tcr_vio$highinc_risk

# 1.02

# Clear memory
rm(tcr_vio)




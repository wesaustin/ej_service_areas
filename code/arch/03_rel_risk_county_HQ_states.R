################################################################################
# County Rel Risk indicators for high quality states 
# National Center for Environmental Economics
# Last edited: 9/27/25
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

## High quality stusps_x s:

# CA, NM, CT, WA, NJ


################################################################################
## Load Data 
## Health violations
################################################################################

# Dataset on health-based violations (since 2015, national) 

HB_vio <- readRDS("data/combined/HB_vio_county.rds")

HB_vio <- HB_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct))  %>%
  filter(stusps_x == "CA" | stusps_x == "NM" | stusps_x == "CT" | stusps_x == "WA" |  stusps_x  == "NJ")

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

## Original: 0.93

# 1.26


rel_risk_black <- HB_vio$black_risk / HB_vio$nhw_risk

## Original: 0.81

#1.21


rel_risk_am_in <- HB_vio$amer_ind_risk / HB_vio$nhw_risk

## Original: 2.14

#2.17


rel_risk_asian <- HB_vio$asian_risk / HB_vio$nhw_risk

## Original: 0.71

#0.92

rel_risk_pac_isl <- HB_vio$pac_isl_risk / HB_vio$nhw_risk

## Original: 0.85

#0.80

rel_risk_hisp <- HB_vio$hispanic_risk / HB_vio$nhw_risk

## Original: 1.01

#1.39

# Generate Relative Risk : Income category

HB_vio <- HB_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(total_violations, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(total_violations, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- HB_vio$lowinc_risk / HB_vio$highinc_risk

## Original:  1.13

#1.21

# Clear memory
rm(HB_vio)

################################################################################
## Load Data 
## LCR violations
################################################################################

# Dataset on LCR violations 

lcr_vio <- readRDS("data/combined/lcr_vio_county.rds")

lcr_vio <- lcr_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct))  %>%
  filter(stusps_x == "CA" | stusps_x == "NM" | stusps_x == "CT" | stusps_x == "WA" |  stusps_x  == "NJ")

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

## Original: 0.84

#1.02

rel_risk_black <- lcr_vio$black_risk / lcr_vio$nhw_risk

## Original: 0.84

#1.05


rel_risk_am_in <- lcr_vio$amer_ind_risk / lcr_vio$nhw_risk

## Original: 0.77

#0.87


rel_risk_asian <- lcr_vio$asian_risk / lcr_vio$nhw_risk

## Original: 0.71

#0.98

rel_risk_pac_isl <- lcr_vio$pac_isl_risk / lcr_vio$nhw_risk

## Original: 0.92

#0.89


rel_risk_hisp <- lcr_vio$hispanic_risk / lcr_vio$nhw_risk

## Original: 0.80

#1.04

# Generate Relative Risk : Income category

lcr_vio <- lcr_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(total_violations, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(total_violations, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- lcr_vio$lowinc_risk / lcr_vio$highinc_risk

## Original:  0.93

#0.999

# Clear memory
rm(lcr_vio)

################################################################################
## Load Data 
## PFAS violations
################################################################################

# Dataset on PFAS violations

pfas_vio <- readRDS("data/combined/pfas_vio_county.rds")

pfas_vio <- pfas_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct))  %>%
  filter(stusps_x == "CA" | stusps_x == "NM" | stusps_x == "CT" | stusps_x == "WA" |  stusps_x  == "NJ")

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

## Original: 1.01

#1.27


rel_risk_black <- pfas_vio$black_risk / pfas_vio$nhw_risk

## Original: 0.74

#1.37


rel_risk_am_in <- pfas_vio$amer_ind_risk / pfas_vio$nhw_risk

## Original: 0.54

#0.53


rel_risk_asian <- pfas_vio$asian_risk / pfas_vio$nhw_risk

## Original: 1.35

#1.31

rel_risk_pac_isl <- pfas_vio$pac_isl_risk / pfas_vio$nhw_risk

## Original: 1.03

#0.83


rel_risk_hisp <- pfas_vio$hispanic_risk / pfas_vio$nhw_risk

## Original: 1.04

#1.28

# Generate Relative Risk : Income category

pfas_vio <- pfas_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(detection, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(detection, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- pfas_vio$lowinc_risk / pfas_vio$highinc_risk

## Original: 0.89

#1.02

# Clear memory
rm(pfas_vio)

################################################################################
## Load Data 
## DBP violations
################################################################################

# Dataset on DBP violations

dbp_vio <- readRDS("data/combined/dbp_vio_county.rds")

dbp_vio <- dbp_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct))  %>%
  filter(stusps_x == "CA" | stusps_x == "NM" | stusps_x == "CT" | stusps_x == "WA" |  stusps_x  == "NJ")

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

## Original: 0.94

#1.00


rel_risk_black <- dbp_vio$black_risk / dbp_vio$nhw_risk

## Original: 0.96

#1.02


rel_risk_am_in <- dbp_vio$amer_ind_risk / dbp_vio$nhw_risk

## Original: 0.97

#0.92


rel_risk_asian <- dbp_vio$asian_risk / dbp_vio$nhw_risk

## Original: 0.97

#1.07

rel_risk_pac_isl <- dbp_vio$pac_isl_risk / dbp_vio$nhw_risk

## Original: 0.97

#0.99


rel_risk_hisp <- dbp_vio$hispanic_risk / dbp_vio$nhw_risk

## Original: 0.92

#0.98

# Generate Relative Risk : Income category

dbp_vio <- dbp_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(combined_dbp, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(combined_dbp, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- dbp_vio$lowinc_risk / dbp_vio$highinc_risk

## Original: 1.01

#0.97

rm(dbp_vio)

################################################################################
## Load Data 
## TCR violations
################################################################################

# Dataset on TCR violations

tcr_vio <- readRDS("data/combined/tcr_vio_county.rds")

tcr_vio <- tcr_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct))  %>%
  filter(stusps_x == "CA" | stusps_x == "NM" | stusps_x == "CT" | stusps_x == "WA" |  stusps_x  == "NJ")

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

## Original: 0.70

#0.97

rel_risk_black <- tcr_vio$black_risk / tcr_vio$nhw_risk

## Original: 0.75

#0.92


rel_risk_am_in <- tcr_vio$amer_ind_risk / tcr_vio$nhw_risk

## Original: 1.16

#1.09


rel_risk_asian <- tcr_vio$asian_risk / tcr_vio$nhw_risk

## Original: 0.74

#0.96

rel_risk_pac_isl <- tcr_vio$pac_isl_risk / tcr_vio$nhw_risk

## Original: 1.03

#1.03


rel_risk_hisp <- tcr_vio$hispanic_risk / tcr_vio$nhw_risk

## Original: 1.16

#0.98

# Generate Relative Risk : Income category

tcr_vio <- tcr_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(detection_share, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(detection_share, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- tcr_vio$lowinc_risk / tcr_vio$highinc_risk

## Original: 0.93

#1.01

# Clear memory
rm(tcr_vio)




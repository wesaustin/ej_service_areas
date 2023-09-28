################################################################################
# Epic Boundaries Rel Risk indicators for high quality states
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

################################################################################
## Notes
################################################################################

# the "Original" tag refers to the values prior to subsetting

# Though we will probably only report the aggregate % non-white, I ran for all the 
# individual racial categories (to check the underlying heterogeneity). The HB
# violations had an extremely high value for %black, which may or may not reflect 
# an error? Did not have the same extreme values when I ran for zc or county data,
# but perhaps this is just the result of the more granular demographic assignment

## High quality states:
# CA, NM, CT, WA, NJ

################################################################################
## Load Data 
## Health violations
################################################################################

# Dataset on health-based violations (since 2015, national) 

HB_vio <- readRDS("data/combined/HB_vio_combined.rds") #this is the PWSID (NOT areal) dataset

HB_vio <- HB_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct))  %>%
  mutate(state_code = substr(pwsid,1,2)) %>%
  filter(state_code == "CA" | state_code == "NM" | state_code == "CT" | state_code == "WA" |  state_code  == "NJ")

HB_vio <- HB_vio %>%
  mutate(minor_served = minorpct*pop_served) %>%
  mutate(black_served = black_pct*pop_served) %>% #black population served
  mutate(amer_ind_served = amer_ind_pct*pop_served) %>% #american indian served
  mutate(asian_served = asian_pct*pop_served) %>% #asian served
  mutate(pac_isl_served = pac_isl_pct*pop_served) %>% #pacific islander served
  mutate(hispanic_served = hispanic_pct*pop_served) %>% #hispanic served
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

## Original: 1.03

# 1.58


rel_risk_black <- HB_vio$black_risk / HB_vio$nhw_risk

## Original: 1.16

# 3.74


rel_risk_am_in <- HB_vio$amer_ind_risk / HB_vio$nhw_risk

## Original: 2.29

#1.54


rel_risk_asian <- HB_vio$asian_risk / HB_vio$nhw_risk

## Original: 0.49

#0.68

rel_risk_pac_isl <- HB_vio$pac_isl_risk / HB_vio$nhw_risk

## Original: 0.54

#0.32

rel_risk_hisp <- HB_vio$hispanic_risk / HB_vio$nhw_risk

## Original: 1.06

#1.57

# Generate Relative Risk : Income category

HB_vio <- HB_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(total_violations, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(total_violations, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- HB_vio$lowinc_risk / HB_vio$highinc_risk

## Original:  1.27

#1.58

# Clear memory
rm(HB_vio)

################################################################################
## Load Data 
## LCR violations
################################################################################

# Dataset on LCR violations 

lcr_vio <- read_rds("data/combined/lcr_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

lcr_vio <- lcr_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) %>%
  mutate(state_code = substr(pwsid,1,2)) %>%
  filter(state_code == "CA" | state_code == "NM" | state_code == "CT" | state_code == "WA" |  state_code  == "NJ")

lcr_vio <- lcr_vio %>%
  mutate(minor_served = minorpct*pop_served) %>% #nw population served
  mutate(black_served = black_pct*pop_served) %>% #black population served
  mutate(amer_ind_served = amer_ind_pct*pop_served) %>% #american indian served
  mutate(asian_served = asian_pct*pop_served) %>% #asian served
  mutate(pac_isl_served = pac_isl_pct*pop_served) %>% #pacific islander served
  mutate(hispanic_served = hispanic_pct*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

lcr_vio <- lcr_vio %>%
  mutate(minor_risk = weighted.mean(total_violations, minor_served, na.rm= TRUE)) %>% #NW population weight
  mutate(black_risk = weighted.mean(total_violations, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(total_violations, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(total_violations, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(total_violations, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(total_violations, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(total_violations, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk 
rel_risk_minor <- lcr_vio$minor_risk / lcr_vio$nhw_risk

## Original : 0.95

# 0.85

rel_risk_black <- lcr_vio$black_risk / lcr_vio$nhw_risk
## Original: 1.07

# 1.35

rel_risk_am_in <- lcr_vio$amer_ind_risk / lcr_vio$nhw_risk

## Orignal 0.72

# 0.76

rel_risk_asian <- lcr_vio$asian_risk / lcr_vio$nhw_risk

## Original : 1.24

#0.98

rel_risk_pac_isl <- lcr_vio$pac_isl_risk / lcr_vio$nhw_risk
## Original : 0.97

# 1.08

rel_risk_hisp <- lcr_vio$hispanic_risk / lcr_vio$nhw_risk

## Original: 0.78

# 0.69

lcr_vio <- lcr_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(total_violations, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(total_violations, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- lcr_vio$lowinc_risk / lcr_vio$highinc_risk

## Original: 0.93

# 0.88

# Clear memory
rm(lcr_vio)

################################################################################
## Load Data 
## PFAS violations
################################################################################

pfas_vio <- read_rds("data/combined/pfas_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

# Note switching to the binary detection variable instead of the total_violations one

pfas_vio <- pfas_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) %>%
  mutate(state_code = substr(pwsid,1,2)) %>%
  filter(state_code == "CA" | state_code == "NM" | state_code == "CT" | state_code == "WA" |  state_code  == "NJ")

pfas_vio <- pfas_vio %>%
  mutate(minor_served = minorpct*pop_served) %>% #nw population served
  mutate(black_served = black_pct*pop_served) %>% #black population served
  mutate(amer_ind_served = amer_ind_pct*pop_served) %>% #american indian served
  mutate(asian_served = asian_pct*pop_served) %>% #asian served
  mutate(pac_isl_served = pac_isl_pct*pop_served) %>% #pacific islander served
  mutate(hispanic_served = hispanic_pct*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

pfas_vio <- pfas_vio %>%
  mutate(minor_risk = weighted.mean(detection, minor_served, na.rm= TRUE)) %>% #NW population weight
  mutate(black_risk = weighted.mean(detection, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(detection, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(detection, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(detection, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(detection, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(detection, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk 
rel_risk_minor <- pfas_vio$minor_risk / pfas_vio$nhw_risk

## Original : 1.37

# 1.24

rel_risk_black <- pfas_vio$black_risk / pfas_vio$nhw_risk
## Original: 1.08

# 1.24

rel_risk_am_in <- pfas_vio$amer_ind_risk / pfas_vio$nhw_risk

## Orignal 1.08

# 0.88

rel_risk_asian <- pfas_vio$asian_risk / pfas_vio$nhw_risk

## Original : 1.64

#1.29

rel_risk_pac_isl <- pfas_vio$pac_isl_risk / pfas_vio$nhw_risk
## Original : 1.35

# 0.98

rel_risk_hisp <- pfas_vio$hispanic_risk / pfas_vio$nhw_risk

## Original: 1.56

# 1.25

pfas_vio <- pfas_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(detection, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(detection, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- pfas_vio$lowinc_risk / pfas_vio$highinc_risk

## Original: 0.98

# 1.04

# Clear memory
rm(pfas_vio)


################################################################################
## Load Data 
## DBP violations
################################################################################

dbp_vio <- read_rds("data/combined/dbp_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

# Note switching to the binary detection variable instead of the total_violations one

dbp_vio <- dbp_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) %>%
  mutate(state_code = substr(pwsid,1,2)) %>%
  filter(state_code == "CA" | state_code == "NM" | state_code == "CT" | state_code == "WA" |  state_code  == "NJ")

dbp_vio <- dbp_vio %>%
  mutate(minor_served = minorpct*pop_served) %>% #nw population served
  mutate(black_served = black_pct*pop_served) %>% #black population served
  mutate(amer_ind_served = amer_ind_pct*pop_served) %>% #american indian served
  mutate(asian_served = asian_pct*pop_served) %>% #asian served
  mutate(pac_isl_served = pac_isl_pct*pop_served) %>% #pacific islander served
  mutate(hispanic_served = hispanic_pct*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

dbp_vio <- dbp_vio %>%
  mutate(minor_risk = weighted.mean(combined_dbp, minor_served, na.rm= TRUE)) %>% #NW population weight
  mutate(black_risk = weighted.mean(combined_dbp, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(combined_dbp, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(combined_dbp, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(combined_dbp, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(combined_dbp, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(combined_dbp, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk 
rel_risk_minor <- dbp_vio$minor_risk / dbp_vio$nhw_risk

## Original : 0.99

# 0.95

rel_risk_black <- dbp_vio$black_risk / dbp_vio$nhw_risk

## Original: 1.08

# 1.12

rel_risk_am_in <- dbp_vio$amer_ind_risk / dbp_vio$nhw_risk

## Orignal: 0.95

# 0.88

rel_risk_asian <- dbp_vio$asian_risk / dbp_vio$nhw_risk

## Original : 0.99

#1.04

rel_risk_pac_isl <- dbp_vio$pac_isl_risk / dbp_vio$nhw_risk

## Original : 0.91

# 0.93

rel_risk_hisp <- dbp_vio$hispanic_risk / dbp_vio$nhw_risk

## Original: 0.94

# 0.89

dbp_vio <- dbp_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(combined_dbp, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(combined_dbp, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- dbp_vio$lowinc_risk / dbp_vio$highinc_risk

## Original: 1.01

# 0.92

# Clear memory
rm(dbp_vio)

################################################################################
## Load Data 
## tcr violations
################################################################################

tcr_vio <- read_rds("data/combined/tcr_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

# Note switching to the binary detection variable instead of the total_violations one

tcr_vio <- tcr_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) %>%
  mutate(state_code = substr(pwsid,1,2)) %>%
  filter(state_code == "CA" | state_code == "NM" | state_code == "CT" | state_code == "WA" |  state_code  == "NJ")

tcr_vio <- tcr_vio %>%
  mutate(minor_served = minorpct*pop_served) %>% #nw population served
  mutate(black_served = black_pct*pop_served) %>% #black population served
  mutate(amer_ind_served = amer_ind_pct*pop_served) %>% #american indian served
  mutate(asian_served = asian_pct*pop_served) %>% #asian served
  mutate(pac_isl_served = pac_isl_pct*pop_served) %>% #pacific islander served
  mutate(hispanic_served = hispanic_pct*pop_served) %>% #hispanic served
  mutate(nhw_served = whitepct*pop_served)#non hispanic white served

tcr_vio <- tcr_vio %>%
  mutate(minor_risk = weighted.mean(detection_share, minor_served, na.rm= TRUE)) %>% #NW population weight
  mutate(black_risk = weighted.mean(detection_share, black_served, na.rm= TRUE)) %>% #black population weight
  mutate(amer_ind_risk = weighted.mean(detection_share, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
  mutate(asian_risk = weighted.mean(detection_share, asian_served, na.rm= TRUE)) %>% #asian pop weight
  mutate(pac_isl_risk = weighted.mean(detection_share, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
  mutate(hispanic_risk = weighted.mean(detection_share, hispanic_served, na.rm= TRUE)) %>% # hispanic
  mutate(nhw_risk = weighted.mean(detection_share, nhw_served, na.rm= TRUE)) #non-hispanic white

# Generate Relative Risk 
rel_risk_minor <- tcr_vio$minor_risk / tcr_vio$nhw_risk

## Original : 1.15

# 1.12

rel_risk_black <- tcr_vio$black_risk / tcr_vio$nhw_risk
## Original: 1.40

# 0.95

rel_risk_am_in <- tcr_vio$amer_ind_risk / tcr_vio$nhw_risk

## Orignal 0.82

# 1.36

rel_risk_asian <- tcr_vio$asian_risk / tcr_vio$nhw_risk

## Original : 1.38

#0.90

rel_risk_pac_isl <- tcr_vio$pac_isl_risk / tcr_vio$nhw_risk
## Original : 0.99

# 0.84

rel_risk_hisp <- tcr_vio$hispanic_risk / tcr_vio$nhw_risk

## Original: 0.93

# 1.30

tcr_vio <- tcr_vio %>%
  mutate(highinc = (1-lowinc)) %>% #create high income category
  mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
  mutate(lowinc_served = lowinc*pop_served) %>%
  mutate(highinc_risk = weighted.mean(detection_share, highinc_served, na.rm= TRUE)) %>% #High income weight
  mutate(lowinc_risk = weighted.mean(detection_share, lowinc_served, na.rm= TRUE)) #low income weight

lowinc_rel_risk <- tcr_vio$lowinc_risk / tcr_vio$highinc_risk

## Original: 1.05

# 1.09

# Clear memory
rm(tcr_vio)



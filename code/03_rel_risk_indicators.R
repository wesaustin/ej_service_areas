

################################################################################
# Demographic Indices for SDWA indicators
# National Center for Environmental Economics
# Last edited: 9/6/25
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
## Data manipulation : Race indicator
################################################################################

## Relative risk of POC and non-Hispanic whites (nhw)

summary(HB_vio$pop_served)

HB_vio <- HB_vio %>%
  filter(pop_served != 0)%>% #remove systems with no pop served
  mutate(whitepct = (1-minorpct)) %>%
  mutate(pop_poc = minorpct*pop_served, na.rm = TRUE )   %>%
  mutate(pop_nhw = whitepct*pop_served , na.rm = TRUE  )  %>%
  mutate(sum_pop_poc = sum(pop_poc, na.rm = TRUE) )  %>%
  mutate(sum_pop_nhw = sum(pop_nhw, na.rm = TRUE) ) %>%
  mutate(num_poc = pop_poc*total_violations, na.rm = TRUE ) %>%
  mutate(num_nhw = pop_nhw*total_violations, na.rm = TRUE ) %>%
  mutate(sum_risk_poc = sum(num_poc, na.rm = TRUE)) %>%
  mutate(sum_risk_nhw = sum(num_nhw, na.rm = TRUE)) 

# Demographic risk Indicator  
HB_vio$poc_risk <- HB_vio$sum_risk_poc / HB_vio$sum_pop_poc
HB_vio$nhw_risk <- HB_vio$sum_risk_nhw / HB_vio$sum_pop_nhw

HB_vio$poc_risk
# 3.12

HB_vio$nhw_risk
# 3.03

# Generate Relative Risk 
rel_risk_race <- HB_vio$poc_risk / HB_vio$nhw_risk
rel_risk_race
# 1.029

################################################################################
## Data manipulation: Income indicator
################################################################################

# Same procedure as above 

HB_vio <- HB_vio %>%
  filter(pop_served != 0)%>%
  mutate(highinc = (1-lowinc)) %>%
  mutate(pop_li = lowinc*pop_served, na.rm = TRUE )   %>%
  mutate(pop_hi = highinc*pop_served , na.rm = TRUE  )  %>%
  mutate(sum_pop_li = sum(pop_li, na.rm = TRUE) )  %>%
  mutate(sum_pop_hi = sum(pop_hi, na.rm = TRUE) ) %>%
  mutate(num_li = pop_li*total_violations, na.rm = TRUE ) %>%
  mutate(num_hi = pop_hi*total_violations, na.rm = TRUE ) %>%
  mutate(sum_risk_li = sum(num_li, na.rm = TRUE)) %>%
  mutate(sum_risk_hi = sum(num_hi, na.rm = TRUE)) 

# Demographic risk Indicator  
HB_vio$li_risk <- HB_vio$sum_risk_li / HB_vio$sum_pop_li
HB_vio$hi_risk <- HB_vio$sum_risk_hi / HB_vio$sum_pop_hi

HB_vio$li_risk
# 3.58

HB_vio$hi_risk
# 2.82

# Generate Relative Risk 
rel_risk_inc <- HB_vio$li_risk / HB_vio$hi_risk
rel_risk_inc
# 1.27


# Clear memory
rm(HB_vio)

################################################################################
## Load Data 
## lcr violations 
################################################################################

lcr_vio <- read_rds("data/lcr_vio_combined.rds")

################################################################################
## Data manipulation : Race indicator
################################################################################

## Relative risk of POC and non-hispanic whites

lcr_vio <- lcr_vio %>%
  filter(pop_served != 0) %>%
  mutate(whitepct = (1-minorpct)) %>%
  mutate(pop_poc = minorpct*pop_served, na.rm = TRUE )   %>%
  mutate(pop_nhw = whitepct*pop_served , na.rm = TRUE  )  %>%
  mutate(sum_pop_poc = sum(pop_poc, na.rm = TRUE) )  %>%
  mutate(sum_pop_nhw = sum(pop_nhw, na.rm = TRUE) ) %>%
  mutate(num_poc = pop_poc*total_violations, na.rm = TRUE ) %>%
  mutate(num_nhw = pop_nhw*total_violations, na.rm = TRUE ) %>%
  mutate(sum_risk_poc = sum(num_poc, na.rm = TRUE)) %>%
  mutate(sum_risk_nhw = sum(num_nhw, na.rm = TRUE)) 

# Demographic risk Indicator  
lcr_vio$poc_risk <- lcr_vio$sum_risk_poc / lcr_vio$sum_pop_poc
lcr_vio$nhw_risk <- lcr_vio$sum_risk_nhw / lcr_vio$sum_pop_nhw

lcr_vio$poc_risk
# 0.994

lcr_vio$nhw_risk
# 1.04

# Generate Relative Risk 
rel_risk_race <- lcr_vio$poc_risk / lcr_vio$nhw_risk
rel_risk_race
# 0.95

## Relative risk of individuals above or below 2X the poverty limit

lcr_vio <- lcr_vio %>%
  filter(pop_served != 0) %>%
  mutate(highinc = (1-lowinc)) %>%
  mutate(pop_li = lowinc*pop_served, na.rm = TRUE )   %>%
  mutate(pop_hi = highinc*pop_served , na.rm = TRUE  )  %>%
  mutate(sum_pop_li = sum(pop_li, na.rm = TRUE) )  %>%
  mutate(sum_pop_hi = sum(pop_hi, na.rm = TRUE) ) %>%
  mutate(num_li = pop_li*total_violations, na.rm = TRUE ) %>%
  mutate(num_hi = pop_hi*total_violations, na.rm = TRUE ) %>%
  mutate(sum_risk_li = sum(num_li, na.rm = TRUE)) %>%
  mutate(sum_risk_hi = sum(num_hi, na.rm = TRUE)) 

# Demographic risk Indicator  
lcr_vio$li_risk <- lcr_vio$sum_risk_li / lcr_vio$sum_pop_li
lcr_vio$hi_risk <- lcr_vio$sum_risk_hi / lcr_vio$sum_pop_hi

lcr_vio$li_risk
# 0.969

lcr_vio$hi_risk
# 1.05

# Generate Relative Risk 
rel_risk_inc <- lcr_vio$li_risk / lcr_vio$hi_risk
rel_risk_inc
# 0.926

# Clear memory
rm(lcr_vio)

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
  filter(pop_served != 0) %>%
  mutate(whitepct = (1-minorpct)) %>%
  mutate(pop_poc = minorpct*pop_served, na.rm = TRUE )   %>%
  mutate(pop_nhw = whitepct*pop_served , na.rm = TRUE  )  %>%
  mutate(sum_pop_poc = sum(pop_poc, na.rm = TRUE) )  %>%
  mutate(sum_pop_nhw = sum(pop_nhw, na.rm = TRUE) ) %>%
  mutate(num_poc = pop_poc*detection, na.rm = TRUE ) %>%
  mutate(num_nhw = pop_nhw*detection, na.rm = TRUE ) %>%
  mutate(sum_risk_poc = sum(num_poc, na.rm = TRUE)) %>%
  mutate(sum_risk_nhw = sum(num_nhw, na.rm = TRUE)) 

# Demographic risk Indicator  
pfas_vio$poc_risk <- pfas_vio$sum_risk_poc / pfas_vio$sum_pop_poc
pfas_vio$nhw_risk <- pfas_vio$sum_risk_nhw / pfas_vio$sum_pop_nhw

pfas_vio$poc_risk
# 0.317

pfas_vio$nhw_risk
# 0.229

# Generate Relative Risk 
rel_risk_race <- pfas_vio$poc_risk / pfas_vio$nhw_risk
rel_risk_race
# 1.39


## Relative risk of individuals above or below 2X the poverty limit

pfas_vio <- pfas_vio %>%
  filter(pop_served != 0) %>%
  mutate(highinc = (1-lowinc)) %>%
  mutate(pop_li = lowinc*pop_served, na.rm = TRUE )   %>%
  mutate(pop_hi = highinc*pop_served , na.rm = TRUE  )  %>%
  mutate(sum_pop_li = sum(pop_li, na.rm = TRUE) )  %>%
  mutate(sum_pop_hi = sum(pop_hi, na.rm = TRUE) ) %>%
  mutate(num_li = pop_li*detection, na.rm = TRUE ) %>%
  mutate(num_hi = pop_hi*detection, na.rm = TRUE ) %>%
  mutate(sum_risk_li = sum(num_li, na.rm = TRUE)) %>%
  mutate(sum_risk_hi = sum(num_hi, na.rm = TRUE)) 

# Demographic risk Indicator  
pfas_vio$li_risk <- pfas_vio$sum_risk_li / pfas_vio$sum_pop_li
pfas_vio$hi_risk <- pfas_vio$sum_risk_hi / pfas_vio$sum_pop_hi

pfas_vio$li_risk
# 0.265

pfas_vio$hi_risk
# 0.269

# Generate Relative Risk 
rel_risk_inc <- pfas_vio$li_risk / pfas_vio$hi_risk
rel_risk_inc
# 0.983


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
  filter(pop_served != 0) %>%
  mutate(whitepct = (1-minorpct)) %>%
  mutate(pop_poc = minorpct*pop_served, na.rm = TRUE )   %>%
  mutate(pop_nhw = whitepct*pop_served , na.rm = TRUE  )  %>%
  mutate(sum_pop_poc = sum(pop_poc, na.rm = TRUE) )  %>%
  mutate(sum_pop_nhw = sum(pop_nhw, na.rm = TRUE) ) %>%
  mutate(num_poc = pop_poc*combined_dbp, na.rm = TRUE ) %>%
  mutate(num_nhw = pop_nhw*combined_dbp, na.rm = TRUE ) %>%
  mutate(sum_risk_poc = sum(num_poc, na.rm = TRUE)) %>%
  mutate(sum_risk_nhw = sum(num_nhw, na.rm = TRUE)) 

# Demographic risk Indicator  
dbp_vio$poc_risk <- dbp_vio$sum_risk_poc / dbp_vio$sum_pop_poc
dbp_vio$nhw_risk <- dbp_vio$sum_risk_nhw / dbp_vio$sum_pop_nhw

dbp_vio$poc_risk
# 39.8

dbp_vio$nhw_risk
# 40.1

# Generate Relative Risk 
rel_risk_race <- dbp_vio$poc_risk / dbp_vio$nhw_risk
rel_risk_race
# 0.99



## Relative risk of individuals above or below 2X the poverty limit

dbp_vio <- dbp_vio %>%
  mutate(highinc = (1-lowinc)) %>%
  mutate(pop_li = lowinc*pop_served, na.rm = TRUE )   %>%
  mutate(pop_hi = highinc*pop_served , na.rm = TRUE  )  %>%
  mutate(sum_pop_li = sum(pop_li, na.rm = TRUE) )  %>%
  mutate(sum_pop_hi = sum(pop_hi, na.rm = TRUE) ) %>%
  mutate(num_li = pop_li*combined_dbp, na.rm = TRUE ) %>%
  mutate(num_hi = pop_hi*combined_dbp, na.rm = TRUE ) %>%
  mutate(sum_risk_li = sum(num_li, na.rm = TRUE)) %>%
  mutate(sum_risk_hi = sum(num_hi, na.rm = TRUE)) 

# Demographic risk Indicator  
dbp_vio$li_risk <- dbp_vio$sum_risk_li / dbp_vio$sum_pop_li
dbp_vio$hi_risk <- dbp_vio$sum_risk_hi / dbp_vio$sum_pop_hi

dbp_vio$li_risk
# 40.323

dbp_vio$hi_risk
# 39.81

# Generate Relative Risk 
rel_risk_inc <- dbp_vio$li_risk / dbp_vio$hi_risk
rel_risk_inc
# 1.0128


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
  filter(pop_served != 0) %>%
  mutate(whitepct = (1-minorpct)) %>%
  mutate(pop_poc = minorpct*pop_served, na.rm = TRUE )   %>%
  mutate(pop_nhw = whitepct*pop_served , na.rm = TRUE  )  %>%
  mutate(sum_pop_poc = sum(pop_poc, na.rm = TRUE) )  %>%
  mutate(sum_pop_nhw = sum(pop_nhw, na.rm = TRUE) ) %>%
  mutate(num_poc = pop_poc*detection_share, na.rm = TRUE ) %>%
  mutate(num_nhw = pop_nhw*detection_share, na.rm = TRUE ) %>%
  mutate(sum_risk_poc = sum(num_poc, na.rm = TRUE)) %>%
  mutate(sum_risk_nhw = sum(num_nhw, na.rm = TRUE)) 

# Demographic risk Indicator  
tcr_vio$poc_risk <- tcr_vio$sum_risk_poc / tcr_vio$sum_pop_poc
tcr_vio$nhw_risk <- tcr_vio$sum_risk_nhw / tcr_vio$sum_pop_nhw

tcr_vio$poc_risk
# 0.297

tcr_vio$nhw_risk
#0.297

# Generate Relative Risk 
rel_risk_race <- tcr_vio$poc_risk / tcr_vio$nhw_risk
rel_risk_race
# 0.9997



## Relative risk of individuals above or below 2X the poverty limit

tcr_vio <- tcr_vio %>%
  mutate(highinc = (1-lowinc)) %>%
  mutate(pop_li = lowinc*pop_served, na.rm = TRUE )   %>%
  mutate(pop_hi = highinc*pop_served , na.rm = TRUE  )  %>%
  mutate(sum_pop_li = sum(pop_li, na.rm = TRUE) )  %>%
  mutate(sum_pop_hi = sum(pop_hi, na.rm = TRUE) ) %>%
  mutate(num_li = pop_li*detection_share, na.rm = TRUE ) %>%
  mutate(num_hi = pop_hi*detection_share, na.rm = TRUE ) %>%
  mutate(sum_risk_li = sum(num_li, na.rm = TRUE)) %>%
  mutate(sum_risk_hi = sum(num_hi, na.rm = TRUE)) 

# Demographic risk Indicator  
tcr_vio$li_risk <- tcr_vio$sum_risk_li / tcr_vio$sum_pop_li
tcr_vio$hi_risk <- tcr_vio$sum_risk_hi / tcr_vio$sum_pop_hi

tcr_vio$li_risk
# 0.031

tcr_vio$hi_risk
# 0.029

# Generate Relative Risk 
rel_risk_inc <- tcr_vio$li_risk / tcr_vio$hi_risk
rel_risk_inc
# 1.072


# Clear memory
rm(tcr_vio)


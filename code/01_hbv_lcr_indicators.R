
################################################################################
## Generate Safe Drinking Water indicators (LCR and Health violations)
## National Council for Environmental Economics
## Last edit: 9.14
################################################################################


##Load Packages

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
  cdlTools, # download CDL data
  rgdal, # required for cdlTools
  prism, # download PRISM data
  stringr # string manipulation
)

##Set wd

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

getwd()
setwd(paste0(my_path))
getwd()

################################################################################
## LCR 
################################################################################


## Load lcr data

lcr_samp <- read_csv("C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems/Data/SDWA raw/SDWA_lcr_SAMPLES.csv")

## find the samples exceeding 0.015 mg/l lead and 1.3mg/L for copper, 
# Group by violation type and by PWSID

# Create different variables for different violation types

## 2 continuous: average sample for each pwsid across the period for lead and for copper 

## 2 discrete : number of lead and number of copper exceedances at each pwsid 

lcr_samp <- lcr_samp %>%
  mutate(cu_vio = case_when(
    CONTAMINANT_CODE == "PB90" ~ NA, 
    SAMPLE_MEASURE >= 1.3 & CONTAMINANT_CODE == "CU90"  ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(pb_vio = case_when(
    SAMPLE_MEASURE >= 0.015 & CONTAMINANT_CODE == "PB90" ~ TRUE, 
    CONTAMINANT_CODE == "CU90"  ~ NA,
    TRUE ~ FALSE
  )) 

lcr_vio <- lcr_samp %>%
  group_by(CONTAMINANT_CODE, PWSID) %>%
  mutate(cu_vio_count = sum(cu_vio, na.rm = TRUE)) %>%
  mutate(pb_vio_count = sum(pb_vio, na.rm = TRUE)) %>%
  mutate(avg_cu_level = ifelse(CONTAMINANT_CODE == "CU90", mean(SAMPLE_MEASURE), NA)) %>%
  mutate(avg_pb_level = ifelse(CONTAMINANT_CODE == "PB90", mean(SAMPLE_MEASURE), NA)) %>%
  mutate(max_cu_level = ifelse(CONTAMINANT_CODE == "CU90", max(SAMPLE_MEASURE), NA)) %>%
  mutate(max_pb_level = ifelse(CONTAMINANT_CODE == "PB90", max(SAMPLE_MEASURE), NA)) %>%
  ungroup(CONTAMINANT_CODE) %>%
  fill(c('avg_pb_level', 'avg_cu_level', 'max_cu_level', 'max_pb_level'), .direction = "downup") %>%
  mutate(cu_vio_count = max(cu_vio_count)) %>%
  mutate(pb_vio_count = max(pb_vio_count)) 

#checking that pb and cu levels are filled in correctly
y <- filter(lcr_vio, PWSID == "IL3049031")

## Clean data, keep only the total number of violations per PWSID and the maximum contamination in each

lcr_vio <- lcr_vio %>%
  distinct(PWSID, .keep_all = TRUE) %>%
  dplyr::select(CONTAMINANT_CODE, PWSID, cu_vio_count, pb_vio_count, 
                avg_cu_level, avg_pb_level, max_cu_level, max_pb_level)

## get counts for number of violations by PWSID 

summary(lcr_vio$pb_vio_count)

write_rds(lcr_vio, "Data/lcr_violations.rds")

################################################################################
## Health-based Violations
################################################################################

### Load SDWA Violations data


SDWA_vio <- read_csv("Data/SDWA_vio_ENFORCEMENT.csv")

## Filter out data to to violations that started after 2015 
## Restrict to only health-based violations

SDWA_vio$COMPL_PER_END_DATE <- 
  mdy(SDWA_vio$COMPL_PER_END_DATE) #index data to MDY format

SDWA_vio$COMPL_PER_BEGIN_DATE <- 
  mdy(SDWA_vio$COMPL_PER_BEGIN_DATE)  #index data to MDY format

SDWA_vio_2015 <- SDWA_vio %>%
  filter(COMPL_PER_BEGIN_DATE > '2015-01-01') %>% #after 2015
  filter(IS_HEALTH_BASED_IND == "Y") #health based violations

## Fill null values for compliance end date to compute violation lengths
## Used date when the data was collected (in this case 07.07.2023)

SDWA_vio_2015$COMPL_PER_END_DATE[is.na(SDWA_vio_2015$COMPL_PER_END_DATE)] <- '2023-07-07' #match the date format, verify

## Calculate the length of the violation 

SDWA_vio_2015 <- SDWA_vio_2015 %>%
  mutate(diff_days = difftime(COMPL_PER_END_DATE, COMPL_PER_BEGIN_DATE, units="days"))

glimpse(SDWA_vio2015) #check that the numbers make sense

## Now we have the non-compliance period in days, which we can use to calculate months/years

## Calculate the total number of violations per PWSID

SDWA_vio_2015 <- SDWA_vio_2015 %>%
  group_by(PWSID) %>%
  mutate(total_violations = n()) #number of violations per PWSID

##Select one observation per PWSID, keeping, for example, longest violation period 

SDWA_vio_2015 <- SDWA_vio_2015 %>%
  ungroup() %>%
  filter(diff_days == max(diff_days), .by = PWSID) %>%
  distinct(PWSID, .keep_all = TRUE)

##Keep necessary columns

SDWA_vio_clean <- SDWA_vio_2015 %>%
  dplyr::select(PWSID, VIOLATION_CATEGORY_CODE, VIOLATION_STATUS, diff_days, total_violations)

glimpse(SDWA_vio_clean)

## I Selected the type of violation, the status (to include whether a violation is still in
## progress), the total number of violations, and the maximum violation time-span

write.csv(SDWA_vio_clean, "Data/health_violations_2015.csv")




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

## Load LCR data

LCR_samp <- read_csv("C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/R_GIS/SDWA_LCR_SAMPLES.csv")

## find the samples exceeding 0.015 mg/l lead and group by PWSID
LCR_exc <- LCR_samp %>%
  filter(SAMPLE_MEASURE > 0.015) 

## Maybe take a look!
glimpse(LCR_exc)

## get counts for number of violations by PWSID 

PWSID_LCR_exc <- LCR_exc %>%
  group_by(PWSID) %>%
  mutate(total_violations = n())

glimpse(PWSID_LCR_exc)

## Now select the max sample measure for each PSWID
PWSID_LCR_2 <- PWSID_LCR_exc %>%
  ungroup(PWSID) %>%
  filter(SAMPLE_MEASURE == max(SAMPLE_MEASURE), .by = PWSID) %>%
  distinct(PWSID, .keep_all = TRUE) 

##check that your data looks good, should have 1 of each violating PWSID and a column with total violations

## Clean data, keep only the total number of violations per PWSID and the maximum contamination in each

LCR_violations_per_PWSID <- PWSID_LCR_2 %>%
  select(PWSID, SAMPLE_MEASURE, UNIT_OF_MEASURE, total_violations, SAMPLING_END_DATE) %>%
  rename(Maximum_sample_exceedence = SAMPLE_MEASURE) %>%
  rename(Units = UNIT_OF_MEASURE)

## I also kept the date, to keep a sense of the distribution for when these violations occurred

write.csv(LCR_violations_per_PWSID, "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/R_GIS/LCR_violations_per_PWSID.csv")

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



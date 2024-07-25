
################################################################################
## Generate Safe Drinking Water indicators (LCR and Health violations)
## National Center for Environmental Economics
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

#TB
#my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

#WA
my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()

################################################################################
## LCR 
################################################################################


## Load lcr data

lcr_samp <- read_csv("data/water quality/SDWA_LCR_SAMPLES.csv")

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

write_rds(lcr_vio, "ej_service_areas/data/indicators/lcr_violations_v2.rds")

################################################################################
## Health-based Violations
################################################################################

### Load SDWA Violations data


SDWA_vio <- read_csv("data/water quality/SDWA_VIOLATIONS_ENFORCEMENT.csv")

## Filter out data to violations that started after 2015 
## Restrict to only health-based violations

sdwa_vio <- SDWA_vio %>%
  mutate(COMPL_PER_END_DATE = mdy(COMPL_PER_END_DATE),
         COMPL_PER_BEGIN_DATE = mdy(COMPL_PER_BEGIN_DATE),
         VIOL_FIRST_REPORTED_DATE = mdy(VIOL_FIRST_REPORTED_DATE))
         
sdwa_vio_2015 <- sdwa_vio[sdwa_vio$COMPL_PER_BEGIN_DATE >= "2015-01-01",]

sdwa_vio_2015 <- sdwa_vio_2015 %>%
  filter(IS_HEALTH_BASED_IND == "Y") #health based violations

## Fill null values for compliance end date to compute violation lengths
## Used date when the data was collected (in this case 07.07.2023)

sdwa_vio_2015$COMPL_PER_END_DATE[is.na(sdwa_vio_2015$COMPL_PER_END_DATE)] <- as.Date('2023-07-07') #match the date format, verify

saveRDS(sdwa_vio_2015, file = "Data/sdwa_hb_vio_2015.rds")

## Calculate the length of the violation 

sdwa_vio_2015 <- sdwa_vio_2015 %>%
  clean_names()

sdwa_vio <- sdwa_vio_2015 %>%
  mutate(calculated_rtc_date = mdy(calculated_rtc_date))

sdwa_vio$compl_per_end_date[sdwa_vio$compl_per_end_date > '2023-07-07'] <- as.Date('2023-07-07')

sdwa_vio <- sdwa_vio %>%
  mutate(diff_days = as.numeric(difftime(compl_per_end_date, compl_per_begin_date, units="days")))

## Calculate the total number of violations per PWSID, then collapse to pwsid level

sdwa_vio_pwsid <- sdwa_vio %>%
  group_by(pwsid) %>%
  mutate(dist_vio = n_distinct(violation_id), 
         dist_enf = n_distinct(enforcement_id),
         dist_vio_type = n_distinct(violation_code),
         diff_days_max = max(diff_days)) %>%
  distinct(pwsid, .keep_all = TRUE)


##Keep necessary columns

sdwa_vio_clean <- sdwa_vio_pwsid %>%
  dplyr::select(pwsid, starts_with("dist"), diff_days_max) %>%
  rename(total_violations = dist_vio)

## I Selected the type of violation, the status (to include whether a violation is still in
## progress), the total number of violations, and the maximum violation time-span

write.csv(sdwa_vio_clean, "ej_service_areas/data/indicators/health_violations_2015.csv")

saveRDS(sdwa_vio_clean, "ej_service_areas/data/indicators/health_violations_2015.rds")


################################################################################
# Demographic Indices for SDWA indicators 
# National Center for Environmental Economics
# Last edited: 11/27/2023
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
## Creating data frames at the indicator level
################################################################################

## Create a loop to get health based violations indicators

 # Set up the function call 

boundary <- c("epic", "usgs", "zc", "county")

my_df <- data.frame(risk_type = c("race", "income"))

for (i in boundary) {
  
  water_data <- readRDS(paste0("data/combined/HB_vio_", i, ".rds"))
  
  rel_risk <- water_data %>%
    filter(pop_served != 0)%>% #remove systems with no pop served
    mutate(whitepct = (1-minorpct)) %>%
    mutate(minor_served = minorpct*pop_served) %>%
    mutate(nhw_served = whitepct*pop_served) %>% #non hispanic white served 
    mutate(minor_risk = weighted.mean(total_violations, minor_served, na.rm= TRUE)) %>% #black population weight
    mutate(nhw_risk = weighted.mean(total_violations, nhw_served, na.rm= TRUE)) %>%   #non-hispanic white
    mutate(highinc = (1-lowinc)) %>% #create high income category
    mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
    mutate(lowinc_served = lowinc*pop_served) %>%
    mutate(highinc_risk = weighted.mean(total_violations, highinc_served, na.rm= TRUE)) %>% #High income weight
    mutate(lowinc_risk = weighted.mean(total_violations, lowinc_served, na.rm= TRUE)) #low income weight
  
    rel_risk_r = as.numeric(rel_risk[1, "minor_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
    rel_risk_i = as.numeric(rel_risk[1, "lowinc_risk"]) / as.numeric(rel_risk[1, "highinc_risk"])

    risks <- data.frame(risk = c(rel_risk_r, rel_risk_i))
    
my_df <- cbind(my_df, risks)

colnames(my_df)[colnames(my_df) == "risk"] <- paste0(i, "_rel_risk")

    rm(risks)
  
}

hb_risk <- my_df %>%
  mutate(vio_type = as.factor("hb"))

write.csv(my_df, file = "data/rel_risk_hb.csv")

  
################################################################################
## Lead exceedances
################################################################################

# Set up the function call 

my_df <- data.frame(risk_type = c("race", "income"))

for (i in boundary) {
  
  water_data <- readRDS(paste0("data/combined/lcr_vio_", i, ".rds"))
  
  rel_risk <- water_data %>%
    filter(pop_served != 0)%>% #remove systems with no pop served
    mutate(whitepct = (1-minorpct)) %>%
    mutate(minor_served = minorpct*pop_served) %>%
    mutate(nhw_served = whitepct*pop_served) %>% #non hispanic white served 
    mutate(minor_risk = weighted.mean(avg_pb_level, minor_served, na.rm= TRUE)) %>% #black population weight
    mutate(nhw_risk = weighted.mean(avg_pb_level, nhw_served, na.rm= TRUE)) %>%   #non-hispanic white
    mutate(highinc = (1-lowinc)) %>% #create high income category
    mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
    mutate(lowinc_served = lowinc*pop_served) %>%
    mutate(highinc_risk = weighted.mean(avg_pb_level, highinc_served, na.rm= TRUE)) %>% #High income weight
    mutate(lowinc_risk = weighted.mean(avg_pb_level, lowinc_served, na.rm= TRUE)) #low income weight
  
  rel_risk_r = as.numeric(rel_risk[1, "minor_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
  rel_risk_i = as.numeric(rel_risk[1, "lowinc_risk"]) / as.numeric(rel_risk[1, "highinc_risk"])
  
  risks <- data.frame(risk = c(rel_risk_r, rel_risk_i))
  
  my_df <- cbind(my_df, risks)
  
  colnames(my_df)[colnames(my_df) == "risk"] <- paste0(i, "_rel_risk")
  
  rm(risks)
  
}

lead_risk <- my_df %>%
  mutate(vio_type = as.factor("lead"))

write.csv(my_df, file = "data/rel_risk_lead.csv")


################################################################################
## PFAS violations
################################################################################

# Set up the function call 

my_df <- data.frame(risk_type = c("race", "income"))

for (i in boundary) {
  
  water_data <- readRDS(paste0("data/combined/pfas_vio_", i, ".rds"))
  
  rel_risk <- water_data %>%
    filter(pop_served != 0)%>% #remove systems with no pop served
    mutate(whitepct = (1-minorpct)) %>%
    mutate(minor_served = minorpct*pop_served) %>%
    mutate(nhw_served = whitepct*pop_served) %>% #non hispanic white served 
    mutate(minor_risk = weighted.mean(detection, minor_served, na.rm= TRUE)) %>% #black population weight
    mutate(nhw_risk = weighted.mean(detection, nhw_served, na.rm= TRUE)) %>%   #non-hispanic white
    mutate(highinc = (1-lowinc)) %>% #create high income category
    mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
    mutate(lowinc_served = lowinc*pop_served) %>%
    mutate(highinc_risk = weighted.mean(detection, highinc_served, na.rm= TRUE)) %>% #High income weight
    mutate(lowinc_risk = weighted.mean(detection, lowinc_served, na.rm= TRUE)) #low income weight
  
  rel_risk_r = as.numeric(rel_risk[1, "minor_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
  rel_risk_i = as.numeric(rel_risk[1, "lowinc_risk"]) / as.numeric(rel_risk[1, "highinc_risk"])
  
  risks <- data.frame(risk = c(rel_risk_r, rel_risk_i))
  
  my_df <- cbind(my_df, risks)
  
  colnames(my_df)[colnames(my_df) == "risk"] <- paste0(i, "_rel_risk")
  
  rm(risks)
  
}

pfas_risk <- my_df %>%
  mutate(vio_type = as.factor("pfas"))

write.csv(my_df, file = "data/rel_risk_pfas.csv")


################################################################################
## Disinfectant Bi-Product monitoring
################################################################################

# Set up the function call 

my_df <- data.frame(risk_type = c("race", "income"))

for (i in boundary) {
  
  water_data <- readRDS(paste0("data/combined/dbp_vio_", i, ".rds"))
  
  rel_risk <- water_data %>%
    filter(pop_served != 0)%>% #remove systems with no pop served
    mutate(whitepct = (1-minorpct)) %>%
    mutate(minor_served = minorpct*pop_served) %>%
    mutate(nhw_served = whitepct*pop_served) %>% #non hispanic white served 
    mutate(minor_risk = weighted.mean(combined_dbp, minor_served, na.rm= TRUE)) %>% #black population weight
    mutate(nhw_risk = weighted.mean(combined_dbp, nhw_served, na.rm= TRUE)) %>%   #non-hispanic white
    mutate(highinc = (1-lowinc)) %>% #create high income category
    mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
    mutate(lowinc_served = lowinc*pop_served) %>%
    mutate(highinc_risk = weighted.mean(combined_dbp, highinc_served, na.rm= TRUE)) %>% #High income weight
    mutate(lowinc_risk = weighted.mean(combined_dbp, lowinc_served, na.rm= TRUE)) #low income weight
  
  rel_risk_r = as.numeric(rel_risk[1, "minor_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
  rel_risk_i = as.numeric(rel_risk[1, "lowinc_risk"]) / as.numeric(rel_risk[1, "highinc_risk"])
  
  risks <- data.frame(risk = c(rel_risk_r, rel_risk_i))
  
  my_df <- cbind(my_df, risks)
  
  colnames(my_df)[colnames(my_df) == "risk"] <- paste0(i, "_rel_risk")
  
  rm(risks)
  
}

dbp_risk <- my_df %>%
  mutate(vio_type = as.factor("dbp"))

write.csv(my_df, file = "data/rel_risk_dbp.csv")


################################################################################
## Total Coliform detections
################################################################################

# Set up the function call 

my_df <- data.frame(risk_type = c("race", "income"))

for (i in boundary) {
  
  water_data <- readRDS(paste0("data/combined/tcr_vio_", i, ".rds"))
  
  rel_risk <- water_data %>%
    filter(pop_served != 0)%>% #remove systems with no pop served
    mutate(whitepct = (1-minorpct)) %>%
    mutate(minor_served = minorpct*pop_served) %>%
    mutate(nhw_served = whitepct*pop_served) %>% #non hispanic white served 
    mutate(minor_risk = weighted.mean(detection_share, minor_served, na.rm= TRUE)) %>% #black population weight
    mutate(nhw_risk = weighted.mean(detection_share, nhw_served, na.rm= TRUE)) %>%   #non-hispanic white
    mutate(highinc = (1-lowinc)) %>% #create high income category
    mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
    mutate(lowinc_served = lowinc*pop_served) %>%
    mutate(highinc_risk = weighted.mean(detection_share, highinc_served, na.rm= TRUE)) %>% #High income weight
    mutate(lowinc_risk = weighted.mean(detection_share, lowinc_served, na.rm= TRUE)) #low income weight
  
  rel_risk_r = as.numeric(rel_risk[1, "minor_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
  rel_risk_i = as.numeric(rel_risk[1, "lowinc_risk"]) / as.numeric(rel_risk[1, "highinc_risk"])
  
  risks <- data.frame(risk = c(rel_risk_r, rel_risk_i))
  
  my_df <- cbind(my_df, risks)
  
  colnames(my_df)[colnames(my_df) == "risk"] <- paste0(i, "_rel_risk")
  
  rm(risks)
  
}

tcr_risk <- my_df %>%
  mutate(vio_type = as.factor("tcr"))

write.csv(my_df, file = "data/rel_risk_tcr.csv")

risk_all <- rbind(hb_risk, lead_risk, pfas_risk, dbp_risk, tcr_risk)

write.csv(risk_all, file = "data/rel_risk_alltcr.csv")


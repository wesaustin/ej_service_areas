################################################################################
# Demographic Indices for SDWA indicators : sensitivity tests
# National Center for Environmental Economics
# Last edited: 6/27/2024
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
  readxl,
  janitor
)

################################################################################
##Set directories
################################################################################

#Tina
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

#WA
# my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()


################################################################################
## Creating data frames at the indicator level
################################################################################

boundary = c("county", "zc", "usgs", "epic", "hm")

indicator = c('hb', 'lcr', 'pfas', 'dbp', 'tcr', 'ars', 'nitrate')

################################################################################
## First will be all the relative risks
################################################################################

## Create a loop to get health based violations indicators

# Set up the function call 


all_risk <- data.frame()

pws_obs <- data.frame(matrix(ncol = 3, nrow = 0))

colnames(pws_obs) <- c("obs", "indicator", "boundary")

for (b in boundary) {
  
  hb_vio <- readRDS(paste0('data/combined/pwsid/hb_vio_',b ,'.rds'))
  
  lcr_vio <- readRDS(paste0('data/combined/pwsid/lcr_vio_',b ,'.rds')) %>%
    mutate(total_violations = pb_vio_count)
  
  pfas_vio <- readRDS(paste0('data/combined/pwsid/pfas_vio_',b ,'.rds')) %>%
    mutate(total_violations = pfas_count)
  
  dbp_vio <- readRDS(paste0('data/combined/pwsid/dbp_vio_',b ,'.rds')) %>%
    mutate(total_violations = combined_dbp)
  
  tcr_vio <- readRDS(paste0('data/combined/pwsid/tcr_vio_',b ,'.rds')) %>%
    mutate(total_violations = detection_share)
  
  ars_vio <- readRDS(paste0('data/combined/pwsid/arsenic_vio_',b ,'.rds')) %>%
    mutate(total_violations = arsenic)
  
  nitrate_vio <- readRDS(paste0('data/combined/pwsid/nitrate_vio_',b ,'.rds')) %>%
    mutate(total_violations = nitrate)
  
  data_list <- list(hb_vio, lcr_vio, pfas_vio, dbp_vio, tcr_vio, ars_vio, nitrate_vio)
  
  my_df <- data.frame(risk_type = c("race", "income"))
  
  for (j in 1:length(data_list)) {
    
    water_data <- data_list[[j]]
    
    i = nth(indicator, j)
    
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
    
    obs <- sum(!is.na(rel_risk$total_violations))
    
    pws_obs[nrow(pws_obs) + 1, ] = c(obs, i, b)
    
  }
  
  write.csv(my_df, file = paste0("Data/rel_risk/rel_risk_", b, ".csv"))
  
  my_df <- my_df %>% 
    mutate(boundary = as.factor(b))
  
  all_risk <- bind_rows(all_risk, my_df)  
  
}

write.csv(all_risk, file = "Data/rel_risk/rel_risk_all.csv")


all_risk <- read.csv("Data/rel_risk/rel_risk_all.csv") 

################################################################################
## Create base to subset to tier 1 and 2 in EPIC and HQ states
################################################################################

boundary = c("county", "zc", "usgs", "epic", "hm")

indicator = c('hb', 'lcr', 'pfas', 'dbp', 'tcr', 'ars', 'nitrate')

tier_12 <- read.csv("Data/demographics/epic_dems.csv") %>%
  clean_names() %>%
  filter(tier != "3") %>%
  distinct(pwsid)

hq_states <-c("CA", "NJ", "NM", "CT", "WA")


################################################################################
## Select tier 1 and tier 2 state boundaries for each type of boundary to compare
################################################################################

## Create a loop to get rel risk for tier 1 & 2 states

# Set up the function call 


all_risk <- data.frame()

pws_obs <- data.frame(matrix(ncol = 3, nrow = 0))

colnames(pws_obs) <- c("obs", "indicator", "boundary")

for (b in boundary) {
  
  hb_vio <- readRDS(paste0('data/combined/pwsid/hb_vio_',b ,'.rds'))
  
  lcr_vio <- readRDS(paste0('data/combined/pwsid/lcr_vio_',b ,'.rds')) %>%
    mutate(total_violations = pb_vio_count)
  
  pfas_vio <- readRDS(paste0('data/combined/pwsid/pfas_vio_',b ,'.rds')) %>%
    mutate(total_violations = pfas_count)
  
  dbp_vio <- readRDS(paste0('data/combined/pwsid/dbp_vio_',b ,'.rds')) %>%
    mutate(total_violations = combined_dbp)
  
  tcr_vio <- readRDS(paste0('data/combined/pwsid/tcr_vio_',b ,'.rds')) %>%
    mutate(total_violations = detection_share)
  
  ars_vio <- readRDS(paste0('data/combined/pwsid/ars_vio_',b ,'.rds')) %>%
    mutate(total_violations = arsenic)
  
  nitrate_vio <- readRDS(paste0('data/combined/pwsid/nitrate_vio_',b ,'.rds')) %>%
    mutate(total_violations = nitrate)
  
  data_list <- list(hb_vio, lcr_vio, pfas_vio, dbp_vio, tcr_vio, ars_vio, nitrate_vio)
  
  my_df <- data.frame(risk_type = c("race", "income"))
  
  for (j in 1:length(data_list)) {
    
    water_data <- data_list[[j]]
    
    i = nth(indicator, j)
    
    rel_risk <- water_data %>%
      filter(pwsid %in% tier_12$pwsid) %>%
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
    
    obs <- sum(!is.na(rel_risk$total_violations))
    
    pws_obs[nrow(pws_obs) + 1, ] = c(obs, i, b)
    
  }
  
  my_df <- my_df %>% 
    mutate(boundary = as.factor(b))
  
  all_risk <- bind_rows(all_risk, my_df)  
  
}

write.csv(all_risk, file = "Data/rel_risk/rel_risk_tier12_all.csv")

all_risk <- read.csv("Data/rel_risk/rel_risk_tier12_all.csv") 


################################################################################
## Select high quality state boundaries for each type of boundary to compare
################################################################################

## Create a loop to get rel risk for HQ states

# Set up the function call 

all_risk <- data.frame()

pws_obs <- data.frame(matrix(ncol = 3, nrow = 0))

colnames(pws_obs)<- c("obs", "indicator", "boundary")

for (b in boundary) {
  
  hb_vio <- readRDS(paste0('data/combined/pwsid/hb_vio_',b ,'.rds'))
  
  lcr_vio <- readRDS(paste0('data/combined/pwsid/lcr_vio_',b ,'.rds')) %>%
    mutate(total_violations = pb_vio_count)
  
  pfas_vio <- readRDS(paste0('data/combined/pwsid/pfas_vio_',b ,'.rds')) %>%
    mutate(total_violations = pfas_count)
  
  dbp_vio <- readRDS(paste0('data/combined/pwsid/dbp_vio_',b ,'.rds')) %>%
    mutate(total_violations = combined_dbp)
  
  tcr_vio <- readRDS(paste0('data/combined/pwsid/tcr_vio_',b ,'.rds')) %>%
    mutate(total_violations = detection_share)
  
  ars_vio <- readRDS(paste0('data/combined/pwsid/ars_vio_',b ,'.rds')) %>%
    mutate(total_violations = arsenic)
  
  nitrate_vio <- readRDS(paste0('data/combined/pwsid/nitrate_vio_',b ,'.rds')) %>%
    mutate(total_violations = nitrate)
  
  data_list <- list(hb_vio, lcr_vio, pfas_vio, dbp_vio, tcr_vio, ars_vio, nitrate_vio)
  
  my_df <- data.frame(risk_type = c("race", "income"))
  
  for (j in 1:length(data_list)) {
    
    water_data <- data_list[[j]]
    
    i = nth(indicator, j)
    
    rel_risk <- water_data %>%
      filter(st_abb %in% hq_states) %>%
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
    
    obs <- sum(!is.na(rel_risk$total_violations))
    
    pws_obs[nrow(pws_obs) + 1, ] = c(obs, i, b)
    
  }
  
  my_df <- my_df %>% 
    mutate(boundary = as.factor(b))
  
  all_risk <- bind_rows(all_risk, my_df)  

}

write.csv(all_risk, file = "Data/rel_risk/rel_risk_HQ_all.csv")

all_risk <- all_risk %>%
  group_by(race_cat)

all_risk <- read.csv("Data/rel_risk/rel_risk_HQ_all.csv") 

################################################################################

################################################################################
## Create base to subset to tier 1 and 2 in EPIC and HQ states
################################################################################

boundary = c("county", "zc", "usgs", "epic", "hm")

indicator = c('hb', 'lcr', 'pfas', 'dbp', 'tcr', 'ars', 'nitrate')

tier_23 <- read.csv("Data/demographics/epic_dems.csv") %>%
  clean_names() %>%
  filter(tier != "1") %>%
  distinct(pwsid)

################################################################################
## Select tier 2/3 state boundaries for each type of boundary to compare
################################################################################

## Create a loop to get rel risk for tier 2 & 3 states

# Set up the function call 


all_risk <- data.frame()

pws_obs <- data.frame(matrix(ncol = 3, nrow = 0))

colnames(pws_obs) <- c("obs", "indicator", "boundary")

for (b in boundary) {
  
  hb_vio <- readRDS(paste0('data/combined/pwsid/hb_vio_',b ,'.rds'))
  
  lcr_vio <- readRDS(paste0('data/combined/pwsid/lcr_vio_',b ,'.rds')) %>%
    mutate(total_violations = pb_vio_count)
  
  pfas_vio <- readRDS(paste0('data/combined/pwsid/pfas_vio_',b ,'.rds')) %>%
    mutate(total_violations = pfas_count)
  
  dbp_vio <- readRDS(paste0('data/combined/pwsid/dbp_vio_',b ,'.rds')) %>%
    mutate(total_violations = combined_dbp)
  
  tcr_vio <- readRDS(paste0('data/combined/pwsid/tcr_vio_',b ,'.rds')) %>%
    mutate(total_violations = detection_share)
  
  ars_vio <- readRDS(paste0('data/combined/pwsid/ars_vio_',b ,'.rds')) %>%
    mutate(total_violations = arsenic)
  
  nitrate_vio <- readRDS(paste0('data/combined/pwsid/nitrate_vio_',b ,'.rds')) %>%
    mutate(total_violations = nitrate)
  
  data_list <- list(hb_vio, lcr_vio, pfas_vio, dbp_vio, tcr_vio, ars_vio, nitrate_vio)  
  my_df <- data.frame(risk_type = c("race", "income"))
  
  for (j in 1:length(data_list)) {
    
    water_data <- data_list[[j]]
    
    i = nth(indicator, j)
    
    rel_risk <- water_data %>%
      filter(pwsid %in% tier_23$pwsid) %>%
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
    
    obs <- sum(!is.na(rel_risk$total_violations))
    
    pws_obs[nrow(pws_obs) + 1, ] = c(obs, i, b)
    
  }
  
  my_df <- my_df %>% 
    mutate(boundary = as.factor(b))
  
  all_risk <- bind_rows(all_risk, my_df)  
  
}

write.csv(all_risk, file = "Data/rel_risk/rel_risk_tier23_all.csv")

all_risk <- read.csv("Data/rel_risk/rel_risk_tier23_all.csv") 


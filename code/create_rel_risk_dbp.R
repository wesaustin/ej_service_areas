## dbp sensitivity analyses

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
  MASS , #For regressions and modeling
  dplyr,
  readxl
)


# Look at changes in relative disparity metrics across dbp measure 

boundary = c("epic", "usgs", "zc", "county", "hm")

vars = c('haa5', 'tthm', 'haa5_alt', 'tthm_alt', 'combined_dbp')

all_risk <- data.frame()

for (b in boundary) {
  
  dbp_vio <- readRDS(paste0('data/combined/pwsid/dbp_vio_',b ,'.rds')) %>%
    mutate(pop_served = as.numeric(pop_served)) %>%
    filter(pop_served != 0) %>%
    rename(frac_poc = minorpct) %>%
    mutate(frac_nhw = 1-frac_poc) %>%
    rename(frac_lowinc = lowinc) %>%
    mutate(frac_highinc = 1-frac_lowinc) %>%
    mutate(poc_served = frac_poc*pop_served) %>%
    mutate(black_served = frac_black*pop_served) %>% #black population served
    mutate(amer_ind_served = frac_amerind*pop_served) %>% #american indian served
    mutate(asian_served = frac_asian*pop_served) %>% #asian served
    mutate(pac_isl_served = frac_pacisl*pop_served) %>% #pacific islander served
    mutate(hispanic_served = frac_hisp*pop_served) %>% #hispanic served
    mutate(nhw_served = frac_nhw*pop_served) %>% #non hispanic white served 
    mutate(highinc_served = frac_highinc*pop_served) %>% #Find total population served in each category
    mutate(lowinc_served = frac_lowinc*pop_served) 
  
  my_df <- data.frame(dem_cat = c("amerind", "asian", "black", "hisp", "pacisl", "poc", "lowinc"))
  
  for (i in vars) {
    
    
    rel_risk <- dbp_vio 
    
    names(rel_risk)[names(rel_risk) == i] <- "total_violations"
    
    rel_risk <- rel_risk %>%
      # mutate(total_violations = as.numeric(total_violations)) %>%
      mutate(poc_risk = weighted.mean(total_violations, poc_served, na.rm= TRUE)) %>% #black population weight
      mutate(black_risk = weighted.mean(total_violations, black_served, na.rm= TRUE)) %>% #black population weight
      mutate(amer_ind_risk = weighted.mean(total_violations, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
      mutate(asian_risk = weighted.mean(total_violations, asian_served, na.rm= TRUE)) %>% #asian pop weight
      mutate(pac_isl_risk = weighted.mean(total_violations, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
      mutate(hispanic_risk = weighted.mean(total_violations, hispanic_served, na.rm= TRUE)) %>% # hispanic
      mutate(nhw_risk = weighted.mean(total_violations, nhw_served, na.rm= TRUE)) %>%   #non-hispanic white
      mutate(highinc_risk = weighted.mean(total_violations, highinc_served, na.rm= TRUE)) %>% #High income weight
      mutate(lowinc_risk = weighted.mean(total_violations, lowinc_served, na.rm= TRUE)) #low income weight
    
    
    amer_ind = as.numeric(rel_risk[1, "amer_ind_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
    asian = as.numeric(rel_risk[1, "asian_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
    black = as.numeric(rel_risk[1, "black_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
    hisp = as.numeric(rel_risk[1, "hispanic_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
    pac_isl = as.numeric(rel_risk[1, "pac_isl_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
    poc_risk = as.numeric(rel_risk[1, "poc_risk"]) / as.numeric(rel_risk[1, "nhw_risk"])
    lowinc_risk = as.numeric(rel_risk[1, "lowinc_risk"]) / as.numeric(rel_risk[1, "highinc_risk"])
    
    risks <- data.frame(risk = c(amer_ind, asian, black, hisp, pac_isl, poc_risk, lowinc_risk)) 
    
    my_df <- cbind(my_df, risks) %>%
      mutate(boundary = as.factor(b))
    
    colnames(my_df)[colnames(my_df) == "risk"] <- paste0(i, "_rel_risk")
    
  }
  
  all_risk <- bind_rows(all_risk, my_df) 
  
}

write.csv(all_risk, file = "Data/rel_risk/rel_risk_dbp_sensitivity.csv")


all_risk <- all_risk %>%
  arrange(dem_cat, boundary) %>%
  relocate(boundary, .after = dem_cat)

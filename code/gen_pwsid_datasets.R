################################################################################
##Generate PWSID level datasets
################################################################################

################################################################################
##Health violations
################################################################################
# Load health violations data and sb_dems_v3 data (containing demographic / shapefile info)

HB_vio <- read.csv("Data/health_violations2015.csv")

HB_vio <- HB_vio %>%
  rename(pwsid = PWSID)

pwsid_dem <- read.csv("Data/epic_boundaries/sb_dems_v3.csv")

pwsid_dem <- pwsid_dem %>% 
  rename(MINORPCT = Minority) %>%
  rename(LOWINC = Low.Income) %>%
  rename(population_served_count = Pop..Count)

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

HB_vio <- left_join(pwsid_dem, HB_vio, by = "pwsid", relationship = "many-to-many")

# For tracts with PWSIDs, replace null values with zeroes

HB_vio$total_violations[is.na(HB_vio$total_violations)] <- 0
HB_vio$diff_days[is.na(HB_vio$diff_days)] <- 0

write_rds(HB_vio, "Data/HB_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(HB_vio) #remove files if unnecessary



################################################################################
##LCR
################################################################################
# Load lcr violations data and sb_dems_v3 data (containing demographic / shapefile info)

lcr_vio <- read.csv("Data/LCR_violations_per_PWSID.csv")

lcr_vio <- lcr_vio %>%
  rename(pwsid = PWSID)

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

lcr_vio <- left_join(pwsid_dem, lcr_vio, by = "pwsid", relationship = "many-to-many")

# For tracts with PWSIDs, replace null values with zeroes

lcr_vio$total_violations[is.na(lcr_vio$total_violations)] <- 0

lcr_vio$Maximum_sample_exceedence[is.na(lcr_vio$Maximum_sample_exceedence)] <- 0


write_rds(lcr_vio, "Data/lcr_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(lcr_vio) #remove files if unnecessary


################################################################################
##PFAS
################################################################################
# Load pfas violations data and sb_dems_v3 data (containing demographic / shapefile info)

pfas_vio <- read.csv("Data/indicators_pfas.csv")

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

pfas_vio <- left_join(pwsid_dem, pfas_vio, by = "pwsid", relationship = "many-to-many")

# For tracts with PWSIDs, replace null values with zeroes

pfas_vio$concentration_sum[is.na(pfas_vio$concentration_sum)] <- 0
pfas_vio$total_samples[is.na(pfas_vio$total_samples)] <- 0
pfas_vio$detection[is.na(pfas_vio$detection)] <- 0
pfas_vio$pfas_count[is.na(pfas_vio$pfas_count)] <- 0
pfas_vio$max[is.na(pfas_vio$max)] <- 0

write_rds(pfas_vio, "Data/pfas_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(pfas_vio) #remove files if unnecessary

################################################################################
##DBP
################################################################################

# Load dbp violations data and sb_dems_v3 data (containing demographic / shapefile info)

dbp_vio <- read.csv("Data/indicator_dbp.csv")

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

dbp_vio <- left_join(pwsid_dem, dbp_vio, by = "pwsid", relationship = "many-to-many")

# For tracts with PWSIDs, replace null values with zeroes

dbp_vio$combined_dbp[is.na(dbp_vio$combined_dbp)] <- 0

write_rds(dbp_vio, "Data/dbp_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(dbp_vio) #remove files if unnecessary


################################################################################
##TCR 
################################################################################
tcr_vio <- read.csv("Data/indicator_tcr.csv")

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

tcr_vio <- left_join(pwsid_dem, tcr_vio, by = "pwsid", relationship = "many-to-many")

tcr_vio$detection_share[is.na(tcr_vio$detection_share)] <- 0

write_rds(tcr_vio, "Data/tcr_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

rm(tcr_vio)
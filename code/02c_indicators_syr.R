
################################################################################
# Code to Generate Drinking Water Indicators from SYR 3 and 4 
# National Center for Environmental Economics
# Last edited: 8/31/23
################################################################################


# load packages

library(readr)
library(dplyr)
library(ggplot2)

# Set file directory 

syr_dir <- 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/mdi/syr/'
output_dir <- 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/ej_service_areas/data/indicators/'


################################################################################
################################################################################
# Load raw Data for Indicators
################################################################################
################################################################################


################################################################################
# Six Year Review 3
################################################################################

load(paste0(syr_dir,"syr3.RData"))

# Tabulate Analyte Names
syr3 %>%
  group_by(analyte_name) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3)) %>%
  print(n = 120)

#write.csv(syr3, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/mdi/syr/syr3.csv", row.names=FALSE, na="")


# filter out DBPs and bacterial samples 
dbp3 <- syr3 %>%  
    filter(analyte_name == c("TTHM","TOTAL HALOACETIC ACIDS (HAA5)",
                             "TRICHLOROACETIC ACID",  "MONOBROMOACETIC ACID","MONOCHLOROACETIC ACID","DICHLOROACETIC ACID","DIBROMOACETIC ACID",
                             "CHLOROFORM","BROMOFORM","BROMODICHLOROMETHANE","DIBROMOCHLOROMETHANE")) 

bact3 <- syr3 %>%  
  filter(analyte_name == c("COLIFORM (TCR)","E. COLI","HETEROTROPHIC BACTERIA (HPC OR SPC)")) 

# clear memory space 
rm(syr3)

################################################################################
# Six Year Review 4
################################################################################

load(paste0(syr_dir,"syr4.RData"))

# Tabulate Analyte Names
data %>%
  group_by(analyte_name) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3)) %>%
  print(n = 120)

# filter out DBPs and bacterial samples 
dbp4 <- data %>%  
  filter(analyte_name == c("TTHM","TOTAL HALOACETIC ACIDS (HAA5)",
                           "TRICHLOROACETIC ACID",  "MONOBROMOACETIC ACID","MONOCHLOROACETIC ACID","DICHLOROACETIC ACID","DIBROMOACETIC ACID",
                           "CHLOROFORM","BROMOFORM","BROMODICHLOROMETHANE","DIBROMOCHLOROMETHANE")) 


bact4 <- data %>%  
  filter(analyte_name == c("COLIFORM (TCR)","E. COLI","HETEROTROPHIC BACTERIA (HPC OR SPC)")) 

# clear space 
rm(data)  


################################################################################
# Combine Datasets 
################################################################################


# Standardize column names
dbp3 <- dbp3 %>%
  rename(analyte_code = analyte_id) 
bact3 <- bact3 %>%
  rename(analyte_code = analyte_id) 

# append datasets 
dbp <- rbind(dbp3, dbp4) %>%
  rename(pwsid = pws_id) 

bact <- rbind(bact3, bact4) %>%
  rename(pwsid = pws_id) 

rm(dbp3, bact3, dbp4, bact4) # clear space 


################################################################################
# Export data files for use in other project 
################################################################################


write.csv(bact, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/6yr review/processed/syr_col.csv", row.names=FALSE)
write.csv(dbp, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/6yr review/processed/syr_dbp.csv", row.names=FALSE)

################################################################################
# Generate Indicators 
################################################################################



# DBP Indicators

# Create a PWS-Specific Average for TTHM and HAA5
# Then, create sum of TTHM and HAA5 and collapse to PWS level 

# First ensure that all units of measurement are the same 

dbp %>%
  group_by(unit) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3)) %>%
  print(n = 120)


dbp_indicator_tthm <- dbp %>%
  filter(analyte_name == c("TTHM")) %>%
  mutate(result_value = ifelse(detect == 0, 0, result_value)) %>%
  group_by(pwsid ) %>%
  mutate(tthm= mean(result_value)) %>%
  distinct(pwsid, .keep_all = TRUE) %>%
  select(pwsid, tthm)

dbp_indicator_haa5 <- dbp %>%
  filter(analyte_code == c("2456")) %>%
  mutate(result_value = ifelse(detect == 0, 0, result_value)) %>%
  group_by(pwsid ) %>%
  mutate(haa5= mean(result_value)) %>%
  distinct(pwsid, .keep_all = TRUE) %>%
  select(pwsid, haa5)


join_dbp_inds <- left_join(dbp_indicator_tthm, dbp_indicator_haa5, by = "pwsid" )
join_dbp_inds  <- join_dbp_inds %>%
  mutate(combined_dbp = tthm + haa5)



# Total Coliform Indicator


tcr_indicator <- bact %>%
  filter(analyte_name == c("COLIFORM (TCR)")) %>%
  group_by(pwsid ) %>%
  mutate(total_samples = n()) %>%
  mutate(detection = ifelse(presence_indicator_code == "P", 1, 0)) 

tcr_indicator <- tcr_indicator %>%
  group_by(pwsid ) %>%
  mutate(detection_share = mean(detection))   %>%
  distinct(pwsid, .keep_all = TRUE) %>%
  select(pwsid, detection_share)
  
  

# Plot the indicators to make sure they look normal 

hist(join_dbp_inds$combined_dbp, 
     main ="Total DBP Concentration by Water System", 
     xlab="Combined Concentration (ug/l)", 
     col = "lightblue",
     border = "black")

hist(tcr_indicator$detection_share, 
     main ="Total Coliform Detection Share by System", 
     xlab="Detection Share", 
     col = "lightblue",
     border = "black")


# Export the indicators for other work 

write.csv(join_dbp_inds, paste0(output_dir,"indicator_dbp.csv"), row.names=FALSE )
write.csv(tcr_indicator, paste0(output_dir,"indicator_tcr.csv"), row.names=FALSE )


# Have a great day!

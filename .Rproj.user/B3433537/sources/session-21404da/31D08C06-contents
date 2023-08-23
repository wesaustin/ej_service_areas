

################################################################################
# National Center for Environmental Economics
# Last edited: 8/8/23
################################################################################


# load packages

library(readr)
library(dplyr)


# Set file directory 


################################################################################
# Import SDWIS Information
################################################################################


# Note change the below to a directory where you've saved these files. For convenience, I also moved these to the 
# NCEE - Water System Service Boundaries\data\water quality folder. 

SDWA_LCR_SAMPLES <- read_csv("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/sdwis/SDWA_LCR_SAMPLES.csv")
SDWA_violations <- read_csv("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/sdwis/SDWA_Violations.csv")


# Tabulate variable
SDWA_violations %>%
  group_by(HEALTH_BASED) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3)) %>%
  print(n = 100)

# Save just the health-based violations 
SDWA_violations <- SDWA_violations %>%
  filter(HEALTH_BASED == "Y") 

save(SDWA_violations, file = "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/water quality/SDWA_vios_healthbased.Rdata")


################################################################################
# Create Water System Indicators
################################################################################

# An indicator is a simple metric that summarizes a lot of information in one number. 
# In many cases, simple is better. An indicator can also be helpful 
# in facilitating comparisons across different groups or areas.  

# We plant to create at least five indicators of drinking water quality across water systems.

# These are:
#   •	PFAS (UCMR and PFAS Analytic Tools) 
#   •	Total Coliforms (SYR 3 & 4)
#   •	TTHM and HAA5 (SYR 3 & 4)
#   •	Health-based violations (SDWA_violations)
#   •	Lead action level exceedances (SDWA_LCR_SAMPLES)

# In this program, we will focus on the latter two, as these are the likely indicators 
# that will eventually be adopted for EJSCREEN. 


# Part 1. Lead Action Level Exceedences. 

# Using the SDWA_LCR_SAMPLES file, generate an variable for the total number of times a 
# 90th percentile sample (sample_result) is above 0.015 mg/l for lead (i.e., pb90). 
# Simplify this and save as a dataframe with one observation per PWSID and one variable 
# for the number of exceedences. Optionally, you could create a variable for the highest 90th
# percentile that was observed per system or any other variable of interest. 






# Part 2. Health based violations

# Using the SDWA_violations file, filter over just violations that are considered 
# health-based. These aren't the only ones that could have health implications, but that's 
# for another time. Filter to violations that started after 2015. Create a variable for 
# the length of time of each violation. Create a variable for the number of times a
# violation occurred happened per system. Collapse the dataframe to be one observation 
# per water system, where there are variables for  







################################################################################
# Map the two indicators 
################################################################################

# This will not be easy to view if we use the actual service areas, hence I recommend
# we collapse all of these to census tract level. This will require using information in 
# (service areas folder/data/demographics/sb_dems_area_v3), merging PWSIDS to CBG IDs, 
# and then collapsing to the census tract level. 




################################################################################
# Create bivariate maps over income and % people of color for each indicator 
################################################################################






################################################################################
# Create Population-Weighted Demographic Averages per Indicator 
################################################################################


# This will require information on the population served per water system 
# (can be found in the service areas folder/data/demographics/sb_dems_v3), and then creating 
# a population-weighted average per group. 







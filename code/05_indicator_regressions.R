

################################################################################
# Regressions for indicators
# National Center for Environmental Economics
# Latest update: 1/23/2024
################################################################################

################################################################################
## Load packages: 
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  raster, # raster data operations
  exactextractr, # fast raster data extraction for polygons
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  stargazer, # regression table generation
  future.apply, # parallel computation
  cowplot, # for bivariate mapping
  stringr, # string manipulation
  magrittr,
  janitor,
  MASS #for regressions
)

################################################################################
##Set directories
################################################################################

#TB
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

#WA
#my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()

# Model Variables: 

# Environment: Ozone, PM2.5, Superfund Site, Hazardous Waste, Underground Waste Storage
# Wasterwater distribution, Toxic Release site, Lead

# Demographic: % Black, % Hispanic, % Pacific Islander, % American Indian, % Asian, 
# % Low Income, System Size

################################################################################
## Load data
################################################################################

sys_code <- read.csv("Data/SDWA_PUB_WATER_SYSTEMS.csv") %>%
  dplyr::select(PWSID, GW_SW_CODE) %>%
  clean_names %>%
  mutate(source_gw = ifelse(gw_sw_code == "GW", 1, 0))

################################################################################

## Health violations
################################################################################

# Dataset on health-based violations (since 2015, national) 

HB_vio <- readRDS("Data/combined/pwsid/HB_vio_epic.rds") %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))


#Negative binomial regression
summary(hb_dem_nb <- glm.nb(total_violations ~  minorpct +  lowinc + primacy_type + SystemSize + source_gw, 
                            data = HB_vio))


summary(hb_dem_poi <- glm(total_violations ~  frac_black + frac_hisp + frac_pacisl + 
                            frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
                          family = "poisson",
                          data = HB_vio))

# summary(hb_dem_nb <- glm.nb(total_violations ~  frac_black + frac_black* lowinc + primacy_type + frac_hisp + frac_hisp* lowinc + primacy_type +
#                               lowinc + primacy_type + SystemSize + source_gw,
#                      data = HB_vio))

# frac_hisp + 

# The above does not converge, likely due to multicolinearity... When I take out hisp the code works

summary(hb_env_poi <- glm(total_violations ~ pre1960pct + ozone + pm25 + ptsdf + 
                              pwdis + pnpl + state_code, family = 'poisson',
                            data = HB_vio))

# Clear memory
rm(HB_vio)

################################################################################
## Load Data 
## lcr violations 
################################################################################

lcr_vio <- read_rds("Data/combined/pwsid/lcr_vio_epic.rds")  %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

## Lead violations (count)

summary(lcr_dem_poi <- glm(pb_vio_count ~ frac_black + frac_hisp + frac_pacisl + 
                           frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
                          family = "poisson", 
                         data = lcr_vio))

summary(lcr_env_poi <- glm(pb_vio_count ~ pre1960pct + ozone + pm25 + ptsdf + 
                               pwdis + pnpl + state_code, family = 'poisson',
                            data = lcr_vio))

## Lead levels

# summary(lcr_dem_lm <- glm(avg_pb_level ~ frac_black + frac_hisp + frac_pacisl + 
#                              frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw,
#                           data = lcr_vio))
# 
# summary(lcr_env_lm <- glm(avg_pb_level ~ pre1960pct + ozone + pm25 + ptsdf + 
#                             pwdis + pnpl + source_gw,
#                           data = lcr_vio))
rm(lcr_vio)


################################################################################
## Load Data 
## PFAS violations
################################################################################

pfas_vio <- read_rds("Data/combined/pwsid/pfas_vio_epic.rds")  %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

# summary(pfas_dem_plm <- glm(detection ~ frac_black + frac_hisp + frac_pacisl + 
#                          frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
#                        family = binomial(link="probit"), data = pfas_vio))

summary(pfas_dem_poi <- glm(pfas_count ~ frac_black + frac_hisp + frac_pacisl + 
                            frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
                          family = "poisson",
                          data = pfas_vio))

summary(pfas_env_poi <- glm(pfas_count ~ pre1960pct + ozone + pm25 + ptsdf + 
                             pwdis + pnpl + state_code, 
                           family = "poisson", data = pfas_vio))

# Clear memory
rm(pfas_vio)


################################################################################
## Load Data 
## DBP violations
################################################################################

dbp_vio <- read_rds("Data/combined/pwsid/dbp_vio_epic.rds")  %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

summary(dbp_dem_lm <- glm(combined_dbp ~ frac_black + frac_hisp + frac_pacisl + 
                       frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
                     data = dbp_vio))

summary(dbp_env_lm <- glm(combined_dbp ~ pre1960pct + ozone + pm25 + ptsdf + 
                           pwdis + pnpl + state_code,
                     data = dbp_vio))


# Clear memory
rm(dbp_vio)


################################################################################
## Load Data 
## TCR violations
################################################################################

tcr_vio <- read_rds("Data/combined/pwsid/tcr_vio_epic.rds") %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

summary(tcr_dem_lm <- glm(detection_share ~ frac_black + frac_hisp + frac_pacisl + 
                       frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
                      # family = binomial(link = "logit"),
                     data = tcr_vio))

summary(tcr_env_lm <- glm(detection_share ~ pre1960pct + ozone + pm25 + ptsdf + 
                           pwdis + pnpl + state_code, 
                     #    family = binomial(link = "logit"),
                     data = tcr_vio))

rm(tcr_vio)

################################################################################
## Regression tables
################################################################################

library(stargazer)

#Dem regressions
stargazer(hb_dem_poi, lcr_dem_poi, pfas_dem_poi, dbp_dem_lm, tcr_dem_lm, title = "Demographic Regression Results", align = FALSE,
          column.labels = c("Health-based", "Lead", "PFAS", "DBP", "TCR"),
          covariate.labels = c("\\% Black", "\\% Hispanic", "\\% Pacific Islander", "\\% Asian", 
                               "\\% American Indian", "\\% Low income^{+}", "Tribal System", "Large system^{++}", "Small system", "Very Large system", "Very small system","Groundwater"),
          omit = 'state_code', 
          omit.labels = "State control",
          keep.stat = c("n", "rsq"))

#notes = "^{+} Low income refers to the Pct of people below 2X the Federal Poverty Limit. ^{++} System size based on population served. Very small is fewer than
#         500, Small fewer than 3,300, Medium fewer than 10,000 and Large between 10,000 and 100,000. Regressions based on EPIC boundary specification.")


#Env regressions
stargazer(hb_env_poi, lcr_env_poi, pfas_env_poi, dbp_env_lm, tcr_env_lm, title = "Environmental Regression Results", align = FALSE,
          column.labels = c("Health-based", "Lead", "PFAS", "DBP", "TCR"),
          covariate.labels = c("Lead Paint", "Ozone", "PM_{2.5}", "Toxic Release Facility", "
                         Wastewater discharge", "Superfund Site"),
          omit = 'state_code', 
          omit.labels = "State control",
          keep.stat = c("n", "rsq"))


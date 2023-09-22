

################################################################################
# Regressions for indicators
# National Center for Environmental Economics
# Latest update: 9/20/2023
################################################################################

################################################################################
## Load packages: 
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  sf, # vector data operations
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
  MASS #for regressions
)

################################################################################
##Set directories
################################################################################

#TB
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

#WA
my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

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
## Health violations
################################################################################

# Dataset on health-based violations (since 2015, national) 

HB_vio <- readRDS("Data/combined/HB_vio_combined.rds") 

HB_vio <- HB_vio %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))


#Negative binomial regression

summary(hb_dem_nb <- glm.nb(total_violations ~ black_pct + hispanic_pct + pac_isl_pct + 
                              asian_pct + amer_ind_pct + lowinc + SystemSize, 
                     data = HB_vio))

summary(hb_env_nb <- glm.nb(total_violations ~ lead + ozone + pm + tsd_facility + 
                              ww_discharge + npl,
                            data = HB_vio))

# Clear memory
rm(HB_vio)

################################################################################
## Load Data 
## lcr violations 
################################################################################

lcr_vio <- read_rds("Data/combined/lcr_vio_combined.rds") %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))


summary(lcr_dem_nb <- glm.nb(total_violations ~ black_pct + hispanic_pct + pac_isl_pct + 
                           asian_pct + amer_ind_pct + lowinc + SystemSize, 
                         data = lcr_vio))

summary(lcr_env_nb <- glm.nb(total_violations ~ lead + ozone + pm + tsd_facility + 
                              ww_discharge + npl,
                            data = lcr_vio))
rm(lcr_vio)


################################################################################
## Load Data 
## PFAS violations
################################################################################

pfas_vio <- read_rds("Data/combined/pfas_vio_combined.rds") %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

summary(pfas_dem_lm <- glm(detection ~ black_pct + hispanic_pct + pac_isl_pct + 
                         asian_pct + amer_ind_pct + lowinc + SystemSize, 
                       family = binomial(link="probit"), data = pfas_vio))

summary(pfas_env_lm <- glm(detection ~ lead + ozone + pm + tsd_facility + 
                             ww_discharge + npl, 
                       family = binomial(link="probit"), data = pfas_vio))

# Clear memory
rm(pfas_vio)


################################################################################
## Load Data 
## DBP violations
################################################################################

dbp_vio <- read_rds("Data/combined/dbp_vio_combined.rds") %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

summary(dbp_dem_lm <- lm(combined_dbp ~ black_pct + hispanic_pct + pac_isl_pct + 
                       asian_pct + amer_ind_pct + lowinc + SystemSize, 
                     data = dbp_vio))

summary(dbp_env_lm <- lm(combined_dbp ~ lead + ozone + pm + tsd_facility + 
                           ww_discharge + npl, 
                     data = dbp_vio))


# Clear memory
rm(dbp_vio)


################################################################################
## Load Data 
## TCR violations
################################################################################

tcr_vio <- read_rds("Data/combined/tcr_vio_combined.rds") %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

summary(tcr_dem_lm <- lm(detection_share ~ black_pct + hispanic_pct + pac_isl_pct + 
                       asian_pct + amer_ind_pct + lowinc + SystemSize, 
                     data = tcr_vio))

summary(tcr_env_lm <- lm(detection_share ~ lead + ozone + pm + tsd_facility + 
                         ww_discharge + npl, 
                     data = tcr_vio))

rm(tcr_vio)

################################################################################
## Regression tables
################################################################################

#Dem regressions
stargazer(hb_dem_nb, lcr_dem_nb, pfas_dem_lm, dbp_dem_lm, tcr_dem_lm, title = "Demographic Regression Results", align = FALSE,
          column.labels = c("Health-Based", "lcr", "PFAS", "DBP", "TCR"),
          covariate.labels = c("Pct Black", "Pct Hispanic", "Pct Pacific Islander", "Pct Asian", 
                               "Pct American Indian", "Pct Low income^{+}", "Medium system^{++}", "Small system", "Large system", "Very small system"),
          keep.stat = c("n", "rsq"))

#notes = "^{+} Low income refers to the Pct of people below 2X the Federal Poverty Limit. ^{++} System size based on population served. Very small is fewer than
#         500, Small fewer than 3,300, Medium fewer than 10,000 and Large between 10,000 and 100,000. Regressions based on EPIC boundary specification.")


#Env regressions
stargazer(hb_env_nb, lcr_env_nb, pfas_env_lm, dbp_env_lm, tcr_env_lm, title = "Environmental Regression Results", align = FALSE,
          column.labels = c("Health-Based", "lcr", "PFAS", "DBP", "TCR"),
          covariate.labels = c("Lead", "Ozone", "PM_{2.5}", "Toxic Release Facility", "
                         Wastewater discharge", "Superfund Site"),
          keep.stat = c("n", "rsq"))





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
<<<<<<<< Updated upstream:code/03_indicator_regressions.R
  MASS , #for regressions
  stargazer
)


## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
# Unload stargazer if loaded
detach("package:stargazer",unload=T)
# Delete it
remove.packages("stargazer")
# Download the source
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# Unpack
untar("stargazer_5.2.3.tar.gz")
# Read the sourcefile with .inside.bracket fun
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# Move the length check 5 lines up so it precedes is.na(.)
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""
# Save back
writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# Compile and install the patched package
install.packages("stargazer", repos = NULL, type="source")


========
  MASS #for regressions
)

>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
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
<<<<<<<< Updated upstream:code/03_indicator_regressions.R
## Load system data on water source 
========
## Load data
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
################################################################################

sys_code <- read.csv("data/demographics/SDWA_PUB_WATER_SYSTEMS.csv") %>%
  dplyr::select(PWSID, GW_SW_CODE) %>%
  clean_names %>%
  mutate(source_gw = ifelse(gw_sw_code == "GW", 1, 0))

################################################################################
<<<<<<<< Updated upstream:code/03_indicator_regressions.R
========

>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
## Health violations
################################################################################

# Dataset on health-based violations (since 2015, national) 

<<<<<<<< Updated upstream:code/03_indicator_regressions.R
HB_vio <- readRDS("data/combined/HB_vio_epic.rds") %>%
========
HB_vio <- readRDS("data/combined/HB_vio_hm_epic.rds") %>%
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
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


<<<<<<<< Updated upstream:code/03_indicator_regressions.R
summary(hb_dem_poi <- glm(total_violations ~  frac_amerind + frac_black + frac_asian + frac_hisp + frac_pacisl + 
                              lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
========
summary(hb_dem_poi <- glm(total_violations ~  frac_black + frac_hisp + frac_pacisl + 
                            frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
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
<<<<<<<< Updated upstream:code/03_indicator_regressions.R
## lcr violations 
################################################################################

lcr_vio <- read_rds("data/combined/lcr_vio_epic.rds")  %>%
========
## Load Data 
## lcr violations 
################################################################################

lcr_vio <- read_rds("Data/combined/pwsid/lcr_vio_epic.rds")  %>%
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

## Lead violations (count)

<<<<<<<< Updated upstream:code/03_indicator_regressions.R
summary(lcr_dem_poi <- glm(pb_vio_count ~ frac_amerind + frac_black + frac_asian + frac_hisp + frac_pacisl + 
                              lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
========
summary(lcr_dem_poi <- glm(pb_vio_count ~ frac_black + frac_hisp + frac_pacisl + 
                           frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
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
<<<<<<<< Updated upstream:code/03_indicator_regressions.R

========
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
rm(lcr_vio)


################################################################################
<<<<<<<< Updated upstream:code/03_indicator_regressions.R
## PFAS detections
################################################################################

pfas_vio <- read_rds("data/combined/pfas_vio_epic.rds")  %>%
========
## Load Data 
## PFAS violations
################################################################################

pfas_vio <- read_rds("Data/combined/pwsid/pfas_vio_epic.rds")  %>%
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
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

<<<<<<<< Updated upstream:code/03_indicator_regressions.R
summary(pfas_dem_poi <- glm(pfas_count ~ frac_amerind + frac_black + frac_asian + frac_hisp + frac_pacisl + 
                              lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
========
summary(pfas_dem_poi <- glm(pfas_count ~ frac_black + frac_hisp + frac_pacisl + 
                            frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
                          family = "poisson",
                          data = pfas_vio))

summary(pfas_env_poi <- glm(pfas_count ~ pre1960pct + ozone + pm25 + ptsdf + 
                             pwdis + pnpl + state_code, 
                           family = "poisson", data = pfas_vio))

# Clear memory
rm(pfas_vio)


################################################################################
<<<<<<<< Updated upstream:code/03_indicator_regressions.R
## DBP concentrations
################################################################################

dbp_vio <- read_rds("data/combined/dbp_vio_epic.rds")  %>%
========
## Load Data 
## DBP violations
################################################################################

dbp_vio <- read_rds("Data/combined/pwsid/dbp_vio_epic.rds")  %>%
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

<<<<<<<< Updated upstream:code/03_indicator_regressions.R
summary(dbp_dem_lm <- glm(combined_dbp ~ frac_amerind + frac_black + frac_asian + frac_hisp + frac_pacisl + 
                              lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
                          data = dbp_vio))
========
summary(dbp_dem_lm <- glm(combined_dbp ~ frac_black + frac_hisp + frac_pacisl + 
                       frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
                     data = dbp_vio))
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R

summary(dbp_env_lm <- glm(combined_dbp ~ pre1960pct + ozone + pm25 + ptsdf + 
                           pwdis + pnpl + state_code,
                     data = dbp_vio))


# Clear memory
rm(dbp_vio)


################################################################################
<<<<<<<< Updated upstream:code/03_indicator_regressions.R
## TCR detection rate 
################################################################################

tcr_vio <- read_rds("data/combined/tcr_vio_epic.rds") %>%
========
## Load Data 
## TCR violations
################################################################################

tcr_vio <- read_rds("Data/combined/pwsid/tcr_vio_epic.rds") %>%
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

<<<<<<<< Updated upstream:code/03_indicator_regressions.R
summary(tcr_dem_lm <- glm(detection_share ~ frac_amerind + frac_black + frac_asian + frac_hisp + frac_pacisl + 
                              lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
========
summary(tcr_dem_lm <- glm(detection_share ~ frac_black + frac_hisp + frac_pacisl + 
                       frac_asian + frac_amerind +  lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
                      # family = binomial(link = "logit"),
                     data = tcr_vio))

summary(tcr_env_lm <- glm(detection_share ~ pre1960pct + ozone + pm25 + ptsdf + 
                           pwdis + pnpl + state_code, 
                     #    family = binomial(link = "logit"),
                     data = tcr_vio))

rm(tcr_vio)

<<<<<<<< Updated upstream:code/03_indicator_regressions.R

################################################################################
## Arsenic concentrations
################################################################################

ars_vio <- read_rds("data/combined/ars_vio_epic.rds")  %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

# re-express these in ppb 
ars_vio <- ars_vio %>%
  mutate(arsenic = arsenic * 1000 )

summary(ars_dem_lm <- glm(arsenic ~ frac_amerind + frac_black + frac_asian + frac_hisp + frac_pacisl + 
                              lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code, 
                     data = ars_vio))

summary(ars_env_lm <- glm(arsenic ~ pre1960pct + ozone + pm25 + ptsdf + 
                           pwdis + pnpl + state_code,
                     data = ars_vio))


# Clear memory
rm(ars_vio)

################################################################################
## Nitrate concentrations
################################################################################

nitrate_vio <- read_rds("data/combined/nitrate_vio_epic.rds")  %>%
  left_join(sys_code) %>%
  mutate(SystemSize = case_when(
    pop_served > 100000 ~ "VeryLarge",
    pop_served > 10000 ~ "Large",
    pop_served > 3300 ~ "Medium",
    pop_served > 500 ~ "Small",
    pop_served <= 500 ~ "VerySmall"
  ))

summary(nitrate_dem_lm <- glm(nitrate ~ frac_amerind + frac_black + frac_asian + frac_hisp + frac_pacisl + 
                              lowinc + primacy_type + relevel(factor(SystemSize), ref = "Medium") + source_gw + state_code,
                     data = nitrate_vio))

summary(nitrate_env_lm <- glm(nitrate ~ pre1960pct + ozone + pm25 + ptsdf + 
                           pwdis + pnpl + state_code,
                     data = nitrate_vio))


# Clear memory
rm(nitrate_vio)

========
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
################################################################################
## Regression tables
################################################################################

<<<<<<<< Updated upstream:code/03_indicator_regressions.R
#Dem regressions

  #checking that table output looks the same as the stargazer output 
  # summary(dbp_dem_lm)
  # summary(ars_dem_lm)
  # summary(nitrate_dem_lm)
  
  # Noting there was an error message when trying to add the arsenic and nitrate regression 
  # results. This was an issue with the Stargazer package when using r versions > 4.2, so there is som e
  # new manual code above that allows this to run.
  # The error was: "Error in if (is.na(s)) { : the condition has length > 1"

  # for some reason the coefficients for Black and Asian were reversed.

stargazer(hb_dem_poi, lcr_dem_poi, pfas_dem_poi, dbp_dem_lm, tcr_dem_lm, ars_dem_lm, nitrate_dem_lm, 
          title = "Demographic Regression Results", align = FALSE,
          column.labels = c("Health-based", "Lead", "PFAS", "DBP", "TCR", "ARS", "NITR"),
          covariate.labels = c("\\% American Indian", "\\% Black", "\\% Asian", "\\% Hispanic", "\\% Pacific Islander",  
                                "\\% Low income^{+}", "Tribal System", "Large system^{++}", "Small system", "Very Large system", "Very small system","Groundwater"),
========
library(stargazer)

#Dem regressions
stargazer(hb_dem_poi, lcr_dem_poi, pfas_dem_poi, dbp_dem_lm, tcr_dem_lm, title = "Demographic Regression Results", align = FALSE,
          column.labels = c("Health-based", "Lead", "PFAS", "DBP", "TCR"),
          covariate.labels = c("\\% Black", "\\% Hispanic", "\\% Pacific Islander", "\\% Asian", 
                               "\\% American Indian", "\\% Low income^{+}", "Tribal System", "Large system^{++}", "Small system", "Very Large system", "Very small system","Groundwater"),
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
          omit = 'state_code', 
          omit.labels = "State control",
          keep.stat = c("n", "rsq"))

<<<<<<<< Updated upstream:code/03_indicator_regressions.R
#Env regressions

stargazer(hb_env_poi, lcr_env_poi, pfas_env_poi, dbp_env_lm, tcr_env_lm, ars_env_lm, nitrate_env_lm, 
          title = "Environmental Regression Results", align = FALSE,
          column.labels = c("Health-based", "Lead", "PFAS", "DBP", "TCR", "ARS", "NITR"),
========
#notes = "^{+} Low income refers to the Pct of people below 2X the Federal Poverty Limit. ^{++} System size based on population served. Very small is fewer than
#         500, Small fewer than 3,300, Medium fewer than 10,000 and Large between 10,000 and 100,000. Regressions based on EPIC boundary specification.")


#Env regressions
stargazer(hb_env_poi, lcr_env_poi, pfas_env_poi, dbp_env_lm, tcr_env_lm, title = "Environmental Regression Results", align = FALSE,
          column.labels = c("Health-based", "Lead", "PFAS", "DBP", "TCR"),
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R
          covariate.labels = c("Lead Paint", "Ozone", "PM_{2.5}", "Toxic Release Facility", "
                         Wastewater discharge", "Superfund Site"),
          omit = 'state_code', 
          omit.labels = "State control",
          keep.stat = c("n", "rsq"))

<<<<<<<< Updated upstream:code/03_indicator_regressions.R



========
>>>>>>>> Stashed changes:code/draft/05_indicator_regressions.R

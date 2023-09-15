

################################################################################
# Regressions for indicators
# National Center for Environmental Economics
# Latest update: 9/14/2023
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

# Model variables

E <- "air_cancer + diesel_pm + lead + ozone + traffic"

X <- "pop_served"

################################################################################
## Load data
## Health violations
################################################################################

# Dataset on health-based violations (since 2015, national) 

HB_vio <- readRDS("data/HB_vio_combined.rds")

ggplot(HB_vio, aes(x=lowinc, y=total_violations)) +
  geom_point()
  
ggplot(HB_vio, aes(x=minorpct, y=total_violations)) +
  geom_point()

summary(hb_lm <- lm(total_violations ~ minorpct + lowinc + 
                      pop_served + air_cancer + diesel_pm + lead + ozone + traffic, 
                    data = HB_vio))

check_heteroskedasticity(hb_lm)

summary(hb_nb <- glm.nb(total_violations ~ minorpct + lowinc + 
                       pop_served + air_cancer + diesel_pm + lead + ozone + traffic, 
                     data = HB_vio))

check_overdispersion(hb_nb)


plot(hb_lm$residuals, pch= 16, col= "red")

# Clear memory
rm(HB_vio)

################################################################################
## Load Data 
## LCR violations 
################################################################################

LCR_vio <- read_rds("data/lcr_vio_combined.rds")

ggplot(LCR_vio, aes(x=lowinc, y=total_violations)) +
  geom_point()

ggplot(LCR_vio, aes(x=minorpct, y=total_violations)) +
  geom_point()

summary(lcr_lm <- lm(total_violations ~ minorpct + lowinc + 
                       pop_served + air_cancer + diesel_pm + lead + ozone + traffic, 
                     data = LCR_vio))

check_heteroskedasticity(lcr_lm)

check_model(lcr_lm)


summary(lcr_nb <- glm.nb(total_violations ~ minorpct + lowinc + 
                          pop_served + air_cancer + diesel_pm + lead + ozone + traffic, 
                        data = LCR_vio))

library(performance)

check_overdispersion(lcr_nb)

plot(lcr_lm$residuals, pch= 16, col= "red")


################################################################################
## Load Data 
## PFAS violations
################################################################################

pfas_vio <- read_rds("data/pfas_vio_combined.rds")

summary(pfas_lm <- glm(detection ~ minorpct + lowinc + 
                         pop_served + air_cancer + diesel_pm + lead + ozone + traffic, 
                       family = binomial(link="probit"), data = pfas_vio))

# Clear memory
rm(pfas_vio)


################################################################################
## Load Data 
## DBP violations
################################################################################

dbp_vio <- read_rds("data/dbp_vio_combined.rds")

summary(dbp_lm <- lm(combined_dbp ~ minorpct + lowinc + 
                       pop_served + air_cancer + diesel_pm + lead + ozone + traffic, 
                     data = dbp_vio))


# Clear memory
rm(dbp_vio)


################################################################################
## Load Data 
## TCR violations
################################################################################

tcr_vio <- read_rds("data/tcr_vio_combined.rds")

summary(tcr_lm <- lm(detection_share ~ minorpct + lowinc + 
                       pop_served + air_cancer + diesel_pm + lead + ozone + traffic, 
                     data = tcr_vio))

rm(tcr_vio)

################################################################################
## Regression tables
################################################################################

stargazer(hb_lm, lcr_lm, pfas_lm, dbp_lm, tcr_lm, title = "Results", align = TRUE)
         
          
#dep.var.labels=c("Total Health-Based Violations", "Total LCR violations", "PFAs detection", "Combined DBP Detections", "TCR Detection Share"))


stargazer(hb_lm, lcr_lm, pfas_lm, dbp_lm, tcr_lm, title = "Results", align = TRUE,
          column.labels = c("Health-Based", "LCR", "PFAS", "DBP", "TCR"))

stargazer(hb_nb, lcr_nb, pfas_lm, dbp_lm, tcr_lm, title = "Results", align = TRUE,
          column.labels = c("Health-Based", "LCR", "PFAS", "DBP", "TCR"),
          notes = "these are my notes")



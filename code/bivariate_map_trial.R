
################################################################################
# Bivariate maps for drinking water indicators
# National Center for Environmental Economics
# Last edited: 8/8/30
################################################################################

# Drawing from code developed by Wes Austin for a biscale function with action levels 

# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  cowplot,
  patchwork,
  dplyr, # data wrangling
  mapview,
  biscale,
  ggplot2,
  tidycensus, #Census data
  MASS #For regressions and modeling
)

# Source code for bi_scale manipulations

source("Code/biscale_fn_action_level.R")

################################################################################
##Set directories
################################################################################

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

getwd()
setwd(paste0(my_path))
getwd()

plot_path <- "Plots"

################################################################################
## Load Data 
################################################################################

# LOAD VIOLATIONS DATASET
cbg_HB_vio <- read_rds("Data/cbg_HB_vio_combined.rds")


################################################################################
## Make a subset for testing, state-level analysis
################################################################################

# Mississippi

HB_vio_MS <- cbg_HB_vio %>%
  filter(ST_ABBREV == "MS") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) #average violations per tract

################################################################################
## Download Census Division Boundaries for Alabama and New Jersey 
## and Join to health violations data  data
################################################################################

## Grab AL block group boundaries

MS_bg <- tigris::block_groups("MS") %>%
  rename(ID = GEOID) %>%
  st_transform(crs = 4326) 

MS_tract <- tigris::tracts("MS") %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(MS_bg) #check CRS
st_crs(MS_tract)

HB_vio_MS <- left_join(MS_bg, HB_vio_MS) #join by CBG

st_as_sf(HB_vio_MS) ##Convert to spatial object

################################################################################
## Create bivariate map
################################################################################

#1. create general biscale output
# data_biscale <- biscale::bi_class(HB_vio_MS,
#                                    x = avg_vio_cbg, y = MINORPCT, 
#                                    style = "fisher", dim = 3) %>%
#   filter(!str_detect(bi_class, 'NA'))

## 2. create biscale output with defined thresholds

# define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(HB_vio_MS$avg_vio_cbg) #distribution

# Establish thresholds for bivariate mapping. For counts, chose 0.9 (no violations) and 4 (75th percentile, high offenders)

action_vector <- c(0.9,4) #Modify for state-specific thresholds

# Create bi_scale

data_biscale_al <- bi_class_al(HB_vio_MS,
                               x = avg_vio_cbg, y = MINORPCT,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: CBG

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Violations",
                    ylab = "% non-White",
                    size = 6)

# Plot with title

cowplot::ggdraw() +
  draw_plot(map, 0, 0, 1, 0.85) +
  draw_plot(legend, 0.4, .08, 0.9, 0.3) +
  draw_label("SDWA Violations and % Non-White", x = 0.5, y = .9, size = 16) + #Title
  draw_label("Mississippi Tract-level", x = 0.5, y = 0.05, size = 12) #Subtitle

#draw_plot(item, x_loc, y_loc, width, height)
#  By default, coordinates run from 0 to 1, and the point (0, 0) is in the lower left corner of the canvas.

#Save as PNG

ggsave(filename = "MS_bimap_HBV_cbg.png", path = plot_path)

# Plot sans title

cowplot::ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, .1, 0.25, 0.25)

ggsave(filename = "MS_bimap_HBV_cbg_notitle.png", path = plot_path)


dev.off()

# Map 2: Tract level

data_biscale_al <- bi_class_al(HB_vio_MS,
                               x = avg_vio_tract, y = MINORPCT,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Violations",
                    ylab = "% non-White",
                    size = 6)

################################################################################
# Compare with fisher breaks
################################################################################

health_vio_MS <- cbg_HB_vio %>%
  filter(ST_ABBREV == "MS") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) #average violations per tract

MS_tract <- tigris::tracts("MS") %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(MS_bg) #check CRS
st_crs(MS_tract)

health_vio_MS <- left_join(MS_tract, health_vio_MS) #join by tract

st_as_sf(health_vio_MS) ##Convert to spatial object

# Create bi_class

bi_data_fisher <- bi_class(HB_vio_MS, x= avg_vio_tract, y = MINORPCT, style = "fisher", dim= 3)

bi_class_breaks(HB_vio_MS, x= avg_vio_tract, y = MINORPCT, style = "fisher", dim= 3) #check the breaks 

# Map 3: fisher breaks

map <- ggplot() +
  geom_sf(data = MS_tract, fill = "white") +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = NA, show.legend = FALSE) +
  bi_scale_fill(pal = "PinkGrn", dim = 3) +
  bi_theme()

legend <- bi_legend(pal = "PinkGrn",
                    dim = 3,
                    xlab = "More Violations",
                    ylab = "Higher % Minority",
                    size = 6)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, .1, 0.25, 0.25)

finalPlot

ggsave(filename = "bivariate_MS_fisher.png", path = plot_path)

library(MASS)

summary(m1 <- glm.nb(avg_vio_CBG ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = health_vio_MS))


rm(health_vio_MS, MS_bg, MS_tract)


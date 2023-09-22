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
  sf,
  cowplot,
  patchwork,
  dplyr, # data wrangling
  mapview,
  biscale,
  ggplot2,
  tidycensus, #Census data
  readr,
  raster, # raster data operations
  exactextractr, # fast raster data extraction for polygons
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  tmap, # for map creation
  cowplot, # for bivariate mapping
  stringr, # string manipulation
  mapview
)


################################################################################
##Set directories
################################################################################

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

getwd()
setwd(paste0(my_path))
getwd()

plot_path <- "Plots"

# Source code for bi_scale manipulations

source("Code/biscale_fn_action_level.R")

################################################################################
## Load PFAS violations data 
################################################################################

cbg_pfas_vio <- read_rds("Data/Combined/cbg_pfas_vio_combined.rds") %>%
  group_by(ID) %>%
  mutate(pfas_count_cbg = sum(pfas_count*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  mutate(avg_conc_cbg = mean(concentration_sum)) %>%
  distinct(ID, .keep_all = TRUE) 

# Subset for demonstration: Massachussetts

pfas_vio_MA <- cbg_pfas_vio %>%
  filter(ST_ABBREV == "MA") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(pfas_count_tract = sum(pfas_count*ACSTOTPOP)/sum(ACSTOTPOP))

################################################################################
## State geometry data: Massachussetts 
################################################################################

## Grab MA block group and tract boundaries

MA_bg <- tigris::block_groups("MA") %>%
  rename(ID = GEOID) %>%
  st_transform(crs = 4326) 

MA_tract <- tigris::tracts("MA") %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

# Combine with pfas data

pfas_vio_MA <- left_join(MA_bg, pfas_vio_MA) #join by CBG

pfas_vio_MA <- left_join(MA_tract, pfas_vio_MA) #join by tract

st_as_sf(pfas_vio_MA) ##Convert to spatial object

################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(pfas_vio_MA$pfas_count_cbg) #distribution CBG

summary(pfas_vio_MA$pfas_count_tract) #distribution tract

#3rd quartile = 2, so we will use that as a limit for "frequent violators"

action_vector <- c(0.9,2) #Modify for state-specific thresholds


#Establish biscale comparison: PFAS count and percent low income (tract)

data_biscale_al <- bi_class_al(pfas_vio_MA,
                               x = pfas_count_tract, y = LOWINCPCT,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Biscale violations: tract level

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Violations",
                    ylab = "% Low income",
                    size = 6)

# Plot and legend

cowplot::ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, .1, 0.3, 0.3)

ggsave(filename = "MA_bimap_pfas_lowinc.png", path = plot_path)


rm(pfas_vio_MA, MA_tract, cbg_pfas_vio)
################################################################################
## Load LCR violations data 
################################################################################

cbg_lcr_vio <- read_rds("Data/Combined/cbg_lcr_vio_combined.rds") %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE) 

################################################################################
## State geometry data: Oklahoma 
################################################################################

# Subset North Carolina

lcr_vio_NC <- cbg_lcr_vio %>%
  filter(ST_ABBREV == "NC") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP))

################################################################################
## State geometry data: Massachussetts 
################################################################################

## Grab NC block group and tract boundaries

NC_bg <- tigris::block_groups("NC") %>%
  rename(ID = GEOID) %>%
  st_transform(crs = 4326) 

NC_tract <- tigris::tracts("NC") %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

# Combine with pfas data

lcr_vio_NC <- left_join(NC_bg, lcr_vio_NC) #join by CBG

lcr_vio_NC <- left_join(NC_tract, lcr_vio_NC) #join by tract

st_as_sf(lcr_vio_NC) ##Convert to spatial object

################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(lcr_vio_NC$avg_vio_cbg) #distribution

#3rd quartile = 0, so we will use 1 as a limit for "frequent violators"

action_vector <- c(0.9,1) #Modify for state-specific thresholds

#Establish biscale comparison: lcr violations and percent people of color

data_biscale_al <- bi_class_al(lcr_vio_NC,
                               x = avg_vio_tract, y = MINORPCT,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Biscale lcr violations: tract level

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Violations",
                    ylab = "% Minority",
                    size = 6)

# Plot and legend

cowplot::ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, .1, 0.3, 0.3)

ggsave(filename = "NC_bimap_lcr_minor.png", path = plot_path)

rm(lcr_vio_NC, NC_tract, cbg_pfas_vio)

################################################################################
## Country bivariate maps : Health Based Violations
################################################################################

################################################################################
# Load Data
################################################################################

#Grab census tract boundaries for US 

US_tract <- tigris::tracts(state = NULL, county = NULL, cb = TRUE) 

US_tract_50 <- US_tract %>%
  rename(tract = GEOID) %>%
  tigris::shift_geometry() %>%
  filter(STATEFP <=56, STATEFP != 43 ) %>%  #remove Puerto Rico and outer territories
  st_transform(crs = 5070)

# Load and Convert Health-based violations data

tract_HB_vio <- read_rds("Data/combined/cbg_HB_vio_combined.rds") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP), na.rm = TRUE) %>% #average violations per tract
  distinct(tract, .keep_all = TRUE) 

HB_vio_all <- left_join(US_tract_50, tract_HB_vio, by = "tract") 

st_as_sf(HB_vio_all)

################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(HB_vio_all$avg_vio_tract) #distribution for quantiles

#3rd quantile = 1.35, so we will use 1 as a limit for "frequent violators"

action_vector <- c(0.9,1) 

#Establish biscale comparison: HB violations / tract and % people of color

data_biscale_al <- bi_class_al(HB_vio_all,
                               x = avg_vio_tract, y = MINORPCT,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA')) 

# Map 3: Biscale HB violations: tract level

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Violations",
                    ylab = "% Minority",
                    size = 6)

# Plot and legend

cowplot::ggdraw() +
  draw_plot(map, 0, 0.15, 1, 1) +
  draw_plot(legend, 0.1, .1, 0.3, 0.3)

ggsave(filename = "US_bimap_hbv_minor.png", path = "Plots/country")


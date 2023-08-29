


################################################################################
# Mapping PFAs violations indicator
# National Center for Environmental Economics
# Latest update: 8/29/23
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
  maps, # to get county boundary data
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  tmap, # for map creation
  modelsummary, # regression table generation
  future.apply, # parallel computation
  cowplot, # for bivariate mapping
  rgdal, # required for cdlTools
  prism, # download PRISM data
  stringr, # string manipulation
  magrittr,
  tidycensus,
  mapview,
  patchwork, #for combining plots
  MASS #for regressions
)

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

# Load pfas violations data and sb_dems_v3 data (containing demographic / shapefile info)

pfas_vio <- read.csv("Data/indicators_pfas.csv")

pwsid_cbg <- read.csv("Data/sb_dems_area_v3.csv")

# Match case for either dataset (if inconsistent)
pfas_vio <- pfas_vio %>%
  rename(PWSID = pwsid)

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

cbg_pfas_vio <- left_join(pwsid_cbg, pfas_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

rm(pfas_vio, pwsid_cbg) #remove files if unnecessary

# For tracts with PWSIDs, replace null values with zeroes

cbg_pfas_vio$concentration_sum[is.na(cbg_pfas_vio$concentration_sum)] <- 0
cbg_pfas_vio$total_samples[is.na(cbg_pfas_vio$total_samples)] <- 0
cbg_pfas_vio$detection[is.na(cbg_pfas_vio$detection)] <- 0
cbg_pfas_vio$pfas_count[is.na(cbg_pfas_vio$pfas_count)] <- 0
cbg_pfas_vio$max[is.na(cbg_pfas_vio$max)] <- 0

# For CBG with more than one PWSID, find average number of violations at the CBG level
# then collapse to include only 1 observation per census tract level (12-digit "ID") 

cbg_pfas_vio <- cbg_pfas_vio %>%
  group_by(ID) %>%
  mutate(pfas_count_cbg = sum(pfas_count*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  mutate(avg_conc_cbg = mean(concentration_sum)) %>%
  distinct(ID, .keep_all = TRUE) 

write_rds(cbg_pfas_vio, "Data/cbg_pfas_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

################################################################################
## Make a subset for testing, state-level analysis
################################################################################

# Alabama

pfas_vio_AL <- cbg_pfas_vio %>%
  filter(ST_ABBREV == "AL") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(pfas_count_tract = sum(pfas_count*ACSTOTPOP)/sum(ACSTOTPOP)) #average PFAs count per tract


################################################################################
## Download Census Division Boundaries for Alabama and New Jersey 
## and Join to health violations data  data
################################################################################

## Grab AL block group boundaries

AL_bg <- tigris::block_groups("AL") %>% #block groups
  rename(ID = GEOID) %>%
  st_transform(crs = 4326) 

AL_tract <- tigris::tracts("AL") %>% #tracts
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(AL_bg) #check CRS
st_crs(AL_tract)

pfas_vio_AL <- left_join(AL_bg, pfas_vio_AL) #join by CBG

st_as_sf(pfas_vio_AL) ##Convert to spatial object


################################################################################
## Maps 
################################################################################

# Map 1 - Alabama CBG GGPLOT
### Plot using ggplot2

plot_AL_bg <- ggplot() + 
  geom_sf(data = pfas_vio_AL, aes(fill = pfas_count_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average PFAs count per CBG", palette = "OrRd", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

plot_AL_bg

##SUCCESS!!

ggsave(filename = 'AL_pfas_count_CBG.png', path = plot_path)

# Map 2 : Avg. PFAs concentration

plot_AL_bg <- ggplot() + 
  geom_sf(data = pfas_vio_AL, aes(fill = avg_conc_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average PFAs concentration per CBG", palette = "OrRd", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

plot_AL_bg

ggsave(filename = 'AL_pfas_conc_CBG.png', path = plot_path)


################################################################################
## Regressions of violations on demographic characteristics at the tract level  
################################################################################

summary(cbg_pfas_vio[c("pfas_count_cbg", "MINORPCT", "PRE1960PCT", "LOWINCPCT")])

##Standard linear model

summary(pfasV_lm <- lm(pfas_count_cbg ~ MINORPCT + LOWINCPCT, data = pfas_vio_AL))

summary(pfasV_lm2 <- lm(avg_conc_cbg ~ MINORPCT + LOWINCPCT, data = pfas_vio_AL))

##Negative binomial regression, per Statman-Weil et al.

library(MASS)

summary(m1 <- glm.nb(pfas_count_cbg ~ MINORPCT + LOWINCPCT, data = cbg_pfas_vio))

summary(m2 <- lm(avg_conc_cbg ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = cbg_pfas_vio))


################################################################################
## Supplemental
################################################################################

##Interactive map (similar to leaflet)

tmap_mode("view")
tm_shape(pfas_vio_AL) +
  tm_polygons(col = "pfas_count_cbg", alpha = 0.5, midpoint = 0) +
  tm_basemap("Esri.WorldTopoMap")


##Country level for all US (note I did not run this because my computer couldn't handle 
##this big a dataset, TB)

US_counties <- tigris::counties() %>%
  rename(county = GEOID) %>%
  st_transform(crs = 4326)

pfas_vio_all <- cbg_pfas_vio %>%
  mutate(county = substring(ID, first=1, last=5)) %>%
  group_by(county) %>%
  mutate(pfas_count_county = sum(pfas_count*ACSTOTPOP)/sum(ACSTOTPOP))

pfas_vio_all <- left_join(US_counties, pfas_vio_all)

st_as_sf(pfas_vio_all)

ggplot() + 
  geom_sf(data = pfas_vio_all, aes(fill = pfas_count_county, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per county", palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

ggsave(filename = 'pfas_vio_counties_USA.png', path = plot_path)


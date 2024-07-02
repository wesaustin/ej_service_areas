


################################################################################
# Mapping DBP violations indicator
# National Center for Environmental Economics
# Latest update: 9/1/23
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

# Load dbp violations data and sb_dems_v3 data (containing demographic / shapefile info)

dbp_vio <- read.csv("Data/indicator_dbp.csv")

pwsid_cbg <- read.csv("Data/sb_dems_area_v3.csv")

# Match case for either dataset (if inconsistent)
dbp_vio <- dbp_vio %>%
  rename(PWSID = pwsid)

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

cbg_dbp_vio <- left_join(pwsid_cbg, dbp_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

rm(dbp_vio, pwsid_cbg) #remove files if unnecessary

# For tracts with PWSIDs, replace null values with zeroes

cbg_dbp_vio$combined_dbp[is.na(cbg_dbp_vio$combined_dbp)] <- 0

write_rds(cbg_dbp_vio, "Data/cbg_dbp_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

# For CBG with more than one PWSID, find average number of violations at the CBG level
# then collapse to include only 1 observation per census tract level (12-digit "ID") 

cbg_dbp_vio <- cbg_dbp_vio %>%
  group_by(ID) %>%
  mutate(avg_dbp_cbg = sum(combined_dbp*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE) 

################################################################################
## Make a subset for testing, state-level analysis
################################################################################

# Texas 
dbp_vio_TX <- cbg_dbp_vio %>%
  filter(ST_ABBREV == "TX") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(combined_dbp_tract = sum(combined_dbp*ACSTOTPOP)/sum(ACSTOTPOP)) #average DBG concentrationper tract


################################################################################
## Download Census Division Boundaries for Alabama and New Jersey 
## and Join to health violations data  data
################################################################################

## Grab TX block group boundaries

TX_bg <- tigris::block_groups("TX") %>% #block groups
  rename(ID = GEOID) %>%
  st_transform(crs = 4326) 

TX_tract <- tigris::tracts("TX") %>% #tracts
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(TX_bg) #check CRS
st_crs(TX_tract)

dbp_vio_TX <- left_join(TX_bg, dbp_vio_TX) #join by CBG

st_as_sf(dbp_vio_TX) ##Convert to spatial object


################################################################################
## Maps 
################################################################################

# Map 1 - Texas CBG GGPLOT
### Plot using ggplot2

plot_TX_bg <- ggplot() + 
  geom_sf(data = dbp_vio_TX, aes(fill = combined_dbp, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average DBG concentration per CBG", palette = "OrRd", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

plot_TX_bg

##SUCCESS!!

ggsave(filename = 'TX_dbp_CBG.png', path = plot_path)


################################################################################
## Regressions of violations on demographic characteristics at the tract level  
################################################################################

summary(cbg_dbp_vio[c("avg_dbp_cbg", "MINORPCT", "PRE1960PCT", "LOWINCPCT")])

##Standard linear model

summary(dbpV_lm <- lm(avg_dbp_cbg ~ MINORPCT + LOWINCPCT, data = dbp_vio_TX))

summary(dbpV_lm2 <- lm(avg_conc_cbg ~ MINORPCT + LOWINCPCT, data = dbp_vio_TX))

##Negative binomial regression, per Statman-Weil et al.

library(MASS)

summary(m1 <- glm.nb(avg_dbp_cbg ~ MINORPCT + LOWINCPCT, data = cbg_dbp_vio))

summary(m2 <- lm(avg_conc_cbg ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = cbg_dbp_vio))


################################################################################
## Supplemental
################################################################################

##Interactive map (similar to leaflet)

tmap_mode("view")
tm_shape(dbp_vio_TX) +
  tm_polygons(col = "avg_dbp_cbg", alpha = 0.5, midpoint = 0) +
  tm_basemap("Esri.WorldTopoMap")


##Country level for all US (note I did not run this because my computer couldn't handle 
##this big a dataset, TB)

US_counties <- tigris::counties() %>%
  rename(county = GEOID) %>%
  st_transform(crs = 4326)

dbp_vio_all <- cbg_dbp_vio %>%
  mutate(county = substring(ID, first=1, last=5)) %>%
  group_by(county) %>%
  mutate(combined_dbp_county = sum(combined_dbp*ACSTOTPOP)/sum(ACSTOTPOP))

dbp_vio_all <- left_join(US_counties, dbp_vio_all)

st_as_sf(dbp_vio_all)

ggplot() + 
  geom_sf(data = dbp_vio_all, aes(fill = combined_dbp_county, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per county", palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

ggsave(filename = 'dbp_vio_counties_USA.png', path = plot_path)


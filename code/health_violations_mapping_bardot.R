


################################################################################
# Mapping Health-based violations indicator
# National Center for Environmental Economics
# Latest update: 8/24/23
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
  patchwork #for combining plots
)

################################################################################
##Set directories
################################################################################

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

getwd()
setwd(paste0(my_path))
getwd()


################################################################################
## Load Data 
################################################################################

# Load health violations data and sb_dems_v3 data (containing demographic / shapefile info)

HB_vio <- read.csv("Data/health_violations2015.csv")

pwsid_cbg <- read.csv("Data/sb_dems_area_v3.csv")

# Rename and convert CBG ID to string with leading 0s if necessary  
pwsid_cbg <- pwsid_cbg %>%
  rename(PWSID = pwsid)

# Left join the full PWSID dataset with the health violations data

cbg_HB_vio <- left_join(pwsid_cbg, HB_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

rm(HB_vio, pwsid_cbg) #remove files if unnecessary

# For tracts with PWSIDs, and null values for the number of violations, replace nulls 
# with zeros

cbg_HB_vio$total_violations[is.na(cbg_HB_vio$total_violations)] <- 0

# For tract with more than one PWSID, find average number of violations at the CBG level
# then collapse to include only 1 observation per census tract level (12-digit "ID") 

cbg_HB_vio <- cbg_HB_vio %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  mutate(avg_vio_length = mean(diff_days)) %>%
  distinct(ID, .keep_all = TRUE) 

write_rds(cbg_HB_vio, "Data/cbg_HB_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

################################################################################
## Make a subset for testing, state-level analysis
################################################################################

# Alabama

HB_vio_AL <- cbg_HB_vio %>%
  filter(ST_ABBREV == "AL") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) #average violations per tract


# New Jersey

HB_vio_NJ <- cbg_HB_vio %>%
  filter(ST_ABBREV == "NJ")  %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) #average violations per tract


################################################################################
## Download Census Division Boundaries for Alabama and New Jersey 
## and Join to health violations data  data
################################################################################

## Grab AL block group boundaries

AL_bg <- tigris::block_groups("AL") %>%
  rename(ID = GEOID) %>%
  st_transform(crs = 4326) 

AL_tract <- tigris::tracts("AL") %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(AL_bg) #check CRS
st_crs(AL_tract)

HB_vio_AL <- left_join(AL_bg, HB_vio_AL) #join by CBG

st_as_sf(HB_vio_AL) ##Convert to spatial object

##Grab NJ block group and tract boundaries

NJ_bg <- tigris::block_groups("NJ") %>%
  mutate(ID=str_pad(GEOID, 12, pad="0")) %>%
  st_transform(crs = 4326)  

NJ_tract <- tigris::tracts("NJ") %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(NJ_bg)
st_crs(NJ_tract)

HB_vio_NJ <- left_join(NJ_tract, HB_vio_NJ) #join by tract
st_as_sf(HB_vio_NJ) ##Convert to spatial object


################################################################################
## Maps 
################################################################################

# Map 1 - Alabama CBG GGPLOT
###Plot using ggplot2

plot_AL_bg <- ggplot() + 
  geom_sf(data = HB_vio_AL, aes(fill = avg_vio_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per CBG", palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

plot_AL_bg

##SUCCESS!!

plot_path <- "Plots"

ggsave(filename = 'AL_health_CBG.png', path = plot_path)


# Map 2 - New Jersey tract GGPLOT

plot_NJ_tract <- ggplot() + 
  geom_sf(data = HB_vio_NJ, aes(fill = avg_vio_tract, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per tract", palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() +  
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

plot_NJ_tract

ggsave(filename = 'NJ_health_tract.png', path = plot_path)

##Success! A map with different census tracts showing the average number of health
##violations per tract


################################################################################
## Regressions of violations on demographic characteristics at the tract level  
################################################################################

summary(cbg_HB_vio[c("avg_vio_cbg", "MINORPCT", "PRE1960PCT", "LOWINCPCT")])

##Standard linear model

summary(HBV_lm <- lm(avg_vio_cbg ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = HB_vio_NJ))

summary(HBV_lm2 <- lm(avg_vio_tract ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = HB_vio_NJ))

##Negative binomial regression, per Statman-Weil et al.

summary(m1 <- glm.nb(avg_vio_cbg ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = HB_vio_NJ))

summary(m2 <- lm(avg_vio_length ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = cbg_HB_vio))


################################################################################
## Supplemental
################################################################################

##Interactive map (similar to leaflet)

tmap_mode("view")
tm_shape(HB_vio_NJ) +
  tm_polygons(col = "avg_vio_cbg", alpha = 0.5, midpoint = 0) +
  tm_basemap("Esri.WorldTopoMap")



##Country level for all US (note I did not run this because my computer couldn't handle 
##this big a dataset, TB)

US_counties <- tigris::counties() %>%
  rename(county = GEOID) %>%
  st_transform(crs = 4326)

HB_vio_all <- cbg_HB_vio %>%
  mutate(county = substring(ID, first=1, last=5)) %>%
  group_by(county) %>%
  mutate(avg_county_viol = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP))

HB_vio_all <- left_join(US_counties, HB_vio_all)

st_as_sf(HB_vio_all)

ggplot() + 
  geom_sf(data = HB_vio_all, aes(fill = avg_county_viol, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per county", palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

ggsave(filename = 'healt_vio_counties_USA.png', path = plot_path)


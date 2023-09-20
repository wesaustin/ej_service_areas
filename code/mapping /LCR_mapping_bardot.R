



################################################################################
# Map the two indicators : Health-based violations and LCR violations
# National Center for Environmental Economics
# Latest update: 8/22/2023
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
  stringr, # string manipulation
  magrittr,
  tidycensus,
  mapview,
  MASS
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

##Load health violations data and sb_dems_v3 data (containing demographic / shapefile info)

lcr_vio <- read.csv("Data/LCR_violations_per_PWSID.csv")

pwsid_cbg <- read.csv("Data/sb_dems_area_v3.csv")

##Left join the full PWSID dataset with the lead content violations data

LCR_vio_cbg <- left_join(pwsid_cbg, lcr_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

rm(lcr_vio, pwsid_cbg)

##For tracts with PWSIDs, and null values for the number of violations, replace nulls 
##with zeros

LCR_vio_cbg$total_violations[is.na(LCR_vio_cbg$total_violations)] <- 0

LCR_vio_cbg$Maximum_sample_exceedence[is.na(LCR_vio_cbg$Maximum_sample_exceedence)] <- 0

##########################################################################################
##########################################################################################

## For tract with more than one PWSID, find average number of violations at the CBG level
## then collapse to include only 1 observation per census tract level (12-digit "ID") 

LCR_vio_cbg <- LCR_vio_cbg %>%
  group_by(ID) %>%
  mutate(avg_vio_CBG = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #average violations per CBG
  distinct(ID, .keep_all = TRUE)

## make a subset for testing, create tract-level unit of analysis

LCR_vio_AL <- LCR_vio_cbg %>%  #make sure to rerun if changing unit of spatial analysis
  filter(ST_ABBREV == "AL") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) 

#New Jersey 
LCR_vio_NJ <- LCR_vio_cbg %>%
  filter(ST_ABBREV == "NJ") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) 

################################################################################
## Download Census Division Boundaries for Alabama and New Jersey 
## and Join to LCR data
################################################################################

# Alabama : block group level

AL_bg <- tigris::block_groups("AL") %>%
  st_transform(crs = 4326)  %>%
  mutate(ID=str_pad(GEOID, 12, pad="0"))

AL_tract <- tigris::tracts("AL") %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(AL_bg)
st_crs(Al_tract)

LCR_vio_AL <- left_join(AL_tract, LCR_vio_AL) ##Join by CBG

st_as_sf(LCR_vio_AL) ##Convert to spatial object


# New Jersey : tract level 

NJ_tract <- tigris::tracts("NJ") %>%
  rename(tract = GEOID) %>%
  st_transform(crs = 4326)

LCR_vio_NJ <- left_join(NJ_tract, LCR_vio_NJ)

st_as_sf(LCR_vio_NJ) 


################################################################################
## Maps 
################################################################################

# Map 1 - Alabama GGPLOT
###Plot using ggplot2

LCR_plot_AL <- ggplot() + 
  geom_sf(data = LCR_vio_AL, aes(fill = avg_vio_CBG, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per CBG", palette = "Purples", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.position = "right")

LCR_plot_AL

plot_path <- "Plots"

ggsave(filename = 'Alabama_LCR_CBG_vio.png', path = plot_path)

##SUCCESS!!


##Now by tract level :NJ

LCR_plot_NJ <- ggplot() + 
  geom_sf(data = LCR_vio_NJ, aes(fill = avg_vio_tract, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per tract", palette = "Purples", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

LCR_plot_NJ

ggsave(filename = 'NJ_tract_LCR_violations.png', path = plot_path)

##Success! A map with different census tracts showing the average number of LCR
##violations per tract

################################################################################
## Regressions of violations on demographic characteristics at the tract level  
################################################################################


summary(LCR_vio_cbg[c("avg_vio_CBG", "MINORPCT", "PRE1960PCT", "LOWINCPCT")])

##Model the relationship between the number of violations and EJ indicators

#CBG-level - National

summary(LCR_lm <- glm.nb(avg_vio_CBG ~ MINORPCT + PRE1960PCT + LOWINCPCT + STATE_NAME, data = LCR_vio_cbg))


##CBG level in subset - NJ

LCR_lmNJ <- glm.nb(avg_vio_CBG ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = LCR_vio_NJ)
summary(LCR_lmNJ)


################################################################################
## Supplemental   
################################################################################

##Interactive map (similar to leaflet)

LCR_vio_NJ <- LCR_vio_cbg %>%
  filter(ST_ABBREV == "NJ")

LCR_vio_NJ <- left_join(NJ_bg, LCR_vio_NJ)

st_as_sf(LCR_vio_NJ)

tmap_mode("view")
tm_shape(LCR_vio_NJ) +
  tm_polygons(col = "avg_vio_CBG", alpha = 0.5, midpoint = 0) +
  tm_basemap("Esri.WorldTopoMap")


## County-level mapping : Washington

WA_counties <- tigris::counties("WA") %>%
  rename(county = GEOID) %>%
  st_transform(crs = 4326)

LCR_vio_WA <- LCR_vio_cbg %>%
  filter(ST_ABBREV == "WA") %>%
  mutate(county = substring(ID, first=1, last=5))  %>%
  group_by(county) %>%
  mutate(avg_county_vio = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP))

LCR_vio_WA <- left_join(WA_counties, LCR_vio_WA)

st_as_sf(LCR_vio_WA)

LCR_plot_WA <- ggplot() + 
  geom_sf(data = LCR_vio_WA, aes(fill = avg_county_vio, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per county", palette = "Purples", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.position = "right")

LCR_plot_WA

ggsave(filename = 'WA_county_LCR_violations.png', path = plot_path)



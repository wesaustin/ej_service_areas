################################################################################
# Map the two indicators : Health-based violations and LCR violations
# National Center for Environmental Economics
# Last edited: 8/22/23
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
  cdlTools, # download CDL data
  rgdal, # required for cdlTools
  prism, # download PRISM data
  stringr, # string manipulation
  magrittr,
  tidycensus,
  mapview
)
library(readxl)

################################################################################
##Set Directories
################################################################################

#my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"
setwd("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/")

#getwd()
#setwd(paste0(my_path))
#getwd()

################################################################################
## Load Data 
################################################################################

##Load health violations data and sb_dems_v3 data (containing demographic / shapefile info)

lcr_vio <- read.csv("ej_service_areas/data/indicators/LCR_violations_per_PWSID.csv")

pwsid_cbg <- read_excel("data/demographics/sb_dems_area_v3.xlsx")

# Rename and convert CBG ID to string with leading 0s if necessary  
pwsid_cbg <- pwsid_cbg %>%
  rename(PWSID = pwsid)      %>%
  mutate(ID=str_pad(ID, 12, pad="0")) # adds a leading zero and converts to charachter length 12 

################################################################################
## Merge the PWSID-specific indicators data with geographic data
################################################################################

##Left join the full PWSID dataset with the lead content violations data

LCR_vio_cbg <- left_join(pwsid_cbg, lcr_vio, by = "PWSID", relationship = "many-to-many")

##For tracts with PWSIDs, and null values for the number of violations, replace nulls 
##with zeros

LCR_vio_cbg$total_violations[is.na(LCR_vio_cbg$total_violations)] <- 0

LCR_vio_cbg$Maximum_sample_exceedence[is.na(LCR_vio_cbg$Maximum_sample_exceedence)] <- 0

################################################################################
## Collapse to CBG by Water System and then tract for better visualization 
################################################################################

##For tract with more than one PWSID, find average number of violations at the CBG level
##then collapse to include only 1 observation per CBG

##Here I keep only the calculated avg. number of violations per tract, the max exceedence, when the sample 
##was taken, and the relevant IDs numbers

###Then, collapse to the census tract level (11-digit "ID") 

LCR_vio_cbg <- LCR_vio_cbg %>%
  group_by(ID) %>%
  mutate(avg_viol_CBG = mean(total_violations)) %>%
  distinct(ID, .keep_all = TRUE) 

##make a subset for testing, create tract-level unit of analysis

#Alabama
Lead_vio_AL <- LCR_vio_cbg %>%
  filter(ST_ABBREV == "AL") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) 

#New Jersey 
Lead_vio_NJ <- LCR_vio_cbg %>%
  filter(ST_ABBREV == "NJ") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) 


################################################################################
## Download Census Division Boundaries for Alabama and New Jersey 
## and Join to LCR data
################################################################################

##Grab AL block group and tract boundaries

AL_bg <- tigris::block_groups("AL") %>%
  st_transform(crs = 4326)  %>%
  mutate(ID=str_pad(GEOID, 12, pad="0"))

AL_tract <- tigris::tracts("AL") %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(AL_bg)
st_crs(Al_tract)

##We actually only have tract-level data for AL? It appears that there is a number (0)
##missing from the CBG ID, which is why I had to substring in order to match the polygons

  #WA: use   mutate(ID=str_pad(ID, 12, pad="0"))  to add a leading zero and convert the 
  # variable from numeric to string. 

Lead_vio_AL <- left_join(AL_tract, Lead_vio_AL) ##Join by CBG

st_as_sf(Lead_vio_AL) ##Convert to spatial object

##Grab NJ block group and tract boundaries

NJ_bg <- tigris::block_groups("NJ") %>%
  mutate(ID=str_pad(GEOID, 12, pad="0")) %>%
  st_transform(crs = 4326)  

NJ_tract <- tigris::tracts("NJ") %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)


st_crs(NJ_bg)
st_crs(NJ_tract)

Lead_vio_NJ <- left_join(NJ_tract, Lead_vio_NJ)
st_as_sf(Lead_vio_NJ) ##Convert to spatial object

################################################################################
## Maps 
################################################################################

# Map 1 - Alabama GGPLOT
###Plot using ggplot2

Lead_plot_AL <- ggplot() + 
  geom_sf(data = Lead_vio_AL, aes(fill = avg_vio_tract, geometry = geometry)) +
  scale_fill_viridis_c(option="plasma", na.value = "white") +
  geom_sf(data = AL_bg, fill = NA) +
  ggthemes::theme_map() + 
  theme(legend.position = "right")

Lead_plot_AL

##SUCCESS!!

# WA - Took this out because it was merging CBGs to tracts spatially, not sure how that worked 
# Map 2 - New Jersey 
# ##NJ : CBG
# Lead_plot_NJ <- ggplot() + 
#   geom_sf(data = Lead_vio_NJ, aes(fill = avg_vio_CBG, geometry = geometry)) +
#   scale_fill_viridis_c(option="plasma", na.value = "white") +
#   geom_sf(data = NJ_bg, fill = NA) +
#   ggthemes::theme_map() + 
#   theme(legend.position = "right")
# Lead_plot_NJ

# Map 2
##Now by tract level 

Lead_plot_NJ <- ggplot() + 
  geom_sf(data = Lead_vio_NJ, aes(fill = avg_vio_tract, geometry = geometry)) +
  scale_fill_viridis_c(option="plasma", na.value = "white") +
  geom_sf(data = NJ_tract, fill = NA) +
  ggthemes::theme_map() + 
  theme(legend.position = "right")

Lead_plot_NJ


##Success! A map with different census tracts showing the average number of LCR


#Map 4 
##Interactive map (similar to leaflet)

tmap_mode("view")
tm_shape(Lead_vio_NJ) +
  tm_polygons(col = "avg_vio_tract", midpoint = 0) +
  tm_basemap("Esri.WorldTopoMap")


################################################################################
## Regressions of violations on demographic characteristics at the tract level  
################################################################################

##violations per tract

LCR_lm <- lm(avg_vio_tract ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = Lead_vio_NJ)
LCR_lm <- lm(avg_vio_tract ~ MINORPCT + CANCER + PTRAF + PM25 + PRE1960PCT + LOWINCPCT, data = Lead_vio_NJ)

summary(LCR_lm)




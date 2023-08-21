


################################################################################
# Map the two indicators : Health-based violations and LCR violations
################################################################################
## Load packages: 

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


##Set wd

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"
setwd("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/")

getwd()
setwd(paste0(my_path))
getwd()


##Merge the PWSID-specific indicators data with mappable geographic data
##Load health violations data and sb_dems_v3 data (containing demographic / shapefile info)
Lead_violations <- read.csv("ej_service_areas/data/indicators/LCR_violations_per_PWSID.csv")
PWSID_geodemog_data <- read_excel("data/demographics/sb_dems_area_v3.xlsx")

PWSID_geodemog_data <- PWSID_geodemog_data %>%
  rename(PWSID = pwsid)


##Left join the full PWSID dataset with the lead content violations data

Lead_violations <- left_join(PWSID_geodemog_data, Lead_violations, by = "PWSID", relationship = "many-to-many")

##For tracts with PWSIDs, and null values for the number of violations, replace nulls 
##with zeros

Lead_violations$total_violations[is.na(Lead_violations$total_violations)] <- 0

Lead_violations$Maximum_sample_exceedence[is.na(Lead_violations$Maximum_sample_exceedence)] <- 0


##For tract with more than one PWSID, find average number of violations at the CBG level
##then collapse to include only 1 observation per CBG

##Here I keep only the calculated avg. number of violations per tract, the max exceedence, when the sample 
##was taken, and the relevant IDs numbers

###Then, collapse to the census tract level (11-digit "ID") 

Lead_violations <- Lead_violations %>%
  group_by(ID) %>%
  mutate(avg_viol_CBG = mean(total_violations)) %>%
  distinct(ID, .keep_all = TRUE) 


##make a subset for testing, create tract-level unit of analysis

Lead_viol_AL <- Lead_violations %>%
  filter(ST_ABBREV == "AL") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(AVG_tract_viol = mean(total_violations)) 


##Grab AL block group boundaries

AL_bg <- tigris::block_groups("AL") %>%
  mutate(ID = substring(GEOID, first=2, last=12)) %>%
  st_transform(crs = 4326) 

AL_bg$ID <- as.numeric(AL_bg$ID)
Lead_viol_AL$ID <- as.numeric(Lead_viol_AL$ID)

##We actually only have tract-level data for AL? It appears that there is a number (0)
##missing from the CBG ID, which is why I had to substring in order to match the polygons

st_crs(AL_bg)

##Join by CBG

Lead_viol_AL <- left_join(AL_bg, Lead_viol_AL)

##Convert to spatial object

st_as_sf(Lead_viol_AL) 

###Plot using ggplot2

Lead_plot_AL <- ggplot() + 
  geom_sf(data = Lead_viol_AL, aes(fill = avg_viol_CBG, geometry = geometry)) +
  scale_fill_viridis_c(option="plasma", na.value = "white") +
  geom_sf(data = AL_bg, fill = NA) +
  ggthemes::theme_map() + 
  theme(legend.position = "right")

Lead_plot_AL

##SUCCESS!!

##NJ : CBG

Lead_viol_NJ <- Lead_violations %>%
  filter(ST_ABBREV == "NJ") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(AVG_tract_viol = mean(total_violations))

NJ_bg <- tigris::block_groups("NJ") %>%
  mutate(ID = GEOID) %>%
  st_transform(crs = 4326)

NJ_bg$ID <- as.numeric(NJ_bg$ID)
Lead_viol_NJ$ID <- as.numeric(Lead_viol_NJ$ID)

Lead_viol_NJ <- left_join(NJ_bg, Lead_viol_NJ)

##Convert to SF for mapping

st_as_sf(Lead_viol_NJ) 

##Plot it!

Lead_plot_NJ <- ggplot() + 
  geom_sf(data = Lead_viol_NJ, aes(fill = avg_viol_CBG, geometry = geometry)) +
  scale_fill_viridis_c(option="plasma", na.value = "white") +
  geom_sf(data = NJ_bg, fill = NA) +
  ggthemes::theme_map() + 
  theme(legend.position = "right")

Lead_plot_NJ

##Now by tract level 


rm(lead_viol_NJ)

Lead_viol_NJ <- Lead_violations %>%
  filter(ST_ABBREV == "NJ") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(AVG_tract_viol = mean(total_violations))

NJ_tract <- tigris::tracts("NJ") %>%
  mutate(tract = GEOID) %>%
  st_transform(crs = 4326)

Lead_viol_NJ <- left_join(NJ_tract, Lead_viol_NJ)
# double check if this is a st_join or a left join, i.e., is this spatially joining locations
# or is it joining by an ID 

st_as_sf(Lead_viol_NJ) 

Lead_plot_NJ <- ggplot() + 
  geom_sf(data = Lead_viol_NJ, aes(fill = AVG_tract_viol, geometry = geometry)) +
  scale_fill_viridis_c(option="plasma", na.value = "white") +
  geom_sf(data = NJ_tract, fill = NA) +
  ggthemes::theme_map() + 
  theme(legend.position = "right")

Lead_plot_NJ


##Success! A map with different census tracts showing the average number of LCR
##violations per tract


LCR_lm <- lm(avg_viol_CBG ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = Lead_viol_NJ)
LCR_lm <- lm(avg_viol_CBG ~ MINORPCT + CANCER + PTRAF + PM25 + PRE1960PCT + LOWINCPCT, data = Lead_viol_NJ)

summary(LCR_lm)


##Interactive map (similar to leaflet)

tmap_mode("view")
tm_shape(Lead_viol_NJ) +
  tm_polygons(col = "AVG_tract_viol", midpoint = 0) +
  tm_basemap("Esri.WorldTopoMap")



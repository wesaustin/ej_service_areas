


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


##Set wd

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

getwd()
setwd(paste0(my_path))
getwd()


##Merge the PWSID-specific indicators data with mappable geographic data
##Load health violations data and sb_dems_v3 data (containing demographic / shapefile info)

health_violations <- read.csv("Data/health_violations2015.csv")

PWSID_geodemog_data <- read.csv("Data/sb_dems_area_v3.csv")


##Left join the full PWSID dataset with the health violations data

health_violations <- left_join(PWSID_geodemog_data, health_violations, by = "PWSID", relationship = "many-to-many")

##For tracts with PWSIDs, and null values for the number of violations, replace nulls 
##with zeros

health_violations$total_violations[is.na(health_violations$total_violations)] <- 0

##For tract with more than one PWSID, find average number of violations at the CBG level
##then collapse to include only 1 observation per census tract level (12-digit "ID") 

health_violations <- health_violations %>%
  group_by(ID) %>%
  mutate(avg_viol_CBG= mean(total_violations)) %>%
  mutate(avg_viol_length = mean(diff_days)) %>%
  distinct(ID, .keep_all = TRUE) 

##Some of the leading zeros for some states may have been lopped off, so add them back in

health_violations$ID <- 
  ifelse(nchar(health_violations$ID) < 12, paste0("0", health_violations$ID), health_violations$ID)

health_violations <- health_violations %>%
  ungroup() %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(AVG_tract_viol = mean(total_violations)) #average violations per tract

glimpse(health_violations) ##check that the columns look okay


##make a subset for testing, create tract-level unit of analysis

health_viol_AL <- health_violations %>%
  filter(ST_ABBREV == "AL")
 
##Grab AL block group boundaries

AL_bg <- tigris::block_groups("AL") %>%
  rename(ID = GEOID) %>%
  st_transform(crs = 4326) 

#check CRS

st_crs(AL_bg)
st_crs(health_viol_AL)

##Join by CBG

health_viol_AL <- left_join(AL_bg, health_viol_AL)

##Convert to spatial object

st_as_sf(health_viol_AL) 


###Plot using ggplot2

health_plot_AL <- ggplot() + 
  geom_sf(data = health_viol_AL, aes(fill = avg_viol_CBG, geometry = geometry), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = AL_bg, fill = NA, colour = NA, size= 0.05) + 
  ggthemes::theme_map() + 
  theme(legend.position = "right")

health_plot_AL

##SUCCESS!!

plot_path <- "Plots"

ggsave(filename = 'AL_health_CBG.png', path = plot_path)


##NJ : Tract level

health_viol_NJ <- health_violations %>%
  filter(ST_ABBREV == "NJ") 

NJ_tract <- tigris::tracts("NJ") %>%
  rename(tract = GEOID) %>%
  st_transform(crs = 4326)

health_viol_NJ <- left_join(NJ_tract, health_viol_NJ)

st_as_sf(health_viol_NJ) 

health_plot_NJ <- ggplot() + 
  geom_sf(data = health_viol_NJ, aes(fill = AVG_tract_viol, geometry = geometry), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = NJ_tract, fill = NA, colour = NA, size= 0.05) + 
  ggthemes::theme_map() + 
  theme(legend.position = "right")

health_plot_NJ

ggsave(filename = 'NJ_health_tract.png', path = plot_path)


##Success! A map with different census tracts showing the average number of health
##violations per tract

WA_counties <- tigris::counties("WA") %>%
  rename(county = GEOID) %>%
  st_transform(crs = 4326)

health_viol_WA <- health_violations %>%
  filter(ST_ABBREV == "WA") %>%
  mutate(county = substring(ID, first=1, last=5)) 

health_viol_WA <- left_join(WA_counties, health_viol_WA)

st_as_sf(health_viol_WA)

health_viol_WA <- health_viol_WA %>%
  group_by(county) %>%
  mutate(avg_county_viol = mean(total_violations)) 

health_plot_WA <- ggplot() + 
  geom_sf(data = health_viol_WA, aes(fill = avg_county_viol, geometry = geometry), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = WA_counties, fill = NA, colour = NA, size= 0.05) + 
  ggthemes::theme_map() + 
  theme(legend.position = "right")

health_plot_WA

ggsave(filename = 'WA_health_county.png', path = plot_path)


##########################################################################################
##########################################################################################

##Model the relationship between the number of violations and EJ indicators


summary(health_violations[c("avg_viol_CBG", "MINORPCT", "PRE1960PCT", "LOWINCPCT")])


HBV_lm <- lm(avg_viol_CBG ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = health_viol_NJ)

summary(HBV_lm)

summary(m1 <- glm.nb(avg_viol_CBG ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = health_viol_NJ))


##Interactive map (similar to leaflet)

tmap_mode("view")
tm_shape(health_viol_NJ) +
  tm_polygons(col = "avg_viol_CBG", alpha = 0.5, midpoint = 0) +
  tm_basemap("Esri.WorldTopoMap")

##Plotting at different levels for the same state

##BG

health_viol_NJ <- health_violations %>%
  filter(ST_ABBREV == "NJ")

NJ_bg <- tigris::block_groups("NJ") %>%
  rename(ID = GEOID) %>%
  st_transform(crs = 4326)

health_viol_NJ <- left_join(NJ_bg, health_viol_NJ)

st_as_sf(health_viol_NJ)

health_plot_NJ <- ggplot() + 
  geom_sf(data = health_viol_NJ, aes(fill = avg_viol_CBG, geometry = geometry), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = NJ_bg, fill = NA, colour = NA, size= 0.05) + 
  ggthemes::theme_map() + 
  theme(legend.position = "right")

health_plot_NJ

ggsave(filename = 'NJ_health_cbg.png', path = plot_path)


##County

health_viol_NJ <- health_violations %>%
  filter(ST_ABBREV == "NJ") %>%
  mutate(county = substring(ID, first=1, last=5)) 
  
NJ_counties <- tigris::counties("NJ") %>%
  rename(county = GEOID) %>%
  st_transform(crs = 4326)

health_viol_NJ <- health_viol_NJ %>%
  group_by(county) %>%
  mutate(avg_county_viol = mean(total_violations))

health_viol_NJ <- left_join(NJ_counties, health_viol_NJ)

st_as_sf(health_viol_NJ)

health_plot_NJ <- ggplot() + 
  geom_sf(data = health_viol_NJ, aes(fill = avg_county_viol, geometry = geometry), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = NJ_counties, fill = NA, colour = NA, size= 0.05) + 
  ggthemes::theme_map() + 
  theme(legend.position = "right")

health_plot_NJ

ggsave(filename = 'NJ_health_counties.png', path = plot_path)


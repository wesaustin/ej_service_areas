


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
  stringr, # string manipulation
  magrittr,
  tidycensus,
  mapview,
  MASS
)


##Set wd

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

getwd()
setwd(paste0(my_path))
getwd()


##Merge the PWSID-specific indicators data with mappable geographic data
##Load health violations data and sb_dems_v3 data (containing demographic / shapefile info)

Lead_violations <- read.csv("Data/LCR_violations_per_PWSID.csv")

PWSID_geodemog_data <- read.csv("Data/sb_dems_area_v3.csv")


##Left join the full PWSID dataset with the lead content violations data

Lead_violations <- left_join(PWSID_geodemog_data, Lead_violations, by = "PWSID", relationship = "many-to-many")

##For tracts with PWSIDs, and null values for the number of violations, replace nulls 
##with zeros

Lead_violations$total_violations[is.na(Lead_violations$total_violations)] <- 0

Lead_violations$Maximum_sample_exceedence[is.na(Lead_violations$Maximum_sample_exceedence)] <- 0

##########################################################################################
##########################################################################################

##For tract with more than one PWSID, find average number of violations at the CBG level
##then collapse to include only 1 observation per census tract level (12-digit "ID") 

Lead_violations <- Lead_violations %>%
  group_by(ID) %>%
  mutate(avg_viol_CBG = mean(total_violations)) %>% #average violations per CBG
  distinct(ID, .keep_all = TRUE) 

##Some of the leading zeros for some states may have been lopped off, so add them back in

Lead_violations$ID <- 
  ifelse(nchar(Lead_violations$ID) < 12, paste0("0", Lead_violations$ID), Lead_violations$ID)

Lead_violations <- Lead_violations %>%
  ungroup() %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(AVG_tract_viol = mean(total_violations)) #average violations per tract

glimpse(Lead_violations) ##check that the columns look okay

##########################################################################################
##########################################################################################

##make a subset for testing

Lead_viol_AL <- Lead_violations %>%
  filter(ST_ABBREV == "AL") 

##Grab AL block group boundaries

AL_bg <- tigris::block_groups("AL") %>%
  rename(ID = GEOID) %>%
  st_transform(crs = 4326) 

##We actually only have tract-level data for AL? It appears that there is a number (0)
##missing from the CBG ID, which is why I had to substring in order to match the polygons

st_crs(AL_bg)
st_crs(health_viol_AL)

##Join by CBG

Lead_viol_AL <- left_join(AL_bg, Lead_viol_AL)

##Convert to spatial object

st_as_sf(Lead_viol_AL) 


###Plot using ggplot2

Lead_plot_AL <- ggplot() + 
  geom_sf(data = Lead_viol_AL, aes(fill = avg_viol_CBG, geometry = geometry), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = AL_bg, fill = NA, colour = NA, size= 0.05) + 
  ggthemes::theme_map() + 
  theme(legend.position = "right")

Lead_plot_AL

plot_path <- "Plots"

ggsave(filename = 'Alabama_LCR_CBG_viol.png', path = plot_path)

##SUCCESS!!


##Now by tract level :NJ

Lead_viol_NJ <- Lead_violations %>%
  filter(ST_ABBREV == "NJ")

NJ_tract <- tigris::tracts("NJ") %>%
  rename(tract = GEOID) %>%
  st_transform(crs = 4326)

Lead_viol_NJ <- left_join(NJ_tract, Lead_viol_NJ)

st_as_sf(Lead_viol_NJ) 

Lead_plot_NJ <- ggplot() + 
  geom_sf(data = Lead_viol_NJ, aes(fill = AVG_tract_viol, geometry = geometry), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = NJ_tract, fill = NA, colour = NA, size= 0.05) + 
  ggthemes::theme_map() + 
  theme(legend.position = "right")

Lead_plot_NJ

ggsave(filename = 'NJ_tract_LCR_violations.png', path = plot_path)

##Success! A map with different census tracts showing the average number of LCR
##violations per tract

##Now, at the county level

WA_counties <- tigris::counties("WA") %>%
  rename(county = GEOID) %>%
  st_transform(crs = 4326)

Lead_viol_WA <- Lead_violations %>%
  filter(ST_ABBREV == "WA") %>%
  mutate(county = substring(ID, first=1, last=5)) 

Lead_viol_WA <- left_join(WA_counties, Lead_viol_WA)

st_as_sf(Lead_viol_WA)

Lead_viol_WA <- Lead_viol_WA %>%
  group_by(county) %>%
  mutate(avg_county_viol = mean(total_violations)) 

Lead_plot_WA <- ggplot() + 
  geom_sf(data = Lead_viol_WA, aes(fill = avg_county_viol, geometry = geometry), color = NA) +
  scale_fill_distiller(palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = WA_counties, fill = NA, colour = NA, size= 0.05) + 
  ggthemes::theme_map() + 
  theme(legend.position = "right")

Lead_plot_WA

ggsave(filename = 'WA_county_LCR_violations.png', path = plot_path)


##########################################################################################
##########################################################################################

summary(Lead_violations[c("avg_viol_CBG", "MINORPCT", "PRE1960PCT", "LOWINCPCT")])

##Model the relationship between the number of violations and EJ indicators

#CBG-level - National

LCR_lm <- glm(avg_viol_CBG ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = Lead_violations, family = negative.binomial(theta = 1.5))
summary(LCR_lm)

##CBG level in subset - NJ

LCR_lmNJ <- glm(avg_viol_CBG ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = Lead_viol_NJ, family = negative.binomial(theta = 1.5))
summary(LCR_lmNJ)

#tract-level : NJ

LCR_lm_tr <- lm(AVG_tract_viol ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = Lead_viol_NJ)
summary(LCR_lm_tr)


##Interactive map (similar to leaflet)

Lead_viol_NJ <- Lead_violations %>%
  filter(ST_ABBREV == "NJ")

NJ_bg <- tigris::block_groups("NJ") %>%
  rename(tract = GEOID) %>%
  st_transform(crs = 4326)

Lead_viol_NJ <- left_join(NJ_bg, Lead_viol_NJ)

st_as_sf(Lead_viol_NJ)

tmap_mode("view")
tm_shape(Lead_viol_NJ) +
  tm_polygons(col = "avg_viol_CBG", alpha = 0.5, midpoint = 0) +
  tm_basemap("Esri.WorldTopoMap")





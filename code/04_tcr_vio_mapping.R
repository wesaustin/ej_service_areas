


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

# Load tcr violations data and sb_dems_v3 data (containing demographic / shapefile info)

tcr_vio <- read.csv("Data/indicator_tcr.csv")

pwsid_cbg <- read.csv("Data/sb_dems_area_v3.csv")

# Match case for either dataset (if inconsistent)
tcr_vio <- tcr_vio %>%
  rename(PWSID = pwsid)

# Left join the full PWSID dataset with the health violations data, pad shortened CBG IDs w/ zeros

cbg_tcr_vio <- left_join(pwsid_cbg, tcr_vio, by = "PWSID", relationship = "many-to-many")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

rm(tcr_vio, pwsid_cbg) #remove files if unnecessary

# For tracts with PWSIDs, replace null values with zeroes

cbg_tcr_vio$detection_share[is.na(cbg_tcr_vio$detection_share)] <- 0

write_rds(cbg_tcr_vio, "Data/cbg_tcr_vio_combined.rds") #save data for future use (as rds because will convert variable types otherwise)

# For CBG with more than one PWSID, find average number of violations at the CBG level
# then collapse to include only 1 observation per census tract level (12-digit "ID") 

cbg_tcr_vio <- cbg_tcr_vio %>%
  group_by(ID) %>%
  mutate(tcr_det_cbg = sum(detection_share*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE)

################################################################################
## Make a subset for testing, state-level analysis
################################################################################

# Maryland

tcr_vio_MD <- cbg_tcr_vio %>%
  filter(ST_ABBREV == "MD") %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(tcr_det_tract = sum(detection_share*ACSTOTPOP)/sum(ACSTOTPOP)) #average PFAs count per tract


################################################################################
## Download Census Division Boundaries for Maryland and New Jersey 
## and Join to health violations data  data
################################################################################

## Grab MD block group boundaries

MD_bg <- tigris::block_groups("MD") %>% #block groups
  rename(ID = GEOID) %>%
  st_transform(crs = 4326) 

MD_tract <- tigris::tracts("MD") %>% #tracts
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(MD_bg) #check CRS
st_crs(MD_tract)

tcr_vio_MD <- left_join(MD_bg, tcr_vio_MD) #join by CBG

st_as_sf(tcr_vio_MD) ##Convert to spatial object


################################################################################
## Maps 
################################################################################

# Map 1 - Maryland CBG GGPLOT
### Plot using ggplot2

plot_MD_bg <- ggplot() + 
  geom_sf(data = tcr_vio_MD, aes(fill = tcr_det_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average TCR detections per CBG", palette = "RdPu", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

plot_MD_bg

##SUCCESS!!

ggsave(filename = 'MD_tcr_det_CBG.png', path = plot_path)

# Map 2 : Avg. PFAs concentration

plot_MD_bg <- ggplot() + 
  geom_sf(data = tcr_vio_MD, aes(fill = avg_conc_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average PFAs concentration per CBG", palette = "RdPu", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

plot_MD_bg

ggsave(filename = 'MD_tcr_conc_CBG.png', path = plot_path)


################################################################################
## Regressions of violations on demographic characteristics at the tract level  
################################################################################

summary(cbg_tcr_vio[c("tcr_det_cbg", "MINORPCT", "PRE1960PCT", "LOWINCPCT")])

##Standard linear model

summary(tcrV_lm <- lm(tcr_det_cbg ~ MINORPCT + LOWINCPCT, data = tcr_vio_MD))

summary(tcrV_lm2 <- lm(avg_conc_cbg ~ MINORPCT + LOWINCPCT, data = tcr_vio_MD))

##Negative binomial regression, per Statman-Weil et al.

library(MASS)

summary(m1 <- glm.nb(tcr_det_cbg ~ MINORPCT + LOWINCPCT, data = cbg_tcr_vio))

summary(m2 <- lm(avg_conc_cbg ~ MINORPCT + PRE1960PCT + LOWINCPCT, data = cbg_tcr_vio))


################################################################################
## Supplemental
################################################################################

##Interactive map (similar to leaflet)

tmap_mode("view")
tm_shape(tcr_vio_MD) +
  tm_polygons(col = "tcr_det_cbg", alpha = 0.5, midpoint = 0) +
  tm_basemap("Esri.WorldTopoMap")


##Country level for all US (note I did not run this because my computer couldn't handle 
##this big a dataset, TB)

US_counties <- tigris::counties() %>%
  rename(county = GEOID) %>%
  st_transform(crs = 4326)

tcr_vio_all <- cbg_tcr_vio %>%
  mutate(county = substring(ID, first=1, last=5)) %>%
  group_by(county) %>%
  mutate(tcr_det_county = sum(tcr_det*ACSTOTPOP)/sum(ACSTOTPOP))

tcr_vio_all <- left_join(US_counties, tcr_vio_all)

st_as_sf(tcr_vio_all)

ggplot() + 
  geom_sf(data = tcr_vio_all, aes(fill = tcr_det_county, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per county", palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.title = element_text(size = 8), legend.position = "right", legend.box.background = element_blank())

ggsave(filename = 'tcr_vio_counties_USA.png', path = plot_path)


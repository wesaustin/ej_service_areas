


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
library(future.apply)
library(parallel)
library(readxl)


##Set wd

setwd("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/")
# setwd("C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/R_GIS/")

LCR_samp <- read_csv("data/water quality/SDWA_LCR_SAMPLES.csv")
SDWA_violations <- read_csv("data/water quality/SDWA_VIOLATIONS.csv")

################################################################################
##### Moving code from "drinking_water_indicators to here#######################
################################################################################

#Lead Action Level Exceedences

#find the samples exceeding 0.015 mg/l lead and group by PWSID
LCR_exc <- LCR_samp %>%
  filter(SAMPLE_MEASURE > 0.015) 

#get counts for number of violations by PWSID 

PWSID_LCR_exc <- LCR_exc %>%
  group_by(PWSID) %>%
  mutate(total_violations = n())
# Parsing errors? 

#Now select the max sample measure for each PSWID
PWSID_LCR_2 <- PWSID_LCR_exc %>%
  ungroup(PWSID) %>%
  filter(SAMPLE_MEASURE == max(SAMPLE_MEASURE), .by = PWSID) 

#We now want to clean this data up, keeping only the total number of violations per PWSID and the maximum contamination in each

LCR_violations_per_PWSID <- PWSID_LCR_2 %>%
  dplyr::select(PWSID, SAMPLE_MEASURE, UNIT_OF_MEASURE, total_violations, SAMPLING_END_DATE) %>%
  rename(Maximum_sample_exceedence = SAMPLE_MEASURE) %>%
  rename(Units = UNIT_OF_MEASURE)

#I also kept the date, to keep a sense of the distribution for when these violations occurred

#### WA: These are doubled up. Could be streamlined. 
write.csv(LCR_violations_per_PWSID, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/ej_service_areas/data/indicators/LCR_violations_per_PWSID.csv")
LCR_violations <- read.csv("ej_service_areas/data/indicators/LCR_violations_per_PWSID.csv")

#PWSID_geodemog_data <- read.csv("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/sb_dems_area_v3.csv")
PWSID_geodemog_data <- read_excel("data/demographics/sb_dems_area_v3.xlsx")

##Double check that the "PWSID" column in both datasets match case to enable join by 
##PWSID, if not use "by = c("col1" = "col2") to merge the datasets
#LCR_viol_merge <- full_join(LCR_violations, PWSID_geodemog_data, by = "PWSID")
#LCR_viol_left <- left_join(LCR_violations, PWSID_geodemog_data, by = "PWSID")


PWSID_geodemog_data <- PWSID_geodemog_data %>%
  rename(PWSID = pwsid)

LCR_viol_inner <- inner_join(LCR_violations, PWSID_geodemog_data, by = "PWSID")

  # Warning message:
  #   In inner_join(LCR_violations, PWSID_geodemog_data, by = "PWSID") :
  #   Detected an unexpected many-to-many relationship between `x` and `y`.
  # ℹ Row 1 of `x` matches multiple rows in `y`.
  # ℹ Row 325534 of `y` matches multiple rows in `x`.
  # ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.

###Collapse to the census block group level (12-digit "ID") 

##First, for CBG with more than one PWSID, find average number of violations at the CBG level
##then collapse to include only 1 observation per CBG
##Here I keep only the calculated avg. number of violations per CBG, the max exceedence, when the sample 
##was taken, and the relevant IDs numbers
##note that since I am using the inner join, there may be some PWSIDs that disappear in the case
##where there are multiple per CBG

LCR_viol_2 <- LCR_viol_inner %>%
  group_by(ID) %>%
  mutate(AVG_CBG_viol= mean(total_violations)) %>%
  distinct(ID, .keep_all = TRUE)

### Mapping EJ screen maps and incorporating Indicator data

##Install / load packages

# install.packages('devtools')
# library(devtools)

epic_data <- st_read("epic_boundaries/temm.gpkg")

## Convert projection

epic_boundaries <-  epic_data %>%
  st_as_sf(sf_column_name=geom, crs=4326)

## Remove missing boundaries 

epic_areas <- epic_data %>% 
  filter(!st_is_empty(.)) %>%
  rename(PWSID = pwsid)

st_write(epic_areas, "generated_boundaries_from_epic.shp")
# 1: In abbreviate_shapefile_names(obj) :
#   Field names abbreviated for ESRI Shapefile driver
# 2: In CPL_write_ogr(obj, dsn, layer, driver, as.character(dataset_options),  :
#                       GDAL Message 1: Value 'FAIR HAVEN BORO,FREEHOLD TWP,HOLMDEL TWP,HOWELL TWP,ASBURY PARK CITY,BRADLEY BEACH BORO,LAVALETTE BORO,LAKEWOOD TWP,BAY HEAD BORO,BRICK TWP,TOMS RIVER,OCEANPORT BORO,RED BANK BORO,RUMSON BORO,SHREWSBURY BORO,UNION BEACH BORO,WEST LONG BRANCH BORO,ABERDEEN TWP,MIDDLETOWN TWP,NEPTUNE TWP,NEPTUNE CITY BORO,TINTON FALLS,OCEAN TWP,KEANSBURG BORO,LITTLE SILVER BORO,LONG BRANCH CITY,EATONTOWN BORO' of field cty_srv has been truncated to 254 characters.  This warning will not be emitted any more for that layer.

popup_id <- paste0("<strong>Name: </strong>", 
                   epic_areas$pws_name)


#mapping the epic areas
library(leaflet)
leaflet(data= epic_areas) %>%
  setView(-81,35, zoom=6) %>%
  addTiles %>%
  addPolygons(popup = popup_id)

##EJ screen data uses centroid, lat and long, as well as shape ID for mapping

##combine Epic boundaries with LCR violations data

LCR_viol_geodata <- inner_join(LCR_viol_2, epic_areas, by = "PWSID") %>%
  distinct(ID, .keep_all = TRUE)

LCR_viol_geo_sf <- st_as_sf(LCR_viol_geodata)


##Now you can plot, example of plot showing violating counties 

###Check CRS: 
st_crs(LCR_viol_geo_sf)

##make a subset for testing

LCR_viol_CA <- LCR_viol_geo_sf %>%
  filter(ST_ABBREV == "CA")

###Plot using ggplot2

LCR_plot_CA <- ggplot(data = LCR_viol_CA, aes(fill = AVG_CBG_viol)) + 
  geom_sf() 

LCR_plot_CA

tmap_mode("view")
tm_shape(LCR_viol_CA) +
  tm_polygons(col = "AVG_CBG_viol", midpoint = 0) +
  tm_basemap("Esri.WorldTopoMap")

##make a subset for testing

LCR_viol_IL <- LCR_viol_geo_sf %>%
  filter(ST_ABBREV == "IL")

LCR_plot_IL <- ggplot(data = LCR_viol_IL, aes(fill = AVG_CBG_viol)) + 
  geom_sf() 

LCR_plot_IL

#error message pops up but the plot appears? 



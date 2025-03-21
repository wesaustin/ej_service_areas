library(tidyverse)
library(sf)
library(leaflet)
library(tigris)

#if not in project subfolder, navigate to main folder
if(str_detect(getwd(), "ej_service_areas/")){
  getwd()
  setwd('../..')
  getwd()  
}


#if subfolder for County boundaries does not exist, create it
mainDir <- "Data/generated_boundaries/"
subDir <- "generated_boundaries_county"
dir.create(file.path(mainDir, subDir))


#get national County map
counties_map <- counties(cb=T, resolution="20m") 
counties_crs <- st_crs(counties_map)
getwd()
#open EPIC boundaries and merge with county boundaries to get crosswalk
epic_areas <- st_read("Data/generated_boundaries/epic_boundaries/generated_boundaries_from_epic.shp") %>% 
  dplyr::select(pwsid, pws_nam) %>% 
  st_transform(counties_crs)

#----------------------------FIND PRINCIPAL COUNTY------------------------------

# principal_county_served_gis <-  st_intersection(st_buffer(epic_areas,0), st_buffer(counties_map,0))
# principal_county_served_gis$area_county <- st_area(principal_county_served_gis$geo)


county_boundaries_using_EPIC <- st_read("Data/generated_boundaries/epic_boundaries/generated_boundaries_from_epic.shp") %>% 
  # filter(pws_nam == "ORANGE WATER & SEWER AUTHORITY") %>% 
  dplyr::select(pwsid, pws_nam) %>% 
  st_transform(counties_crs) %>% 
  st_join(counties_map, join=st_intersection) %>% 
  ungroup %>% 
  mutate(area_county = st_area(geometry))
# county_boundaries_using_EPIC$area_county <- st_area(county_boundaries_using_EPIC$geometry)

county_boundaries_using_EPIC_df <- county_boundaries_using_EPIC %>% 
  st_drop_geometry() %>% 
  data.frame() %>% 
  group_by(pwsid) %>% 
  mutate(max_area = max(area_county)) %>% 
  ungroup %>% 
  filter(area_county==max_area)

#use crosswalk to melt county map to PWSID using EPIC boundaries
counties_map_pwsids <- counties_map %>% 
  left_join(county_boundaries_using_EPIC_df) %>% 
  filter(!is.na(pwsid))  %>% 
  group_by(pwsid, pws_nam) %>% 
  summarize(geometry = st_union(geometry))
st_write(counties_map_pwsids, paste0("Data/generated_boundaries/",subDir,"/generated_boundaries_using_county.shp"), update = TRUE)

# 
# counties_map_pwsids <- st_read(paste0("Data/generated_boundaries/",subDir,"/generated_boundaries_using_county.shp"))

# 
# #visualize all water systems using county boundaries (a bit a mess, hard to see because of overlapping boundaries)
# leaflet() %>%
#   setView(-81,35, zoom=6) %>%
#   addTiles %>%
#   addPolygons(data= counties_map_pwsids)


#visualize difference between EPIC boundary and County Aggregation using Cherokee Water System
# another good example #ORANGE WATER & SEWER AUTHORITY
subset_epic <- epic_areas[epic_areas$pws_nam=="ORANGE WATER & SEWER AUTHORITY",]
subset_counties <- counties_map_pwsids[counties_map_pwsids$pws_nam=="ORANGE WATER & SEWER AUTHORITY",]
popup_id_county <- paste0("<strong>Name: </strong>",
                   subset_counties$pws_nam)
popup_id_epic <- paste0("<strong>Name: </strong>",
                        subset_epic$pws_nam)
leaflet() %>%
  setView(-81,35, zoom=6) %>%
  addTiles %>%
  addPolygons(data = subset_epic, popup = subset_epic, color="red")  %>%
  addPolygons(data = subset_counties, popup = subset_counties, color="blue") 


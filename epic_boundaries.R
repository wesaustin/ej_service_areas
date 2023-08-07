library(tidyverse)
library(sf)
library(leaflet)

getwd()
setwd('../..')
getwd()



#limiting to first 100 PWS IDs to develop code
epic_data <- st_read("Data/epic_areas/temm.gpkg")[1:100,]

epic_areas_with_missing_polygons <- epic_data[st_is_empty(epic_data),,drop=T]

epic_areas <- epic_data %>% 
  filter(!st_is_empty(.))

st_write(epic_areas, "Data/epic_areas/generated_boundaries_from_epic.shp")

popup_id <- paste0("<strong>Name: </strong>", 
                   epic_areas$pws_name)

leaflet(data= epic_areas) %>%
  setView(-81,35, zoom=6) %>%
  addTiles %>%
  addPolygons(popup = popup_id)
 

  

 
  
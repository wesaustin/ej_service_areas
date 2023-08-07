library(tidyverse)
library(sf)
library(leaflet)

getwd()
setwd('../..')
getwd()



 
epic_data <- st_read("Data/epic_areas/temm.gpkg")

popup_id <- paste0("<strong>Name: </strong>", 
                   epic_data$pws_name)

leaflet(data= epic_data[1:100,]) %>%
  setView(-81,35, zoom=6) %>%
  addTiles %>%
  addPolygons(popup = popup_id)
 

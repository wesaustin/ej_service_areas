rm(list=ls(all=TRUE))
# detach("package:EJSCREENbatch", unload=TRUE)
# devtools::install_github(repo = "USEPA/EJSCREENbatch"
#                          ,ref="main"
#                          ,force=TRUE)

library(tidyverse)
library(sf)
library(leaflet)
library(EJSCREENbatch)
library(pool)


#if not in project subfolder, navigate to main folder
if(str_detect(getwd(), "ej_service_areas/")){
  getwd()
  setwd('../..')
  getwd()  
}
getwd()
setwd("/Users/arelkhattabi/Dropbox/shared/ej_service_areas/Data/")
list.files()

#data to loop through
epic_data <- st_read("generated_boundaries/epic_boundaries/generated_boundaries_from_epic.shp") %>% 
  filter(row_number()<11)%>% 
  st_transform(4326)

leaflet() %>% 
  setView(-81,35, zoom=6) %>% 
  addTiles %>% 
  addPolygons(data=epic_data)


path.raster.layer <- "geotiff"
ej.results1 <- EJfunction(data_type="landbased",
                          LOI_data = epic_data,
                          working_dir = getwd(),
                          gis_option="robust",
                          raster_data=path.raster.layer,
                          input_name = "pws_nam",
                          web=F,
                          buffer = c(1),
                          produce_ancillary_tables = F)




###############################################################################
# Load Shapefile of Counties Served by PWSIDs 
# National Center for Environemental Economics 
# Last edited 9/18/23
###############################################################################


# Load libraries, directories 
library(tidyverse)
library(sf)
library(leaflet)
library(tigris)
library(readxl)
library(writexl)
library(janitor) 

#if not in project subfolder, navigate to main folder
# if(str_detect(getwd(), "ej_service_areas/")){
#   getwd()
#   setwd('../..')
#   getwd()  
# }

#if subfolder for County boundaries does not exist, create it
mainDir <- "data/generated_boundaries/"
subDir <- "generated_boundaries_county"
getwd()


###############################################################################
# Get shapefiles and county boundaries 
###############################################################################

# National map of counties
counties_map <- counties(cb=T, resolution="20m") 
counties_crs <- st_crs(counties_map)

# Service areas linked to counties
sb_county <- sf::st_read('C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/generated_boundaries/county/generated_boundaries_using_county.shp')

###############################################################################
# Clean county boundaries
###############################################################################

# Note must use the principal county because in many cases these overlap with multiple counties
  
fill.me <- vector(mode = 'list', length = dim(counties_map)[1])
for (i in 1:dim(counties_map)[1]){
  fill.me[[i]] <- counties_map %>%
    dplyr::slice(i) %>%
    sf::st_transform(crs = 4326)
  
  if(i %% 10 == 0){
    print(paste0('Row ',i,' complete.'))
  }
} 

together <- data.table::rbindlist(fill.me) %>%
  sf::st_as_sf(., crs = 4326) %>%
  sf::st_make_valid()

###############################################################################
# EJfunction 
###############################################################################

# Tabulate states
together %>%
  group_by(STUSPS) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3)) %>%
  print(n = 100)

together <- together  %>% 
  filter(!(STUSPS %in% c("GU","MP","VI","AS","PR","AK","HI")))

# Source modified files from EJSCREENbatch
sapply(list.files('C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/pfas_npdwr_ej/2023_analysis/R', full.names=TRUE), source)

# Call modified EJfunction
batch.output <- EJfunction(LOI_data = together, data_year = 2021, buffer = 0, raster = T)

###############################################################################
# Merge County-level Demographic Data back to the Full PWS Dataset and Save Files
###############################################################################

county_data <- batch.output$EJ.loi.data$LOI_radius_2021_0mi %>% 
  st_drop_geometry
pws_county_dems <- left_join(sb_county,  county_data, by = 'GEOID' )  %>% 
  st_drop_geometry
  

# Save output from batch tool

pws_county_dems %>%
    write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/county_dems.csv')
write_xlsx(pws_county_dems, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/county_dems.xlsx")





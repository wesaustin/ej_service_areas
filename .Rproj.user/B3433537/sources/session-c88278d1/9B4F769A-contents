###############################################################################
# Load Shapefile of Counties Served by PWSIDs 
# National Center for Environemental Economics 
# Last edited 9/18/23
###############################################################################


# Description of program: This program runs the EJSCREENbatch tool to derive population-
# level statistics for all counties in the US. It then joins these data to a PWS-county 
# database to impute county demographic information across all public water systems. 


###############################################################################
# Load libraries, directories 
###############################################################################

library(devtools)
#install_github('USEPA/EJSCREENbatch', force = TRUE)

list.of.packages <- c("janitor","writexl","readxl","tigris","leaflet","sf","tidyverse","EJSCREENbatch")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)


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
  filter(!(STUSPS %in% c("GU","MP","VI","AS","PR")))


# Call modified EJfunction
batch.output <- EJfunction(LOI_data = together, data_year = 2021, buffer = 0, raster = T)



###############################################################################
# Merge County-level Demographic Data back to the Full PWS Dataset and Save Files
###############################################################################


county_data <- batch.output$EJ.loi.data$LOI_radius_2021_0mi %>% 
  st_drop_geometry
pws_county_dems <- left_join(sb_county,  county_data, by = 'GEOID' )  %>% 
  st_drop_geometry 
  

###############################################################################
# Add in population
###############################################################################


pops <- read_csv("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/SDWA_PUB_WATER_SYSTEMS.csv") %>%
  select(PWSID, POPULATION_SERVED_COUNT) %>%
  rename(pwsid = PWSID) %>%
  rename(population_served_count = POPULATION_SERVED_COUNT) %>%
  distinct(pwsid, .keep_all = TRUE)

pws_county_dems <- pws_county_dems %>% 
  distinct(pwsid, .keep_all = TRUE) %>%
  left_join(county_dems, pops, by = 'pwsid' )
  
# Save output from batch tool

pws_county_dems %>%
    write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/county_dems.csv')
write_xlsx(pws_county_dems, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/county_dems.xlsx")







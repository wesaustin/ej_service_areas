

###############################################################################
# EJSCREENBATCH for the Water System Service Boundaries - USGS Boundaries
# National Center for Environmental Economics 
# Last edited 10/23/23
###############################################################################


#install.packages('devtools')
library(devtools)

# Other libraries
list.of.packages <- c("magrittr","cowplot","devtools","sf","tidyverse","magick","mapdata","sp",
                      "tidycensus","areal","stringr","openxlsx","readxl",
                      "lubridate","leaflet","biscale","mapview",
                      "tidyr","stringr","ggplot2","writexl","maps","janitor")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)


###############################################################################
# Import Locations
###############################################################################

# Import USGS boundaries
sb <- st_read("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/USGS_boundaries/WSA_v1.shp") %>%
  clean_names()
# 18,806 systems

# Convert projection, rename vars, add state variable
sb_sf <-  sb %>%
  st_as_sf(sf_column_name=geom, crs=4326) %>%
  rename(pwsid = wsa_agidf) %>%
  mutate(state = substr(pwsid,1,2)) 

sb_sf <- st_set_geometry(sb_sf, "geometry") 
sb_sf <-  sf::st_make_valid(sb_sf)

# Tabulate states
table(sb_sf$state)
# sb_sf <- !(sb_sf$state %in% c("GU","MP","VI","AS"))
#Not necessary to drop anything

# Map The Sites - or a subset for speed 
#mapview(sb_sf)
sb_sf_subset <- sb_sf[1:1000,]
mapview(sb_sf)


###############################################################################
# Land Screen Call Over Zipcodes 
###############################################################################

# Source modified files from EJSCREENbatch
# sapply(list.files('C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/pfas_npdwr_ej/2023_analysis/R', full.names=TRUE), source)

# Call modified EJfunction
usgs_data <- EJfunction(LOI_data = sb_sf, 
                           data_year = 2021, 
                           buffer = 0, 
                           raster = T)

# Save Just the zipcode data

save <- usgs_data$EJ.loi.data$LOI_radius_2021_0mi %>% 
  st_drop_geometry %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/usgs_dems.csv')
write_xlsx(save, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/usgs_dems.xlsx")



################################################################################
# Done saving file. Have a great day!
################################################################################




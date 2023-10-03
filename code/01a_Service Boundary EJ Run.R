###############################################################################
# EJSCREENBATCH for the Water System Service Boundaries
# National Center for Environmental Economics 
# Last edited 8/6/23
###############################################################################

# set working directory
setwd("D:/Dropbox (Personal)/Batch_EJSCREEN/")
raster.path <- "D:/Dropbox (Personal)/Batch_EJSCREEN/geotiff/"

# dir.create('US Census Grid_SF2010_TIFF')
# download.file('https://adamtheising.github.io/Papers/uspop10.zip', './US Census Grid_SF2010_TIFF/uspop10.zip')
# raster.path <- paste0(getwd(), '/US Census Grid_SF2010_TIFF')
# unzip(zipfile = "./US Census Grid_SF2010_TIFF/uspop10.zip", exdir = raster.path)

myPaths <- .libPaths()
myPaths <- c(myPaths, 'D:/packages')

# requires devtools to install

install.packages('devtools')
library(devtools)

# install from repository
install_github('USEPA/EJSCREENbatch', build_vignettes=FALSE)
library(EJSCREENbatch)

# Other libraries

list.of.packages <- c("magrittr","cowplot","devtools","sf","tidyverse","magick","mapdata","sp",
                      "tidycensus","areal","stringr","openxlsx","readxl",
                      "lubridate","leaflet","biscale","mapview",
                      "tidyr","stringr","ggplot2","writexl","maps")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

###############################################################################
# Import Locations
###############################################################################

# Service Boundaries : Last downloaded 8/2/23, using version 3.0 of the data. 
  # Link: https://www.hydroshare.org/resource/9ebc0a0b43b843b9835830ffffdd971e/

sb <- st_read("D:/Dropbox (Personal)/Batch_EJSCREEN/data/temm.gpkg")

# Convert projection
sb_sf <-  sb %>%
          st_as_sf(sf_column_name=geom, crs=4326)

# Rename geom to geometry so that the EJfunction doesn't trip 
sb_sf <- st_set_geometry(sb_sf, "geometry") 

# Map The Sites - or a subset for speed 
  #mapview(sb_sf)
sb_sf_subset <- sb_sf[1:1000,]
mapview(sb_sf_subset)


###############################################################################
# Land Screen Call
###############################################################################

# Remove missing geometries - Necessary for function to run. 
# sb_sf_narm <- sb_sf[!is.na(st_geometry(sb_sf)),]

landdata <- EJfunction(data_type = "landbased",
                       LOI_data = sb_sf,
                       gis_option = "robust", 
                       buffer = 0,
                       produce_ancillary_tables = F,
                       raster_data = raster.path)

# Rename the output for convenience 

  # This is water-system level information. 
  pws_dem_data <- landdata$EJ.facil.data$facil_robust_radius0mi %>%
    filter(geography == "raw") # There are three obs for each system, one for raw data, another for 
    # state comparisons, and another for national ones. Keeping just the raw data. 
  
  # This is the CBG-level info. 
  cbg_dem_data <- landdata$EJ.list.data$area3_robust_radius0mi

# Drop geographies from PWS-level dataset and join to PWSIDs by shape_ID 
  
  pws_dem_data <- pws_dem_data %>%
      select(-geometry)
  
# Join PWS-level info to the info at the CBG level for PWSIDs and tiering information 
  
pws_dem_data <- pws_dem_data  %>%
    left_join(cbg_dem_data  %>%
    select('shape_ID','pwsid','pws_name','tier')  %>%
    distinct() , 
  by = 'shape_ID')


# Save Data 

write_xlsx(pws_dem_data,"D:/Dropbox (Personal)/Batch_EJSCREEN/sb_dems_v3.xlsx")
write_xlsx(cbg_dem_data,"D:/Dropbox (Personal)/Batch_EJSCREEN/sb_dems_area_v3.xlsx")

write_xlsx(pws_dem_data,"D:/Dropbox (Personal)/wildfire_water/demographics/sb_dems_v3.xlsx")
write_xlsx(cbg_dem_data,"D:/Dropbox (Personal)/wildfire_water/demographics/sb_dems_area_v3.xlsx")



###############################################################################
# EJSCREENBATCH for the Water System Service Boundaries
# National Center for Environmental Economics 
# Last edited 8/18/22
###############################################################################

# set working directory
setwd("D:/Dropbox (Personal)/Batch_EJSCREEN/")
raster.path <- "D:/Dropbox (Personal)/Batch_EJSCREEN/geotiff/"

# Download Raster 
dir.create('US Census Grid_SF2010_TIFF')
download.file('https://adamtheising.github.io/Papers/uspop10.zip', './US Census Grid_SF2010_TIFF/uspop10.zip')
raster.path <- paste0(getwd(), '/US Census Grid_SF2010_TIFF')
unzip(zipfile = "./US Census Grid_SF2010_TIFF/uspop10.zip", exdir = raster.path)

# Set package location 
myPaths <- .libPaths()
myPaths <- c(myPaths, 'D:/packages')

# Install all necessary packages and import to library 

#install.packages('devtools')
library(devtools)

#install_github('USEPA/EJSCREENbatch', build_vignettes=TRUE)
library(EJSCREENbatch)

# Other libraries
list.of.packages <- c("magrittr","cowplot","devtools","sf","tidyverse","magick","mapdata","sf","sp","tidycensus","areal","stringr","openxlsx","readxl","maps","data.table","lubridate","leaflet","biscale","mapview","tidyr","stringr","ggplot2","writexl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)


###############################################################################
# Import Locations
###############################################################################

# Service Boundaries : Last downloaded 8/18/22
sb <- read_sf("D:/Dropbox (Personal)/Batch_EJSCREEN/temm.geojson")

# Convert to Geodata with the correct projection
sb_sf <-  sb %>%
  st_as_sf(sf_column_name=geometry, crs=4326)

# Map The Sites
mapview(sb_sf$geometry)

# Import State Data
state.data <- map_data('state')


###############################################################################
# Land Screen Call
###############################################################################

rm(landdata)
landdata <- EJfunction(data_type = "landbased",
                       LOI_data = sb_sf,
                       gis_option = "fast", 
                       buffer = 0.01,
                       raster_data = raster.path)


# Save Data 

write_xlsx(landdata$EJ.facil.data$facil_fast_radius0mi,"D:/Dropbox (Personal)/toxic flooding/sb_dems.xlsx")


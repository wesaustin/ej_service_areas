

###############################################################################
# EJSCREENBATCH for the Water System Service Boundaries - ZIPCODES SERVED
# National Center for Environemental Economics 
# Last edited 8/22/23
###############################################################################

# requires devtools to install

#install.packages('devtools')
library(devtools)

# install from repository
#install_github('USEPA/EJSCREENbatch', build_vignettes=FALSE)
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


sb <- st_read("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/ZIP_Codes_Served/UCMR_3_5/generated_boundaries_using_zip_codes.shp")

# Convert projection
sb_sf <-  sb %>%
  st_as_sf(sf_column_name=geom, crs=4326)

sb_sf$state <- substr(sb_sf$PWSID,1,2)

# Tabulate states
sb_sf %>%
  group_by(state) %>%
  summarise(n = n()) %>%
  mutate(
    totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumuPer = round(cumsum(freq = n / sum(n)), 3)) %>%
  print(n = 100)


sb_sf <- !(sb_sf$state %in% c("GU","MP","VI","AS"))
table(sb_sf$state)
unique(sb_sf[,'state'])

# Rename geom to geometry so that the EJfunction doesn't trip 
sb_sf <- st_set_geometry(sb_sf, "geometry") 

# Map The Sites - or a subset for speed 
#mapview(sb_sf)
sb_sf_subset <- sb_sf[1:1000,]
mapview(sb_sf)


###############################################################################
# Land Screen Call Over Zipcodes 
###############################################################################

# Remove missing geometries - Necessary for function to run. 
# sb_sf_narm <- sb_sf[!is.na(st_geometry(sb_sf)),]

# zipcodes <- EJfunction(LOI_data = sb_sf,
#                        data_year = 2021, 
#                        buffer = 0.0001,
#                        raster = T)

zc_data <- EJfunction(LOI_data = zipcodeboundaries,
                       data_year = 2021, 
                       buffer = 0.0001,
                       raster = T)


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



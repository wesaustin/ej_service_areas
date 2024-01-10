###############################################################################
# EJSCREENBATCH for the Water System Service Boundaries
# National Center for Environmental Economics 
# Last edited 11/30/23
###############################################################################

#install.packages('devtools')
library(devtools)

# Other libraries
list.of.packages <- c("magrittr","cowplot","devtools","sf","tidyverse","magick","mapdata","sp",
                      "tidycensus","areal","stringr","openxlsx","readxl",
                      "lubridate","leaflet","biscale","mapview",
                      "tidyr","stringr","ggplot2","writexl","maps","janitor", "EJSCREENbatch")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

###############################################################################
# Import Locations
###############################################################################

# Service Boundaries : Last downloaded 8/2/23, using version 3.0 of the data. 
  # Link: https://www.hydroshare.org/resource/9ebc0a0b43b843b9835830ffffdd971e/

sb <- st_read("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/epic_boundaries/temm.gpkg")

# Convert projection
sb_sf <-  sb %>%
          st_as_sf(sf_column_name=geom, crs=4326)

# Rename geom to geometry for the EJSB function and drop unnecessary fields
sb_sf <- st_set_geometry(sb_sf, "geometry") %>%
  filter(!(state_code %in% c("GU","MP","VI","AS","PR"))) %>%
  select(-c('primacy_agency_code', 'city_served', 'owner_type_code', 
            'is_wholesaler_ind', 'centroid_lat','centroid_lon','geometry_source_detail',
            'pred_05','pred_50','pred_95'))



###############################################################################
# Land Screen Call
###############################################################################


# Adding a new ID number for joining things later. 
sb_sf <- sb_sf %>%
  dplyr::mutate(sb_sf, newid = row_number())

chunks <- 49
chunk_size <- 1000
epic_list <- split(sb_sf, rep(1:49, each = 1000, length.out = nrow(sb_sf)))
results_list <- list()

# Call modified EJfunction over each chunk of data

for (i in 1:49) {
  result <- EJfunction(LOI_data = epic_list[[i]], data_year = 2021, buffer = 0, raster = T)
  results_list[[i]] <- result
}

# Need to combine all of the USGS and CBG files into one


for (i in 1:49) {
  EJ.loi.data <- results_list[[i]]$EJ.loi.data
  EJ.cbg.data <- results_list[[i]]$EJ.cbg.data
  loi_df <- EJ.loi.data$LOI_radius_2021_0mi
  cbg_df <- EJ.cbg.data$CBG_radius_2021_0mi
  if (i == 1){
    combined_loi <- loi_df
    combined_cbg <- cbg_df
  } else{
    combined_loi <- rbind(combined_loi, loi_df)
    combined_cbg <- rbind(combined_cbg, cbg_df)
  }
}


# Create key file to link LOIs to CBGs

cbg.shapes <- combined_cbg %>%
  dplyr::left_join(data.tog %>% dplyr::select('ID'), by = 'ID') %>%
  sf::st_as_sf() %>%
  tibble::rowid_to_column(var = 'cbg_ID')

loi.buffer <- combined_loi

linking.list <- sf::st_intersects(cbg.shapes, sb_sf) %>%
  as.data.frame() %>%
  dplyr::rename(cbg_ID = row.id) %>%
  dplyr::rename(shape_ID = col.id) %>%
  dplyr::left_join(cbg.shapes %>% 
                     dplyr::select(cbg_ID, ID) %>%
                     sf::st_drop_geometry(),
                   by = 'cbg_ID') %>%
  dplyr::left_join(loi.buffer %>%
                     dplyr::select(shape_ID, newid, pwsid) %>%
                     sf::st_drop_geometry(),
                   by = 'shape_ID') %>%
  dplyr::select(-cbg_ID)

linking.list <- linking.list %>%
  select(ID, pwsid) %>%
  drop_na(pwsid) %>%
  distinct() 


cbg_and_pwsid <- left_join(combined_cbg, linking.list, by = 'ID', relationship = "many-to-many") %>%
  distinct()

###############################################################################
# Save the EPIC data
###############################################################################

cbg_and_pwsid <- cbg_and_pwsid %>% 
  st_drop_geometry %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/epic_dems_area.csv')
# Too many observations for XLSX
#write_xlsx(cbg_and_pwsid, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/epic_dems_area.xlsx")


combined_loi <- combined_loi %>% 
  st_drop_geometry %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/epic_dems.csv')
write_xlsx(combined_loi, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/epic_dems.xlsx")



###############################################################################
# Old Code
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



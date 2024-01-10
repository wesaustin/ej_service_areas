

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


###############################################################################
# Service areas Hall & Murray 
###############################################################################

# sb_hm <- sf::st_read('C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/hall_murray/PWS_121423.gdb')
sb_hm <- sf::st_read('C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/hall_murray/hall_murray_sf.shp')


# clean dataframe
sb_hm <- sb_hm %>%
  clean_names() %>%
  sf::st_make_valid()  %>%
  st_transform(crs = 4326)

sb_hm <- sb_hm %>%
  st_make_valid() 

# Adding a new ID number for joining things later. 
sb_hm <- sb_hm %>%
  dplyr::mutate(sb_hm, newid = row_number())

# Removing 3-dimensional features
sb_hm <- sb_hm %>%
  st_zm()

#converting all polygons to multipolygons to keep one geometry type and preserve information 
sb_hm <- st_cast(sb_hm, "MULTIPOLYGON")

gts <- st_geometry_type(sb_hm)
unique(gts)


###############################################################################
# Service areas Hall & Murray - EJSB Run
###############################################################################


#result <- EJfunction(LOI_data = sb_hm, data_year = 2021, buffer = 0, raster = T)

chunks <- 28
chunk_size <- 1000
hm_list <- split(sb_hm, rep(1:28, each = 1000, length.out = nrow(sb_hm)))
rm(result,results_list)
results_list <- list()

# Call modified EJfunction over each chunk of data

for (i in 1:28) {
  result <- EJfunction(LOI_data = hm_list[[i]], data_year = 2021, buffer = 0, raster = T)
  results_list[[i]] <- result
}

# Need to combine all of the USGS and CBG files into one

for (i in 1:28) {
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

linking.list <- sf::st_intersects(cbg.shapes, sb_hm) %>%
  as.data.frame() %>%
  dplyr::rename(cbg_ID = row.id) %>%
  dplyr::rename(shape_ID = col.id) %>%
  dplyr::left_join(cbg.shapes %>% 
                     dplyr::select(cbg_ID, ID) %>%
                     sf::st_drop_geometry(),
                   by = 'cbg_ID') %>%
  dplyr::left_join(loi.buffer %>%
                     dplyr::select(shape_ID, pwsid_fina) %>%
                     sf::st_drop_geometry(),
                   by = 'shape_ID') %>%
  dplyr::select(-cbg_ID)

linking.list <- linking.list %>%
  select(ID, pwsid_fina) %>%
  drop_na(pwsid_fina) %>%
  distinct() 


cbg_and_pwsid <- left_join(combined_cbg, linking.list, by = 'ID', relationship = "many-to-many") %>%
  distinct()

###############################################################################
# Save the H&M data
###############################################################################

cbg_and_pwsid <- cbg_and_pwsid %>% 
  st_drop_geometry %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/epic_dems_area.csv')
write_xlsx(cbg_and_pwsid, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/epic_dems_area.xlsx")


combined_loi <- combined_loi %>% 
  st_drop_geometry %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/epic_dems.csv')
write_xlsx(combined_loi, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/epic_dems.xlsx")


linking.list <- linking.list %>% 
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/epic_cbg_links.csv')
write_xlsx(combined_loi, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/epic_cbg_links.xlsx")


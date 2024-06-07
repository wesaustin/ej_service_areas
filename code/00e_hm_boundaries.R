

###############################################################################
# Load Shapefile of Counties Served by PWSIDs 
# National Center for Environmental Economics 
# Last edited 9/18/23
###############################################################################


# Description of program: This program runs the EJSCREENbatch tool to derive population-
# level statistics for all counties in the US. It then joins these data to a PWS-county 
# database to impute county demographic information across all public water systems. 


###############################################################################
# Load libraries, directories 
###############################################################################

#TB directory 
#my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"
#setwd(paste0(my_path))

#WA
my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"
setwd(paste0(my_path))

library(devtools)
#install_github('USEPA/EJSCREENbatch', force = TRUE)

list.of.packages <- c("janitor","writexl","readxl","tigris","leaflet","sf","tidyverse","EJSCREENbatch","exactextractr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)


###############################################################################
# Service areas Hall & Murray 
###############################################################################

#sb_hm <- sf::st_read('Data/PWS_121423.gdb')
#sb_hm_shp <- sf::st_read("data/hall_murray_final/Service_Area_Boundaries/CWS_032424.shp")
sb_hm_shp <- sf::st_read("data/hall_murray_final/Final.shp")
#sb_hm_shp <- sf::st_read("data/hall_murray_final/Water_System_Boundaries.geojson")
#st_write(st_as_sf(sb_hm_shp), "data/hall_murray_final/Final_check.shp")

#sb_hm_shp$geometry <- st_cast(sb_hm_shp$geometry, "MULTIPOLYGON")


sb_hm_shp  <- sb_hm_shp %>%
  st_make_valid() %>%
  clean_names()

## The following just checks that all the geometries are valid 
  # 6/4/2024 latest version: all 44,415 polygons are valid!

 sb_hm_shp  <- sb_hm_shp %>%
   mutate(valid_id = case_when(
     st_is_valid(sb_hm_shp$geometry) == TRUE ~ T,
     TRUE ~ F
   ))
 
 valid <- sb_hm_shp %>%
   filter(valid_id == TRUE)
 
 not_valid <- sb_hm_shp %>%
   filter(valid_id == FALSE)

 not_valid <- write.csv(not_valid, 'data/hall_murray_final/hm_nonvalid_polyg.csv')

 sb_hm_shp <- sb_hm_shp %>%
  filter(!st_is_empty(geometry)) %>%
  st_transform(crs = 4326) %>%
  dplyr::mutate(newid = row_number()) %>%
  filter(valid_id == TRUE)

#  filter(pwsid != "DE0000825") #This is the problematic PWSID that gives an error in EJfunction

# Double check only multipolygons are present

gts <- st_geometry_type(sb_hm_shp)
unique(gts)

# sb_hm_val <- st_cast(sb_hm_val, "MULTIPOLYGON") %>%
#   st_zm() %>%
#   st_make_valid() %>%
#   mutate(dim = st_dimension(sb_hm_val))

# sb_hm_val<- filter(sb_hm_val, !sf::st_is_empty(geometry))
# sb_hm_val_v2 <- sb_hm_val[complete.cases(sb_hm_val), ]
# null_geoms <- st_is_empty(sb_hm_shp)
# summary(sb_hm_val)


###############################################################################
# Service areas Hall & Murray - EJSB Run
###############################################################################

chunks <- 45
chunk_size <- 1000
hm_list <- split(sb_hm_shp, rep(1:45, each = 1000, length.out = nrow(sb_hm_shp)))
results_list <- list()

# Call modified EJfunction over each chunk of data

for (i in 1:45) {
  result <- EJfunction(LOI_data = hm_list[[i]], data_year = 2021, buffer = 0, raster = T)
  results_list[[i]] <- result
}

# saveRDS(cbg_df, file = "Data/cbg_df_ejscreen.rds")

# Need to combine all of the USGS and CBG files into one
for (i in 1:45) {
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

linking.list2 <- sf::st_intersects(cbg.shapes, sb_hm_val) %>%
  as.data.frame() %>%
  dplyr::rename(cbg_ID = row.id) %>%
  dplyr::rename(newid = col.id) %>%
  dplyr::left_join(cbg.shapes %>% 
                     dplyr::select(cbg_ID, ID) %>%
                     sf::st_drop_geometry(),
                   by = 'cbg_ID') 

linking.list3 <- linking.list2 %>%
  dplyr::left_join(loi.buffer %>%
                     dplyr::select(shape_ID, newid, pwsid) %>%
                     sf::st_drop_geometry(),
                   by = 'newid') 

linking.list <- linking.list3 %>%
  select(ID, pwsid) %>%
  drop_na(pwsid) %>%
  distinct() 

cbg_and_pwsid <- left_join(combined_cbg, linking.list, by = 'ID', relationship = "many-to-many") %>%
  distinct() 





cbg_and_pwsid <- cbg_and_pwsid %>% 
  st_drop_geometry %>%
  write_csv( 'data/demographics/hm_dems_area_final.csv')

combined_loi <- combined_loi %>% 
  st_drop_geometry %>%
  write_csv( 'data/demographics/hm_dems_final.csv') 

linking.list <- linking.list %>% 
  write_csv( 'data/demographics/hm_cbg_links_final.csv')




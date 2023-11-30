

###############################################################################
# EJSCREENBATCH for the Water System Service Boundaries - USGS Boundaries
# National Center for Environmental Economics 
# Last edited 11/30/23
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


chunks <- 19
chunk_size <- 1000
usgs_list <- split(sb_sf, rep(1:19, each = 1000, length.out = nrow(sb_sf)))
results_list <- list()

# Call modified EJfunction over each chunk of data

for (i in 1:19) {
  result <- EJfunction(LOI_data = usgs_list[[i]], data_year = 2021, buffer = 0, raster = T)
  results_list[[i]] <- result
}

# Need to combine all of the USGS and CBG files into one


for (i in 1:19) {
  EJ.loi.data <- results_list[[i]]$EJ.loi.data
  EJ.cbg.data <- results_list[[i]]$EJ.cbg.data
  loi_df <- EJ.loi.data$LOI_radius_2021_0mi
  cbg_df <- EJ.cbg.data$CBG_radius_2021_0mi
  if (i == 1){
    combined_loi <- loi_df
    combined_cbg <- cbg_df
  } else{
    combined_loi <- rbind(combined_loi, loi_df)
    combined_cbg <- rbind(combined_cbg, loi_df, fill = TRUE)
  }
}


# Save the USGS data

combined_cbg <- combined_cbg %>% 
  st_drop_geometry %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/usgs_dems_area.csv')
write_xlsx(combined_cbg, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/usgs_dems_area.xlsx")


combined_loi <- combined_loi %>% 
  st_drop_geometry %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/usgs_dems.csv')
write_xlsx(combined_loi, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/usgs_dems.xlsx")




################################################################################
# Done saving USGS demographic data. Have a great day!
################################################################################




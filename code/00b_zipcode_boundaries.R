###############################################################################
# EJSCREENBATCH for the Water System Service Boundaries
# National Center for Environmental Economics 
# Last edited 8/6/23
###############################################################################


# Description of program: This program runs the EJSCREENbatch tool to derive population-
# level statistics for all zipcodes that are associated with public water systems
# in the UCMR 3, UCMR 4, UCMR 5, and in SDWIS. The first section 


###############################################################################
# Load libraries, directories 
###############################################################################

library(devtools)
#install_github('USEPA/EJSCREENbatch', force = TRUE)

list.of.packages <- c("janitor","writexl","readxl","tigris","leaflet","sf","tidyverse","EJSCREENbatch",
                      "tidycensus","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

options(tigris_use_cache = TRUE)
 
#===============================================================================
#---------------------------US STATE MAP----------------------------------------
#===============================================================================

# Get data from the 2016-2020 5-year ACS
us_states <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2020,
  survey = "acs5",
  geometry = TRUE,
  resolution = "20m"
) %>%
  st_transform(4326)
st_crs(us_states)

#plot(us_states$geometry)


 
#==============================================================================
#---------------------------ZIP CODES------------------------------------------
#==============================================================================

#Import UCMR 3, 4, and 5
# ucmr5 <- https://www.epa.gov/dwucmr/occurrence-data-unregulated-contaminant-monitoring-rule#5
zip_links <- c("https://www.epa.gov/sites/default/files/2017-02/ucmr-3-occurrence-data.zip",
               "https://www.epa.gov/sites/default/files/2020-04/ucmr_4_occurrence_data.zip",
               "https://www.epa.gov/system/files/other-files/2023-08/ucmr5-occurrence-data.zip")

names(zip_links) <- c("ucmr3", "ucmr4", "ucmr5")
 
for(i in 1:length(zip_links)){
  temp <- tempfile()
  download.file(zip_links[i],temp)
  con <- unz(temp, paste0(toupper(names(zip_links)[i]),"_ZipCodes.txt"))
  data.zip <- read.table(con, header=T, skip=0, fill=T) %>% 
    write_csv(paste0("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/ucmr/zipcode_areas/",names(zip_links)[i],".csv"))
  head(data.zip)
  assign(names(zip_links)[i], data.zip)
  unlink(temp)
}

ucmr5 <- read_csv("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/ucmr/zipcode_areas/ucmr5.csv")

# Import SDWIS 
sdwis <- read.csv("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/sdwis/geographic_areas_v2.csv")  %>% 
  clean_names()   %>% 
  filter(!is.na(zip_code_served))  %>% 
  rename(ZIPCODE = zip_code_served ) %>% 
  rename(PWSID = pwsid)  %>%
  select('PWSID', 'ZIPCODE')     

 
# Combine ZipCodes
zipcodes <- list(ucmr3, ucmr4, ucmr5, sdwis) %>% 
  reduce(rbind) %>% 
  mutate(ZIPCODE = str_replace_all(ZIPCODE, "-", ""),
         ZIPCODE_numeric = as.numeric(ZIPCODE)) %>% 
  group_by(PWSID, ZIPCODE_numeric) %>% 
  mutate(dup = max(row_number()),
         row_n = row_number(),
         to_delete = ifelse(dup>1 & row_n>1,1,0)) %>% 
  arrange(-dup, ZIPCODE_numeric, row_n) %>% 
  filter(to_delete==0) %>% 
  dplyr::select(PWSID, ZIPCODE, ZIPCODE_numeric)

#Generate zip code boundaries for each PWSID
zipcodeboundaries <- st_read("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/ucmr/zipcode_areas/tl_2019_us_zcta510/tl_2019_us_zcta510.shp") %>% 
  st_transform(4326) %>% 
  mutate(ZIPCODE_numeric = as.numeric(ZCTA5CE10))  %>% 
  left_join(zipcodes) %>% 
  filter(!is.na(PWSID)) %>%
  group_by(PWSID) %>% 
  summarize(geometry = st_union(geometry))


st_write(zipcodeboundaries, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/ZIP_Codes_Served/UCMR_3_5/generated_boundaries_using_zip_codes.shp")

# For Elaine Hill
#st_write(zipcodeboundaries, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/exports/zipcodes/generated_boundaries_using_zip_codes.shp")

zipcodes %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/exports/zipcodes/pwsid_zip_link.csv')
write_xlsx(zipcodes, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/exports/zipcodes/pwsid_zip_link.xlsx")



################################################################################
# Load the Zipcode layer just produced
################################################################################

zipcodeboundaries<- st_read( "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/ZIP_Codes_Served/UCMR_3_5/generated_boundaries_using_zip_codes.shp") %>%
  st_transform(crs = 4326) %>%
  dplyr::mutate(state = substr(PWSID,1,2)) %>%
  sf::st_make_valid() 

###############################################################################
# EJfunction over Zipcodes
###############################################################################

# Tabulate states to figure out which to drop 
  # tmp <- zipcodeboundaries  %>%
  #   st_drop_geometry
  # 
  # tmp %>%
  #   group_by(state) %>%
  #   summarise(n = n()) %>%
  #   mutate(
  #     totalN = (cumsum(n)),
  #     percent = round((n / sum(n)), 3),
  #     cumuPer = round(cumsum(freq = n / sum(n)), 3)) %>%
  #   print(n = 100)

zipcodeboundaries <- zipcodeboundaries  %>% 
  filter(!(state %in% c("GU","MP","VI","AS","PR")))
zipcodeboundaries <- st_set_geometry(zipcodeboundaries,"geometry")


#Check to see if any are outside of CONUS - keep only the zips in CONUS
# mapview(zipcodeboundaries)
# intersects <- st_intersects(zipcodeboundaries, us_states )
# tmp <- zipcodeboundaries[intersects,]


# Source modified files from EJSCREENbatch
# sapply('C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/ej_service_areas/code/data_cleaning/EJSCREENBufferRaster.R', source)
# sapply(list.files('C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/pfas_npdwr_ej/2023_analysis/R', full.names=TRUE), source)


chunks <- 17
chunk_size <- 1000
zip_list <- split(zipcodeboundaries, rep(1:17, each = 1000, length.out = nrow(zipcodeboundaries)))
results_list <- list()

# Call modified EJfunction over each chunk of data
for (i in 13:17) {
  result <- EJfunction(LOI_data = zip_list[[i]], data_year = 2021, buffer = 0, raster = T)
  results_list[[i]] <- result
}

# Need to combine all of the Zipcode and CBG files into one


for (i in 1:17) {
  EJ.loi.data <- results_list[[i]]$EJ.loi.data
  EJ.cbg.data <- results_list[[i]]$EJ.cbg.data
  loi_df <- EJ.loi.data$LOI_radius_2021_0mi
  cbg_df <- EJ.cbg.data$CBG_radius_2021_0mi
if (i == 1){
  combined_loi <- loi_df
  combined_cbg <- cbg_df
} else{
  combined_loi <- rbind(combined_loi, loi_df)
  combined_cbg <- rbind(combined_cbg, cbg_df, fill = TRUE)
}
}

# Save Just the zipcode data

combined_cbg <- combined_cbg %>% 
  st_drop_geometry %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/zc_dems_area.csv')
write_xlsx(combined_cbg, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/zc_dems_area.xlsx")


combined_loi <- combined_loi %>% 
  st_drop_geometry %>%
  rename(pwsid = PWSID)  %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/zc_dems.csv')
write_xlsx(combined_loi, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/zc_dems.xlsx")




# Merge County-level Demographic Data back to the Full PWS Dataset and Save Files

county_dems <- read_excel("NCEE - Water System Service Boundaries/data/demographics/county_dems.xlsx")

comb <- anti_join(zipcode_data,  county_dems,  by = 'pwsid' )  %>% 
  st_drop_geometry


# Save output from batch tool

pws_county_dems %>%
  write_csv( 'C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/county_dems.csv')
write_xlsx(pws_county_dems, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/data/demographics/county_dems.xlsx")



################################################################################
# Done saving file, rest of code is optional or left over.
# Have a great day!
################################################################################






################################################################################
################################################################################
################################################################################
################################################################################

leaflet() %>%
  setView(-101,35, zoom=3) %>%
  addTiles %>%
  addPolygons(data= zipcodeboundaries,
              popup = ~ PWSID)  


# for(i in unique(zipcodeboundaries$state_abbv)){
#   ggplot() +
#     geom_sf(data=us_states %>% 
#               filter(state_abbv==i), fill=NA) +
#     geom_sf(data=zipcodeboundaries %>% 
#               filter(state_abbv==i)) +
#     theme_minimal()
#   ggsave(paste0("boundary_images/boundaries_",i,"_using_zip_codes.png"))
# }

#==============================================================================
#---------------------------Census Designated Places---------------------------
#==============================================================================
   
cdp <- st_read("Data/zipcode_areas/tl_2019_us_uac10/tl_2019_us_uac10.shp") %>% 
  st_transform(4326) %>% 
  st_join(zipcodeboundaries, join=st_intersects) %>% 
  filter(!is.na(PWSID))  %>% 
  group_by(NAMELSAD10) %>% 
  mutate(row = row_number(),
         row = paste0("test",row))  %>% 
  pivot_wider(names_from = "row",
              values_from = "PWSID") %>% 
  ungroup %>% 
  tidyr::unite("PWSIDs", starts_with("test"), sep=" " ) %>%  
  mutate(PWSIDs = str_replace_all(PWSIDs,"NA ",""),
         PWSIDs = str_replace_all(PWSIDs," NA",""))
st_write(cdp, "Data/zipcode_areas/generated_boundaries_cdp/generated_boundaries_using_cdp.shp")
 
# plot(cdp$geometry)
  
leaflet() %>%
  setView(-101,35, zoom=3) %>%
  addTiles %>%
  addPolygons(data= cdp,
              popup = ~ PWSIDs)  


library(tidyverse)
library(data.table)
library(sf)
library(tidycensus)
library(leaflet)
options(tigris_use_cache = TRUE)
 


getwd()
setwd('../..')
getwd()

#===============================================================================
#---------------------------US STATE MAP----------------------------------------
#===============================================================================

us_states <- get_acs(
  geography = "state",
  variables = "B01002_001",
  year = 2019,
  survey = "acs1",
  geometry = TRUE,
  resolution = "20m"
) %>%
  st_transform(4326)
st_crs(us_states)

plot(us_states$geometry)


 
#==============================================================================
#---------------------------ZIP CODES------------------------------------------
#==============================================================================
#Use UCMR 3 & 4 for now. Can add or replace with UCMR 5 later when available.
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
 
 
#---------------------------Combine ZipCodes------------------------------------
zipcodes <- list(ucmr3, ucmr4, ucmr5) %>% 
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

#---------------------------Zip Boundaries--------------------------------------
#Generate zip code boundaries for each PWSID
zipcodeboundaries <- st_read("C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/DWDB/Data/ucmr/zipcode_areas/tl_2019_us_zcta510/tl_2019_us_zcta510.shp") %>% 
  st_transform(4326) %>% 
  mutate(ZIPCODE_numeric = as.numeric(ZCTA5CE10))  %>% 
  left_join(zipcodes) %>% 
  filter(!is.na(PWSID)) %>%
  group_by(PWSID) %>% 
  summarize(geometry = st_union(geometry))
st_write(zipcodeboundaries, "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries/ZIP_Codes_Served/UCMR_3_5/generated_boundaries_using_zip_codes.shp")




################################################################################
# Done saving file, rest of code is optional. 
# Have a great day!
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


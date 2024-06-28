
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  sf, # vector data operations
  raster, # raster data operations
  exactextractr, # fast raster data extraction for polygons
  maps, # to get county boundary data
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  tidycensus,
  mapview,
  janitor,
  units,
  EJSCREENbatch, # Download EJscreen 
  areal #for areal interpolation
)

# Areal apportionment 

# First read in polygon data

target <- sf::st_read("Data/hall_murray/Water_System_Boundaries.gpkg") 

# CBG data with EJScreen info
source <- readRDS("Data/data.tog.rds") %>%
  st_as_sf() %>%
  st_transform(crs = st_crs(target))

# Target ID (for calculating  block group into into the target polygons)
tid = "PWSID"

# Source ID (to assign targets to source polygons)
sid = "ID"

# Grab the names of the variables you want to calculate
data.nog <- source %>%
  st_drop_geometry() %>%
  mutate(ID = as.character(ID), 
         ID = str_pad(ID, 12, "left", pad = "0"))

# Extensive margin : counts
ext <- "ACSTOTPOP"

# Intensive margin : percents 
int <- names(data.nog[, c(5:56)])

rm(data.nog)


interpolate <- function (source, sid, target, tid, ext, int) {
  
  # Find intersecting polygons
  
  intersect <- st_intersection(target, source) 
  
  #Create df with area of intersection
  intersect_area <- intersect %>% 
    mutate(intersect_area = st_area(.)) %>% # create new column with shape area 
    st_drop_geometry() %>% #drop geometry: easier to compute
    dplyr::select(c(tid, sid, 'intersect_area')) # only select columns needed to merge
  
  intersect_area <- drop_units(intersect_area) 
  
  intersect_area <- intersect_area %>%
    filter('intersect_area' > 0)
  
  #Calculate the area of the source polygons (for extensive interpolation)
  source_area <- mutate(source, sid_area = st_area(source))  %>%
    dplyr::select(sid, 'sid_area') %>% # Calculate the area of the source polygon
    st_drop_geometry()
  
  source_area <- drop_units(source_area)
  
  #merge by sid, keep tid
  source_area <- left_join(source_area, intersect_area, by = sid, relationship="many-to-many") %>% #Merge by id no, relationship many
    mutate(source_coverage = intersect_area/sid_area) # Calculate % area of each source polygon to the overall target area
  
  #Calculate the area of the target polygons (for intensive interpolation)
  target_area <- mutate(target, tid_area = st_area(target)) %>%
    dplyr::select(tid, 'tid_area') %>% # Calculate the area of the target polygon
    st_drop_geometry()
  
  target_area <- drop_units(target_area)
  
  # Merge by tid, keep sid 
  target_area <- left_join(target_area, intersect_area, by = tid, relationship="many-to-many") %>% #Merge by id no, relationship many
    mutate(target_coverage = as.numeric(intersect_area/tid_area)) # Calculate % area of each source polygon to the overall target area
  
  intersect_w <- left_join(intersect, target_area, by = c(tid, sid), relationship="many-to-many") %>%
    left_join(source_area, by = c(sid, tid), relationship="many-to-many") %>%
    rowwise %>%
    mutate(across(all_of(int), ~ .x*target_coverage))  %>% #multiply intensive vars by target area
    mutate(across(all_of(ext), ~ .x*source_coverage)) %>% # multiply extensive vars by source area
    st_drop_geometry() # don't need geometry anymore
  
  # Use percent coverage from source and target data intersect to create pwsid level weighted interpolation
  
  target_dat <- intersect %>%
    group_by(pick(tid)) %>%
    mutate(across(all_of(ext), ~sum(.x, na.rm=T))) %>% # average 
    mutate(across(all_of(int), ~mean(.x, na.rm=T))) %>%
    mutate(sids = paste(pick(sid), collapse = ", "))%>%  # record CBGs contributing to data
    ungroup 
  
  target_dat <- target_dat %>%
    distinct(pick(tid), .keep_all =T)   # select distinct tid's
  
  
  return(list(intersect, intersect_w, target_dat))
}

areal_int <- interpolate(source=source, sid = "ID", target=target, tid="PWSID", ext="ACSTOTPOP", int)

intersect <- areal_int[[1]] # retreive datasets

intersect_w <- areal_int[[2]]

target_dat <- areal_int[[3]]

saveRDS(areal_int, file = paste0(save_here, "areal_int.rds"))

write.csv(intersect_w, "Data/demographics/area/hm_dems_area.csv")

write.csv(target_dat, "Data/demographics/area/hm_dems_area.csv")

##################

#create data frame with single CBG (if necessary)

source_dat <- intersect %>%
  group_by(c(sid)) %>%
  mutate(across(all_of(ext), ~mean(.x, na.rm=T))) %>%
  mutate(across(all_of(int), ~mean(.x, na.rm=T))) %>%
  mutate(tids = paste(pick(tid), sep = ", ")) %>%
  ungroup() %>%
  st_drop_geometry()

source_dat <- source_dat %>%
  distinct(pick(sid), .keep_all =T)                   # Using duplicated function

saveRDS(source_dat, file = paste0(save_here,"source_dat.rds"))

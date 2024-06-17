
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
  EJSCREENbatch, # Download EJscreen 
  areal #for areal interpolation
)

# hm interpolation

save_here <- "Data/hm_interp/"

# sb_hm_shp <- sf::st_read("Data/hall_murray/Water_System_Boundaries.gpkg") 
# 
# data.tog <- readRDS("Data/data.tog.rds") %>%
#   st_as_sf() %>%
#   st_transform(crs = st_crs(sb_hm_shp))
# 
# st_crs(data.tog)
# st_crs(sb_hm_shp)

# data.nog <- data.tog %>%
#   st_drop_geometry() %>%
#   mutate(ID = as.character(ID), 
#          ID = str_pad(ID, 12, "left", pad = "0"))
# 
# ext <- names(data.nog[, c(4)])
# 
# int <- names(data.nog[, c(5:56)])
# 
# # Need to make data planar for areal interpolation
# sb_hm_sim  <- sb_hm_shp %>% 
#   st_make_valid() %>%
#   st_simplify %>% 
#   st_transform(crs = 3857)
# 
# data.pl <- data.tog %>%
#   st_transform(crs = 3857)

# data.pl <- spTransform(data.tog, CRS("+proj=utm +zone=10 +datum=WGS84"))

#Validation protocol
areal::ar_validate(target = sb_hm_sim, source= data.pl, varList = int, verbose = TRUE)

hm_interp <- areal::aw_interpolate(sb_hm_sim,
                                   tid = "PWSID",
                                   sid = 'ID',
                                   source = data.pl,
                                   weight = "sum",
                                   output = 'sf',
                                   extensive = "ACSTOTPOP",
                                   intensive = int)


target <- sf::st_read("Data/hall_murray/Water_System_Boundaries.gpkg") 

source <- readRDS("Data/data.tog.rds") %>%
  st_as_sf() %>%
  st_transform(crs = st_crs(target))

tid = "PWSID"

sid = "ID"

data.nog <- source %>%
  st_drop_geometry() %>%
  mutate(ID = as.character(ID), 
         ID = str_pad(ID, 12, "left", pad = "0"))

ext <- "ACSTOTPOP"

int <- names(data.nog[, c(5:56)])

rm(data.nog)


interpolate <- function (source, sid, target, tid, ext, int) {
  
  # Find intersecting polygons

  intersect <- st_intersection(target, source)
  
  #Create df with area of intersection
  intersect_area <- intersect %>% 
    mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
    st_drop_geometry() %>% #drop geometry: easier to compute
    dplyr::select(c(tid, sid, 'intersect_area')) # only select columns needed to merge
  
  intersect_area <- drop_units(intersect_area) 
  
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
    mutate(across(all_of(ext), ~mean(.x, na.rm=T))) %>% # average 
    mutate(across(all_of(int), ~mean(.x, na.rm=T))) %>%
    mutate(sids = paste(pick(sid), collapse = ", "))%>%  # record CBGs contributing to data
    ungroup 

  target_dat <- target_dat %>%
    distinct(pick(tid), .keep_all =T)   # select distinct tid's
 }

saveRDS(intersect, file = paste0(save_here, "intersect.rds"))

saveRDS(intersect_w, file = paste0(save_here, "intersect_weighted.rds"))

saveRDS(target_dat, file = paste0(save_here,"target_dat.rds"))

#create data frame with single CBG (if necessary)

source_dat <- intersect %>%
  group_by(c(sid)) %>%
  mutate(across(all_of(ext), ~mean(.x, na.rm=T))) %>%
  mutate(across(all_of(int), ~mean(.x, na.rm=T))) %>%
  mutate(tids = paste(pick(tid), sep = ", ")) %>%
  ungroup()

source_dat <- source_dat %>%
  distinct(pick(sid), .keep_all =T)                   # Using duplicated function

saveRDS(source_dat, file = paste0(save_here,"source_dat.rds"))


interpolate(source=data.tog, sid = "ID", target=sb_hm_shp, tid="PWSID", ext="ACSTOTPOP", int)


rm(taget_area, source_area, intersect_area)



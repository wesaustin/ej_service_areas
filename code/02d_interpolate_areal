# hm interpolation

save_here <- "Data/hm_interp/"

sb_hm_shp <- sf::st_read("Data/hall_murray/Water_System_Boundaries.gpkg") 

data.tog <- readRDS("Data/data.tog.rds") %>%
  st_as_sf() %>%
  st_transform(crs = st_crs(sb_hm_shp))

st_crs(data.tog)
st_crs(sb_hm_shp)

data.nog <- data.tog %>%
  st_drop_geometry() %>%
  mutate(ID = as.character(ID), 
         ID = str_pad(ID, 12, "left", pad = "0"))

ext <- names(data.nog[, c(4)])

int <- names(data.nog[, c(5:56)])

# Need to make data planar for areal interpolation
sb_hm_sim  <- sb_hm_shp %>% 
  st_make_valid() %>%
  st_simplify %>% 
  st_transform(crs = 3857)

data.pl <- data.tog %>%
  st_transform(crs = 3857)

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


interpolate <- function (source, sid, target, tid, ext, int) {
  
  # Find intersecting polygons

  intersect <- st_intersection(target, source)

  #Create df with area of intersection
  intersect_area <- intersect %>% 
    mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
    st_drop_geometry() %>% #drop geometry: easier to compute
    dplyr::select(c(tid, sid, 'intersect_area')) # only select columns needed to merge
  
  #Calculate the area of the source polygons (for extensive interpolation)
  source_area <- mutate(source, sid_area = st_area(source))  %>%
    dplyr::select(sid, 'sid_area') %>% # Calculate the area of the source polygon
    st_drop_geometry()
  
  #merge by sid, keep tid
  source_area <- left_join(source_area, intersect_area, by = sid, relationship="many-to-many") %>% #Merge by id no, relationship many
    mutate(source_coverage = intersect_area/sid_area) # Calculate % area of each source polygon to the overall target area
  
  #Calculate the area of the target polygons (for intensive interpolation)
  target_area <- mutate(target, tid_area = st_area(target)) %>%
    dplyr::select(tid, 'tid_area') %>% # Calculate the area of the target polygon
    st_drop_geometry()
  
  # Merge by tid, keep sid 
  target_area <- left_join(target_area, intersect_area, by = tid, relationship="many-to-many") %>% #Merge by id no, relationship many
    mutate(target_coverage = as.numeric(intersect_area/tid_area)) # Calculate % area of each source polygon to the overall target area
  
  intersect <- left_join(intersect, target_area, by = c(tid, sid), relationship="many-to-many") %>%
    left_join(source_area, by = c(sid, tid), relationship="many-to-many") %>%
    group_by(c(sid)) %>%
    mutate(across(all_of(int), ~ .x*target_coverage))  %>%
    ungroup() %>%
    group_by(c(tid)) %>%
    mutate(across(all_of(ext), ~ .x*source_coverage))  
  
  target_dat <- intersect %>%
    group_by(c(tid)) %>%
    mutate(across(all_of(ext), ~mean(.x, na.rm=T))) %>%
    mutate(across(all_of(int), ~mean(.x, na.rm=T))) %>%
    mutate(sids = paste(c(sid), sep = ", ")) %>%
    ungroup() 
  
  target_dat <- target_dat %>%
    distinct(pick(tid), .keep_all =T)                   # Using duplicated function
  
  source_dat <- intersect %>%
    group_by(c(sid)) %>%
    mutate(across(all_of(ext), ~mean(.x, na.rm=T))) %>%
    mutate(across(all_of(int), ~mean(.x, na.rm=T))) %>%
    mutate(tids = paste(c(tid), sep = ", ")) %>%
    ungroup()
  
  source_dat <- source_dat %>%
    distinct(pick(sid), .keep_all =T)                   # Using duplicated function
  
  
}

saveRDS(target_dat, file = paste0(save_here,"target_dat.rds"))

saveRDS(source_dat, file = paste0(save_here,"source_dat.rds"))

interpolate(source=data.tog, sid = "ID", target=sb_hm_shp, tid="PWSID", ext="ACSTOTPOP", int)

rm(taget_area, source_area, intersect_area)

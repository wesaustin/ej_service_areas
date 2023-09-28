
################################################################################
# Mapping all indicators nationally : tract and tract level
# National Center for Environmental Economics
# Latest update: 9/8/23
################################################################################

# NOTE : Color scheme 
  # HB : Greens
  # LCR : RdPu
  # PFAS : Reds
  # DBT : Blues
  # COL : YlOrRd

################################################################################
## Load packages: 
################################################################################

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
  tmap, # for map creation
  modelsummary, # regression table generation
  future.apply, # parallel computation
  cowplot, # for bivariate mapping
  rgdal, # required for cdlTools
  prism, # download PRISM data
  stringr, # string manipulation
  magrittr,
  tidycensus,
  mapview,
  patchwork #for combining plots
)

################################################################################
##Set directories
################################################################################

#TB
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

#WA
my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()

plot_path <- "Plots/country"


################################################################################
## Load tract data for all US
################################################################################

# Following code to retreive ALL CBG and tract data for the US

US_cbg <- tigris::block_groups(state = NULL, county = NULL, cb = TRUE) 

US_tract <- tigris::tracts(state = NULL, county = NULL, cb = TRUE) 

US_cbg_50 <- US_cbg %>%
  rename(ID = GEOID) %>%
  tigris::shift_geometry() %>%
  filter(STATEFP <=56, STATEFP != 43 ) %>%  #remove Puerto Rico and outer territories
  st_transform(crs = 5070)

US_tract_50 <- US_tract %>%
  rename(tract = GEOID) %>%
  tigris::shift_geometry() %>%
  filter(STATEFP <=56, STATEFP != 43 ) %>%  #remove Puerto Rico and outer territories
  st_transform(crs = 5070)

################################################################################
## Health violations
################################################################################

cbg_HB_vio <- read_rds("Data/combined/tract_HB_vio_combined.rds") 

# For cbg's with more than one PWSID, find average number of violations at the tract level
# then collapse to include only 1 observation per census tract level (12-digit "ID") 

cbg_HB_vio <- cbg_HB_vio %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  mutate(avg_vio_length = mean(diff_days)) %>%
  distinct(ID, .keep_all = TRUE) 

HB_vio_all <- left_join(US_cbg_50, cbg_HB_vio) 

st_as_sf(HB_vio_all)

summary(HB_vio_all$avg_vio_cbg)

# Tract level 

tract_HB_vio <- tract_HB_vio %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP), na.rm = TRUE) %>% #average violations per tract
  distinct(tract, .keep_all = TRUE) 

HB_vio_all <- left_join(US_tract_50, tract_HB_vio, by = "tract") #note to rerun this if you are doing tract and tract level, right now comb share the same name

st_as_sf(HB_vio_all)

summary(HB_vio_all$avg_vio_tract)
#3rd qt = 1.35

################################################################################
# HB Violations Maps
################################################################################

# Map 1 : HB violations at CBG

ggplot() + 
  geom_sf(data = HB_vio_all, aes(fill = avg_vio_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations\nper CBG", palette = "Greens", direction = 1, limits=c(0,50), breaks = c(0,50), labels = c(0,">50"), na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'),legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 


ggsave(filename = 'HB_vio_cbg_US.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)


# Map 2 : HB violations at tract

ggplot() + 
  geom_sf(data = HB_vio_all, aes(fill = avg_vio_tract, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations\nper tract", palette = "Greens", direction = 1, limits=c(0,50), breaks = c(0,50), labels = c(0,">50"), na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'),legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 


ggsave(filename = 'HB_vio_tract_US.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)

rm(HB_vio_all, tract_HB_vio, cbg_HB_vio)


################################################################################
## LCR violations
################################################################################

cbg_lcr_vio <- read_rds("Data/combined/cbg_LCR_vio_comb.rds") #load lcr data

# CBG

cbg_lcr_vio <- cbg_lcr_vio %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #average violations per cbg
  distinct(ID, .keep_all = TRUE)

lcr_vio_all <- left_join(US_cbg_50, cbg_lcr_vio) 

summary(lcr_vio_all$avg_vio_cbg) #distribution

st_as_sf(lcr_vio_all) #set as spatial object

# Tract level 

tract_lcr_vio <- tract_lcr_vio %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP), na.rm = TRUE) %>% #average violations per tract
  distinct(tract, .keep_all = TRUE) 

lcr_vio_all <- left_join(US_tract_50, tract_lcr_vio, by = "tract") #note to rerun this if you are doing tract and tract level, right now comb share the same name

summary(lcr_vio_all$avg_vio_tract) #distribution
#3rd qt = 1.25

st_as_sf(lcr_vio_all) #set as spatial object

################################################################################
## LCR violations Maps
################################################################################

# Map 3 : LCR violations : CBG

ggplot() + 
  geom_sf(data = lcr_vio_all, aes(fill = avg_vio_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations\nper CBG", palette = "RdPu", direction = 1, limits=c(0,2), breaks = c(0,2), labels = c(0,">2"), na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 


ggsave(filename = 'lcr_vio_cbg_US_5.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)

# Map 4 : LCR violations tract

ggplot() + 
  geom_sf(data = lcr_vio_all, aes(fill = avg_vio_tract, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations\nper tract", palette = "RdPu", direction = 1, limits=c(0,2), breaks = c(0,2), labels = c(0,">2"), na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 

ggsave(filename = 'lcr_vio_tract_US.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)

rm(lcr_vio_all, tract_lcr_vio, cbg_lcr_vio)


################################################################################
## PFAS violations
################################################################################

cbg_pfas_vio <- read_rds("Data/combined/cbg_pfas_vio_combined.rds") #load pfas data

# CBG

cbg_pfas_vio <- cbg_pfas_vio %>%
  group_by(ID) %>%
  mutate(pfas_count_cbg = sum(pfas_count*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #avg pfas count per cbg
  mutate(avg_conc_cbg = sum(concentration_sum*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #avg pfas concentration per cbg
  distinct(ID, .keep_all = TRUE) 

pfas_vio_all <- left_join(US_cbg_50, cbg_pfas_vio) 

summary(pfas_vio_all$pfas_count_cbg) #distribution
summary(pfas_vio_all$avg_conc_cbg) 

st_as_sf(pfas_vio_all) #set as spatial object

# Tract

tract_pfas_vio <- cbg_pfas_vio %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(pfas_count_tract = sum(pfas_count*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #avg pfas count per tract
  mutate(avg_conc_tract = sum(concentration_sum*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #avg pfas concentration per tract
  distinct(tract, .keep_all = TRUE) 

pfas_vio_all <- left_join(US_tract_50, tract_pfas_vio, by = "tract") 

st_as_sf(pfas_vio_all)

summary(pfas_vio_all$pfas_count_tract) #distribution
#3rd QT = 1
summary(pfas_vio_all$avg_conc_tract) 

################################################################################
## PFAS Violations Maps
################################################################################

# Map 5 : PFAS violations cbg

ggplot() + 
  geom_sf(data = pfas_vio_all, aes(fill = pfas_count_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "PFAS Detections", palette = "Reds", 
                       direction = 1, limits=c(0,1), breaks = c(0,1), labels = c(0,">1"), 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 


ggsave(filename = 'pfas_count_cbg_US.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)

# Map 5 : PFAS violations tract

ggplot() + 
  geom_sf(data = pfas_vio_all, aes(fill = pfas_count_tract, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "PFAS Detections", palette = "Reds", 
                       direction = 1, limits=c(0,1), breaks = c(0,1), labels = c(0,">1"), 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 

ggsave(filename = 'pfas_count_tract_US.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)


rm(cbg_pfas_vio, pfas_vio_all, tract_pfas_vio)

################################################################################
## DBP violations
################################################################################

cbg_dbp_vio <- read_rds("Data/combined/cbg_dbp_vio_combined.rds")

# CBG

cbg_dbp_vio <- cbg_dbp_vio %>%
  group_by(ID) %>%
  mutate(avg_dbp_cbg = sum(combined_dbp*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE) 

dbp_vio_all <- left_join(US_cbg_50, cbg_dbp_vio) 

summary(dbp_vio_all$avg_dbp_cbg) #distribution

st_as_sf(dbp_vio_all) #set as spatial object

# Tract

tract_dbp_vio <- cbg_dbp_vio %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_dbp_tract = sum(combined_dbp*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(tract, .keep_all = TRUE) 

dbp_vio_all <- left_join(US_tract_50, tract_dbp_vio, by = "tract") 

summary(dbp_vio_all$avg_dbp_tract) #distribution

#3rd QT = 63

st_as_sf(dbp_vio_all) #set as spatial object

################################################################################
## TCR violations Maps
################################################################################

# Map 8 : DBP violations CBG

ggplot() + 
  geom_sf(data = dbp_vio_all, aes(fill = avg_dbp_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average combined\nconcentration per cbg", palette = "Blues", 
                       direction = 1, limits=c(0,100), breaks = c(0,100), labels = c(0,">100"), 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 

ggsave(filename = 'dbp_conc_cbg_US.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)

# Map 9 : DBP violations tract

ggplot() + 
  geom_sf(data = dbp_vio_all, aes(fill = avg_dbp_tract, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average combined\nconcentration per tract", palette = "Blues", 
                       direction = 1, limits=c(0,60), breaks = c(0,60), labels = c(0,">60"), 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 

ggsave(filename = 'dbp_conc_tract_US.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)

rm(cbg_dbp_vio, dbp_vio_all, tract_dbp_vio)

################################################################################
## TCR violations
################################################################################

cbg_tcr_vio <-read_rds("Data/combined/cbg_tcr_vio_combined.rds")

# CBG

cbg_tcr_vio <- cbg_tcr_vio %>%
  group_by(ID) %>%
  mutate(tcr_det_cbg = sum(detection_share*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE)

tcr_vio_all <- left_join(US_cbg_50, cbg_tcr_vio) 

summary(tcr_vio_all$tcr_det_cbg) #distribution

st_as_sf(tcr_vio_all) #set as spatial object

# Tract

tract_tcr_vio <- cbg_tcr_vio %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(tcr_det_tract = sum(detection_share*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(tract, .keep_all = TRUE)

tcr_vio_all <- left_join(US_tract_50, tract_tcr_vio, by = "tract") 

summary(tcr_vio_all$tcr_det_tract) #distribution 

st_as_sf(tcr_vio_all) #set as spatial object

################################################################################
## TCR violations Maps
################################################################################

# Map 10 : TCR violations CBG

ggplot() + 
  geom_sf(data = tcr_vio_all, aes(fill = tcr_det_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average detection\nshare per cbg", palette = "YlOrRd", 
                       direction = 1, limits=c(0,0.25), breaks = c(0,0.25), labels = c(0,">0.25"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 

ggsave(filename = 'tcr_det_cbg_US.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)

# Map 11 : TCR violations tract

ggplot() + 
  geom_sf(data = tcr_vio_all, aes(fill = tcr_det_tract, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average detection\nshare per tract", palette = "YlOrRd", 
                       direction = 1, limits=c(0,0.25), breaks = c(0,0.25), labels = c(0,">0.25"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 

ggsave(filename = 'tcr_det_tract_US.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)

rm(cbg_tcr_vio, tcr_vio_all, tract_tcr_vio)

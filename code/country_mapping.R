
################################################################################
# Mapping all indicators nationally
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

plot_path <- "Plots"


################################################################################
## Load CBG data for all US
################################################################################

#Create an index for all US states

fips.states <- unique(fips_codes$state)[1:51] 

state_borders <- tigris::states(cb = FALSE, resolution = '20m') %>%
  filter(!STUSPS %in% c('HI', 'AK', 'PR', 'GU', 'VI', 'AS', 'MP')) %>% 
  dplyr::select(NAME, STUSPS)

# Following code to retreive ALL CBG data for the US

US_cbg <- tigris::block_groups(state = NULL, county = NULL, cb = TRUE) 

US_cbg_50 <- US_cbg %>%
  rename(ID = GEOID) %>%
  tigris::shift_geometry() %>%
  filter(STATEFP <=56, STATEFP != 43 ) %>%  #remove Puerto Rico and outer territories
  st_transform(crs = 5070)

################################################################################
## Health violations
################################################################################

cbg_HB_vio <- read_rds("Data/cbg_HB_vio_combined.rds") 

# For tract with more than one PWSID, find average number of violations at the CBG level
# then collapse to include only 1 observation per census tract level (12-digit "ID") 

cbg_HB_vio <- cbg_HB_vio %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  mutate(avg_vio_length = mean(diff_days)) %>%
  distinct(ID, .keep_all = TRUE) 

HB_vio_all <- left_join(US_cbg_50, cbg_HB_vio) 

st_as_sf(HB_vio_all)

summary(HB_vio_all$avg_vio_cbg)

# Map 1 : HB violations

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

rm(HB_vio_all, cbg_HB_vio)


################################################################################
## LCR violations
################################################################################

cbg_lcr_vio <- read_rds("Data/cbg_LCR_vio_comb.rds") #load lcr data

cbg_lcr_vio <- cbg_lcr_vio %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #average violations per CBG
  distinct(ID, .keep_all = TRUE)

lcr_vio_all <- left_join(US_cbg_50, cbg_lcr_vio) 

summary(lcr_vio_all$avg_vio_cbg) #distribution

st_as_sf(lcr_vio_all) #set as spatial object

# Map 2 : LCR violations

ggplot() + 
  geom_sf(data = lcr_vio_all, aes(fill = avg_vio_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations\nper CBG", palette = "RdPu", direction = 1, limits=c(0,5), breaks = c(0,5), labels = c(0,">5"), na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 


ggsave(filename = 'lcr_vio_cbg_US_5.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)

rm(lcr_vio_all, cbg_lcr_vio)


################################################################################
## PFAS violations
################################################################################

cbg_pfas_vio <- read_rds("Data/cbg_pfas_vio_combined.rds") #load pfas data

cbg_pfas_vio <- cbg_pfas_vio %>%
  group_by(ID) %>%
  mutate(pfas_count_cbg = sum(pfas_count*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #avg pfas count per cbg
  mutate(avg_conc_cbg = sum(concentration_sum*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #avg pfas concentration per cbg
  distinct(ID, .keep_all = TRUE) 

pfas_vio_all <- left_join(US_cbg_50, cbg_pfas_vio) 

summary(pfas_vio_all$pfas_count_cbg) #distribution
summary(pfas_vio_all$avg_conc_cbg) 

st_as_sf(pfas_vio_all) #set as spatial object

# Map 3 : PFAS violations

ggplot() + 
  geom_sf(data = pfas_vio_all, aes(fill = pfas_count_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Violation Detections", palette = "Reds", 
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

rm(cbg_pfas_vio, pfas_vio_all)


################################################################################
## DBP violations
################################################################################

cbg_dbp_vio <- read_rds("Data/cbg_dbp_vio_combined.rds")

cbg_dbp_vio <- cbg_dbp_vio %>%
  group_by(ID) %>%
  mutate(avg_dbp_cbg = sum(combined_dbp*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE) 

dbp_vio_all <- left_join(US_cbg_50, cbg_dbp_vio) 

summary(dbp_vio_all$avg_dbp_cbg) #distribution

st_as_sf(dbp_vio_all) #set as spatial object

# Map 4 : DBP violations

ggplot() + 
  geom_sf(data = dbp_vio_all, aes(fill = avg_dbp_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average combined\nconcentration per CBG", palette = "Blues", 
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

rm(cbg_dbp_vio, dbp_vio_all)

################################################################################
## TCR violations
################################################################################

cbg_tcr_vio <-read_rds("Data/cbg_tcr_vio_combined.rds")

cbg_tcr_vio <- cbg_tcr_vio %>%
  group_by(ID) %>%
  mutate(tcr_det_cbg = sum(detection_share*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE)

tcr_vio_all <- left_join(US_cbg_50, cbg_tcr_vio) 

summary(tcr_vio_all$tcr_det_cbg) #distribution

st_as_sf(tcr_vio_all) #set as spatial object

# Map 5 : TCR violations

ggplot() + 
  geom_sf(data = tcr_vio_all, aes(fill = tcr_det_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average concentration\nper CBG", palette = "YlOrRd", 
                       direction = 1, 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() + 
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(family = "serif"), legend.title = element_text(family = "serif", size = 8), legend.position = "right", legend.box.background = element_blank()) 

ggsave(filename = 'tcr_conc_cbg_US.png', path = plot_path,
       scale = 1.5,
       width = 7,
       height = 5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)

rm(cbg_tcr_vio, tcr_vio_all)

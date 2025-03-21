
################################################################################
# Mapping all indicators nationally : cbg and tract level
# National Center for Environmental Economics
# Latest update: 10/6/23
################################################################################

# NOTE : Color scheme 
# HB : Reds
# Copper : Greens
# Lead : Greys
# PFAS : PuRed
# DBT : Blues
# TCR : YlOrRd

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
  cowplot, # for bivariate mapping
  rgdal, # required for cdlTools
  stringr, # string manipulation
  magrittr,
  tidycensus,
  mapview,
  janitor,
  patchwork #for combining plots
)


################################################################################
##Set directories
################################################################################

#TB
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

#WA
#my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

setwd(paste0(my_path))
getwd()

plot_path <- "Plots/country/hm/"


################################################################################
## Load tract data for all US
################################################################################

#Retreive census geography

US_cbg <- tigris::block_groups(state = NULL, county = NULL, cb = TRUE, year = 2021) %>%
  tigris::shift_geometry()  # this puts Alaska and Hawaii underneath the CUS
  
  st_crs(US_cbg)

#Limit to 50 states

US_cbg_50 <- US_cbg %>%
  rename(ID = GEOID) %>%
  mutate(STATEFP = as.numeric(STATEFP)) %>%
  dplyr::filter(STATEFP <= 56, STATEFP != 43) %>% #remove Puerto Rico and outer territories
  st_transform(crs = 5070) 

summary(US_cbg_50$STATEFP) #quick check that we have the states we want

# Grab state boundaries to delineate states in maps

US_50 <- tigris::states(cb = TRUE) %>%
  rename(ID = GEOID) %>%
  mutate(STATEFP = as.numeric(STATEFP))  %>%
  dplyr::filter(STATEFP <= 56, STATEFP != 43) %>% #remove Puerto Rico and outer territories
  tigris::shift_geometry() %>%
  st_transform(crs = 5070)  # fix projection


## Following code not run; only to get the bounding box to fix mapping issue

# US_48 <- tigris::states(cb = TRUE) %>%
#   rename(ID = GEOID) %>%
#   mutate(STATEFP = as.numeric(STATEFP)) %>%
#   dplyr::filter(STATEFP <= 56, STATEFP != 43, STATEFP != 2, STATEFP != 15) %>% #remove Puerto Rico and outer territories
#   st_transform(crs = 5070)

#Get the bounding box for the continental US to guide the cropping

# st_bbox(US_48) 

# xmin       ymin       xmax       ymax 
# -2356113.7   269573.6  2258200.2  3172567.9 

bbox <- st_bbox(c(xmin = -3056113.7 , ymin =  -63573.6, 
                  xmax = 2258200.2, ymax = 3172567.9 ), 
                crs = st_crs(5070))

US_cbg_crop <- st_crop(US_cbg_50, bbox)

US_st_crop <- st_crop(US_50, bbox)

## Save files to save on future computation time

saveRDS(US_cbg_crop, file = "Data/census_geo/US_cbg_crop.rds")

saveRDS(US_st_crop, file = "Data/census_geo/US_st_crop.rds")

rm(US_48, US_cbg, US_cbg_50, US_50)

US_cbg_crop <- readRDS("Data/census_geo/US_cbg_crop.rds") %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID)) 

US_st_crop <- readRDS("Data/census_geo/US_st_crop.rds")

################################################################################
## Health violations
################################################################################

cbg_HB_vio <- read_rds("Data/combined/area/HB_vio_hm_area.rds")  %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(total_violations)) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup

# Combine with geographic data for US cbgs

HB_vio_all <- left_join(US_cbg_crop, cbg_HB_vio, by = "ID", relationship = "many-to-many") %>% 
  st_as_sf()


# Tract level 

tract_HB_vio <- cbg_HB_vio %>%
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

HB_vio_plot <- ggplot() + 
  geom_sf(data = HB_vio_all, aes(fill = avg_cbg_vio, geometry = geometry), 
          color = NA) +
  scale_fill_distiller(name = "", palette = "Reds", 
                       direction = 1, 
                       # limits=c(0,10), 
                       #breaks = c(0,10), 
                       # labels = c(0,">10"), 
                       na.value = scales::alpha("#DCDCDC", 0.20)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

# Following saves as a png, but takes a minute; maintains the lack of boundaries  

png(file = paste0(plot_path,"HB_vio_cbg_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 32,
    bg = "transparent")

print(HB_vio_plot)

dev.off()

## Map with truncated values 

hb_vio_trunc <- HB_vio_all %>%
  mutate(cbg_vio_trunc = ifelse(avg_cbg_vio >10, 10, avg_cbg_vio))

HB_vio_plot <- ggplot() + 
  geom_sf(data = hb_vio_trunc, aes(fill = cbg_vio_trunc, geometry = geometry), 
          color = NA) +
  scale_fill_distiller(name = "", palette = "Reds", 
                       direction = 1, 
                       limits=c(0,10), breaks = c(0,10), 
                       labels = c(0,">10"), 
                       na.value = scales::alpha("#DCDCDC", 0.20)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank())  

# Following saves as a png, but takes a minute; maintains the lack of boundaries  

png(file = paste0(plot_path,"HB_vio_trunc_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 32,
    bg = "transparent")

print(HB_vio_plot)

dev.off()

rm(HB_vio_all, HB_vio_plot, cbg_HB_vio, hb_vio_trunc)


################################################################################
## LCR violations
################################################################################

cbg_lcr_vio <- readRDS("Data/combined/area/lcr_vio_hm_area.rds") #load lcr data

# CBG

cbg_lcr_vio <- cbg_lcr_vio %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID)) %>%
  group_by(ID) %>%
  mutate(pb_vio_cbg = mean(pb_vio_count)) %>% #average number of lead violations per CBG
  mutate(cu_vio_cbg = mean(cu_vio_count)) %>% #average number of copper violations per CBG
  mutate(avg_pb_cbg = mean(avg_pb_level)) %>% #average lead levels per CBG
  mutate(avg_cu_cbg = mean(avg_cu_level)) %>% #average copper levels per CBG
  distinct(ID, .keep_all = TRUE)

lcr_summary <- cbg_lcr_vio %>%
  group_by(state_name) %>%
  summarize(
    quant25 = quantile(avg_pb_cbg, probs = .25),
    median = median(avg_pb_cbg, na.rm = TRUE),
    quant75 = quantile(avg_pb_cbg, probs = .75),
    max = max(avg_pb_cbg)
  ) %>%
  arrange(desc(quant75))

summary(cbg_lcr_vio$avg_pb_cbg)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.002636 0.004313 0.014924 0.007520 6.289051 

summary(cbg_lcr_vio$pb_vio_cbg)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.000   1.201   1.000  35.000

lcr_vio_all <- left_join(US_cbg_crop, cbg_lcr_vio) 

################################################################################
## LCR violations Maps
################################################################################

# Map 3 : LCR violations : CBG

gr_scale <- c(
  '#ffffff',
  '#d9d9d9',
  '#bdbdbd',
  '#969696',
  '#737373',
  '#525252',
  '#252525',
  '#000000'
)

# summary(lcr_vio_all$avg_pb_level) #distribution

lcr_plot <- ggplot() + 
  geom_sf(data = lcr_vio_all, aes(fill = avg_pb_cbg, geometry = geometry), color = NA) +
  scale_fill_gradientn(name = "", colours = gr_scale, 
                        # direction = 1, 
                       # limits=c(0,0.015), breaks = c(0,0.005, 0.010, 0.015), 
                       # labels = c(0, 0.005, 0.010, ">0.015"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pb_levels_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(lcr_plot)

dev.off()

# Truncated values

lcr_vio_trunc <- lcr_vio_all %>%
  mutate(pb_lvl_trunc = ifelse(avg_pb_cbg >= 0.015, 0.015, avg_pb_cbg)) %>%
  mutate(pb_ale_trunc = ifelse(pb_vio_cbg >= 5, 5, pb_vio_cbg))

summary(lcr_vio_all$pb_vio_cbg) #distribution

pb_trunc_plot <- ggplot() + 
  geom_sf(data = lcr_vio_trunc, aes(fill = pb_lvl_trunc, geometry = geometry), color = NA) +
  scale_fill_gradientn(name = "", colours = gr_scale, 
                       #  direction = 1, 
                       limits=c(0,0.015), breaks = c(0,0.005, 0.010, 0.015), 
                       labels = c(0, 0.005, 0.010, ">0.015"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pb_conc_trunc_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(pb_trunc_plot)

dev.off()

# ALEs

pb_ale_plot <- ggplot() + 
  geom_sf(data = lcr_vio_trunc, aes(fill = pb_ale_trunc, geometry = geometry), color = NA) +
  scale_fill_gradientn(name = "", colours = gr_scale, 
                       #  direction = 1, 
                       limits=c(0,5), breaks = c(0,5),
                       labels = c(0, ">5"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pb_ale_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(pb_ale_plot)

dev.off()

rm(lcr_vio_all, cbg_lcr_vio, lcr_plot, pb_trunc_plot)

################################################################################
## PFAS violations
################################################################################

cbg_pfas_vio <- read_rds("Data/combined/area/pfas_vio_hm_area.rds")  %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID)) %>% #load pfas data
  group_by(ID) %>%
  mutate(cbg_pfas_count = mean(pfas_count, na.rm = T)) %>% #avg pfas count per cbg
  mutate(avg_conc_cbg = mean(concentration_avg, na.rm = T)) %>% #avg pfas concentration per cbg
  mutate(avg_max_pfas = mean(concentration_max, na.rm = T)) %>%
  mutate(trunc_max = ifelse(avg_max_pfas >1000, 1000, avg_max_pfas)) %>%
  mutate(avg_det_share = mean(detection_share, na.rm = T)) %>%
  mutate(conc_avg_trunc = ifelse(avg_conc_cbg > 70, 70, avg_conc_cbg)) %>%
  distinct(ID, .keep_all = TRUE) 

pfas_vio_all <- left_join(US_cbg_crop, cbg_pfas_vio) 

summary(cbg_pfas_vio$cbg_pfas_count) 
# 2.0

summary(cbg_pfas_vio$avg_max_pfas) 

summary(cbg_pfas_vio$avg_conc_cbg) 
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   0.000   0.000   7.579   6.259 836.900   25474 

################################################################################
## PFAS Violations Maps
################################################################################

# Map 5 : PFAS concentrations cbg

pfas_plot <- ggplot() + 
  geom_sf(data = pfas_vio_all, aes(fill = cbg_pfas_count, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "GnBu", 
                       direction = 1, 
                       # limits=c(0,9),
                       # breaks = c(0,9),
                       # labels = c(0, ">9"),
                       na.value = scales::alpha("#DCDCDC", 0.75)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pfas_count_cbg_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(pfas_plot)

dev.off()

# Max PFAS 

pfas_plot <- ggplot() + 
  geom_sf(data = pfas_vio_all, aes(fill = trunc_max, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "GnBu", 
                       direction = 1, 
                       limits=c(0,1000),
                       breaks = c(0,1000),
                       labels = c(0, ">1000"),
                       na.value = scales::alpha("#DCDCDC", 0.75)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pfas_max_1k_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(pfas_plot)

dev.off()
# Truncated data

pfas_plot <- ggplot() + 
  geom_sf(data = pfas_vio_all, aes(fill = avg_det_share, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "GnBu", 
                       direction = 1, 
na.value = scales::alpha("#DCDCDC", 0.75)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pfas_det_share_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(pfas_plot)

dev.off()

# Average PFAS

pfas_plot <- ggplot() + 
  geom_sf(data = pfas_vio_all, aes(fill = conc_avg_trunc, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "GnBu", 
                       direction = 1, 
                       limits=c(0,70),
                       breaks = c(0,70),
                       labels = c(0, ">70"),
                       na.value = scales::alpha("#DCDCDC", 0.75)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pfas_conc_av_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(pfas_plot)

dev.off()

##

rm(pfas_vio_all, cbg_pfas_vio, pfas_plot)


################################################################################
## DBP violations
################################################################################

cbg_dbp_vio <- read_rds("Data/combined/area/dbp_vio_hm_area_v2.rds") %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID)) %>%
  group_by(ID) %>%
  mutate(avg_dbp_cbg = mean(combined_dbp), 
         dbp_cbg_trunc = ifelse(avg_dbp_cbg >= 80, 80, avg_dbp_cbg)) %>%
  distinct(ID, .keep_all = TRUE) 

dbp_vio_all <- left_join(US_cbg_crop, cbg_dbp_vio) 

summary(dbp_vio_all$avg_dbp_cbg) #distribution

################################################################################
## DBP violations Maps
################################################################################

# Map 8 : DBP violations CBG

dbp_plot <- ggplot() + 
  geom_sf(data = dbp_vio_all, aes(fill = dbp_cbg_trunc, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "Blues", 
                       direction = 1, limits=c(0,80), breaks = c(0,80), labels = c(0,">80"), 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"combined_dbp_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(dbp_plot)

dev.off()


rm(dbp_vio_all, cbg_dbp_vio, dbp_plot)

################################################################################
## TCR violations
################################################################################

cbg_tcr_vio <-read_rds("Data/combined/area/tcr_vio_hm_area.rds") %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID)) %>%
  group_by(ID) %>%
  mutate(tcr_det_cbg = mean(detection_share),
         tcr_det_trunc = ifelse(tcr_det_cbg >= 0.10, 0.10, tcr_det_cbg)) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup

count <- count(cbg_tcr_vio, st_abbrev)

tcr_vio_all <- left_join(US_cbg_crop, cbg_tcr_vio) 

summary <- tcr_vio_all %>%
  st_drop_geometry() %>%
  filter(!is.na(tcr_det_cbg)) %>%
  group_by(st_abbrev) %>%
  summarize(mean = mean(tcr_det_cbg, na.rm =TRUE),
            obs = n()) %>%
  mutate(mean = format(mean, scientific = FALSE))

summary(tcr_vio_all$tcr_det_cbg) #distribution

################################################################################
## TCR violations Maps
################################################################################

# Map 10 : TCR violations CBG

tcr_plot <- ggplot() + 
  geom_sf(data = tcr_vio_all, aes(fill = tcr_det_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "YlOrRd", 
                       direction = 1, 
                       #limits=c(0,0.10), breaks = c(0,0.10), labels = c(0,">0.10"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"tcr_vio_cbg_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(tcr_plot)

dev.off()

tcr_plot <- ggplot() + 
  geom_sf(data = tcr_vio_all, aes(fill = tcr_det_trunc, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "YlOrRd", 
                       direction = 1, limits=c(0,0.10), breaks = c(0,0.10), labels = c(0,">0.10"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"tcr_det_share_trunc_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(tcr_plot)

dev.off()

rm(tcr_plot, tcr_vio_all, cbg_tcr_vio)

################################################################################
## Arsenic concentration Maps
################################################################################


cbg_ars_vio <- read_rds("Data/combined/area/arsenic_vio_hm_area.rds")  %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(arsenic_ngl = as.numeric(arsenic)*1000) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(as.numeric(arsenic_ngl)))  %>%
  mutate(ars_tr = ifelse(avg_cbg_vio > 20, 20, avg_cbg_vio)) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup

summary(cbg_ars_vio$avg_cbg_vio)

# Combine with geographic data for US cbgs

ars_vio_all <- left_join(US_cbg_crop, cbg_ars_vio, by = "ID", relationship = "many-to-many") %>% 
  st_as_sf()

summary(cbg_ars_vio$avg_cbg_vio)

summary(ars_vio_all$avg_cbg_vio)

summary(ars_vio_all$ars_tr)


################################################################################
## Arsenic violations Maps
################################################################################

# Map 10 : arsenic violations CBG

ars_plot <- ggplot() + 
  geom_sf(data = ars_vio_all, aes(fill = avg_cbg_vio, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "Blues", 
                       direction = 1, 
                       #limits=c(0,0.25), breaks = c(0,0.25), labels = c(0,">0.25"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"ars_vio_cbg_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(ars_plot)

dev.off()

ars_plot <- ggplot() + 
  geom_sf(data = ars_vio_all, aes(fill = ars_tr, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "Blues", 
                       direction = 1, 
                       limits=c(0,20), breaks = c(0,20), labels = c(0,">20"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"ars_vio_trunc_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(ars_plot)

dev.off()

rm(ars_plot, cbg_ars_vio, ars_vio_all)


################################################################################
## Nitrate violations
################################################################################

cbg_nitrate_vio <- read_rds("Data/combined/area/nitrate_vio_hm_area.rds")  %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(as.numeric(nitrate)),
        nitrate_tr = if_else(avg_cbg_vio > 20, 20, avg_cbg_vio)) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup

# Combine with geographic data for US cbgs

nitrate_vio_all <- left_join(US_cbg_crop, cbg_nitrate_vio, by = "ID", relationship = "many-to-many") %>% 
  st_as_sf() 

summary(cbg_nitrate_vio$avg_cbg_vio)

summary(nitrate_vio_all$avg_cbg_vio)

################################################################################
## Arsenic violations Maps
################################################################################

# Map 10 : nitrate violations CBG

nitrate_plot <- ggplot() + 
  geom_sf(data = nitrate_vio_all, aes(fill = avg_cbg_vio, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "RdPu", 
                       direction = 1, 
                       #limits=c(0,0.25), 
                       # breaks = c(0,0.1), labels = c(0,">0.1"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"nitrate_vio_cbg_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(nitrate_plot)

dev.off()

nitrate_plot <- ggplot() + 
  geom_sf(data = nitrate_vio_all, aes(fill = nitrate_tr, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "RdPu", 
                       direction = 1, 
                       limits=c(0,20), 
                       breaks = c(0,20), labels = c(0,">20"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"nitrate_tr_cbg_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(nitrate_plot)

dev.off()

rm(nitrate_vio_all, cbg_nitrate_vio, nitrate_plot)


################################################################################
## Country demographic maps
################################################################################

hm_cbg <- read.csv("Data/demographics/hm_dems.csv")

pwsid_cbg <- read.csv("Data/demographics/hm_dems.csv") %>%
  dplyr::select(-contains("P_")) %>%
  clean_names() %>%
  mutate(ID=str_pad(id, 12, pad="0"))

pwsid_cbg <- left_join(US_cbg_crop, pwsid_cbg) 

st_as_sf(pwsid_cbg) #set as spatial object

################################################################################
## Race and Income demographics 
################################################################################

# Map 11 : Racial demographics 

race_plot <- ggplot() + 
  geom_sf(data = pwsid_cbg, aes(fill = minorpct, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "BuPu", 
                       direction = 1, 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"race_cbg_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(race_plot)

dev.off()


# Map 12 : Income demographics 

inc_plot <- ggplot() + 
  geom_sf(data = pwsid_cbg, aes(fill = lowincpct, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "RdPu", 
                       direction = 1, 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 32), 
        legend.title = element_text(family = "serif", size = 32), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"inc_cbg_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(inc_plot)

dev.off()

rm(pwsid_cbg, inc_plot, race_plot)


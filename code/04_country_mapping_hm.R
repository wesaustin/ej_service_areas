
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
my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

setwd(paste0(my_path))
getwd()

plot_path <- "ej_service_areas/output/country maps/hm/"


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

saveRDS(US_cbg_crop, file = "ej_service_areas/data/census_geo/US_cbg_crop.rds")

saveRDS(US_st_crop, file = "ej_service_areas/data/census_geo/US_st_crop.rds")

rm(US_48, US_cbg, US_cbg_50, US_50)

US_cbg_crop <- readRDS("ej_service_areas/data/census_geo/US_cbg_crop.rds") 

US_st_crop <- readRDS("ej_service_areas/data/census_geo/US_st_crop.rds")

################################################################################
## Health violations
################################################################################

cbg_HB_vio <- read_rds("data/combined/HB_vio_hm_area.rds")  %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(total_violations)) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup

# Combine with geographic data for US cbgs

HB_vio_all <- left_join(US_cbg_crop, cbg_HB_vio, by = "ID", relationship = "many-to-many") %>% 
  st_as_sf()

summary(US_cbg_crop$ID)

summary(cbg_HB_vio$total_violations)

summary(cbg_HB_vio$avg_cbg_vio)


# # Tract level 
# 
# tract_HB_vio <- cbg_HB_vio %>%
#   mutate(tract = substring(ID, first=1, last=11)) %>%
#   group_by(tract) %>%
#   mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP), na.rm = TRUE) %>% #average violations per tract
#   distinct(tract, .keep_all = TRUE) 
# 
# HB_vio_all <- left_join(US_tract_50, tract_HB_vio, by = "tract") #note to rerun this if you are doing tract and tract level, right now comb share the same name
# 
# st_as_sf(HB_vio_all)
# 
# summary(HB_vio_all$avg_vio_tract)
# #3rd qt = 1.35

################################################################################
# HB Violations Maps
################################################################################

# Map 1 : HB violations at CBG

HB_vio_plot <- ggplot() + 
  geom_sf(data = HB_vio_all, aes(fill = avg_cbg_vio, geometry = geometry), 
          color = NA) +
  scale_fill_distiller(name = "", palette = "Reds", 
                       direction = 1, limits=c(0,25), breaks = c(0,25), 
                       labels = c(0,">25"), 
                       na.value = scales::alpha("#DCDCDC", 0.20)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

# Following saves as a png, but takes a minute; maintains the lack of boundaries  

png(file = paste0(plot_path,"HB_vio_cbg_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 28,
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
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank())  

# Following saves as a png, but takes a minute; maintains the lack of boundaries  

png(file = paste0(plot_path,"HB_vio_trunc_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 28,
    bg = "transparent")

print(HB_vio_plot)

dev.off()

rm(HB_vio_all, HB_vio_plot, cbg_HB_vio)


################################################################################
## LCR violations
################################################################################

cbg_lcr_vio <- read_rds("data/combined/lcr_vio_hm_area.rds") #load lcr data

# CBG

cbg_lcr_vio <- cbg_lcr_vio %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID)) %>%
  group_by(ID) %>%
  mutate(tids = paste(pwsid, sep = ", "))  %>%
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

summary(lcr_vio_all$avg_pb_level) #distribution

lcr_plot <- ggplot() + 
  geom_sf(data = lcr_vio_all, aes(fill = avg_pb_cbg, geometry = geometry), color = NA) +
  scale_fill_gradientn(name = "90th Percentile\nLead levels\n(1991-2020)", colours = gr_scale, 
                       #  direction = 1, 
                       limits=c(0,0.015), breaks = c(0,0.005, 0.010, 0.015), 
                       labels = c(0, 0.005, 0.010, ">0.015"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pb_levels_US.png"), 
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
  scale_fill_gradientn(name = "Average lead\n90th percentile\nvalues\n(1991-2021)", colours = gr_scale, 
                       #  direction = 1, 
                       limits=c(0,0.015), breaks = c(0,0.005, 0.010, 0.015), 
                       labels = c(0, 0.005, 0.010, ">0.015"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pb_trunc_US.png"), 
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
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pb_ale_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(pb_ale_plot)

dev.off()

rm(lcr_vio_all, cbg_lcr_vio, lcr_plot, pb_trunc_plot)

################################################################################
## PFAS violations
################################################################################

cbg_pfas_vio <- read_rds("data/combined/pfas_vio_hm_area.rds") %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID)) %>% #load pfas data
  group_by(ID) %>%
  mutate(tids = paste(pwsid, sep = ", "))  %>%
  mutate(cbg_pfas_count = mean(pfas_count)) %>% #avg pfas count per cbg
  mutate(avg_conc_cbg = mean(concentration_sum)) %>% #avg pfas concentration per cbg
  distinct(ID, .keep_all = TRUE) 

pfas_vio_all <- left_join(US_cbg_crop, cbg_pfas_vio) 

summary(cbg_pfas_vio$cbg_pfas_count) 
# 2.0

################################################################################
## PFAS Violations Maps
################################################################################

# Map 5 : PFAS concentrations cbg

pfas_plot <- ggplot() + 
  geom_sf(data = pfas_vio_all, aes(fill = cbg_pfas_count, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Unique PFAS\ndetected\n(2013-2023)", palette = "GnBu", 
                       direction = 1, 
                       # limits=c(0,9),
                       # breaks = c(0,9),
                       # labels = c(0, ">9"),
                       na.value = scales::alpha("#DCDCDC", 0.75)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pfas_vio_cbg_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(pfas_plot)

dev.off()

# Truncated data

pfas_vio_trunc <- pfas_vio_all %>%
  mutate(pfas_count_trunc = ifelse(cbg_pfas_count >= 9, 9, cbg_pfas_count))

pfas_plot <- ggplot() + 
  geom_sf(data = pfas_vio_trunc, aes(fill = pfas_count_trunc, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Unique PFAS\ndetected\n(2013-2023)", palette = "GnBu", 
                       direction = 1, 
                       limits=c(0,9),
                       breaks = c(0,9),
                       labels = c(0, ">9"),
                       na.value = scales::alpha("#DCDCDC", 0.75)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"pfas_vio_trunc_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(pfas_plot)

dev.off()

rm(pfas_vio_trunc, pfas_vio_all, cbg_pfas_vio, pfas_plot)



################################################################################
## DBP violations
################################################################################

cbg_dbp_vio <- read_rds("data/combined/dbp_vio_hm_area_v2.rds") %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID))

# CBG

cbg_dbp_vio <- cbg_dbp_vio %>%
  group_by(ID) %>%
  mutate(avg_dbp_cbg = mean(combined_dbp)) %>%
  distinct(ID, .keep_all = TRUE) 

dbp_vio_all <- left_join(US_cbg_crop, cbg_dbp_vio) 

summary(dbp_vio_all$avg_dbp_cbg) #distribution

################################################################################
## DBP violations Maps
################################################################################

# Map 8 : DBP violations CBG

dbp_plot <- ggplot() + 
  geom_sf(data = dbp_vio_all, aes(fill = avg_dbp_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "Blues", 
                       direction = 1, limits=c(0,60), breaks = c(0,60), labels = c(0,">60"), 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"dbp_vio_cbg_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(dbp_plot)

dev.off()

# Truncated plot

dbp_vio_trunc <- dbp_vio_all %>%
  mutate(dbp_cbg_trunc = ifelse(avg_dbp_cbg >= 80, 80, avg_dbp_cbg))

dbp_plot <- ggplot() + 
  geom_sf(data = dbp_vio_trunc, aes(fill = dbp_cbg_trunc, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "Blues", 
                       direction = 1, limits=c(0,80), breaks = c(0,80), labels = c(0,">80"), 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"dbp_vio_trunc_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(dbp_plot)

dev.off()

rm(dbp_vio_all, cbg_dbp_vio, dbp_plot)

################################################################################
## TCR violations
################################################################################

cbg_tcr_vio <-read_rds("data/combined/tcr_vio_hm_area.rds") %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID)) %>%
  group_by(ID) %>%
  mutate(tcr_det_cbg = mean(detection_share)) %>%
  mutate(tids = paste(pwsid, sep = ", "))  %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup

count <- count(cbg_tcr_vio, st_abbrev)

summary <- cbg_tcr_vio %>%
  group_by(st_abbrev) %>%
  summarize(mean = mean(tcr_det_cbg, na.rm =TRUE),
            obs = n()) %>%
  mutate(mean = format(mean, scientific = FALSE))

tcr_vio_all <- left_join(US_cbg_crop, cbg_tcr_vio) 

summary(tcr_vio_all$tcr_det_cbg) #distribution

################################################################################
## TCR Detection Rate Maps
################################################################################

# Map 10 : TCR violations CBG

tcr_plot <- ggplot() + 
  geom_sf(data = tcr_vio_all, aes(fill = tcr_det_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "YlOrRd", 
                       direction = 1, limits=c(0,0.10), breaks = c(0,0.10), labels = c(0,">0.10"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"tcr_vio_cbg_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(tcr_plot)

dev.off()

tcr_vio_trunc <- tcr_vio_all %>%
  mutate(tcr_det_trunc = ifelse(tcr_det_cbg >= 0.10, 0.10, tcr_det_cbg))

tcr_plot <- ggplot() + 
  geom_sf(data = tcr_vio_trunc, aes(fill = tcr_det_trunc, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "YlOrRd", 
                       direction = 1, limits=c(0,0.10), breaks = c(0,0.10), labels = c(0,">0.10"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"tcr_vio_trunc_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(tcr_plot)

dev.off()

rm(tcr_plot, tcr_vio_all, cbg_tcr_vio)


################################################################################
## Arsenic Concentrations
################################################################################



cbg_ars_vio <- read_rds("data/combined/arsenic_vio_hm_area.rds")  %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(as.numeric(arsenic))) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup

cbg_ars_vio <- cbg_ars_vio %>%
  mutate(avg_cbg_vio = avg_cbg_vio * 1000 ) #convert to ppb


# Combine with geographic data for US cbgs

ars_vio_all <- left_join(US_cbg_crop, cbg_ars_vio, by = "ID", relationship = "many-to-many") %>% 
  st_as_sf() %>%
  mutate(ars_tr = case_when(avg_cbg_vio > 20 ~ 20, 
                            TRUE ~ avg_cbg_vio))


summary(cbg_ars_vio$avg_cbg_vio)

summary(ars_vio_all$avg_cbg_vio)

summary(ars_vio_all$ars_tr)


################################################################################
## Arsenic Concentrations Map
################################################################################

ars_plot <- ggplot() + 
  geom_sf(data = ars_vio_all, aes(fill = ars_tr, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "Blues", 
                       direction = 1, 
                       #limits=c(0,0.25), breaks = c(0,0.25), labels = c(0,">0.25"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"ars_vio_cbg_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(ars_plot)

dev.off()

################################################################################
## Nitrate Concentrations
################################################################################

cbg_nitrate_vio <- read_rds("data/combined/nitrate_vio_hm_area.rds")  %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(as.numeric(nitrate))) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup

# Combine with geographic data for US cbgs

nitrate_vio_all <- left_join(US_cbg_crop, cbg_nitrate_vio, by = "ID", relationship = "many-to-many") %>% 
  st_as_sf() 
# %>%
#  mutate(nitrate_tr = if_else(avg_cbg_vio > .1, .1, avg_cbg_vio))

summary(cbg_nitrate_vio$avg_cbg_vio)
summary(nitrate_vio_all$avg_cbg_vio)

################################################################################
## Nitrate Concentration Maps
################################################################################


nitrate_plot <- ggplot() + 
  geom_sf(data = nitrate_vio_all, aes(fill = nitrate, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "", palette = "RdPu", 
                       direction = 1, 
                       #limits=c(0,0.25), 
                       breaks = c(0,20 ), labels = c(0,">20"),
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"nitrate_vio_cbg_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(nitrate_plot)

dev.off()


################################################################################
## Country demographic maps
################################################################################

hm_cbg <- read.csv("data/demographics/hm_dems.csv")

pwsid_cbg <- read.csv("data/demographics/hm_dems.csv") %>%
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
  scale_fill_distiller(name = "% People of \nColor", palette = "BuPu", 
                       direction = 1, 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"race_cbg_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(race_plot)

dev.off()


# Map 12 : Income demographics 

inc_plot <- ggplot() + 
  geom_sf(data = pwsid_cbg, aes(fill = lowincpct, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "% Below 2X Federal\nPoverty Line", palette = "RdPu", 
                       direction = 1, 
                       na.value = scales::alpha("#DCDCDC", 0.5)) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

png(file = paste0(plot_path,"inc_cbg_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 20,
    bg = "transparent")

print(inc_plot)

dev.off()

rm(pwsid_cbg, inc_plot, race_plot)

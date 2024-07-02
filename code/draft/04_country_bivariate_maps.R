################################################################################
# Bivariate maps for drinking water indicators : country maps
# National Center for Environmental Economics
# Last edited: 10/6/30
################################################################################

# Drawing from code developed by Wes Austin for a biscale function with action levels 

# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  sf,
  cowplot,
  patchwork,
  dplyr, # data wrangling
  mapview,
  biscale,
  ggplot2,
  tidycensus, #Census data
  readr,
  raster, # raster data operations
  exactextractr, # fast raster data extraction for polygons
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  tmap, # for map creation
  cowplot, # for bivariate mapping
  stringr, # string manipulation
  mapview,
  pals
)

################################################################################
##Set directories
################################################################################

##my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"
my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"


getwd()
setwd(paste0(my_path))
getwd()

#plot_path <- "Plots/country/"
plot_path <- "ej_service_areas/output/bivariate maps/"


# Source code for bi_scale manipulations

source("ej_service_areas/code/biscale_fn_action_level.R")

custom_pal3_4 <- c(
  "1-1" = "#d3d3d3", # low x, low y
  "2-1" = "#9c9ecd",
  "3-1" = "#646ac7", # high x, low y
  "1-2" = "#c3c594",
  "2-2" = "#909390", # medium x, medium y
  "3-2" = "#5c638c",
  "1-3" = "#b1b552", # low x, high y
  "2-3" = "#83884f",
  "3-3" = "#545b4d" # high x, high y
)


################################################################################
## Load tract data for all US
################################################################################

# Following code to retreive ALL CBG-level data for the US

US_cbg <- tigris::block_groups(state = NULL, county = NULL, cb = TRUE) 

st_crs(US_cbg)

US_cbg_50 <- US_cbg %>%
  rename(ID = GEOID) %>%
  tigris::shift_geometry() %>%
  mutate(STATEFP = as.numeric(STATEFP)) %>%
  mutate(ID = as.numeric(ID)) %>%
  dplyr::filter(STATEFP <= 56 & STATEFP != 43 ) %>%  #remove Puerto Rico and outer territories
  st_transform(crs = 5070) # this CRS gives the neatest projections

################################################################################
## Health violations
################################################################################

cbg_HB_vio <- read_rds("data/combined/HB_vio_hm_area.rds") 

# For cbg with more than one PWSID, find average number of violations at the cbg level
# then collapse to include only 1 observation per census cbg level (12-digit "ID") 

cbg_HB_vio <- cbg_HB_vio %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = mean(total_violations)) %>%
  distinct(ID, .keep_all = TRUE)  

HB_vio_all <- left_join(US_cbg_50, cbg_HB_vio) 

st_as_sf(HB_vio_all)

################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(HB_vio_all$avg_vio_cbg)

#3rd qt = 2

#3rd quartile = 2, so we will use that as a limit for "frequent violators"

action_vector <- c(0.9,2) #Modify for indicator-specific

#Establish biscale comparison: HB violations and % POC

data_biscale_al <- bi_class_al(HB_vio_all,
                               x = avg_vio_cbg, y = minorpct, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Health based violations and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Violations",
                    ylab = "% POC",
                    size = 20)

# Plot and legend

hb_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)


png(file = paste0(plot_path,"hb_minor_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(hb_minor)

dev.off()


#Establish biscale comparison: HB violations and % Low Income

data_biscale_al <- bi_class_al(HB_vio_all,
                               x = avg_vio_cbg, y = lowinc, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 2: Biscale violations: HB violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Violations",
                    ylab = "% Low Inc",
                    size = 20)

# Plot and legend

hb_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"hb_lowinc_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(hb_lowinc)

dev.off()


rm(HB_vio_all, cbg_HB_vio, hb_lowinc, hb_minor)


################################################################################
## LCR violations
################################################################################


cbg_lcr_vio <- read_rds("data/combined/lcr_vio_hm_area.rds") #load lcr data

# CBG

cbg_lcr_vio <- cbg_lcr_vio %>%
  group_by(ID) %>%
  mutate(pb_vio_cbg = mean(pb_vio_count)) %>% #average number of lead violations per CBG
  distinct(ID, .keep_all = TRUE)

lcr_vio_all <- left_join(US_cbg_50, cbg_lcr_vio) 

st_as_sf(lcr_vio_all) #set as spatial object

################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(lcr_vio_all$pb_vio_cbg) #distribution

#3rd quartile = 0.01, so we will use that as a limit for "frequent violators"

action_vector <- c(0.9,2) #Modify for indicator-specific

#Establish biscale comparison: lcr violations and % minority 

data_biscale_al <- bi_class_al(lcr_vio_all,
                               x = pb_vio_cbg, y = minorpct,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 4: Lead levels and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Violations",
                    ylab = "% POC",
                    size = 6)

# Plot and legend

pb_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)


png(file = paste0(plot_path,"pb_lvl_POC_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pb_minor)

dev.off()

#Establish biscale comparison: PB levels and % Low Income

data_biscale_al <- bi_class_al(lcr_vio_all,
                               x = avg_pb_cbg, y = LOWINCPCT, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 2: Biscale violations: HB violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = custom_pal3_4,
                    dim = 3,
                    xlab = "Violations",
                    ylab = "% Low Inc",
                    size = 20)

# Plot and legend

pb_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"pb_lowinc_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pb_lowinc)

dev.off()


rm(lcr_vio_all, cbg_lcr_vio)

################################################################################
## PFAS violations
################################################################################

cbg_pfas_vio <- read_rds("data/combined/cbg_pfas_vio_combined.rds") #load pfas data

# CBG

cbg_pfas_vio <- cbg_pfas_vio %>%
  group_by(ID) %>%
  mutate(pfas_count_cbg = sum(pfas_count*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #avg pfas count per cbg
  mutate(avg_conc_cbg = sum(concentration_sum*ACSTOTPOP)/sum(ACSTOTPOP)) %>% #avg pfas concentration per cbg
  distinct(ID, .keep_all = TRUE) 

pfas_vio_all <- left_join(US_cbg_50, cbg_pfas_vio) 

st_as_sf(pfas_vio_all) #set as spatial object

################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(pfas_vio_all$avg_conc_cbg)

summary(pfas_vio_all$pfas_count_cbg)

#3rd quartile = 1, so we will use that as a limit for "frequent violators"

action_vector <- c(0.01,1) #Modify for indicator-specific

#Establish biscale comparison: pfas violations and % POC

data_biscale_al <- bi_class_al(pfas_vio_all,
                               x = pfas_count_cbg, y = MINORPCT, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))


# Map 1: Health based violations and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "BlueOr", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "BlueOr",
                    dim = 3,
                    xlab = "# PFAS Detected",
                    ylab = "% POC",
                    size = 20)

# Plot and legend

pfas_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"pfas_minor_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pfas_minor)

dev.off()


#Establish biscale comparison: pfas violations and % low income

data_biscale_al <- bi_class_al(pfas_vio_all,
                               x = pfas_count_cbg, y = LOWINCPCT, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Biscale violations: pfas violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal3_4, dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = custom_pal3_4,
                    dim = 3,
                    xlab = "# PFAS Detected",
                    ylab = "% Low Inc",
                    size = 20)

# Plot and legend

pfas_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"pfas_lowinc_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pfas_lowinc)

dev.off()


rm(pfas_vio_all, cbg_pfas_vio, pfas_lowinc, pfas_minor)

################################################################################
## DBP violations
################################################################################

cbg_dbp_vio <- read_rds("data/combined/cbg_dbp_vio_combined.rds")

# CBG

cbg_dbp_vio <- cbg_dbp_vio %>%
  group_by(ID) %>%
  mutate(avg_dbp_cbg = sum(combined_dbp*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE) 

dbp_vio_all <- left_join(US_cbg_50, cbg_dbp_vio) 

st_as_sf(dbp_vio_all) #set as spatial object

################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(dbp_vio_all$avg_dbp_cbg) #distribution

#3rd quartile = 64, so we will use that as a limit for "frequent violators"

action_vector <- c(0.9,60) #Modify for indicator-specific

#Establish biscale comparison: dbp violations and % POC

data_biscale_al <- bi_class_al(dbp_vio_all,
                               x = avg_dbp_cbg, y = MINORPCT, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Health based violations and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "BlueOr", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "BlueOr",
                    dim = 3,
                    xlab = "Higher DBP Concentration",
                    ylab = "Higher % POC",
                    size = 20)

# Plot and legend

dbp_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"dbp_minor_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(dbp_minor)

dev.off()


#Establish biscale comparison: dbp violations and % low income

data_biscale_al <- bi_class_al(dbp_vio_all,
                               x = avg_dbp_cbg, y = LOWINCPCT, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Biscale violations: dbp violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal3_4, dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = custom_pal3_4,
                    dim = 3,
                    xlab = "Higher DBP Concentration",
                    ylab = "% Low Income",
                    size = 20)

# Plot and legend

dbp_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"dbp_lowinc_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(dbp_lowinc)

dev.off()


rm(dbp_vio_all, cbg_dbp_vio, dbp_lowinc, dbp_minor)

################################################################################
## TCR violations
################################################################################

cbg_tcr_vio <-read_rds("data/combined/cbg_tcr_vio_combined.rds")

# CBG

cbg_tcr_vio <- cbg_tcr_vio %>%
  group_by(ID) %>%
  mutate(tcr_det_cbg = sum(detection_share*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE)

tcr_vio_all <- left_join(US_cbg_50, cbg_tcr_vio) 

st_as_sf(tcr_vio_all) #set as spatial object


################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(tcr_vio_all$tcr_det_cbg) #distribution

#3rd quartile = 0.01, but we can use .25 as a limit for "high concentrations"

action_vector <- c(0.01,.25) #Modify for indicator-specific

#Establish biscale comparison: tcr violations and % POC

data_biscale_al <- bi_class_al(tcr_vio_all,
                               x = tcr_det_cbg, y = MINORPCT, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Health based violations and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "BlueOr", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "BlueOr",
                    dim = 3,
                    xlab = "Higher TCR Concentration",
                    ylab = "Higher % POC",
                    size = 20)

# Plot and legend

tcr_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"tcr_minor_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(tcr_minor)

dev.off()


#Establish biscale comparison: tcr violations and % low income

data_biscale_al <- bi_class_al(tcr_vio_all,
                               x = tcr_det_cbg, y = LOWINCPCT, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Biscale violations: tcr violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal3_4, dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = custom_pal3_4,
                    dim = 3,
                    xlab = "Higher TCR Concentration",
                    ylab = "% Low Income",
                    size = 20)

# Plot and legend

tcr_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"tcr_lowinc_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(tcr_lowinc)

dev.off()


rm(tcr_vio_all, cbg_tcr_vio, tcr_lowinc, tcr_minor)













################################################################################
## Arsenic Concentrations
################################################################################



cbg_ars_vio <-read_rds("data/combined/arsenic_vio_epic_area.rds")

# CBG

cbg_ars_vio <- cbg_ars_vio %>%
  group_by(ID) %>%
  mutate(arsenic_cbg = sum(arsenic*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE)

tcr_vio_all <- left_join(US_cbg_50, cbg_tcr_vio) 

st_as_sf(tcr_vio_all) #set as spatial object


################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(tcr_vio_all$tcr_det_cbg) #distribution

#3rd quartile = 0.01, but we can use .25 as a limit for "high concentrations"

action_vector <- c(0.01,.25) #Modify for indicator-specific

#Establish biscale comparison: tcr violations and % POC

data_biscale_al <- bi_class_al(tcr_vio_all,
                               x = tcr_det_cbg, y = MINORPCT, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Health based violations and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "BlueOr", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "BlueOr",
                    dim = 3,
                    xlab = "Higher TCR Concentration",
                    ylab = "Higher % POC",
                    size = 20)

# Plot and legend

tcr_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"tcr_minor_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(tcr_minor)

dev.off()


#Establish biscale comparison: tcr violations and % low income

data_biscale_al <- bi_class_al(tcr_vio_all,
                               x = tcr_det_cbg, y = LOWINCPCT, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Biscale violations: tcr violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal3_4, dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = custom_pal3_4,
                    dim = 3,
                    xlab = "Higher TCR Concentration",
                    ylab = "% Low Income",
                    size = 20)

# Plot and legend

tcr_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"tcr_lowinc_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(tcr_lowinc)

dev.off()


rm(tcr_vio_all, cbg_tcr_vio, tcr_lowinc, tcr_minor)



################################################################################
## Nitrate Concentrations
################################################################################



cbg_tcr_vio <-read_rds("data/combined/cbg_tcr_vio_combined.rds")

# CBG

cbg_tcr_vio <- cbg_tcr_vio %>%
  group_by(ID) %>%
  mutate(tcr_det_cbg = sum(detection_share*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  distinct(ID, .keep_all = TRUE)

tcr_vio_all <- left_join(US_cbg_50, cbg_tcr_vio) 

st_as_sf(tcr_vio_all) #set as spatial object


################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(tcr_vio_all$tcr_det_cbg) #distribution

#3rd quartile = 0.01, but we can use .25 as a limit for "high concentrations"

action_vector <- c(0.01,.25) #Modify for indicator-specific

#Establish biscale comparison: tcr violations and % POC

data_biscale_al <- bi_class_al(tcr_vio_all,
                               x = tcr_det_cbg, y = MINORPCT, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Health based violations and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "BlueOr", dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "BlueOr",
                    dim = 3,
                    xlab = "Higher TCR Concentration",
                    ylab = "Higher % POC",
                    size = 20)

# Plot and legend

tcr_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"tcr_minor_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(tcr_minor)

dev.off()


#Establish biscale comparison: tcr violations and % low income

data_biscale_al <- bi_class_al(tcr_vio_all,
                               x = tcr_det_cbg, y = LOWINCPCT, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Biscale violations: tcr violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal3_4, dim = 3) +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = custom_pal3_4,
                    dim = 3,
                    xlab = "Higher TCR Concentration",
                    ylab = "% Low Income",
                    size = 20)

# Plot and legend

tcr_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(legend, x = 0.1, y = 0.05, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"tcr_lowinc_biv_US.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(tcr_lowinc)

dev.off()


rm(tcr_vio_all, cbg_tcr_vio, tcr_lowinc, tcr_minor)




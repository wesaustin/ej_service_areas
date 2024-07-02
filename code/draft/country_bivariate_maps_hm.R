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

# library(remotes)
# 
# remotes::install_github("chris-prener/biscale", force = TRUE)
library(biscale)

################################################################################
##Set directories
################################################################################

my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

getwd()
setwd(paste0(my_path))
getwd()

plot_path <- "Plots/country/bivariate_maps/"

# Source code for bi_scale manipulations

source("Code/biscale_fn_action_level.R")

custom_pal = c(
  "1-1" = "#d3d3d3", # low x, low y
  "2-1" = "#d38d82",
  "3-1" = "#d3250b", # high x, low y
  "1-2" = "#7eb6c3",
  "2-2" = "#7e7a78", # medium x, medium y
  "3-2" = "#7e200a",
  "1-3" = "#1e95b1", # low x, high y
  "2-3" = "#1e646d",
  "3-3" = "#1e1a09"  # high x, high y
)


################################################################################
## Load cbg data for all US
################################################################################

US_cbg_crop <- readRDS("Data/census_geo/US_cbg_crop.rds") 

US_st_crop <- readRDS("Data/census_geo/US_st_crop.rds")


################################################################################
## Health violations
################################################################################

cbg_HB_vio <- readRDS("Data/combined/area/HB_vio_hm_area.rds") %>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

# For cbg with more than one PWSID, find average number of violations at the cbg level
# then collapse to include only 1 observation per census cbg level (12-digit "ID") 

cbg_HB_vio <- cbg_HB_vio %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = mean(total_violations, na.rm = TRUE)) %>%
  mutate(avg_vio_length = mean(diff_days_max)) %>%
  distinct(ID, .keep_all = TRUE) 

#205662 obs

HB_vio_all <- left_join(US_cbg_crop, cbg_HB_vio) 

st_as_sf(HB_vio_all)

################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(HB_vio_all$avg_vio_cbg)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00    0.00    0.00    0.93    0.50  176.00   77727 

summary(cbg_HB_vio$avg_vio_cbg)

state_sum <- HB_vio_all %>%
  st_drop_geometry %>%
  group_by(state_name) %>%
  summarize(count = n(),
            avg_vio = mean(total_violations, na.rm = TRUE))


summary(HB_vio_all$minorpct)

summary(cbg_HB_vio$minorpct)


#3rd quartile = 0.5, so we will use that as a limit for "frequent violators"

action_vector <- c(0.9,2) #Modify for indicator-specific

## Race

#Establish biscale comparison: HB violations and % POC

data_biscale_al <- bi_class_al(HB_vio_all,
                               x = avg_vio_cbg, y = minorpct, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Health based violations and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA,
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  bi_theme() +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  theme(plot.background = element_blank()) 

breaks <- list(bi_x = c("0", "1", "2", "176"),
               bi_y = c("0", "0.11", "0.63", "1"))

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Violations →",
                    ylab = "% People of color →",
                    size = 20,
                    breaks = breaks,
                    arrows = FALSE,
                    base_family = "serif")


# Plot and legend

hb_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)


png(file = paste0(plot_path,"hb_POC_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(hb_minor)

dev.off()

## Low income

summary(cbg_HB_vio$lowinc)


#Establish biscale comparison: HB violations and % Low Income

data_biscale_al <- bi_class_al(HB_vio_all,
                               x = avg_vio_cbg, y = lowinc, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 2: Biscale violations: HB violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA, 
          size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  bi_theme() +
  theme(plot.background = element_blank())

breaks <- list(bi_x = c("0", "1", "2", "176"),
               bi_y = c("0", "0.15", "0.47", "1"))


legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Violations →",
                    ylab = "% Low income →",
                    size = 20,
                    breaks = breaks,
                    arrows = FALSE,
                    base_family = "serif")


# Plot and legend

hb_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"hb_lowinc_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(hb_lowinc)

dev.off()

rm(HB_vio_all, cbg_HB_vio, hb_lowinc, hb_minor)


################################################################################
## LCR violations
################################################################################

cbg_lcr_vio <- read_rds("Data/combined/area/lcr_vio_hm_area.rds")  %>%
  mutate(ID=str_pad(ID, 12, pad="0"))%>%
  mutate(ID = as.character(ID)) #load lcr data

# CBG

cbg_lcr_vio <- cbg_lcr_vio %>%
  group_by(ID) %>%
  mutate(pb_vio_cbg = mean(pb_vio_count)) %>% #average number of lead violations per CBG
  mutate(cu_vio_cbg = mean(cu_vio_count)) %>% #average number of copper violations per CBG
  mutate(avg_pb_cbg = mean(avg_pb_level)) %>% #average lead levels per CBG
  mutate(avg_cu_cbg = mean(avg_cu_level)) %>% #average copper levels per CBG
  distinct(ID, .keep_all = TRUE)



lcr_vio_all <- left_join(US_cbg_crop, cbg_lcr_vio) 

st_as_sf(lcr_vio_all) #set as spatial object

################################################################################
## Create bivariate map : Lead levels
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(lcr_vio_all$avg_pb_level) #distribution

#3rd quartile = 0.01, so we will use that as a limit for "frequent violators"

action_vector <- c(0.005, 0.015) #Modify for indicator-specific

#Establish biscale comparison: lcr violations and % minority 

data_biscale_al <- bi_class_al(lcr_vio_all,
                               x = avg_pb_cbg, y = minorpct,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 4: Lead levels and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  bi_theme(bg_color = "#ffffff")

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Lead Levels →",
                    ylab = "% POC →",
                    size = 20,
                    arrows = FALSE,
                    base_family = "serif")

# Plot and legend

pb_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)


png(file = paste0(plot_path,"pb_lvl_POC_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pb_minor)

dev.off()

#Establish biscale comparison: PB levels and % Low Income

data_biscale_al <- bi_class_al(lcr_vio_all,
                               x = avg_pb_cbg, y = lowinc, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 2: Biscale violations: Lead levels and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), 
          color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  bi_theme() +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "Lead Levels →",
                    ylab = "% Low Inc →",
                    size = 20,
                    arrows = FALSE,
                    base_family = "serif")

# Plot and legend

pb_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"pb_lvl_lowinc_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pb_lowinc)

dev.off()

rm(cbg_lcr_vio, pb_minor, pb_lowinc)

################################################################################
## Create bivariate map : Lead violations (count)
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(lcr_vio_all$pb_vio_cbg) #distribution

#3rd quartile = 1, so we will use that as a limit for "frequent violators"

action_vector <- c(0.9, 1) #Modify for indicator-specific

#Establish biscale comparison: lcr violations and % minority 

data_biscale_al <- bi_class_al(lcr_vio_all,
                               x = pb_vio_cbg, y = minorpct,
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 4: Lead levels and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "Violations →",
                    ylab = "% POC →",
                    size = 20,
                    arrows = FALSE,
                    base_family = "serif")

# Plot and legend

pb_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"pb_vio_POC_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pb_minor)

dev.off()

#Establish biscale comparison: PB levels and % Low Income

data_biscale_al <- bi_class_al(lcr_vio_all,
                               x = pb_vio_cbg, y = lowinc, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 2: Biscale violations: HB violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  bi_theme() +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "Violations →",
                    ylab = "% Low Inc →",
                    size = 20,
                    arrows = FALSE,
                    base_family = "serif")

# Plot and legend

pb_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"pb_vio_lowinc_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pb_lowinc)

dev.off()


rm(lcr_vio_all, cbg_lcr_vio, pb_minor, pb_lowinc)

################################################################################
## PFAS violations
################################################################################

cbg_pfas_vio <- cbg_pfas_vio %>%
  group_by(ID) %>%
  mutate(cbg_det_share = mean(detection_share)) %>% #avg pfas count per cbg
  mutate(avg_conc_cbg = mean(concentration_sum)) %>% #avg pfas concentration per cbg
  distinct(ID, .keep_all = TRUE) 

pfas_vio_all <- left_join(US_cbg_crop, cbg_pfas_vio) 

summary(cbg_pfas_vio$avg_conc_cbg) 
# 0.012
summary(cbg_pfas_vio$cbg_det_share) #distribution
# 0.02

################################################################################
## PFAS Violations Maps
################################################################################

# Map 5 : PFAS concentrations cbg

pfas_vio_conc <- pfas_vio_all %>%
  mutate(concentration_sum_ppt = concentration_sum*1000) %>%
  mutate(concentration_sum_ppt = ifelse(concentration_sum_ppt > 1, 1, concentration_sum_ppt))

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(pfas_vio_all$avg_conc_cbg)

summary(pfas_vio_all$cbg_det_share)

#3rd quartile = 1, so we will use that as a limit for "frequent violators"

action_vector <- c(0.01,.02) #Modify for indicator-specific

#Establish biscale comparison: pfas violations and % POC

data_biscale_al <- bi_class_al(pfas_vio_all,
                               x = avg_conc_cbg, y = minorpct, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))


# Map 1: Health based violations and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  bi_theme() +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "PFAS Concentration →",
                    ylab = "% POC →",
                    size = 20,
                    arrows = FALSE,
                    base_family = "serif")

# Plot and legend

pfas_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"pfas_minor_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pfas_minor)

dev.off()


#Establish biscale comparison: pfas violations and % low income

data_biscale_al <- bi_class_al(pfas_vio_all,
                               x = pfas_count_cbg, y = lowinc, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Biscale violations: pfas violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  bi_theme() +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "# PFAS Detected →",
                    ylab = "% Low Inc →",
                    size = 20,
                    arrows = FALSE,
                    base_family = "serif")

# Plot and legend

pfas_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"pfas_lowinc_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(pfas_lowinc)

dev.off()


rm(pfas_vio_all, cbg_pfas_vio, pfas_lowinc, pfas_minor)

################################################################################
## DBP violations
################################################################################

cbg_dbp_vio <- read_rds("Data/combined/area/dbp_vio_hm_area.rds")%>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

# CBG

cbg_dbp_vio <- cbg_dbp_vio %>%
  group_by(ID) %>%
  mutate(avg_dbp_cbg = mean(combined_dbp)) %>%
  distinct(ID, .keep_all = TRUE) 

dbp_vio_all <- left_join(US_cbg_crop, cbg_dbp_vio) 

st_as_sf(dbp_vio_all) #set as spatial object

################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(dbp_vio_all$avg_dbp_cbg) #distribution

#3rd quartile = 64, so we will use that as a limit for "frequent violators"

action_vector <- c(10,60) #Modify for indicator-specific

#Establish biscale comparison: dbp violations and % POC

data_biscale_al <- bi_class_al(dbp_vio_all,
                               x = avg_dbp_cbg, y = minorpct, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Health based violations and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  bi_theme() +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "DBP Concentration →",
                    ylab = "% POC →",
                    size = 20,
                    arrows = FALSE,
                    base_family = "serif")

# Plot and legend

dbp_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"dbp_minor_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(dbp_minor)

dev.off()


#Establish biscale comparison: dbp violations and % low income

data_biscale_al <- bi_class_al(dbp_vio_all,
                               x = avg_dbp_cbg, y = lowinc, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Biscale violations: dbp violations and low income


map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  bi_theme() +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  theme(plot.background = element_blank())


legend <- biscale::bi_legend(pal = "DkCyan",
                             dim = 3,
                             xlab = "DBP Concentration →",
                             ylab = "% Low Income →",
                             size = 20,
                             arrows = FALSE,
                             base_family = "serif")

# Plot and legend

dbp_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"dbp_lowinc_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(dbp_lowinc)

dev.off()


rm(dbp_vio_all, cbg_dbp_vio, dbp_lowinc, dbp_minor)

################################################################################
## TCR violations
################################################################################

cbg_tcr_vio <- read_rds("Data/combined/area/tcr_vio_hm_area.rds") %>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID=str_pad(ID, 12, pad="0"))

# CBG

cbg_tcr_vio <- cbg_tcr_vio %>%
  group_by(ID) %>%
  mutate(tcr_det_cbg = mean(detection_share)) %>%
  distinct(ID, .keep_all = TRUE)

tcr_vio_all <- left_join(US_cbg_crop, cbg_tcr_vio) 

st_as_sf(tcr_vio_all) #set as spatial object


################################################################################
## Create bivariate map
################################################################################

# 1. define the action level. Currently, the function takes two numbers for the action_vector, can be modified

summary(tcr_vio_all$tcr_det_cbg) #distribution

#3rd quartile = 0.01, but we can use .25 as a limit for "high concentrations"

action_vector <- c(0.01,.10) #Modify for indicator-specific

#Establish biscale comparison: tcr violations and % POC

data_biscale_al <- bi_class_al(tcr_vio_all,
                               x = tcr_det_cbg, y = minorpct, #% POC
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Health based violations and % POC

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = custom_pal, dim = 3) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = custom_pal,
                    dim = 3,
                    xlab = "TCR Concentration →",
                    ylab = "% POC →",
                    size = 20,
                    arrows = FALSE,
                    base_family = "serif")

# Plot and legend

tcr_minor <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"tcr_minor_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(tcr_minor)

dev.off()


#Establish biscale comparison: tcr violations and % low income

data_biscale_al <- bi_class_al(tcr_vio_all,
                               x = tcr_det_cbg, y = lowinc, #%Low income
                               style = "quantile", dim = 3,
                               action_level = T, action_vector = action_vector) %>%
  filter(!str_detect(bi_class, 'NA'))

# Map 1: Biscale violations: tcr violations and low income

map <- ggplot(data_biscale_al) +
  geom_sf(data = data_biscale_al, mapping = aes(fill = bi_class), color = NA , size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkCyan", dim = 3) +
  geom_sf(data = US_st_crop, fill = NA, color = "#969696") +
  bi_theme() +
  theme(plot.background = element_blank())

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "TCR Concentration →",
                    ylab = "% Low Income →",
                    size = 20,
                    arrows = FALSE,
                    base_family = "serif")


# Plot and legend

tcr_lowinc <- cowplot::ggdraw() +
  draw_plot(map, x = 0, y = 0, width = 0.95, height = 0.95) +
  draw_plot(legend, x = 0.75, y = 0.15, width = 0.2, height = 0.2)

png(file = paste0(plot_path,"tcr_lowinc_biv_US_hm.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 12,
    bg = "transparent")

print(tcr_lowinc)

dev.off()


rm(tcr_vio_all, cbg_tcr_vio, tcr_lowinc, tcr_minor)


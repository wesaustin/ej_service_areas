# Arizona

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
my_path <- "~/private"

#WA
# my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

setwd(paste0(my_path))
getwd()

plot_path <- "Plots/epic/"

################################################################################
## Load state data
################################################################################

az_border <- tigris::states(cb = TRUE) %>%
  filter(STATEFP == "04")

az_bg <- tigris::block_groups(state = "AZ", cb=TRUE) %>%
  rename(ID = GEOID)

################################################################################
## Load epic data
################################################################################

cbg_HB_vio <- read_rds("Data/epic/HB_vio_epic_area.rds")  %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(total_violations))%>%
  ungroup () %>%
  distinct(ID, .keep_all = TRUE)

az_hb_vio_epic <- left_join(az_bg, cbg_HB_vio)

## Map

HB_vio_plot <- ggplot() + 
  geom_sf(data = az_hb_vio_epic, aes(fill = avg_cbg_vio, geometry = geometry), 
          color = NA) +
  scale_fill_distiller(name = "Number Violations\n(2015-2023)", palette = "Reds", 
                       direction = 1, limits=c(0,10), breaks = c(0,10), 
                       labels = c(0,">10"), 
                       na.value = scales::alpha("#DCDCDC", 0.20)) +
  geom_sf(data = az_border, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

# Following saves as a png, but takes a minute; maintains the lack of boundaries  

png(file = paste0(plot_path,"HB_vio_cbg_AZ.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 28,
    bg = "transparent")

print(HB_vio_plot)

dev.off()

################################################################################
## Load Hall and Murray data
################################################################################

cbg_HB_vio <- read_rds("Data/hm/HB_vio_hm_area.rds")%>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  mutate(ID = as.character(ID)) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(total_violations)) %>%
  mutate(repeat_vio = case_when(
    total_violations > 1 ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  mutate(sum_repeat_vio = sum(repeat_vio))%>%
  ungroup () %>%
  distinct(ID, .keep_all = TRUE)

az_hb_vio_hm <- left_join(az_bg, cbg_HB_vio)

## HM plot

HB_vio_plot <- ggplot() + 
  geom_sf(data = az_hb_vio_hm, aes(fill = avg_cbg_vio, geometry = geometry), 
          color = NA) +
  scale_fill_distiller(name = "Number Violations\n(2015-2023)", palette = "Reds", 
                       direction = 1, limits=c(0,10), breaks = c(0,10), 
                       labels = c(0,">10"), 
                       na.value = scales::alpha("#DCDCDC", 0.20)) +
  geom_sf(data = az_border, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

# Following saves as a png, but takes a minute; maintains the lack of boundaries  

png(file = paste0(hm_plot_path,"HB_vio_cbg_AZ.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 28,
    bg = "transparent")

print(HB_vio_plot)

dev.off()

################################################################################
## Map of missing systems: Load epic data
################################################################################

az_counties <- tigris::counties("AZ", cb = TRUE) 

ggplot(az_counties) +
  geom_sf() +
  geom_sf_text(aes(label = NAME))

missing_sys <- read.csv("Data/missing systems.csv")

cbg_HB_vio <- read_rds("Data/epic/HB_vio_epic_area.rds") %>%
  mutate(ID=str_pad(ID, 12, pad="0")) %>%
  group_by(ID) %>%
  mutate(avg_cbg_vio = mean(total_violations))


# `%nin%` = Negate('%in%')

# cbg_missing <- cbg_HB_vio[cbg_HB_vio$pwsid %in% missing_sys$pwsid,]
# 
# cbg_az_missing <- cbg_missing %>%
#   filter(st_abbrev == "AZ")
# 
# cbg_nomatch <- filter(az_bg, az_bg$ID %nin% cbg_az_missing$ID)
# 
# cbg_nomatch <- filter(cbg_az_missing, cbg_az_missing$ID %nin% az_bg$ID )

az_hb_vio_epic <- left_join(az_bg, cbg_HB_vio)

az_hb_subset <- subset(az_hb_vio_epic, pwsid %in% missing_sys$pwsid) %>%
  distinct(ID, .keep_all = TRUE)

summary(az_hb_vio_epic$avg_cbg_vio)


## Map

HB_vio_plot <- ggplot() + 
  geom_sf(data = az_bg, fill = NA, color = NA) +
  geom_sf(data = az_hb_subset, aes(fill = avg_cbg_vio, geometry = geometry), 
          color = NA) +
  scale_fill_distiller(name = "Number Violations\n(2015-2023)", palette = "Reds", 
                       direction = 1, limits=c(0,1), breaks = c(0,1), 
                       labels = c(0,">1"), 
                       na.value = scales::alpha("#DCDCDC", 0.20)) +
  geom_sf(data = az_border, fill = NA, color = "#969696") +
  geom_sf(data = az_counties, fill = NA, color = "#969696") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(2, 'cm'),legend.text = element_text(family = "serif", size = 28), 
        legend.title = element_text(family = "serif", size = 28), legend.position = "right", 
        legend.box.background = element_blank()) 

length(!is.na(az_hb_vio_epic$pwsid))
# Following saves as a png, but takes a minute; maintains the lack of boundaries  

png(file = paste0(plot_path,"HB_vio_cbg_AZ_missing_sys.png"), 
    width = 1915, height = 1077, units = "px", pointsize = 28,
    bg = "transparent")

print(HB_vio_plot)

dev.off()


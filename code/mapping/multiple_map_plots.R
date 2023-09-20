

################################################################################
# Plotting multiple indicator plots
# National Center for Environmental Economics
# Latest update: 8/31/2023
################################################################################


################################################################################
## Load packages: 
################################################################################


if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  sf, # vector data operations
  raster, # raster data operations
  dplyr, # data wrangling
  lubridate, # Date object handling
  tmap, # for map creation
  cowplot,
  stringr, # string manipulation
  magrittr,
  tidycensus,
  mapview,
  MASS
)


################################################################################
## Load data: 
################################################################################


cbg_HB_vio <- read_rds("Data/cbg_HB_vio_combined.rds")

plot_path <- "Plots"

# Mapping multiple plots

cbg_HB_vio <- cbg_HB_vio %>%
  group_by(ID) %>%
  mutate(avg_vio_cbg = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) %>%
  mutate(avg_vio_length = mean(diff_days)) %>%
  distinct(ID, .keep_all = TRUE) 

################################################################################
##Grab TIGRIS boundaries for area of interest
################################################################################

NJ_bg <- tigris::block_groups("NJ") %>%
  mutate(ID=str_pad(GEOID, 12, pad="0")) %>%
  st_transform(crs = 4326)  

NJ_tract <- tigris::tracts("NJ") %>%
  mutate(tract=str_pad(GEOID, 11, pad="0")) %>%
  st_transform(crs = 4326)

NJ_counties <- tigris::counties("NJ") %>%
  mutate(county = str_pad(GEOID, 5, pad="0")) %>%
  st_transform(crs = 4326)

st_crs(NJ_bg)
st_crs(NJ_tract)

################################################################################
## Plot at administrative divisions to compare 
################################################################################

##Map 1 : Block group

HB_vio_NJ <- cbg_HB_vio %>%
  filter(ST_ABBREV == "NJ")  %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) #average violations per tract

HB_vio_NJ <- left_join(NJ_bg, HB_vio_NJ)

st_as_sf(HB_vio_NJ)

HB_NJ_bg <- ggplot() + 
  geom_sf(data = HB_vio_NJ, aes(fill = avg_vio_cbg, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per CBG", palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() +
  theme(legend.key.size = unit(0.5, "cm"), # Change legend key size
        legend.title = element_text(size = 8), # Change legend title font size
        legend.text = element_text(size = 6),  # Change legend text font size
        legend.position = "right"
  ) 

HB_NJ_bg

ggsave(filename = 'NJ_HB_cbg.png', path = plot_path)

# Map 2 - Tract 

HB_vio_NJ <- cbg_HB_vio %>%
  filter(ST_ABBREV == "NJ")  %>%
  mutate(tract = substring(ID, first=1, last=11)) %>%
  group_by(tract) %>%
  mutate(avg_vio_tract = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP)) #average violations per tract

HB_vio_NJ <- left_join(NJ_tract, HB_vio_NJ) #join by tract
st_as_sf(HB_vio_NJ) ##Convert to spatial object

HB_NJ_tract <- ggplot() + 
  geom_sf(data = HB_vio_NJ, aes(fill = avg_vio_tract, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per tract", palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() +
  theme(legend.key.size = unit(0.5, "cm"), # Change legend key size
        legend.title = element_text(size = 8), # Change legend title font size
        legend.text = element_text(size = 6),  # Change legend text font size
        legend.position = "right"
  ) 

HB_NJ_tract

ggsave(filename = 'NJ_HB_tract.png', path = plot_path)


## Map 3 : County

HB_vio_NJ <- cbg_HB_vio %>%
  filter(ST_ABBREV == "NJ") %>%
  mutate(county = substring(ID, first=1, last=5))  %>%
  group_by(county) %>%
  mutate(avg_vio_county = sum(total_violations*ACSTOTPOP)/sum(ACSTOTPOP))

HB_vio_NJ <- left_join(NJ_counties, HB_vio_NJ)

st_as_sf(HB_vio_NJ)

HB_NJ_county <- ggplot() + 
  geom_sf(data = HB_vio_NJ, aes(fill = avg_vio_county, geometry = geometry), color = NA) +
  scale_fill_distiller(name = "Average violations per county", palette = "Greens", direction = 1, na.value = scales::alpha("#DCDCDC", 0.5)) +
  ggthemes::theme_map() +
  theme(legend.key.size = unit(0.5, "cm"), # Change legend key size
         legend.title = element_text(size = 8), # Change legend title font size
         legend.text = element_text(size = 6),  # Change legend text font size
         legend.position = "right"
  ) 

HB_NJ_county

ggsave(filename = 'NJ_HB_county.png', path = plot_path)


library(patchwork)

patchwork <- HB_NJ_bg + HB_NJ_tract + HB_NJ_county +  
  plot_layout(ncol = 2)


patchwork + plot_annotation(
  title = 'New Jersey Health-based Violations',
  caption = 'Comparison of violations for different Census levels',
  theme = theme(plot.title = element_text(size = 12), plot.caption = element_text(size = 8))
)


ggsave2(filename = 'NJ_HB_maps.png', path = plot_path)


# Data Cleaning to Ensure all PWS Demographic Files use Consistent population served 





##############################################
# Zipcodes
##############################################


zc_dem <- read.csv(paste0("data/demographics/zc_dems.csv"))

zc_dem_comb <- read.csv(paste0("Data/demographics/epic_dems.csv")) %>%
  dplyr::select(pwsid, population_served_count) %>%
  right_join(zc_dem, by = 'pwsid') %>%
    clean_names() %>%
  write.csv("data/demographics/zc_dems.csv")

##############################################
# Counties
##############################################

county_dem <- read.csv(paste0("data/demographics/county_dems.csv")) 

duplicated_columns <- duplicated(t(county_dem))

# Show the Names of the Duplicated Columns
colnames(county_dem[duplicated_columns])

# Remove the Duplicated Columns
county_dem <- county_dem[!duplicated_columns]
county_dem <- rename_with(county_dem, ~ tolower(gsub(".x", "", .x, fixed = TRUE)))

county_dem_comb <- read.csv(paste0("data/demographics/epic_dems.csv")) %>%
  dplyr::select(pwsid, population_served_count) %>%
  right_join(county_dem, by = 'pwsid') %>%
  clean_names() %>%
  write.csv("data/demographics/county_dems.csv")


##############################################
# H&M aka EPA ORD Boundaries
##############################################

hm_dem <- read.csv(paste0("data/demographics/hm_dems.csv")) %>%
  clean_names() 

hm_dem_comb <- read.csv(paste0("data/demographics/epic_dems.csv")) %>%
  dplyr::select(pwsid, population_served_count) %>%
  right_join(hm_dem, by = 'pwsid') %>%
  clean_names() %>%
  write.csv("data/demographics/hm_dems.csv")
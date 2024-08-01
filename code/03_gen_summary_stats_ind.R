################################################################################
# Generate summary statistics for indicators
# National Center for Environmental Economics
# Last edited: 1/23/2024
################################################################################

# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readr,
  tidyverse,
  data.table, # data wrangling
  dplyr, # data wrangling
  lubridate, # Date object handling
  modelsummary, # regression table generation
  stringr, # string manipulation
  magrittr,
  tidycensus, #Census data
  MASS , #For regressions and modeling
  dplyr,
  readxl,
  kableExtra
)

################################################################################
##Set directories
################################################################################

#Tina
my_path <- "C:/Users/tbardot/OneDrive - Environmental Protection Agency (EPA)/Documents/EJ Water systems"

#WA
#my_path <- "C:/Users/gaustin/OneDrive - Environmental Protection Agency (EPA)/NCEE - Water System Service Boundaries"

getwd()
setwd(paste0(my_path))
getwd()


################################################################################

boundary = c("county", "zc", "usgs", "epic", "hm")

indicator = c('hb', 'lcr', 'pfas', 'dbp', 'tcr', 'ars', 'nitrate')

################################################################################
## Creating data frames for boundary for summary stats
################################################################################

for (b in boundary) {

  hb_vio <- readRDS(paste0('data/combined/pwsid/hb_vio_',b ,'.rds')) %>%
    rename(total.violations = total_violations)
  
  lcr_vio <- readRDS(paste0('data/combined/pwsid/lcr_vio_',b ,'.rds')) %>%
    mutate(total.violations = pb_vio_count)
  
  pfas_vio <- readRDS(paste0('data/combined/pwsid/pfas_vio_',b ,'.rds')) %>%
    mutate(total.violations = pfas_count)
  
  dbp_vio <- readRDS(paste0('data/combined/pwsid/dbp_vio_',b ,'.rds')) %>%
    mutate(total.violations = combined_dbp)
  
  tcr_vio <- readRDS(paste0('data/combined/pwsid/tcr_vio_',b ,'.rds')) %>%
    mutate(total.violations = detection_share) 

  ars_vio <- readRDS(paste0('data/combined/pwsid/ars_vio_',b ,'.rds')) %>%
    mutate(total.violations = arsenic*1000)
  
  nitrate_vio <- readRDS(paste0('data/combined/pwsid/nitrate_vio_',b ,'.rds')) %>%
    mutate(total.violations = nitrate)
  
  data_list <- list(hb_vio, lcr_vio, pfas_vio, dbp_vio, tcr_vio, ars_vio, nitrate_vio)
  
  
  for (j in 1:length(data_list)) {
    
    water_data <- data_list[[j]]
    
    i = nth(indicator, j)
    
    summary <- water_data %>%
      filter(!is.na(total.violations)) %>%
      summarize(
       # min = min(total.violations),
        # q25 = quantile(total_violations, 0.25),
        mean = mean(total.violations, na.rm = TRUE),
        median = quantile(total.violations, 0.5),
        max = max(total.violations),
        pws_count = n(),
        pct_zero = sum(total.violations == 0),
        tot_pop = sum(pop_served, na.rm = TRUE)) %>%
      mutate_if(is.numeric,
                round,
                digits = 3) %>%
      mutate(boundary = b,
             indicator = i, .before = 1) %>%
      rowwise %>%
      mutate(pct_zero = pct_zero/pws_count)
    
    if (j == 1) {
      sum_indicator = summary
    }
    else {
    sum_indicator <- bind_rows(sum_indicator, summary)
    }
    
  }
  
  if (b == "county") {
    sum_total <- sum_indicator
  }
  
  else {
    sum_total <- bind_rows(sum_total, sum_indicator)
  }
  
}

write.csv(sum_total, "Data/boundary_sum_stats.csv")

saveRDS(sum_total, "Data/boundary_sum_stats.rds")


## Make into a latex table

library(xtable)

sum_total <- readRDS("Data/boundary_sum_stats.rds") 
  #mutate(across(everything(), as.character)) %>%
sum_total$tot_pop <- lapply(signif(sum_total$tot_pop, digits = 2), format, digits = 2, scientific=F, big.mark =  ",")
sum_total[c(5:6)] <- lapply(sum_total[c(5:6)], format, digits = 2, nsmall = 0, big.mark=",", trim=F, scientific = F)
sum_total[c(3:4)] <- lapply(sum_total[c(3:4)], format, digits = 2, nsmall=2, trim = T)


library(kableExtra)
sum_total <- sum_total %>%
  arrange(match(indicator, c("hb", "lcr", "pfas", "dbp", 'tcr', 'ars', 'nitrate')))

indicators<- c(hb="Health-based violations", lcr="Lead ALEs", pfas="Count PFAs", 
      dbp="Combined DBP concentration", tcr="TCR detection share", ars = "Arsenic Concentration",
      nitrate='Nitrate concentration')

boundaries <- c(county = "County", zc = "Zip Code", usgs = "USGS", epic = "EPIC", hm = "EPA ORD")

text_table <- sum_total

text_table <- text_table %>%
  arrange(match(indicator, c("hb", "lcr", "pfas", "dbp", 'tcr', 'ars', 'nitrate'))) %>%
  rename(Boundary = boundary,
         Mean = mean,
         Median = median,
         Max = max) %>% 
  mutate(pct_zero = round(pct_zero, digits = 2)) %>%
  mutate(tot_pop = gsub(",000,000", "M", tot_pop)) %>%
  relocate(indicator, .before=Boundary)

text_table$indicator <- factor(indicators[sum_total$indicator])
text_table$Boundary <- as.character(boundaries[sum_total$boundary])


text_table

## Using kableExtra package

kable_table <- kbl(text_table[,2:8], booktabs = T, 
                   caption = "Summary Statistics", 
                   format = "latex")  %>%
  kable_styling(full_width = T) %>%
  column_spec(1, width="5cm") %>%
  pack_rows("Health-based Violations (2015-2022)", 1, 5) %>%
  pack_rows("Lead Action Level Exceedences (1991-2021)", 6, 10) %>%
  pack_rows("PFAS Detected (2013-2023)", 11, 15) %>%
  pack_rows("TTHM HAA5 Concentrations (2006-2019) ", 16, 20) %>%
  pack_rows("Total Coliform Detection Share (2006-2019)", 21, 25) %>%
  pack_rows("Arsenic Concentration  (2006-2019)", 26, 30) %>%
  pack_rows("Nitrate Concentration (2006-2019)", 31, 35) 
  
print(kable_table)

################################################################################

boundary = c("county", "zc", "usgs", "epic", "hm")

indicator = c('hb', 'lcr', 'pfas', 'dbp', 'tcr', 'ars', 'nitrate')

################################################################################
## Generate summary stats for systems, omitting zeroes
################################################################################

for (b in boundary) {
  
  hb_vio <- readRDS(paste0('data/combined/pwsid/hb_vio_',b ,'.rds')) %>%
    rename(total.violations = total_violations)
  
  lcr_vio <- readRDS(paste0('data/combined/pwsid/lcr_vio_',b ,'.rds')) %>%
    mutate(total.violations = pb_vio_count)
  
  pfas_vio <- readRDS(paste0('data/combined/pwsid/pfas_vio_',b ,'.rds')) %>%
    mutate(total.violations = pfas_count)
  
  dbp_vio <- readRDS(paste0('data/combined/pwsid/dbp_vio_',b ,'.rds')) %>%
    mutate(total.violations = combined_dbp)
  
  tcr_vio <- readRDS(paste0('data/combined/pwsid/tcr_vio_',b ,'.rds')) %>%
    mutate(total.violations = detection_share) 
  
  ars_vio <- readRDS(paste0('data/combined/pwsid/ars_vio_',b ,'.rds')) %>%
    mutate(total.violations = arsenic*1000)
  
  nitrate_vio <- readRDS(paste0('data/combined/pwsid/nitrate_vio_',b ,'.rds')) %>%
    mutate(total.violations = nitrate)
  
  data_list <- list(hb_vio, lcr_vio, pfas_vio, dbp_vio, tcr_vio, ars_vio, nitrate_vio)
  
  
  for (j in 1:length(data_list)) {
    
    water_data <- data_list[[j]]
    
    i = nth(indicator, j)
    
    summary <- water_data %>%
      filter(!is.na(total.violations) & total.violations > 0) %>%
      summarize(
        # min = min(total.violations),
        # q25 = quantile(total_violations, 0.25),
        mean = mean(total.violations, na.rm = TRUE),
        median = quantile(total.violations, 0.5),
        max = max(total.violations),
        pws_count = n(),
        tot_pop = sum(pop_served, na.rm = TRUE)) %>%
      mutate_if(is.numeric,
                round,
                digits = 3) %>%
      mutate(boundary = b,
             indicator = i, .before = 1) 
    
    if (j == 1) {
      sum_indicator = summary
    }
    else {
      sum_indicator <- bind_rows(sum_indicator, summary)
    }
    
  }
  
  if (b == "county") {
    sum_total <- sum_indicator
  }
  
  else {
    sum_total <- bind_rows(sum_total, sum_indicator)
  }
  
}

write.csv(sum_total, "Data/boundary_sum_stats_noZ.csv")

saveRDS(sum_total, "Data/boundary_sum_stats_noZ.rds")


## Make into a latex table

library(xtable)

sum_total <- readRDS("Data/boundary_sum_stats_noZ.rds") 
#mutate(across(everything(), as.character)) %>%
sum_total$tot_pop <- lapply(signif(sum_total$tot_pop, digits = 2), format, digits = 2, scientific=F, big.mark =  ",")
sum_total[c(5:6)] <- lapply(sum_total[c(5:6)], format, digits = 2, nsmall = 0, big.mark=",", trim=F, scientific = F)
sum_total[c(3:4)] <- lapply(sum_total[c(3:4)], format, digits = 2, nsmall=2, trim = T)


library(kableExtra)
sum_total <- sum_total %>%
  arrange(match(indicator, c("hb", "lcr", "pfas", "dbp", 'tcr', 'ars', 'nitrate')))

indicators<- c(hb="Health-based violations", lcr="Lead ALEs", pfas="Count PFAs", 
               dbp="Combined DBP concentration", tcr="TCR detection share", ars = "Arsenic Concentration",
               nitrate='Nitrate concentration')

boundaries <- c(county = "County", zc = "Zip Code", usgs = "USGS", epic = "EPIC", hm = "EPA ORD")

text_table <- sum_total

text_table <- text_table %>%
  arrange(match(indicator, c("hb", "lcr", "pfas", "dbp", 'tcr', 'ars', 'nitrate'))) %>%
  rename(Boundary = boundary,
         Mean = mean,
         Median = median,
         Max = max) %>% 
  mutate(tot_pop = gsub(",000,000", "M", tot_pop)) %>%
  relocate(indicator, .before=Boundary)

text_table$indicator <- factor(indicators[sum_total$indicator])
text_table$Boundary <- as.character(boundaries[sum_total$boundary])


text_table

## Using kableExtra package

kable_table <- kbl(text_table[,2:7], booktabs = T, 
                   caption = "Summary Statistics", 
                   format = "latex")  %>%
  kable_styling(full_width = T) %>%
  column_spec(1, width="5cm") %>%
  pack_rows("Health-based Violations (2015-2022)", 1, 5) %>%
  pack_rows("Lead Action Level Exceedences (1991-2021)", 6, 10) %>%
  pack_rows("PFAS Detected (2013-2023)", 11, 15) %>%
  pack_rows("TTHM HAA5 Concentrations (2006-2019) ", 16, 20) %>%
  pack_rows("Total Coliform Detection Share (2006-2019)", 21, 25) %>%
  pack_rows("Arsenic Concentration  (2006-2019)", 26, 30) %>%
  pack_rows("Nitrate Concentration (2006-2019)", 31, 35) 

print(kable_table)

################################################################################
## Creating data frames for boundary for summary stats : by race 
################################################################################

boundary = c("county", "zc", "usgs", "epic", "hm")

indicator = c('hb', 'lcr', 'pfas', 'dbp', 'tcr', 'ars', 'nitrate')


# Get demographic vars

for (b in boundary) {
  
  hb_vio <- readRDS(paste0('data/combined/pwsid/hb_vio_',b ,'.rds')) %>%
    rename(total.violations = total_violations)
  
  lcr_vio <- readRDS(paste0('data/combined/pwsid/lcr_vio_',b ,'.rds')) %>%
    mutate(total.violations = pb_vio_count)
  
  pfas_vio <- readRDS(paste0('data/combined/pwsid/pfas_vio_',b ,'.rds')) %>%
    mutate(total.violations = pfas_count)
  
  dbp_vio <- readRDS(paste0('data/combined/pwsid/dbp_vio_',b ,'.rds')) %>%
    mutate(total.violations = combined_dbp)
  
  tcr_vio <- readRDS(paste0('data/combined/pwsid/tcr_vio_',b ,'.rds')) %>%
    mutate(total.violations = detection_share) 
  
  ars_vio <- readRDS(paste0('data/combined/pwsid/ars_vio_',b ,'.rds')) %>%
    mutate(total.violations = arsenic*1000)
  
  nitrate_vio <- readRDS(paste0('data/combined/pwsid/nitrate_vio_',b ,'.rds')) %>%
    mutate(total.violations = nitrate)
  
  data_list <- list(hb_vio, lcr_vio, pfas_vio, dbp_vio, tcr_vio, ars_vio, nitrate_vio)
  
  for (j in 1:length(data_list)) {
    
    water_data <- data_list[[j]]
    
    i = nth(indicator, j)
    
    rel_risk <- water_data %>%
      mutate(pop_served = as.numeric(pop_served)) %>%
      filter(pop_served != 0)%>% #remove systems with no pop served
      rowwise %>%
      mutate(whitepct = (1-minorpct)) %>%
      mutate(minor_served = minorpct*pop_served) %>%
      mutate(black_served = frac_black*pop_served) %>% #black population served
      mutate(amer_ind_served = frac_amerind*pop_served) %>% #american indian served
      mutate(asian_served = frac_asian*pop_served) %>% #asian served
      mutate(pac_isl_served = frac_pacisl*pop_served) %>% #pacific islander served
      mutate(hispanic_served = frac_hisp*pop_served) %>% #hispanic served
      mutate(nhw_served = whitepct*pop_served) %>% #non hispanic white served 
      mutate(minor_risk = weighted.mean(total.violations, minor_served, na.rm= TRUE)) %>% #black population weight
      mutate(black_risk = weighted.mean(total.violations, black_served, na.rm= TRUE)) %>% #black population weight
      mutate(amer_ind_risk = weighted.mean(total.violations, amer_ind_served, na.rm= TRUE)) %>% #american indian weight
      mutate(asian_risk = weighted.mean(total.violations, asian_served, na.rm= TRUE)) %>% #asian pop weight
      mutate(pac_isl_risk = weighted.mean(total.violations, pac_isl_served, na.rm= TRUE)) %>% # pacific islander
      mutate(hispanic_risk = weighted.mean(total.violations, hispanic_served, na.rm= TRUE)) %>% # hispanic
      mutate(nhw_risk = weighted.mean(total.violations, nhw_served, na.rm= TRUE)) %>%   #non-hispanic white
      mutate(highinc = (1-lowinc)) %>% #create high income category
      mutate(highinc_served = highinc*pop_served) %>% #Find total population served in each category
      mutate(lowinc_served = lowinc*pop_served) %>%
      mutate(highinc_risk = weighted.mean(total.violations, highinc_served, na.rm= TRUE)) %>% #High income weight
      mutate(lowinc_risk = weighted.mean(total.violations, lowinc_served, na.rm= TRUE)) %>% #low income weight 
      ungroup
    
    
    summary <- rel_risk %>%
      filter(!is.na(total.violations)) %>%
        summarize(across(ends_with("_risk"), .fns = list(
        mean=mean), na.rm=TRUE)) %>%
      mutate_if(is.numeric,
                round,
                digits = 3) %>%
      mutate(indicator = i,
             boundary = b, .before = 1) 
    
    if (j == 1) {
      sum_indicator = summary
    }
    else {
      sum_indicator <- bind_rows(sum_indicator, summary)
    }
    
  }
  
  if (b == "county") {
    sum_total <- sum_indicator
  }
  
  else {
    sum_total <- bind_rows(sum_total, sum_indicator)
  }
  
}



write.csv(sum_total, "Data/dems_risk_sum_stats.csv")

saveRDS(sum_total, "Data/dems_risk_sum_stats.rds")


## Make into a latex table

library(kableExtra)

boundaries <- c(county = "County", zc = "Zip Code", usgs = "USGS", epic = "EPIC", hm = "EPA ORD")

sum_total <- readRDS("Data/dems_risk_sum_stats.rds") 

sum_total$boundary <- as.character(boundaries[sum_total$boundary])

# sum_total[c(3:9)] <- lapply(sum_total[c(3:9)],   format, nsmall=2, trim = T)

text_table <- sum_total

text_table <- text_table %>%
  dplyr::select(-starts_with(c("high", "minor"))) %>%
  arrange(match(indicator, c("hb", "lcr", "pfas", "dbp", 'tcr', 'ars', 'nitrate'))) %>%
  dplyr::select(c(1:2), sort(colnames(.))) %>%
  relocate(starts_with("lowinc"), .after = last_col()) %>%
  mutate_if(is.numeric, ~round(., digits = 2)) 

text_table

## Using kableExtra package

kable_table <- kbl(text_table[,2:9], booktabs = T, 
                   caption = "Mean Values by Demographic Group", 
                   col.names = NULL,
                   format = "latex")  %>%
  kable_styling(full_width = T) %>%
  add_header_above(c("Boundary" = 1, "Amer. Ind." = 1, "Asian" = 1, "Black" = 1, "Hispanic" = 1,"NH White" = 1, "Pac. Isl" = 1, "Low inc." = 1))%>%
  column_spec(1, width="4cm") %>%
  pack_rows("Health-based Violations (2015-2022)", 1, 5) %>%
  pack_rows("Lead Action Level Exceedences (1991-2021)", 6, 10) %>%
  pack_rows("PFAS Detected (2013-2023)", 11, 15) %>%
  pack_rows("TTHM HAA5 Concentrations (2006-2019) ", 16, 20) %>%
  pack_rows("Total Coliform Detection Share (2006-2019)", 21, 25) %>%
  pack_rows("Arsenic Concentration (2006-2019)", 26, 30) %>%
  pack_rows("Nitrate Concentration (2006-2019)", 31, 35)

print(kable_table)



################################################################################
## Summarize EJ communities using EJ indexes
################################################################################


df <- data.frame(matrix(nrow = 0, ncol = 3))

colnames(df) <- c('ej_coms', 'indicator', 'boundary')

for (b in boundary) {
  # b = "epic"
  
  hb_vio <- readRDS(paste0('data/combined/pwsid/hb_vio_',b ,'.rds')) %>%
    rename(total.violations = total_violations)
  
  lcr_vio <- readRDS(paste0('data/combined/pwsid/lcr_vio_',b ,'.rds')) %>%
    mutate(total.violations = pb_vio_count)
  
  pfas_vio <- readRDS(paste0('data/combined/pwsid/pfas_vio_',b ,'.rds')) %>%
    mutate(total.violations = pfas_count)
  
  dbp_vio <- readRDS(paste0('data/combined/pwsid/dbp_vio_',b ,'.rds')) %>%
    mutate(total.violations = combined_dbp)
  
  tcr_vio <- readRDS(paste0('data/combined/pwsid/tcr_vio_',b ,'.rds')) %>%
    mutate(total.violations = detection_share)
  
  
  data_list <- list(hb_vio, lcr_vio, pfas_vio, dbp_vio, tcr_vio)
  
  
  for (j in 1:length(data_list)) {
    
    water_data <- data_list[[j]]
    
    i = nth(indicator, j)
    
    ej_pwsids <- water_data %>%
      rowwise %>% 
      mutate(dem_index = (minorpct+lowinc)/2, # calculate indexes and scores 
             pm25_sc = pm25*dem_index, 
             ozone_sc =ozone*dem_index, 
             dslpm_sc =dslpm*dem_index,
             cancer_sc = cancer*dem_index,
             resp_sc =resp*dem_index,
             ptraf_sc =ptraf*dem_index,
             pnpl_sc =pnpl*dem_index,
             prmp_sc =prmp*dem_index,
             pre1960pct_sc =pre1960pct*dem_index,
             ptsdf_sc =ptsdf*dem_index,
             pwdis_sc =pwdis*dem_index,
             ust_sc =ust*dem_index) %>%
      ungroup %>%
      mutate(pm25_pct = rank(pm25_sc)/length(pm25_sc), # calculate percentiles
             ozone_pct = rank(ozone_sc)/length(ozone_sc),
             dlspm_pct = rank(dslpm_sc)/length(dslpm_sc),
             cancer_pct =rank(cancer_sc)/length(cancer_sc),
             resp_pct = rank(resp_sc)/length(resp_sc),
             ptaf_pct = rank(ptraf_sc)/length(ptraf_sc),
             pnpl_pct = rank(pnpl_sc)/length(pnpl_sc),
             prmp_pct = rank(prmp_sc)/length(prmp_sc),
             pre1960pct_pct = rank(pre1960pct_sc)/length(pre1960pct_sc),
             ptsdf_pct = rank(ptsdf_sc)/length(ptsdf_sc),
             pwdis_pct = rank(pwdis_sc)/length(pwdis_sc),
             ust_pct = rank(ust_sc)/length(ust_sc)) %>%
      mutate(ej_community =  case_when(
        pm25_pct >= 0.8 | ozone_pct >= 0.8 | ozone_pct >= 0.8 |
          dlspm_pct >= 0.8 | cancer_pct >= 0.8 | resp_pct >= 0.8 |
          ptaf_pct >= 0.8 | pnpl_pct >= 0.8 | prmp_pct >= 0.8 |
          pre1960pct_pct >= 0.8 | ptsdf_pct >= 0.8 | pwdis_pct >= 0.8 |
          ust_pct >= 0.8  ~ TRUE, 
        TRUE ~ FALSE
      ))
    
    ej_coms <- length(which(ej_pwsids$ej_community == 'TRUE' & !is.na(ej_pwsids$total.violations)))
    
    df[nrow(df) + 1, ] <- c(ej_coms, i, b)
    
  }}

write.csv(df, "Data/ej_index_summaries.csv")

df$inds_fac <- factor(df$indicator, levels = c("hb", "lcr", "pfas", "dbp", "tcr"))

# make a histogram

geom.text.size = 5
theme.size = (3) * geom.text.size

ej_plot <- df %>%
  mutate(ej_coms = as.numeric(ej_coms)) %>% 
  ggplot(aes(x = boundary, y = ej_coms, fill = inds_fac))+
  geom_bar(stat = "identity", 
           position = position_dodge2()) +
  scale_fill_manual(values = c("#fee685", 
                               "#70e17b",
                               "#99deea",
                               "#ffbc78",
                               "#f2938c"),
                    labels = c("Health-based", 
                               "Lead ALE",
                               "PFAS",
                               "DBP",
                               "TCR"),
                    name = "Indicator") +
  theme(text = element_text(family = "serif", size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = ej_coms), vjust = -.2, 
            position = position_dodge2(.9), 
            size = 8, 
            check_overlap = TRUE,
            family = "serif") +
  scale_x_discrete(labels = c("County", "Epic", "Hall & Murray", "USGS", "Zip code")) +
  xlab("Boundary") +
  ylab("# EJ water systems") 
# +
#   ggtitle("Number of systems identified as EJ communities",
#           subtitle = "Systems that are above the 80th percentile for any of the 13 EPA EJ indexes")

png(file = "Plots/ej_communities_his.png", 
    width = 2000, height = 750, units = "px", 
    # pointsize = 16,
    bg = "transparent")

print(ej_plot)

dev.off()


ej_plot <- df %>%
  mutate(ej_coms = as.numeric(ej_coms)) %>% 
  ggplot(aes(x = inds_fac, y = ej_coms, fill = boundary))+
  geom_bar(stat = "identity", 
           position = position_dodge2()) +
  scale_fill_manual(values = c("#fee685", 
                               "#70e17b",
                               "#99deea",
                               "#ffbc78",
                               "#f2938c"),
                    labels = c("County",
                               "Epic",
                               "Hall & Murray",
                               "USGS",
                               "Zip code"),
                    name = "Boundary")  +
  theme(text = element_text(family = "serif", size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) +
  geom_text(aes(label = ej_coms), vjust = -.2, 
            position = position_dodge2(.9), 
            size = 8, 
            check_overlap = TRUE,
            family = "serif") +
  scale_x_discrete(labels = c("Health-based", "Lead ALE", "PFAS","DBP", "TCR"),
                   name = "Indicator") +
  xlab("Boundary") +
  ylab("# EJ water systems") 


png(file = "Plots/ej_communities_his_2.png", 
    width = 2000, height = 750, units = "px", 
    # pointsize = 16,
    bg = "transparent")

print(ej_plot)

dev.off()

library(tidycensus)
options(tigris_use_cache = TRUE)

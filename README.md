# ej_service_areas
 A project to test the implications of adopting different service area types on the conclusions of environmental justice analyses.
 
 The project aims to test how disparate risk may appear to vary when using county-based, zipcode, SimpleLab/EPIC, and/or Hall/Murray service area products. 

Code is organized as follows:

- 00 programs produce shapefile boundaries and conduct areal apportionment of service areas to join demographics with water system IDs. These are basic data preparation programs. 
- 01 programs create drinking water quality measures. There are separate scripts for lead ALEs & health-based violations, PFAS, arsenic & nitrate, coliforms, and DBPs (which require a separate script to fill in TTHM and HAA5 values where only the constituent chemicals are listed). 
- 02 programs join the drinking water indicators to water system IDs and to census block groups associated with each water system ID. These programs also loop through all water quality measures to produce relative risks by demographic group. 
- 03 files generate data visualizations for the paper. 
- 04 programs produce maps according to each  the different drinking water indicators. 
- 05 conduct regressions. 

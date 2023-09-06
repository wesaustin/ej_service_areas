# ej_service_areas
 A project to test the implications of adopting different service area types on the conclusions of environmental justice analyses.
 
 The project aims to test how disparate risk may appear to vary when using county-based, zipcode, SimpleLab/EPIC, and/or Hall/Murray service area products. 

Code is organized as follows:

- 00 programs produce shapefile boundaries or convert raw data to be merged with service areas. These are basic data preparation programs. 
- 01 programs conduct the EJSCREENbatch protocol using different service boundary products. 
- 02 programs generate drinking water indicators. 
- 03 files generate relative risk indicators by demographic group. 
- 04 programs produce maps using the different drinking water indicators. 
- 05 conduct regressions. 

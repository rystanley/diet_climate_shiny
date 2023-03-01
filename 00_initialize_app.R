### ----------------------------------------------
### ----------------------------------------------
### This script loads and wrangles data 
### CSV spreadsheets of model outputs, 
### and shapefiles of areas of interest
### ----------------------------------------------
### ----------------------------------------------

### App data -------------------------
library(sf)

# Import csv crab and shrimp files ----------------------------------------
crab <- read.csv("data/crab_data_1995-2018_shiny_app.csv")
shrimp <- read.csv("data/shrimp_data_1995-2018_shiny_app.csv")



# Import shapefiles -------------------------------------------------------

##### NOTE #######
# I think uploading all these shapefiles and saving as .rds in the "data" folder 
# will speed up initialization time when running the app. Will need to test once
# the server is set up to see. Currently, loading is the slowest part. 
# also sf_simplify might make some of the bigger objects render faster

# upload SFAs
sfa <- read_sf("data/SFAs/SFAs_PANOMICS_Fall2020.shp", stringsAsFactors = T) %>%
  st_transform(sfa, crs = 4326)

# upload NAFOs
nafo <- read_sf("data/NAFOs/Divisions.shp",stringsAsFactors = T) %>% 
  st_transform(crs = 4326)# %>%
# st_crop(xmin=-80,xmax = -40, ymin = 40, ymax = 70)

# upload MPAs
mpa <- read_sf("data/MPAs/DFO_MPA_MPO_ZPM.shp", stringsAsFactors = T) %>% 
  st_transform(crs = 4326) %>%
  st_crop(xmin=-80,xmax = -40, ymin = 40, ymax = 70)

# upload OECMs
oecm <- read_sf("data/OECMs/DFO_OEABCM_MPO_AMCEZ.shp", stringsAsFactors = T) %>%
  st_transform(crs = 4326) %>%
  filter(!(REGION_E %in% c("Pacific")))

# upload dfo regions
dfo <- read_sf("data/DFO_Regions/All_areas_merged_fixed.shp", stringsAsFactors = T)

# upload bioregions
bioregions <- read_sf("data/Bioregions/DFO_Marine_Bioregions_Clipped_1M_CAEAC_2012_05_31.shp", stringsAsFactors = T) %>%
  st_transform(crs = 4326) %>%
  filter(!(REGION %in% c("Pacific","Great Lakes"))) %>%
  filter(!(Legend %in% c("5. Arctic Basin / Bassin Arctique","6. Western Arctic / Arctique de l'Ouest","7. Arctic Archipelago / Archipel Arctique")))



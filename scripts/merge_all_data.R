# Merge Census + Shapefile for Map Use
library(sf)
library(dplyr)
library(readr)

# Load cleaned datasets
guam_census_combined <- readRDS("data/cleaned/guam_census_combined.rds")
shapefile_path <- "data/shapefiles/tl_2020_66_place.shp"

# Load shapefile
guam_places <- st_read(shapefile_path)

# Merge on NAME
guam_merged <- guam_places %>%
  left_join(guam_census_combined, by = c("NAME" = "District"))

# Save merged dataset
saveRDS(guam_merged, "data/combined/guam_places_with_all_data.rds")

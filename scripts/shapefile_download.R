# Guam Village Shapefile Downloader
library(sf)

url <- "https://www2.census.gov/geo/tiger/TIGER2020/PLACE/tl_2020_66_place.zip"
dest_zip <- "data/raw/tl_2020_66_place.zip"
dest_dir <- "data/shapefiles"

if (!file.exists(dest_zip)) {
  download.file(url, destfile = dest_zip, mode = "wb")
}
if (!dir.exists(dest_dir)) {
  unzip(dest_zip, exdir = dest_dir)
}

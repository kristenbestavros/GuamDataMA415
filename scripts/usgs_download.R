# USGS Site Data Downloader & Cleaner
library(httr)
library(readr)
library(dplyr)

# Download raw USGS site list
guam_sites_url <- "https://waterservices.usgs.gov/nwis/site/?format=rdb&stateCd=gu"
guam_sites_raw <- read_lines(guam_sites_url)

# Remove header lines
guam_sites_clean_txt <- guam_sites_raw[!grepl("^#", guam_sites_raw)]
write_lines(guam_sites_clean_txt, "data/raw/guam_sites.txt")

# Parse and clean
guam_df <- read_delim("data/raw/guam_sites.txt", delim = "\t", skip = 1, show_col_types = FALSE)

guam_sites_clean <- guam_df %>%
  filter(!is.na(`16s...5`), !is.na(`16s...6`)) %>%
  transmute(
    site_no       = `15s`,
    station_name  = `50s`,
    site_type     = `7s`,
    latitude      = `16s...5`,
    longitude     = `16s...6`,
    datum         = `10s...8`
  )

# Save cleaned data
saveRDS(guam_sites_clean, "data/cleaned/guam_sites.rds")

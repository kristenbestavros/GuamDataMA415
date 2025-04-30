library(readr)
library(dplyr)
library(stringr)

# --- Table 02: Population ---
pop_url <- "https://www2.census.gov/programs-surveys/decennial/2020/data/island-areas/guam/population-and-housing-unit-counts/guam-phc-table02.csv"
download.file(pop_url, "data/raw/guam_population.csv", mode = "wb")

guam_population <- read_csv("data/raw/guam_population.csv", skip = 1, show_col_types = FALSE) %>%
  rename(District = 1, Population = 2) %>%
  filter(str_detect(Population, "^[0-9,]+$")) %>%
  mutate(
    District = str_remove(District, " CDP$"),
    District = str_trim(District),
    Population = as.numeric(str_remove_all(Population, ","))
  )

# --- Table 04: Housing Units (2020) ---
housing_url <- "https://www2.census.gov/programs-surveys/decennial/2020/data/island-areas/guam/population-and-housing-unit-counts/guam-phc-table04.csv"
download.file(housing_url, "data/raw/guam_housing_2020.csv", mode = "wb")

guam_housing <- read_csv("data/raw/guam_housing_2020.csv", skip = 1, show_col_types = FALSE) %>%
  rename(District = 1, Housing_Units = 2) %>%
  filter(str_detect(Housing_Units, "^[0-9,]+$")) %>%
  mutate(
    District = str_remove(District, " CDP$"),
    District = str_trim(District),
    Housing_Units = as.numeric(str_remove_all(Housing_Units, ","))
  )

# Merge population and housing
guam_census_combined <- guam_population %>%
  left_join(guam_housing, by = "District")

# Save
saveRDS(guam_census_combined, "data/cleaned/guam_census_combined.rds")


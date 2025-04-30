# USGS and Census-Based Analysis of Guam

**Author**: Kristen Bestavros  
**Course**: MA415 - Final Project  
**Institution**: Boston University

---

## Overview

This interactive Shiny app explores publicly available data from the **U.S. Geological Survey (USGS)** and the **2020 Decennial Census** for the island of **Guam**.

It integrates spatial, demographic, and infrastructure data to reveal patterns in:
- Population distribution
- Housing unit availability and density
- Water monitoring site coverage by type

---

## Features

### **Interactive Map**
- View a choropleth of population, housing units, or people per housing unit by village
- Overlay USGS water monitoring sites by type (e.g., stream, groundwater, atmosphere)
- Click on villages or sites for detailed information

### **Charts Tab**
- Bar chart of top villages by population and housing
- People per housing unit density ranking
- Distribution of USGS site types across Guam
- Scatter plot of population vs. housing units, including a regression line

### **Data Explorer**
- Searchable, sortable table of village-level metrics

---

## Data Sources

- **USGS National Water Information System (NWIS)**:  
  [https://waterdata.usgs.gov](https://waterdata.usgs.gov)

- **2020 Decennial Census - Island Areas**:  
  [https://www.census.gov](https://www.census.gov)

- **Geographic shapefiles**:  
  TIGER/Line 2020 place boundaries for Guam

---

## File Structure

├── app.R # Main Shiny application 
├── README.md # Project description and documentation 
├── data/ 
└──── raw/ # Unprocessed downloaded files 
└──── cleaned/ # Cleaned and structured data
└──── combined/ # Final merged datasets for app use
├── scripts/
└──── download_usgs.R 
└──── download_census.R 
└──── download_shapefile.R 
└──── merge_all_data.R

## How to Run

1. Open the project in RStudio.
2. Run all four scripts in the `scripts/` folder to download and clean the data. You can run the scripts in any order, except merge_all_data.R, which must be run last.
3. Launch the app by running `app.R`.

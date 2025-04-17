# Install and load necessary packages
install.packages("tidyverse")  # For data manipulation
install.packages("sf")         # For handling spatial data
install.packages("mapview")

library(tidyverse)
library(sf)
library(mapview)

# Read the Historic Redlining Indicator data
HRI_data <- read.csv("data-raw/Historic_Redlining_Indicator_Detroit_2020.csv")
head(HRI_data)

# Load the Detroit boundary shapefile
detroit_census_boundaries <- st_read("data-raw/CensusTracts2020.shp")

# Load the Overall Detroit Boundary shapefile
detroit_boundary <- st_read("data-raw/City_of_Detroit_Boundary.shp")

# Inspect the shapefiles
str(detroit_census_boundaries)
head(detroit_census_boundaries)
st_crs(detroit_census_boundaries)

str(detroit_boundary)
head(detroit_boundary)
st_crs(detroit_boundary)

# Ensure the GEOID column in HRI_data is character type
HRI_data <- HRI_data %>%
  mutate(GEOID = as.character(GEOID))

# Join HRI_data with detroit_census_boundaries
detroit_joined <- detroit_census_boundaries %>%
  left_join(HRI_data, by = "GEOID")

# View the joined data
head(detroit_joined)

# View HRI values by census tracts using mapview
# Define the color scale
col_palette <- colorRampPalette(c("blue", "red"))  # Adjust colors as needed

# Define the breaks for the legend
legend_breaks <- seq(1, 4, by = 1)  # Adjust the interval as needed

# Plot the map with custom legend and overlay the Detroit boundary
mapview(detroit_joined, 
        zcol = "HRI2020", 
        layer.name = "Redlining Values", 
        at = legend_breaks,  # Define the breaks for the legend
        col.regions = col_palette,  # Use the color palette
        na.color = "white") +  # Set NA values to white
  mapview(detroit_boundary,  # Add the Detroit boundary layer
          layer.name = "Detroit Boundary", 
          color = "red",  # Boundary color
          lwd = 2)  # Boundary line width

# Save as a shapefile
st_write(detroit_joined, "detroit_joined_data.shp")

# Save as a GeoJSON
st_write(detroit_joined, "detroit_joined_data.geojson")
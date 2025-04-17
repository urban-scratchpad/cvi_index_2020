## Count points within polygons


## load libraries
library(sf)
library(tidyverse)
library(mapview) # use at end to view counts by census tract

## Load the Overall Detroit Boundary shapefile

detroit_boundary <- st_read("data-raw/City_of_Detroit_Boundary.shp")

## Inspect the detroit boundary file
str(detroit_boundary)
head(detroit_boundary)
st_crs(detroit_boundary)


#### IMPORT 2020 Census Tracts Shapefile ####


tracts_2020 <- st_read("data-raw/CensusTracts2020.shp")

# View the file. Note the coordinate reference system (CRS), which will be referenced later.
# This will be next to the labels "Geodetic CRS" or "Projected CRS" 
# for this dataset, "Projected CRS: NAD_1983_StatePlane_Michigan_South_FIPS_2113_Feet"

tracts_2020


# View what the census tracts look like:
plot(st_geometry(tracts_2020))


#### IMPORT Improve Detroit Issues File ####
## uses readr library that was imported as a part of tidyverse ##
## Some data type problems were detected on first import due to a number zip code values - likely data entry errors
## also specified data types to read identifiers as characters and street numbers as character (though letters such as those for
## apartments likely have been removed from this column.)
## Another option for importing would be to skip the columns that are not needed.
## Note: reading in zip file to keep file sizes smaller

IDI <- read_csv("data-raw/Improve_Detroit_Issues.csv")

View(IDI)

#### View column names of imported dataset. 
## note that there are 2 variables related to longitude & latitude, the system added ones (X and Y) and the named ones
## ("latitude" and "longitude"). The named versions are from our Base Units geocoder and are the ones that should be used 
## for the next steps here.

colnames(IDI)

# learn number of rows & columns (FYI): 579298 x 24
dim(IDI)

## Filter rows to exclude items where there is no value in "latitude" or "longitude" 
#### Will not be able to convert these rows to an sf object based on lat, lon


IDI <- IDI %>%
  filter(!is.na(Latitude) | !is.na(Longitude))


## check to see how many rows filtered out
#### none filtered out

dim(IDI)

## Filter rows by date to include only those that occurred in 2022-2024
#### Default is to use 3 year time window 
#### "Created_At" is corresponding field to find this date in the original data.
#### first, convert to date with the ymd_hms() function from lubridate library
#### (this considers the format, 
#### "2005/02/01 00:00:00+00" 
#### in the dataset. 
#### Then use the between() function from dplyr to filter to the time range.
#### Note lubridate and dplyr were imported as part of the tidyverse library.

IDI <- IDI |>
  dplyr::mutate(Created_At_ymd = ymd_hms(Created_At)) |>
  filter(between(Created_At_ymd, as.Date('2022-01-01'), as.Date('2024-12-31')))


## The dataset is now reduced to 199106 rows and expanded to 25 columns, as seen by:

dim(IDI)

## Convert longitude and latitude points to an sf object

IDI_sf <- st_as_sf(IDI, coords = c("Longitude", "Latitude"), crs = 4326)


## Transform CRS for the ‘tracts_2020’ file to be the same as the crs for IDI_sf
####  (i.e., to "4326" from: "NAD_1983_StatePlane_Michigan_South_FIPS_2113_Feet").
#### Note: 4326 (i.e., EPSG code: 4326) is the default output for ESRI resources. This is also referred to as WGS84
#### in some places. 
#### Note: I saved the transformed dataset as a new dataset, adding "_t" to the end of the name


## Transform CRS for tracts
tracts_2020_t <- st_transform(tracts_2020, crs = 4326)

## Define request types to analyze
request_types <- c(
  "Clogged Drain",
  "Investigate Service Leak",
  "Investigate Water in Basement (Basement Backup)",
  "Investigate Water Main Break",
  "Vacant Service Leak",
  "Water in Basement Investigation",
  "Water Main Break"
)

## Process each request type and store results in a list
tract_counts_list <- lapply(request_types, function(req_type) {
  IDI_filtered <- IDI %>% filter(Request_Type_Title == req_type)
  
  IDI_sf_filtered <- st_as_sf(
    IDI_filtered,
    coords = c("Longitude", "Latitude"),
    crs = 4326
  )
  
  IDI_sf_tracts <- st_join(tracts_2020_t, IDI_sf_filtered, join = st_covers)
  
  tract_count <- count(as_tibble(IDI_sf_tracts), GEOID, name = "n") |>
    arrange(desc(n))
  
  left_join(tracts_2020_t, tract_count, by = "GEOID")
}) |> setNames(request_types)  # Name the list elements

## Create base map with boundary
base_map <- mapview(
  detroit_boundary,
  color = "red",        # Outline color
  lwd = 2,               # Line width
  alpha.regions = 0,     # Fully transparent fill
  layer.name = "Detroit Boundary"
)

## Add all water-issue layers
final_map <- Reduce(`+`, lapply(names(tract_counts_list), function(name) {
  mapview(
    tract_counts_list[[name]],
    zcol = "n",
    layer.name = name,
    alpha.regions = 0.6
  )
})) + base_map  # Overlay boundary on top

## Display the map
final_map

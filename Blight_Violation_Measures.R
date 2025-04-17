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


#### IMPORT Blight Violations Point File ####
## uses readr library that was imported as a part of tidyverse ##
## Some data type problems were detected on first import due to a number zip code values - likely data entry errors
## also specified data types to read identifiers as characters and street numbers as character (though letters such as those for
## apartments likely have been removed from this column.)
## Another option for importing would be to skip the columns that are not needed.
## Note: reading in zip file to keep file sizes smaller

blight_points <- read_csv("data-raw/Blight_Violations_20250228.zip", 
                          col_types = cols(ticket_id = col_character(), 
                                           violation_street_number = col_character(), 
                                           violation_zip_code = col_character(), 
                                           violator_id = col_integer(), zip_code = col_character(), 
                                           violation_date = col_character(), 
                                           hearing_time = col_character(), address_id = col_character()))

#### View column names of imported dataset. 
## note that there are 2 variables related to longitude & latitude, the system added ones (X and Y) and the named ones
## ("latitude" and "longitude"). The named versions are from our Base Units geocoder and are the ones that should be used 
## for the next steps here.

colnames(blight_points)

# learn number of rows & columns (FYI): 667902 x 46
dim(blight_points)

## Filter rows to exclude items where there is no value in "latitude" or "longitude" 
#### Will not be able to convert these rows to an sf object based on lat, lon


blight_points <- blight_points %>%
  filter(!is.na(latitude) | !is.na(longitude))


## check to see how many rows filtered out b/c there is no address_id
#### ~33-34,000 filtered, new dims are: 634787 rows, 46 columns

dim(blight_points)

## Filter rows by date to include only those that occurred in 2022-2024
#### Default is to use 3 year time window 
#### "violation_date" is corresponding field to find this date in the original data.
#### first, convert to date with the ymd_hms() function from lubridate library
#### (this considers the format, 
#### "2005/02/01 00:00:00+00" 
#### in the dataset. 
#### Then use the between() function from dplyr to filter to the time range.
#### Note lubridate and dplyr were imported as part of the tidyverse library.

blight_points <- blight_points |>
  dplyr::mutate(violation_date_ymd = ymd_hms(violation_date)) |>
  filter(between(violation_date_ymd, as.Date('2022-01-01'), as.Date('2024-12-31')))


## The dataset is now reduced to 112521 rows and expanded to 47 columns, as seen by:

dim(blight_points)

## Convert longitude and latitude points to an sf object
## Note that, as with many systems, longitude is identified before latitude.

blight_points_sf <- st_as_sf(blight_points, coords = c("longitude", "latitude"), crs = 4326)


## Transform CRS for the ‘tracts_2020’ file to be the same as the crs for blight_points_sf
####  (i.e., to "4326" from: "NAD_1983_StatePlane_Michigan_South_FIPS_2113_Feet").
#### Note: 4326 (i.e., EPSG code: 4326) is the default output for ESRI resources. This is also referred to as WGS84
#### in some places. 
#### Note: I saved the transformed dataset as a new dataset, adding "_t" to the end of the name


tracts_2020_t <- st_transform(tracts_2020, crs = 4326)


## Perform the spatial join between ‘blight_points_sf’ and ‘tracts_2020’ using st_join function.
## Note. Search ‘?st_join()’ for more information related to different ‘join=’ types.

blight_points_sf_tracts_2020 <- st_join(tracts_2020_t, blight_points_sf, join = st_covers)

##  Create a new dataset from the ‘blight_points_sf_tracts_2020’ by counting the number of points 
#### within the census tracts using the 'name' variable. The newly created variable will be named ‘n’.

tract_blight_tickets <- count(as_tibble(blight_points_sf_tracts_2020), GEOID) |>
  arrange(desc(n))


## Perform the a left join between the tract data with transformed geography and the 
#### counts of blight tickets per tract (tract_blight_tickets) file using left_join function.

tract_blight_count <- left_join(tracts_2020_t, tract_blight_tickets, by = c("GEOID" = "GEOID"))

## View counts by census tract to see distribution visually

mapview(tract_blight_count, zcol = "n", layer.name = "Blight Violations")

## Look up units for area

st_area(tracts_2020_t)

## Calculate the area of each census tract in square miles
tracts_2020_t$area_sq_mile <- as.numeric(st_area(tracts_2020_t)) / 27878400  # Convert square feet to square miles (1 square mile = 27878400 square feet)

## Join the area data with the blight violation counts
tract_blight_count <- left_join(tracts_2020_t, tract_blight_tickets, by = c("GEOID" = "GEOID"))

## View the counts and area by census tract to see distribution visually
mapview(tract_blight_count, zcol = "n", layer.name = "Blight Violations") +
  mapview(tract_blight_count, zcol = "area_sq_mile", layer.name = "Area (Square Miles)")


## write file.... to csv: need to decide how to connect across tracts. Use geoid? OR...
#### something easier to read?
### Note: only include needed fields.

#### 1. Write Census Tract Data to CSV ####

## Create a simplified dataset with only the necessary fields
tract_data_to_export <- tract_blight_count %>%
  st_drop_geometry() %>%  # Drop geometry column (not needed in CSV)
  select(GEOID, n, area_sq_mile)  # Select only the necessary fields

## Write the tract data to a CSV file
write_csv(tract_data_to_export, "tract_blight_data.csv")

#### 2. Write Blight Violation Data to CSV ####

## Select relevant columns from blight_points
blight_points_relevant <- blight_points %>%
  select(
    ticket_id, ticket_number, violator_name, violator_id,
    violation_street_number, violation_street_name, violation_address, violation_zip_code,
    violation_date, ticket_issued_time,
    judgment_amount, payment_status, balance_due,
    longitude, latitude, address_id,
    ordinance_description, disposition
  )

## Calculate violations per square mile
tract_blight_count$violations_per_sq_mile <- tract_blight_count$n / tract_blight_count$area_sq_mile

# Create the map:
# - Color tracts by raw count (n)
# - Show density (violations_per_sq_mile) on hover
# - Overlay Detroit boundary
mapview(tract_blight_count,
        zcol = "n",  # Color by raw count
        layer.name = "Total Blight Violations",
        alpha.regions = 0.6,
        col.regions = viridisLite::viridis,  # Color scale for counts
        label = paste(                       # Custom hover text
          "Tract: ", tract_blight_count$GEOID, "<br/>",
          "Violations (total): ", tract_blight_count$n, "<br/>",
          "Violations per sq mile: ", round(tract_blight_count$violations_per_sq_mile, 1)
        )) +
  mapview(detroit_boundary,  # Add the Detroit boundary layer
          layer.name = "Detroit Boundary", 
          color = "red",  # Boundary color
          lwd = 2)  # Boundary line width
## Update the CSV export to include this new metric
tract_data_to_export <- tract_blight_count %>%
  st_drop_geometry() %>%  # Drop geometry column
  select(GEOID, n, area_sq_mile, violations_per_sq_mile)  # Add new column

## Write the updated tract data to CSV
write_csv(tract_data_to_export, "tract_blight_data.csv")

## Write the blight violation data to a CSV file
write_csv(blight_points_relevant, "blight_violations_relevant.csv")

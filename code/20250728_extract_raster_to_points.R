##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:20250728
## purpose: extract values from raster with points

##### packages #####
library(sf)
library(terra)
library(tidyverse)

##### load raster #####
## Load the raster using terra function 'rast'
r <- rast('indata/Apr_2014_AShantharam.tif')

# Set spatial extent
# Remember: terra uses xmin, xmax, ymin, ymax
ext(r) <- c(-180, 180, -90, 90)

# Set CRS (Equidistant Cylindrical = Plate Carrée)
# This is a lat/lon system with linear spacing in degrees
crs(r) <- "EPSG:4326"  # WGS 84 is typically used for Plate Carrée

##### load points to dataframe #####
points <- read.csv('indata/site_points_AShantharam.csv')

##### create a 'SpatVector' from points using the terra function 'vect' #####
pts <- vect(points,
            geom = c("Longitude", "Latitude"),
            crs = "EPSG:4326")

##### extract raster values at point locations #####
vals <- terra::extract(r, pts)

##### join extracted values to the original points data frame #####
points_with_vals_joined <- points %>%
  mutate(ID = row_number()) %>%
  left_join(vals, by = "ID")

##### map it with ggplot2 #####
## Calculate bounding box around points (shifted longitudes)
x_range <- range(points_with_vals_joined$Lon_shifted, na.rm = TRUE)
y_range <- range(points_with_vals_joined$Latitude, na.rm = TRUE)

ggplot() +
  geom_raster(data = r_df, aes(x = x_shifted, y = y, fill = value)) +
  geom_point(data = points_with_vals_joined, aes(x = Lon_shifted, y = Latitude), color = "red", size = 2) +
  scale_fill_viridis_c(na.value = "transparent") +
  scale_x_continuous(
    limits = c(0, 360),  # Keep full range for breaks and labels
    breaks = seq(0, 360, 60),
    labels = function(x) ifelse(x > 180, x - 360, x)
  ) +
  coord_fixed(
    xlim = x_range,
    ylim = y_range,
    expand = FALSE
  ) +
  theme_minimal() +
  labs(fill = "Raster Value", x = "Longitude", y = "Latitude",
       title = "Zoomed Map to Points Extent")



##### map it with leaflet #####
library(leaflet)

# Fix column name before joining
colnames(vals)[2] <- "value"

points_with_vals_joined <- points %>%
  mutate(ID = row_number()) %>%
  left_join(vals, by = "ID")

# Leaflet interactive map
leaflet(data = points_with_vals_joined) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    radius = 5,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0("Raster Value: ", value)
  ) %>%
  addMiniMap(toggleDisplay = TRUE) %>%
  addScaleBar(position = "bottomleft")


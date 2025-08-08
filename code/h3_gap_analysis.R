##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20250731
## purpose:taking a look at the H3 gap analysis
## link: https://github.com/NOAA-OceanExploration/ExplorationGapAnalysis/
## link: https://github.com/thomasAmorrow/ExplorationGapAnalysis/
## link: https://zenodo.org/records/15490756
## dataversion: Published May 23, 2025 | Version 0.2.0 on Zenodo | https://zenodo.org/records/15490756


##### linkage #####
filename <- 'h3_gap_analysis' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.Rmd', sep = '')
# browseURL(github_link)

##### packages #####
library(tidyverse)
library(sf)
library(terra)
library(maptiles)      # To download basemap tiles
library(tidyterra)     # To plot the raster basemap with ggplot2
library(ggspatial)     # For map elements like north arrow and scale bar
library(h3jsr)

##### ***** Level 3 ***** #####
##### load the geojson hexes data to sf (h3 level3) #####
my_geojson_file <- "indata/h3_gap/h3_hexagons_03.geojson"
level3_sp <- st_read(my_geojson_file)

##### load the CSV data (h3 level3) #####
my_csv_file <- "indata/h3_gap/h3_scores_03.csv"
level3_tab <- read.csv(my_csv_file)

##### load the AOI#####
shapefile_path <- "C:/rworking/deepseatools/indata/20221104_protected_areas_update_HColeman/20221104_protected_areas.shp"
protected_areas <- st_read(shapefile_path)
aoi_ne_canyons <- protected_areas %>% filter(Sitename == 'NE Canyons and Seamounts')

##### harmonize the CRSs #####
aoi_ne_canyons_transform <- st_transform(aoi_ne_canyons, crs = crs(level3_sp))

##### filter by geography #####
level3_geofilter <- level3_sp %>% st_filter(aoi_ne_canyons_transform)

##### then filter on scores #####
hexes <- level3_geofilter %>%
  pull(h3_index) %>%
  unique()

##### get the corresponding filtered hexagons from the spatial layer #####
level3_sp_filtered <- level3_sp %>% filter(h3_index %in% hexes)

##### download the esri ocean basemap tiles #####
# This gets the tiles for the specific area covered by your filtered data
ocean_basemap <- get_tiles(level3_sp_filtered, provider = "Esri.OceanBasemap")

##### download the topo_basemap #####
topo_basemap <- get_tiles(level3_sp_filtered, provider = "Esri.WorldTopoMap")

##### create the map with ggplot2 #####
ggplot() +
  # 1. Add the basemap layer first
  geom_spatraster_rgb(data = ocean_basemap) +

  # 2. Add hexagons, colored by the 'combined' score
  geom_sf(
    data = level3_sp_filtered,
    aes(fill = combined),
    color = "black",
    alpha = 0.6
  ) +

  # Reverse the color scale
  scale_fill_viridis_c(direction = -1) +

  # Add the AOI outline on top 🗺️
  geom_sf(
    data = aoi_ne_canyons_transform,
    fill = NA,          # Make the polygon transparent
    color = "red",       # Set a distinct outline color
    linewidth = 1        # Set the outline thickness
  ) +

  # 3. Add map elements like scale bar and north arrow
  annotation_scale(location = "tr", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +

  # 4. Add titles and captions, including a legend title
  labs(
    title = "H3 Hexagons by Combined Score",
    subtitle = "Plotted on ESRI Ocean Basemap",
    x = "Longitude",
    y = "Latitude",
    fill = "Combined Score"
  ) +

  # 5. Use a clean theme
  theme_minimal() +

  # 6. Zoom to a specific area 🔍
  coord_sf(
    xlim = c(-70, -64.5), # <-- Replace with your min/max Longitude
    ylim = c(38, 42),   # <-- Replace with your min/max Latitude
    expand = FALSE
  )

##### create an interactive map with leaflet #####
# Load necessary libraries
library(leaflet)
library(sf)

# 1. Create a color palette function
# This function will map the 'combined' score to a color from the Viridis palette.
pal <- colorNumeric(
  palette = "viridis",
  domain = level3_sp_filtered$combined,
  reverse = TRUE # Set to TRUE to match your reversed ggplot scale
)

# 2. Create labels for the popups
# Using sprintf for clean HTML formatting in the popup text.
popup_labels <- sprintf(
  "<strong>Combined Score:</strong><br/>%.2f",
  level3_sp_filtered$combined
) %>% lapply(htmltools::HTML)


# 3. Build the interactive map
leaflet_map <- leaflet(data = level3_sp_filtered) %>%

  # Set the initial view (long, lat, zoom level)
  setView(lng = -67.0, lat = 38, zoom = 7) %>%

  # Add the high-resolution basemap layer
  addProviderTiles(
    providers$Esri.OceanBasemap,
    group = "Topographic"
  ) %>%

  # Add the hexagon layer
  addPolygons(
    fillColor = ~pal(combined), # Use the palette function for fill color
    fillOpacity = 0.6,
    color = "black",        # Outline color
    weight = 1,             # Outline width in pixels
    popup = popup_labels,   # Add the interactive popups
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%

  # Add the legend
  addLegend(
    pal = pal,
    values = ~combined,
    opacity = 0.7,
    title = "Combined Score",
    position = "bottomright"
  )

# Display the map
leaflet_map
##### check #####
st_bbox(level3_geofilter)

##### ***** Level 4 ***** #####
##### load the geojson data to sf (h3 level4) #####
my_geojson_file <- "indata/h3_gap/h3_hexagons_04.geojson"
level4_sp <- st_read(my_geojson_file)

##### load the CSV data (h3 level4) #####
my_csv_file <- "indata/h3_gap/h3_scores_04.csv"
level4_tab <- read.csv(my_csv_file)

##### load the AOI#####
shapefile_path <- "C:/rworking/deepseatools/indata/20221104_protected_areas_update_HColeman/20221104_protected_areas.shp"
protected_areas <- st_read(shapefile_path)
aoi_ne_canyons <- protected_areas %>% filter(Sitename == 'NE Canyons and Seamounts')

##### harmonize the CRSs #####
aoi_ne_canyons_transform <- st_transform(aoi_ne_canyons, crs = crs(level4_sp))

##### filter by geography #####
level4_geofilter <- level4_sp %>% st_filter(aoi_ne_canyons_transform)

##### then filter on scores #####
hexes <- level4_geofilter %>%
  pull(h3_index) %>%
  unique()

##### get the corresponding filtered hexagons from the spatial layer #####
level4_sp_filtered <- level4_sp %>% filter(h3_index %in% hexes)

##### download the esri ocean basemap tiles #####
# This gets the tiles for the specific area covered by your filtered data
ocean_basemap <- get_tiles(level4_sp_filtered, provider = "Esri.OceanBasemap")

##### download the topo_basemap #####
topo_basemap <- get_tiles(level4_sp_filtered, provider = "Esri.WorldTopoMap")

##### create the map with ggplot2 #####
ggplot() +
  # 1. Add the basemap layer first
  geom_spatraster_rgb(data = ocean_basemap) +

  # 2. Add hexagons, colored by the 'combined' score
  geom_sf(
    data = level4_sp_filtered,
    aes(fill = combined),
    color = "black",
    alpha = 0.6
  ) +

  # Reverse the color scale
  scale_fill_viridis_c(direction = -1) +

  # Add the AOI outline on top 🗺️
  geom_sf(
    data = aoi_ne_canyons_transform,
    fill = NA,          # Make the polygon transparent
    color = "red",       # Set a distinct outline color
    linewidth = 1        # Set the outline thickness
  ) +

  # 3. Add map elements like scale bar and north arrow
  annotation_scale(location = "tr", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +

  # 4. Add titles and captions, including a legend title
  labs(
    title = "H3 Hexagons by Combined Score (Level 4)",
    subtitle = "Plotted on ESRI Ocean Basemap",
    x = "Longitude",
    y = "Latitude",
    fill = "Combined Score"
  ) +

  # 5. Use a clean theme
  theme_minimal() +

  # 6. Zoom to a specific area 🔍
  coord_sf(
    xlim = c(-70, -64.5), # <-- Replace with your min/max Longitude
    ylim = c(38, 42),   # <-- Replace with your min/max Latitude
    expand = FALSE
  )

##### check #####
st_bbox(level3_geofilter)


##### ***** experiment: This is how it really should be done ***** #####
##### load the CSV data (h3 level5) #####
my_csv_file <- "indata/h3_gap/h3_scores_05.csv"
level5_tab <- read.csv(my_csv_file)

##### get all level 5 children #####
h3_indexes_lvl5 <- unlist(lapply(hexes, function(idx) {
  get_children(idx, res = 5)
}))

h3_sf_polygons <- cell_to_polygon(h3_indexes_lvl5, simple = FALSE)

level5_tab <- rename(level5_tab, h3_address = h3_index)
yo <- left_join(h3_sf_polygons, level5_tab)

##### map #####
ggplot() +
  # 1. Add the basemap layer first
  geom_spatraster_rgb(data = ocean_basemap) +

  # 2. Add hexagons, colored by the 'combined' score
  geom_sf(
    data = yo,
    aes(fill = combined),
    color = "black",
    alpha = 0.6
  ) +

  # Reverse the color scale
  scale_fill_viridis_c(direction = -1) +

  # Add the AOI outline on top 🗺️
  geom_sf(
    data = aoi_ne_canyons_transform,
    fill = NA,          # Make the polygon transparent
    color = "red",       # Set a distinct outline color
    linewidth = 1        # Set the outline thickness
  ) +

  # 3. Add map elements like scale bar and north arrow
  annotation_scale(location = "tr", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +

  # 4. Add titles and captions, including a legend title
  labs(
    title = "H3 Hexagons by Combined Score (Level 5)",
    subtitle = "Plotted on ESRI Ocean Basemap",
    x = "Longitude",
    y = "Latitude",
    fill = "Combined Score"
  ) +

  # 5. Use a clean theme
  theme_minimal() +

  # 6. Zoom to a specific area 🔍
  coord_sf(
    xlim = c(-70, -64.5), # <-- Replace with your min/max Longitude
    ylim = c(38, 42),   # <-- Replace with your min/max Latitude
    expand = FALSE
  )






##### create an interactive map with leaflet #####
# Load necessary libraries
library(leaflet)
library(sf)

# 1. Create a color palette function
# This function will map the 'combined' score to a color from the Viridis palette.
pal <- colorNumeric(
  palette = "viridis",
  domain = yo$combined,
  reverse = TRUE # Set to TRUE to match your reversed ggplot scale
)

# 2. Create labels for the popups
# Using sprintf for clean HTML formatting in the popup text.
popup_labels <- sprintf(
  "<strong>Combined Score:</strong><br/>%.2f",
  level3_sp_filtered$combined
) %>% lapply(htmltools::HTML)


# 3. Build the interactive map
leaflet_map <- leaflet(data = yo) %>%

  # Set the initial view (long, lat, zoom level)
  setView(lng = -67.0, lat = 38, zoom = 7) %>%

  # Add the high-resolution basemap layer
  addProviderTiles(
    providers$Esri.OceanBasemap,
    group = "Topographic"
  ) %>%

  # Add the hexagon layer
  addPolygons(
    fillColor = ~pal(combined), # Use the palette function for fill color
    fillOpacity = 0.1,
    color = "black",        # Outline color
    weight = 1,             # Outline width in pixels
    popup = popup_labels,   # Add the interactive popups
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%

  # Add the legend
  addLegend(
    pal = pal,
    values = ~combined,
    opacity = 0.7,
    title = "Combined Score",
    position = "bottomright"
  )

# Display the map
leaflet_map

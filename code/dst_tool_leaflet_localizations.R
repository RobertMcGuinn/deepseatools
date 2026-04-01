# Install required packages if needed:
# install.packages(c("leaflet", "htmltools", "glue"))

library(leaflet)
library(htmltools)
library(glue)
library(rerddap)

##### source ndb #####
source("c:/rworking/deepseatools/code/dst_tool_load_current_ndb.R")

##### make the customized function to draw boxes #####
draw_dynamic_box <- function(img_url, x, y, w, h) {
  # Convert HTML to character so Leaflet can render it in the popup
  as.character(HTML(glue(
    "
    <div style='position: relative; display: inline-block;'>
      <img src='{img_url}' style='max-width: 300px; height: auto;' />

      <div style='
        position: absolute;
        left: {x * 100}%;
        top: {y * 100}%;
        width: {w * 100}%;
        height: {h * 100}%;
        border: 3px solid #00FF00; /* Neon green box */
        box-sizing: border-box;
        pointer-events: none; /* Lets users click through the box if needed */
      '></div>
    </div>
    <br><b>Organism Detected!</b>
    "
  )))
}

##### create a query result #####
z <- filt %>% filter()

##### query ERDDAP for list of CatalogNumbers #####
## get the list of CatalogNumbers from the query into an variable
catlist <- z$CatalogNumber

# Your list of catalog numbers from your original script
catlist_q <- paste0("(", paste(catlist, collapse = "|"), ")")

# Fetch the data
z_data <- tabledap(
  datasetid = "deep_sea_corals",
  url = ncei_url,
  # This is the filter: CatalogNumber matches your regex list
  `CatalogNumber=~` = catlist_q,
  # List the columns you want, or leave blank for all
  fields = c("ScientificName", "CatalogNumber", "latitude", "longitude", "ObservationDate")
)

# Convert to a standard data frame/tibble
z_df <- as_tibble(z_data) %>%
  mutate(across(c(latitude, longitude), as.numeric)) # Ensure coords are numbers

# 3. Apply the HTML function to every row in your dataframe
erddap_data$popup_content <- mapply(
  FUN = draw_dynamic_box,
  img_url = erddap_data$image_url,
  x = erddap_data$bbox_x,
  y = erddap_data$bbox_y,
  w = erddap_data$bbox_w,
  h = erddap_data$bbox_h,
  SIMPLIFY = TRUE
)

# 4. Build the Leaflet Map
leaflet(data = erddap_data) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>% # Good basemap for marine data
  addCircleMarkers(
    ~longitude, ~latitude,
    radius = 6,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~popup_content,  # Pass the generated HTML here!
    label = ~ScientificName  # Hover text
  )

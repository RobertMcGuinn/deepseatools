##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20251216
## purpose: FishCouncilRegion summary for Heather Coleman.

##### parameters: manual input #####
## add filename, should be the same name as this file
filename <- 'dst_analysis_FishCouncilRegion_20251216'

##### linkage #####
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
# browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- issuenumber <- sub(".*_(.*)$", "\\1", filename)
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(ggspatial)
library(tidyverse)

##### load database #####
# source('code/load_current_ndb.r')

##### table of records added #####
## FishCouncilRegion within a specified period

# sumtable <- filt %>%
#   filter(as.Date(EntryDate) > as.Date('2021-10-01') &
#            as.Date(EntryDate) < as.Date('2025-10-01')) %>%
#   pull(FishCouncilRegion) %>% table(useNA = 'always')
#
# print(sumtable)

##### check #####
  # filt %>% filter(as.Date(EntryDate) > as.Date('2021-10-01') &
  #          as.Date(EntryDate) < as.Date('2025-10-01')) %>%
  #   pull(EntryDate) %>% table(useNA = 'always')

##### cleanup #####
y <- setdiff(ls(), "filt")
rm(y)

##### static map generation ####
# 1. Prepare Data and Remove NAs
map_data <- filt %>%
  mutate(
    EntryDate = as.Date(EntryDate),
    period = ifelse(EntryDate >= as.Date("2021-10-01"), "Added between 2022-25", "Added before 2022")
  ) %>%
  # Filter out any rows where period ended up as NA
  filter(!is.na(period)) %>%
  select(all_of(c("Longitude", "Latitude", "period"))) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# 2. Get Basemap
countries <- ne_countries(scale = "medium", returnclass = "sf")

# 3. Define Map Bounds (with 3-degree padding)
bbox <- st_bbox(map_data)
xlims <- c(bbox["xmin"] - 3, bbox["xmax"] + 3)
ylims <- c(bbox["ymin"] - 3, bbox["ymax"] + 3)

# 4. Build the Map
map_plot <- ggplot() +
  # Basemap
  geom_sf(data = countries, fill = "grey95", color = "grey70", linewidth = 0.2) +

  # Data Points (Layered with specific period colors)
  geom_sf(data = map_data, aes(color = period), size = 1.5, alpha = 0.6) +

  # Coordinate System & Graticule Labels
  coord_sf(
    xlim = xlims,
    ylim = ylims,
    datum = st_crs(4326),
    expand = FALSE
  ) +

  # North Arrow
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.5, "cm"),
    style = north_arrow_fancy_orienteering()
  ) +

  # Visual Styling & Legend Cleanup
  scale_color_manual(
    values = c("Added before 2022" = "#1f78b4", "Added between 2022-25" = "#e31a1c"),
    na.translate = FALSE # Explicitly prevents NA from appearing in legend
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 9, color = "black"),
    panel.grid.major = element_line(color = "grey85", linetype = "dotted", linewidth = 0.5),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# 5. Export
ggsave(
  filename = "images/20251222-2_FY2022_and_beyond_map.png",
  plot = map_plot,
  width = 10,
  height = 7,
  dpi = 300
)

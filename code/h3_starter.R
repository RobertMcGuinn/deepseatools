##### header #####
## author:Robert McGuinn | robert.mcguinn@noaa.gov | rpm@lumni.duke.edu
## start_date: 20230922
## forked_from: https://cran.r-project.org/web/packages/h3jsr/vignettes/intro-to-h3jsr.html
## purpose: experimentation with 'h3jsr'package

##### packages #####
local_options <- options()
library(sf)
library(tidyverse)
library(ggplot2)
library(h3jsr)
options(stringsAsFactors = FALSE)


##### working #####
# This is the location of the Brisbane Town Hall:
bth <- sf::st_sfc(sf::st_point(c(153.023503, -27.468920)), crs = 4326)

# where is the Brisbane Town Hall at resolution 15?
point_to_cell(bth, res = 15)
#> [1] "8fbe8d12acad2f3"

nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
nc_pts <- st_centroid(nc)
nc_pts <- st_transform(nc_pts, crs = 4326)
nc_pts <- dplyr::select(nc_pts, CNTY_ID, NAME)

# Give me the address for the center of each NC county at every resolution
nc_all_res <- point_to_cell(nc_pts,
                            res = seq(0, 15),
                            simple = FALSE)
head(nc_all_res[, c(1:5)])

#>   CNTY_ID        NAME h3_resolution_0 h3_resolution_1 h3_resolution_2
#> 1    1825        Ashe 802bfffffffffff 812abffffffffff 8244dffffffffff
#> 2    1827   Alleghany 802bfffffffffff 812abffffffffff 8244dffffffffff
#> 3    1828       Surry 802bfffffffffff 812abffffffffff 822a8ffffffffff
#> 4    1831   Currituck 802bfffffffffff 812afffffffffff 822af7fffffffff
#> 5    1832 Northampton 802bfffffffffff 812afffffffffff 822af7fffffffff
#> 6    1833    Hertford 802bfffffffffff 812afffffffffff 822af7fffffffff


# plot a few
ashe_hexes <- unlist(nc_all_res[1, c(6,7,8,9,10)], use.names = FALSE)
ashe_hexes <- cell_to_polygon(ashe_hexes, simple = FALSE)
ggplot(nc[1,]) +
  geom_sf(fill = NA, colour = 'black') +
  geom_sf(data = ashe_hexes, aes(fill = h3_address), alpha = 0.5) +
  scale_fill_viridis_d() +
  ggtitle('H3 hexagons over County Ashe, NC', subtitle = 'Resolutions 6-10') +
  theme_minimal() +
  coord_sf()

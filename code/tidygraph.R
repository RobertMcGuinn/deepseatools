##### Header #####
# author: Robert McGuinn
# started on: 20191213
# original code from: Lucas van der Meer, Robin Lovelace & Lorena Abad September 26, 2019
# link: https://www.r-spatial.org/r/2019/09/26/spatial-networks.html

##### install packages #####
# We'll use remotes to install packages, install it if needs be:

library(remotes)
library(sf)
library(tidygraph)
library(igraph)
# install.packages("osmdata")
library(osmdata)
library(dplyr)
library(tibble)
library(ggplot2)
library(units)
# install.packages("tmap")
library(tmap)
# install.packages("rgrass7")
library(rgrass7)
# install.packages("link2GI")
library(link2GI)
# install.packages('nabor')
library(nabor)

##### get data #####
muenster <- opq(bbox =  c(7.61, 51.954, 7.636, 51.968)) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf() %>%
  osm_poly2line()

muenster_center <- muenster$osm_lines %>%
  select(highway)

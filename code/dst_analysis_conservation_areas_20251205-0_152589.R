##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate: 20251205
## purpose: for Heather Coleman, product tabular outputs from GIS data for her review.  Conservation areas.

##### linkage #####
filename <- 'dst_analysis_conservation_areas_20251205-0_152589' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- issuenumber <- sub(".*_(.*)$", "\\1", filename)
redmine_link <- paste(redmine_path, issuenumber, sep = '')
# browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(googlesheets4)
library(robis)

library(sf)

##### load geodatabase layer #####
## path to the geodatabase
gdb_path <- "indata/US_FisheryManagementConservationAreas.gdb"

## list the layers in the GDB
st_layers(gdb_path) %>% View()

## read a specific layer (replace "layer_name" with an actual layer)
data <- st_read(gdb_path, layer = "US_FisheryManagementConservationAreas")

##### drop geometry for tabular output #####

## drop geometry
data_no_geom <- data %>% st_drop_geometry()

## write CSV
write.csv(data_no_geom,
          "indata/US_FisheryManagementConservationAreas.csv")



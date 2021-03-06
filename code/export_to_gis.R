##### header #####
# author: Robert P. McGuinn
# date_started: 20191121
# purpose: transform point frame to spdf and feature-class
# input: x (dataframe) with coordinates
# output: x_geo (spdf) | x_geo (feature-class)

##### load packages #####
library(tidyverse)
library(rgdal)
library(arcgisbinding)
arc.check_product()

##### __OR__ import the data frame from disk #####

x <- "20201001-4_NOAA_OER_EX1811_Oceano_Profundo_2018_2018"
setwd("C:/rworking/deepseatools/indata")
x <- read.csv(paste(x,'.csv', sep = ''), header = T)

##### __OR__ create x from sub #####

x <- sub


##### modify Lats/Longs in some way #####

x <- x %>%  mutate(Latitude = Latitude-1.169330553,
            Longitude = Longitude+2.110869447

)

##### filter data #####

# get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 | Longitude != -999)
# make copy to turn into spatial points data frame.
x_geo <- x

##### create spatial points data fram #####

coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### create feature-class #####

fgdb_path <- 'C:/rworking/sf/sf.gdb'
arc.write(file.path(fgdb_path, 'x_geo'), data=x_geo, overwrite = TRUE)


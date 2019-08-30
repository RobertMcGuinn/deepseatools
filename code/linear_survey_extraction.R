##### header ##### 
# Title: Extracting linear surveys where available, from the National Database for Deep Sea Corals and Sponges
# Author: Robert P. McGuinn robert.mcguinn@noaa.gov; rpm@alumni.duke.edu
# Original Creation Date: 2018-07-12
##### installation/loading of required packages ##### 
#install.packages("pacman")
# library(pacman)
# #pacman::p_load(captioner, bundesligR)
# library(captioner, bundesligR)
# #install.packages("beanplot")
# library(beanplot)
# #install.packages("stringr")
# library(stringr)
# #install.packages("knitr")
library(knitr)
# #install.packages("tidyr")
# library(tidyr)
# #install.packages("sp")
# library(sp)
# #install.packages("maptools")
# library(maptools)
# #install.packages("maps")
# library(maps)
# #install.packages("reshape")
# library(reshape)
# #install.packages("reshape2")
# library(reshape2)
# #install.packages("psych")
# library(psych)
# #install.packages("ggplot2")
library(ggplot2)
# #install.packages("data.table")
# library(data.table)
# #install.packages("dplyr")
library(dplyr)
# #install.packages("car")
# library(car)
# #install.packages("gdata")
# library(gdata)
# #install.packages("digest")
# library(digest)
# #install.packages("rgdal")
# library(rgdal)
# #install.packages("ggmap")
# library(ggmap)
# #install.packages("rerddap")
# library(rerddap)
# #install.packages("raster")
# library(raster)
# #install.packages("rworldxtra")
# library(rworldxtra)
# #install.packages("ggrepel")
# library(ggrepel)
# #install.packages("xtable")
# library(xtable)
# library(taxize)
library(rgdal)
# library(dplyr)
# #install.packages("tidyverse")
# library(tidyverse)
# #install.packages("extrafont")
library(leaflet)
library(extrafont)
library(RColorBrewer)
# library(googlesheets)
# library(googledrive)
library(sp)

##### setting input data #####
setwd("C:/rworking/digs/indata")
indata<-read.csv("DSCRTP_NatDB_20180718-0.csv", header = T)

##### geographic and attribute filtering #####
x <- indata %>% 
  filter(
    Flag == "0", 
    Latitude > 25 , 
    Latitude < 30, 
    Longitude > -100, 
    Longitude < -90)

##### filter for data that has start and end lats and longitudes (not all data will) ##### 
d <- x %>%
  filter(
    StartLatitude != "-999", 
    StartLongitude != "-999", 
    EndLatitude != "-999", 
    EndLongitude != "-999")

# check number of records
length(d$CatalogNumber)

##### create a SpatialPointsDataFrame #####
d2 <- d #%>%
#filter(StartLatitude != "-999")
coordinates(d2) <- c("Longitude", "Latitude")
proj4string(d2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### creating spatial lines #####
begin.coord <- data.frame(lon=d2$StartLongitude, lat=d2$StartLatitude)
end.coord <- data.frame(lon=d2$EndLongitude, lat=d2$EndLatitude)

l <- vector("list", nrow(begin.coord))
library(sp)
for (i in seq_along(l)) {
  l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}
lines<-SpatialLines(l)
class(lines)

##### turn the lines file into a SpatialLinesDataFrame for use with the writeOGR function below #####
lines2 <- SpatialLinesDataFrame(lines, d2@data)
class(lines2)
names(lines2)

##### adding a line length variable ##### 
lines2$length <- SpatialLinesLengths(lines, longlat = T)
proj4string(lines2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
lines2
names(lines2)

##### writing out a shapefile of the lines ##### 
setwd("C:/rworking/digs/outdata")
writeOGR(lines2, dsn="lines", 
         layer= "lines", 
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"), 
         overwrite_layer = T)

##### getting a summary of the line lengths #####
summary(lines2$length)
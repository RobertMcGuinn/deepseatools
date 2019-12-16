##### header #####
# Title: Extracting linear surveys where available, from the National Database for Deep Sea Corals and Sponges
# Author: Robert P. McGuinn robert.mcguinn@noaa.gov; rpm@alumni.duke.edu
# Original Creation Date: 2018-07-12
##### installation/loading of required packages #####
library(knitr)
library(ggplot2)
library(dplyr)
library(rgdal)
library(leaflet)
library(extrafont)
library(RColorBrewer)
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

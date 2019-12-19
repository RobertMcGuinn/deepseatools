##### Header #####
# author: Robert P. McGuinn, rpm@alumni.duke.edu, robert.mcguinn@noaa.gov
# date started: 20160105
# purpose: extract elevation data from ETOPO and CRM web services.

##### load packages #####

library(sp)
library(tidyverse)
library(raster)

##### load data (piece of NDB) from CSV #####

# data must be in NDB format
setwd("C:/rworking/deepseatools/indata")
indata<-read.csv("DSCRTP_NatDB_20190920-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

##### setting bounding box coordinates #####
minLon <- -85
maxLon <- -82
minLat <- 23
maxLat <- 26

##### get ETOPO1 data from NCEI #####

url <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/",
             "wcs.groovy?filename=etopo1_bedrock.tif&",
             "request=getcoverage&version=1.0.0&service=wcs&",
             "coverage=etopo1_bedrock&CRS=EPSG:4326&format=geotiff&",
             "resx=0.000833333333333334&resy=0.000833333333333334&bbox=",
             minLon, ",", minLat, ",", maxLon, ",", maxLat, sep="")
fname <- "etopo_test.tif"
download.file(url, fname, mode="wb", cacheOK="false")
etopo <- raster(fname)

##### get CRM data from NCEI #####

url.hi <- paste("http://maps.ngdc.noaa.gov/mapviewer-support/wcs-proxy/",
                "wcs.groovy?filename=crm.tif&",
                "request=getcoverage&version=1.0.0&service=wcs&",
                "coverage=crm&CRS=EPSG:4326&format=geotiff&",
                "resx=0.000833333333333334&resy=0.000833333333333334&bbox=",
                minLon, ",", minLat, ",", maxLon, ",", maxLat, sep="")
fname.hi <- "crm_test.tif"

download.file(url.hi, fname.hi, mode="wb", cacheOK="false")

crm <- raster::raster(fname.hi)

##### optional: write the raster for GIS usage #####

# setwd("C:/rworking/digs/outdata")
# writeRaster(crm,'crm.tif')

##### filtering the coral data to match elevation data extraction #####
filt <- filter(indata, as.numeric(Latitude) > minLat,
               as.numeric(Latitude) < maxLat,
               as.numeric(Longitude) < maxLon,
               as.numeric(Longitude) > minLon,
               Flag == "0")
#View(filt)
coordinates(filt) <- c("Longitude","Latitude")
proj4string(filt) <- proj4string(etopo)

##### extract raster CRM and ETOPO data to points #####
filt$gisCRM <- raster::extract(crm,filt)
filt$gisETOPO <- raster::extract(etopo,filt)

##### changing the sign of the depth values to match NDB formatting ####

filt$gisCRM <- filt$gisCRM * -1
filt$gisETOPO <- filt$gisETOPO * -1

# names(filt)
filtdata <- as.data.frame(filt)
#names(filtdata)

##### plotting in ggplot #####

p <- ggplot(filtdata, aes(DepthInMeters,gisETOPO))
p <- p + geom_point(size = .7) +
  geom_vline(aes(xintercept = 50)) +
  geom_hline(aes(yintercept = 50)) +
  geom_abline(col = "gray60")

p




##### Header #####
# author: Robert P. McGuinn, rpm@alumni.duke.edu, robert.mcguinn@noaa.gov
# date started: 20160105
# purpose: extract elevation data from ETOPO and CRM web services.

##### load packages #####

library(ncdf4)
library(sp)
library(tidyverse)
library(raster)
library(leaflet)

##### load data (piece of NDB) from CSV #####

# data must be in NDB format
setwd("C:/rworking/deepseatools/indata")
indata<-read.csv("DSCRTP_NatDB_20190920-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0")

##### load GEBCO 2019 bathymetry from local netcdf file #####

# https://www.gebco.net/data_and_products/gridded_bathymetry_data/
# code: https://www.benjaminbell.co.uk/2019/08/bathymetric-maps-in-r-getting-and.html

# Load GEBCO_2019
# this is just a test piece exracted from the gebco data extract tool.
gebco2019 <- raster("C:/data/BaseLayers/GEBCO2019/gebco_2019.nc")

##### setting bounding box coordinates#####

# do it by hand -OR-
minLon <- -85
maxLon <- -82
minLat <- 23
maxLat <- 26

# -OR- set bounding box coordinates to match the GEBCO extraction that you have.

x <- bbox(gebco2019)
minLat <- x[2,1]
maxLat <- x[2,2]
maxLon <- x[1,2]
minLon <- x[1,1]

##### filtering the coral data to match elevation data extraction#####

filt <- filter(indata, as.numeric(Latitude) > minLat,
               as.numeric(Latitude) < maxLat,
               as.numeric(Longitude) < maxLon,
               as.numeric(Longitude) > minLon,
               Flag == "0", ScientificName == "Lophelia pertusa")

#View(filt)

coordinates(filt) <- c("Longitude","Latitude")
proj4string(filt) <- proj4string(gebco2019)

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

# setwd("C:/rworking/deepseatools/outdata")
# writeRaster(crm,'crm.tif')

##### extract raster CRM and ETOPO data to points #####
# note: point extent must match raster extent (see above section on
# setting max and min lats and longs.

filt$gisCRMDepth <- raster::extract(crm,filt)
filt$gisEtopoDepth <- raster::extract(etopo,filt)
filt$gisGEBCO2019 <- raster::extract(gebco2019, filt)

##### changing the sign of the depth values to match NDB formatting ####

filt$gisCRMDepth <- filt$gisCRMDepth * -1
filt$gisEtopoDepth <- filt$gisEtopoDepth * -1
filt$gisGEBCO2019 <- filt$gisGEBCO2019 * -1

##### setting as data
filtdata <- as.data.frame(filt)
# names(filtdata)

##### plotting in ggplot #####

p <- ggplot(filtdata, aes(DepthInMeters, gisGEBCO2019))
p <- p + geom_point(size = .7) +
  geom_vline(aes(xintercept = 50), col = 'pink') +
  geom_hline(aes(yintercept = 50), col = 'pink') +
  geom_abline(col = "gray60")

p


##### map it in leaflet #####
palRaster <- colorNumeric("Spectral", domain = gebco2019@data@values, na.color = "transparent")

leaflet() %>% addProviderTiles("Esri.OceanBasemap") %>%
  addRasterImage(gebco2019,
                 colors = palRaster,
                 opacity = .5) %>%
  addCircles(lng = filt$Longitude, lat = filt$Latitude,
             radius = .5, opacity = .2, col = "blue",
             popup = paste("CatalogNumber:", filt$CatalogNumber, "<br>",
                           "ScientificName:", filt$ScientificName, "<br>",
                           "DepthInMeters:", filt$DepthInMeters, "<br>",
                           "ImageURL:", filt$ImageURL, "<br>",
                           "DatasetID:", filt$DatasetID, "<br>",
                           "Vessel:", filt$Vessel, "<br>",
                           "SurveyID:", filt$SurveyID, "<br>",
                           "SampleID:", filt$SampleID, "<br>",
                           "TrackingID:", filt$TrackingID, "<br>",
                           "Station:", filt$Station, "<br>",
                           "Observation Year:", filt$ObservationYear))








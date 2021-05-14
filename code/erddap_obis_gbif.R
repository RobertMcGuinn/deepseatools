##### Robert McGuinn #####
# Author: Robert McGuinn, rpm@alumni.duke.edu, robert.mcguinn@noaa.gov, 843-762-8640
# Start date: 20170222

##### installation/Loading of Packages #####
library(stringr)
library(knitr)
library(tidyr)
library(reshape2)
library(rerddap)
library(sp)
library(sf)
library(raster)
# install.packages("robis")
library(robis)
library(leaflet)
# install.packages("rgbif")
library(rgbif)

##### setting working directories to intake #####
#setwd("C:/rworking/deep-sea-workbench/InData/IntakePipe")
setwd("C:/rworking/digs/indata")

##### bringing in National Database from a local copy #####
#from csv
indata<-read.csv("DSCRTP_NatDB_20170215-0.csv", header = T)

# list all datasets on server
x <- head(ed_datasets('table', url = "https://ecowatch.ncddc.noaa.gov/erddap/"))
fix(x)

x <- head(ed_datasets('grid', url = "https://ecowatch.ncddc.noaa.gov/erddap/"))
fix(x)

# Get info on a datasetid, then get data given information learned
info('deep_sea_corals', url = "https://ecowatch.ncddc.noaa.gov/erddap/")$variables

x <- tabledap('deep_sea_corals',
              fields=c('latitude','longitude','ScientificName', "ImageURL"),
              url = "https://ecowatch.ncddc.noaa.gov/erddap/")

x <- x[is.na(x$ImageURL) == F,]

View(x)

##### get occurrences from OBIS, GBIF and the NDB and map them #####
# set the species of interest
spec <- "Lophelia pertusa"

#get data from OBIS
obis <- occurrence(spec)
obis <- obis %>%
  filter(grepl(spec, scientificName))#,collectionCode != "NOAA_DSC_RTP") #collectionCode != "NOAA_DSC_RTP
obis <- obis[, c('scientificName', 'decimalLongitude', 'decimalLatitude', 'datasetName')]
names(obis)[2:3] <- c('longitude', 'latitude')
View(obis)

# get the occurrences from the national database
#filter ndb
ndb <- indata %>%
  filter(grepl(spec, ScientificName), Flag == "0")
ndb <- ndb %>%
  filter(grepl(spec, ScientificName))
ndb <- ndb[, c('ScientificName', 'Longitude', 'Latitude', 'ImageURL')]
names(ndb)[2:3] <- c('longitude','latitude')
# View the results
View(ndb)

# get data from GBIF
key <- name_backbone(name=spec)$speciesKey
dat <- occ_search(taxonKey=key, return='data')
dat <- data.frame(dat)
dat <- dat %>%
  filter(grepl(spec, name))#, ownerInstitutionCode != "DSC_RTP")
gbif <- dat[, c('name', 'decimalLongitude', 'decimalLatitude', "ownerInstitutionCode")]
names(gbif)[2:3] <- c('longitude', 'latitude')
View(gbif)

##### create an interactive map using leaflet #####
m <- leaflet()
m <- addProviderTiles(m, "CartoDB.DarkMatter") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=obis,
                      radius=8,
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = obis$datasetName
)
m <- addCircleMarkers(m, data=ndb,
                      radius=6,
                      weight=0,
                      fillColor= "black",
                      fillOpacity=1
                      #popup = zz$ScientificName
)
m <- addCircleMarkers(m, data=gbif,
                      radius=5,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1,
                      popup = gbif$ownerInstitutionCode
)

# draw map
m


##### _____ Working with the Coastal Relief Model and ETOPO####
##### setting bounding box coordinates #####
minLon <- -87
maxLon <- -83
minLat <- 26
maxLat <- 30
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

crm <- raster(fname.hi)

##### filtering the coral data to match elevation data extraction #####
filt <- filter(indata, as.numeric(Latitude) > minLat,
               as.numeric(Latitude) < maxLat,
               as.numeric(Longitude) < maxLon,
               as.numeric(Longitude) > minLon,
               Flag == "0")
#View(filt)
coordinates(filt) <- c("Longitude","Latitude")
proj4string(filt) <- proj4string(crm)

# country boundaries
library(rworldxtra)
data(countriesHigh)

##### extract CRM and ETOPO data to points #####
filt$gisCRM <- extract(crm,filt)
filt$gisETOPO <- extract(etopo,filt)

# changing the sign of the depth to match schema directions
filt$gisCRM <- filt$gisCRM * -1
filt$gisETOPO <- filt$gisETOPO * -1

names(filt)
filtdata <- as.data.frame(filt)
names(filtdata)

p <- ggplot(filtdata, aes(DepthInMeters,gisCRM))
p + geom_point(size = .7) +
  geom_vline(aes(xintercept = 50)) +
  geom_hline(aes(yintercept = 50)) +
  geom_abline(col = "gray60")

##### make some maps of this stuff #####
bplt <- spplot(etopo, col.regions=topo.colors(64),
               colorkey=FALSE, scales=list(draw=TRUE),
               xlim = c(minLon, maxLon), ylim=c(minLat, maxLat),
               sp.layout=list(
                 # add contours:
                 list("sp.lines", rasterToContour(etopo,
                                                  levels=c(-1000, -100, -50, 0, 1000))),
                 list("sp.polygons", countriesHigh, lwd=2, fill="grey"),
                 # add trawl locations coded by cruise & gear:
                 list("sp.points", filt,
                      lwd=2, cex=2, col="black"
                 )
               )
)
plot(bplt)

# black and white version
bplt <- spplot(crm, col.regions=NA, colorkey=FALSE,
               scales=list(draw=TRUE),
               xlim = c(minLon, maxLon), ylim=c(minLat, maxLat),
               sp.layout=list(
                 # add contours:
                 list("sp.lines",
                      rasterToContour(crm, levels=c(-1000, -100, -50, 0, 1000))),
                 list("sp.polygons", countriesHigh, lwd=2, fill="grey"),
                 # add trawl locations coded by cruise & gear:
                 list("sp.points", filt,
                      lwd=2, cex=2, col="black")))
plot(bplt)

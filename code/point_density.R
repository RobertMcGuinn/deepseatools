##### Header #####
# created by Robert McGuinn
# date started: 20191214
# purpose: exploring point desity mapping with leaflet
# original source: https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package

##### ____ contour way #####

##### packages #####
library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")

##### LOAD DATA #####
## Also, clean up variable names, and convert dates

setwd("C:/rworking/deepseatools/indata")
inurl <- "https://data.cityofchicago.org/api/views/22s8-eq8h/rows.csv?accessType=DOWNLOAD"
infile <- "mvthefts.csv"

if(!file.exists(infile)){
  download.file(url = inurl, destfile = infile)
}

dat <- data.table::fread(infile)
setnames(dat, tolower(colnames(dat)))
setnames(dat, gsub(" ", "_", colnames(dat)))
dat <- dat[!is.na(longitude)]
dat[ , date := as.IDate(date, "%m/%d/%Y")]

###### clean variable names #####

setnames(dat, tolower(colnames(dat)))
setnames(dat, gsub(" ", "_", colnames(dat)))

##### get rid of records without longitudes #####
dat <- dat[!is.na(longitude)]

##### converting dates to integer dates #####
dat[ , date := as.IDate(date, "%m/%d/%Y")]

##### make contour lines #####
#note: bandwidth choice is based on MASS::bandwidth.nrd()

# getting kernal density
kde <- bkde2D(dat[ , list(longitude, latitude)],
              bandwidth=c(.0045, .0068),
              gridsize = c(100,100))

##### making contour lines and polygons #####

CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

# extract contour line levels

LEVS <- as.factor(sapply(CL, `[[`, "level"))

# calculate number of levels

NLEV <- length(levels(LEVS))

# contour lines to polygons using lapply
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))

# create spatil polygong
spgons = SpatialPolygons(pgons)

##### create maps #####

# leaflet map with just polygons
leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])

# leaflet map with polygons and
leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(lng = dat$longitude, lat = dat$latitude,
             radius = .5, opacity = .2, col = "blue")

##### _____ raster way #####

library("KernSmooth")
library("raster")

inurl <- "https://data.cityofchicago.org/api/views/22s8-eq8h/rows.csv?accessType=DOWNLOAD"
infile <- "mvthefts.csv"

## LOAD DATA
## Also, clean up variable names, and convert dates
if(!file.exists(infile)){
  download.file(url = inurl, destfile = infile)
}
dat <- data.table::fread(infile)
setnames(dat, tolower(colnames(dat)))
setnames(dat, gsub(" ", "_", colnames(dat)))
dat <- dat[!is.na(longitude)]
dat[ , date := as.IDate(date, "%m/%d/%Y")]

## Create kernel density output
kde <- bkde2D(dat[ , list(longitude, latitude)],
              bandwidth=c(.0045, .0068), gridsize = c(100,100)) #use gridsize = c(1000,1000) to make smoother
# Create Raster from Kernel Density output
KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

#create pal function for coloring the raster
palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)

## Leaflet map with raster
leaflet() %>% addTiles() %>%
  addRasterImage(KernelDensityRaster,
                 colors = palRaster,
                 opacity = .8) %>%
  addLegend(pal = palRaster,
            values = KernelDensityRaster@data@values,
            title = "Kernel Density of Points")

##### erase very low density cells #####

#set low density cells as NA so we can make them transparent with the colorNumeric function
KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] <- NA

#create pal function for coloring the raster
palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")

## Redraw the map
leaflet() %>% addTiles() %>%
  addRasterImage(KernelDensityRaster,
                 colors = palRaster,
                 opacity = .8) %>%
  addLegend(pal = palRaster,
            values = KernelDensityRaster@data@values,
            title = "Kernel Density of Points")

##### make it a binned raster #####
palRaster <- colorBin("Spectral", bins = 7, domain = KernelDensityRaster@data@values, na.color = "transparent")

## Leaflet map with raster
leaflet() %>% addTiles() %>%
  addRasterImage(KernelDensityRaster,
                 colors = palRaster,
                 opacity = .8) %>%
  addLegend(pal = palRaster,
            values = KernelDensityRaster@data@values,
            title = "Kernel Density of Points")






##### Header #####
# created by Robert McGuinn
# date started: 20191214
# purpose: exploring point density mapping with leaflet
# original source: https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package

##### options #####
# options(viewer = NULL)

##### packages #####

library("leaflet")
library("data.table")
library("sp")
library("rgdal")
library("KernSmooth")
library("raster")

##### load data #####

# # set file pointer
setwd("C:/rworking/deepseatools/indata")

# read it in as a data table from file
# filt <- data.table::fread(infile)

##### filter dat #####

dat <- filt %>% filter(# gisMEOW == 'Sea of Japan/East Sea',
                       Genus == 'Anomocora'#,
                       # Vessel == 'Silver Bay R/V',
                       # FishCouncilRegion == 'South Atlantic'
)

dat <- as.data.table(dat)

##### kernel density surface and map #####

kde <- bkde2D(dat[ , list(Longitude, Latitude)],
              bandwidth= c(.8,.8),
              gridsize = c(1000,1000))

# create raster from kernel density output

KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

# KernelDensityRaster = mask(x=KernelDensityRaster, mask=worldcropr)

# checking

# v <- getValues(KernelDensityRaster)
# length(v)
# head(v)
# getValues(KernelDensityRaster, row=100)
# rm(v)
# rm(x)

# create pal function for coloring the raster

palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)

# leaflet map with raster

leaflet() %>% addProviderTiles("Esri.OceanBasemap") %>%
  addRasterImage(KernelDensityRaster,
                 colors = palRaster,
                 opacity = .5) %>%
  addCircles(lng = dat$Longitude, lat = dat$Latitude,
             radius = .5, opacity = .2, col = "blue",
             popup = paste("CatalogNumber:", dat$CatalogNumber, "<br>",
                           "ScientificName:", dat$ScientificName, "<br>",
                           "DatasetID:", dat$DatasetID, "<br>",
                           "Vessel:", dat$Vessel, "<br>",
                           "SurveyID:", dat$SurveyID, "<br>",
                           "SampleID:", dat$SampleID, "<br>",
                           "TrackingID:", dat$TrackingID, "<br>",
                           "Station:", dat$Station, "<br>",
                           "Observation Year:", dat$ObservationYear)) # %>%
  # addLegend(pal = palRaster,
  #           values = KernelDensityRaster@data@values,
  #           title = "Kernel Density of Points")

# erase very low density cells

# set low density cells as NA so we can make them transparent with the colorNumeric function
KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 5.721019e-05)] <- NA

# create pal function for coloring the raster
palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")

# leaflet map with raster
# leaflet() %>% addProviderTiles("Esri.OceanBasemap") %>%
#   addRasterImage(KernelDensityRaster,
#                  colors = palRaster,
#                  opacity = .5) %>%
#   addCircles(lng = dat$Longitude, lat = dat$Latitude,
#              radius = .5, opacity = .2, col = "blue",
#              popup = paste("CatalogNumber:", dat$CatalogNumber, "<br>",
#                            "ScientificName:", dat$ScientificName, "<br>",
#                            "DatasetID:", dat$DatasetID, "<br>",
#                            "Vessel:", dat$Vessel, "<br>",
#                            "SurveyID:", dat$SurveyID, "<br>",
#                            "SampleID:", dat$SampleID, "<br>",
#                            "TrackingID:", dat$TrackingID, "<br>",
#                            "Station:", dat$Station, "<br>",
#                            "Observation Year:", dat$ObservationYear)) #%>%
  # addLegend(pal = palRaster,
  #           values = KernelDensityRaster@data@values,
  #           title = "Kernel Density of Points")




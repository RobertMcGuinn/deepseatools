##### started: 20170814-0 #####
##### Name: GIS-Locality Assignment - Robert P. McGuinn, rpm@alumni.duke.edu #####
##### _____ NGIA and GEBCO GIS Locality Assignment #####
##### Installation/Loading of Packages ##### 
#install.packages("beanplot")
library(beanplot)
#install.packages("stringr")
library(stringr)
#install.packages("knitr")
library(knitr)
#install.packages("tidyr")
library(tidyr)
#install.packages("sp")
library(sp)
#install.packages("maptools")
library(maptools)
#install.packages("maps")
library(maps)
#install.packages("reshape")
library(reshape)
#install.packages("reshape2")
library(reshape2)
#install.packages("psych")
library(psych)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("data.table")
library(data.table)
#install.packages("dplyr")
library(dplyr)
#install.packages("car")
library(car)
#install.packages("gdata")
library(gdata)
#install.packages("digest")
library(digest)
#install.packages("rgdal")
library(rgdal)
#install.packages("ggmap")
library(ggmap)
#install.packages("rerddap")
library(rerddap)

#install.packages("raster")
library(raster)
#install.packages("rworldxtra")
library(rworldxtra)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("xtable")
library(xtable)
library(taxize)
library(rgdal)
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)

##### Bringing in input datasets #####
#from csv
setwd("C:/rworking/digs/indata")
indata<-read.csv("DSCRTP_NatDB_20170807-1.csv", header = T)

##### Defining an input subset for the operation #####
# Creating gisNGIA field
#defining subset of d to use, keeping the original data frame intact (this is a subset of d)
sub <- indata %>% 
  filter(Flag == "0") %>%
  dplyr::select(CatalogNumber, Locality, Latitude, Longitude)

# length(sub$CatalogNumber)

##### Making the SPDF from the subset
# defining coordinates "sub"
coords <- subset(sub, select = c("Longitude", "Latitude"))

# making coords vectors numeric
coords$Latitude<-as.numeric(coords$Latitude)
coords$Longitude<-as.numeric(coords$Longitude)

# creating SpatialPointsDataFrame from the subset. 
spdf<-SpatialPointsDataFrame(coords, sub, 
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                             match.ID = TRUE)

##### Bring in NGIA #####
library(rgdal)
library(maptools)
setwd("C:/data/BaseLayers/UnderseaFeatures")
ngia <- readShapePoints("undersea_features_20131028.shp")
ngia$FULL_NAME <- iconv(ngia$FULL_NAME, "UTF-8", "latin1") 

#names(x)
#class(x)

##### Define distance vectors #####
# Define these vectors, used in the loop. 
# the closestSiteVector is the vector of ID's from the ngia layer that was ID'd as the closest
# named locality.  the minDistVec gives the actual distance between the occurrence point and the name (ngia) point
closestSiteVec <- vector(mode = "numeric",length = nrow(spdf))
minDistVec     <- vector(mode = "numeric",length = nrow(spdf))

# Warning: this can take long time with the whole dataset. Much faster for subsets. 

for (i in 1 : nrow(spdf))
{
  distVec <- spDistsN1(ngia,spdf[i,],longlat = TRUE) 
  #units of distance are in kilometers Great Circle Distance
  minDistVec[i] <- min(distVec)
  closestSiteVec[i] <- which.min(distVec)
}

# #checking the names that were generated
# names(ngia)
# names(spdf)

# defining the NGIA names vector
# this pulls out the FULL_NAME from the ngia spatial points 
# data frame based on the closestSiteVec calculated in the loop above
gisNGIA <- as(ngia[closestSiteVec,]$FULL_NAME,"character")

# assign the Full Name vector back to the data in the appropriate place
sub$gisNGIA <- gisNGIA

# assign the minDistVec to a new variable called gisLocalityDist (units = km).  Adding this 
# variable allows us to threshhold the distance
# units of distance are in kilometers Great Circle Distance
sub$gisNGIADist <- minDistVec

# # checking the new variables 
# sub2 <- sub %>%
#   group_by(Locality, gisNGIA) %>%
#   summarize(n_records = n())
# View(sub2)
# sub2 <- sub[gisNGIA == "Abraham Canyon", ]


# # checking the results of the NGIA gisLocality assignments 
# table(factor(data$gisNGIA), useNA = "always")
# table(factor(data$Locality), useNA = "always")
# 
# sub2 <- subset(sub, gisNGIADist < 50)
# head(sub2[,c("Locality", "gisNGIADist", "gisNGIA")], n = 1000)
# tail(sub2[,c("Locality", "gisNGIADist", "gisNGIA")], n = 1000)
# View(sub2)

# #output
# setwd("C:/rworking/digs/outdata")
# write.csv(data,"gisNGIA_Localities.csv", row.names = F, quote = T)
# names(data)

##### GEBCO Point Locality Assignment #####
##### Bring in GEBCO Localities #####
library(rgdal)
setwd("C:/data/BaseLayers/GEBCO_feature_names/features")
gebco <- readOGR(".", "features-point") #note: to make this work 
#I needed to modify the undersea file in QGIS (vector/multipoint to single point conversion)
proj4string(gebco)
gebco<-spTransform(gebco, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#create a unified variable
gebco$nametype <- paste(gebco$name, gebco$type, sep = " ")

# checking
names(gebco)

##### creating distance vectors #####
# NGIA (gisLocality) Point to point intersection with NGIA (long run time for big sets)
# Define these vectors, used in the loop. 
# the closestSiteVector is the vector of ID's from the ngia layer that was ID'd as the closest
# named locality.  the minDistVec gives the actual distance between the occurrence point and the name (ngia) point
closestSiteVec <- vector(mode = "numeric",length = nrow(spdf))
minDistVec     <- vector(mode = "numeric",length = nrow(spdf))

# Warning: this can take long time with the whole dataset. Much faster for subsets. 

for (i in 1 : nrow(spdf))
{
  distVec <- spDistsN1(gebco,spdf[i,],longlat = TRUE) 
  #units of distance are in kilometers Great Circle Distance
  minDistVec[i] <- min(distVec)
  closestSiteVec[i] <- which.min(distVec)
}


# #checking the names that were generated
# names(ngia)
# names(spdf)

# defining the NGIA names vector
# this pulls out the FULL_NAME from the ngia spatial points 
# data frame based on the closestSiteVec calculated in the loop above
gisGEBCO <- as(gebco[closestSiteVec,]$nametype,"character")

# assign the Full Name vector back to the data in the appropriate place
sub$gisGEBCO<- gisGEBCO

# assign the minDistVec to a new variable called gisLocalityDist (units = km).  Adding this 
# variable allows us to threshhold the distance
# units of distance are in kilometers Great Circle Distance
sub$gisGEBCODist <- minDistVec

# #plot the data
# plot(ngia)
# #plot(gebco)
# points(spdf, col = "red", cex = .6)

# #look at output
# sub2 <- sub %>%
#   group_by(Locality, gisNGIA, gisGEBCO) %>%
#   summarise(n=n())
# fix(sub2)

##### fixing digits in sub#####
sub$gisGEBCODist <- round(sub$gisGEBCODist, digits = 1)
sub$gisNGIADist <- round(sub$gisNGIADist, digits = 1)

##### fixing names is sub #####
x <- which(colnames(sub) == "gisNGIA")
colnames(sub)[x] <- "gisNGIALocality"

x <- which(colnames(sub) == "gisGEBCO")
colnames(sub)[x] <- "gisGEBCOLocality"

##### mapping with leaflet #####
library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=sub, 
                      radius=8, 
                      weight=0, 
                      fillColor= "green", 
                      fillOpacity=1,
                      popup = paste("catalognumber:", sub$CatalogNumber, "<br>",
                                    "locality:", sub$Locality, "<br>",
                                    "ngia:", sub$gisNGIA, "<br>",
                                    "ngia_d:", sub$gisNGIADist, "<br>",
                                    "gebco:", sub$gisGEBCO, "<br>",
                                    "gebco_d:", sub$gisGEBCODist, "<br>"
                                    )
                                    
)
m <- addCircleMarkers(m, data=ngia, 
                      radius=6, 
                      weight=0, 
                      fillColor= "black", 
                      fillOpacity=1,
                      popup = ngia$FULL_NAME
                  
)
m <- addCircleMarkers(m, data=gebco,
                      radius=3, 
                      weight=0, 
                      fillColor= "red", 
                      fillOpacity=1,
                      popup = gebco$nametype
)
m

##### output csv #####
setwd("C:/rworking/digs/outdata")
write.csv(sub,"20170816-1_gis_localities_for_DSCRTP_NatDB_20170807-1.csv", row.names = F, quote = T)
# names(sub)

##### output shapefile #####

# coordinates(sub) <- c("Longitude", "Latitude")
# proj4string(sub) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
# 
# setwd("C:/rworking/digs/outdata")
# writeOGR(sub, dsn="sub", 
#          layer= "sub", 
#          driver = "ESRI Shapefile",
#          dataset_options=c("NameField=CatalogNumber"), 
#          overwrite_layer = T)
# 
# # hist(sub$gisNGIADist)
# # hist(sub$gisGEBCODist)
# 
# names(sub)

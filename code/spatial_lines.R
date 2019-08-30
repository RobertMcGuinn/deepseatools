##### getting line segments and calculatng length#####
# make the point file into a SpatialPointsDataFrame

d <- sub %>%
  filter(StartLatitude != "-999", StartLongitude != "-999", EndLatitude != "-999", EndLongitude != "-999")


d2 <- d #%>%
#filter(StartLatitude != "-999")
coordinates(d2) <- c("Longitude", "Latitude")
proj4string(d2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

# creating spatial lines
begin.coord <- data.frame(lon=d2$StartLongitude, lat=d2$StartLatitude)
end.coord <- data.frame(lon=d2$EndLongitude, lat=d2$EndLatitude)

l <- vector("list", nrow(begin.coord))
library(sp)
for (i in seq_along(l)) {
  l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
}
lines<-SpatialLines(l)
class(lines)

# this step turns the lines file into a SpatialLinesDataFrame for use with the writeOGR function
lines2 <- SpatialLinesDataFrame(lines, d2@data)
class(lines2)
names(lines2)

# adding a length variable

lines2$length <- SpatialLinesLengths(lines, longlat = T)
proj4string(lines2) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
lines2
names(lines2)

setwd("C:/data/aprx/explore/shapefiles")
writeOGR(lines2, dsn="lines", 
         layer= "lines", 
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"), 
         overwrite_layer = T)

summary(lines2$length)


##### points #####


coordinates(sub) <- c("Longitude", "Latitude")
proj4string(sub) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"


setwd("C:/data/aprx/explore/shapefiles")
writeOGR(sub, dsn="sub", 
         layer= "sub", 
         driver = "ESRI Shapefile",
         dataset_options=c("NameField=CatalogNumber"), 
         overwrite_layer = T)



##### look at some problem lines ##### 

x <- sub %>% filter(CatalogNumber %in%  c(915165,
                                     915166,
                                     915167,
                                     915168,
                                     915169,
                                     915170,
                                     915171,
                                     915172,
                                     915173,
                                     915174,
                                     915175,
                                     915176,
                                     915177,
                                     915178,
                                     915179
))

setwd("C:/rworking/digs/outdata")
write.csv(x, '20181130_0_really_long_transect_RPMcGuinn.csv')

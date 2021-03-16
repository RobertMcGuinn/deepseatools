## Area of Interest Map


##### using marmap and ggplot2 to make a map #####

# create a spatial points data frame
spdf<-sub2
coordinates(spdf) <- c("Longitude", "Latitude", "DepthInMeters")
proj4string(spdf) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
x<-bbox(spdf)
# install.packages('marmap')
# library(marmap)
zoom <- 2 # as number gets bigger you achieve a wider extent to your download
cont <- getNOAA.bathy(lon1 = x[1,1]-zoom, lon2 = x[1,2]+zoom,
                      lat1 = x[2,1]-zoom, lat2 = x[2,2]+zoom, resolution = 2,
                      keep = FALSE, antimeridian = FALSE)

##### if you have linear stuff you want to map use this #####
# begin.coord <- data.frame(lon=sub$StartLongitude, lat=sub$StartLatitude)
# end.coord <- data.frame(lon=sub$EndLongitude, lat=sub$EndLatitude)
#
# l <- vector("list", nrow(begin.coord))
# library(sp)
# for (i in seq_along(l)) {
#   l[[i]] <- Lines(list(Line(rbind(begin.coord[i, ], end.coord[i,]))), as.character(i))
# }
# lines<-SpatialLines(l)
# class(lines)
#
# linesdf <- SpatialLinesDataFrame(lines, data = begin.coord)
#
# lines_fortify <- fortify(linesdf)

##### make ggplot based map #####


# topographical color scale, see ?scale_fill_etopo
g <- autoplot(cont, geom=c("raster", "contour")) +
  scale_fill_gradient2(low="dodgerblue4", mid="gainsboro", high="darkgreen") +
  labs(x = 'Longitude') +
  labs(y = 'Latitude')

# add sampling locations

g + geom_point(aes(x=Longitude, y=Latitude), data=sub2, alpha=0.5, color = 'red', size = 2)

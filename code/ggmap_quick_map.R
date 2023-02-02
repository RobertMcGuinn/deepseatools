##### Using ggmap libary #####
library(ggmap)
sub2 <- filt %>%
  filter(DatasetID == "OET_NA114") %>%
  filter(Latitude != '-999' , Longitude != '-999')

library(ggplot2)
gis<-sub2
coordinates(gis) <- c("Longitude", "Latitude", "DepthInMeters")
proj4string(gis) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

x<-bbox(gis)
zoom <- 0
m <- map_data("world",
              xlim = c(x[1,1]-zoom,x[1,2]+zoom),
              ylim = c(x[2,1]-zoom,x[2,2]+zoom))

z <- ggplot(m, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  geom_point(data=sub2,
             color = "red",
             size = 3,
             aes(x=Longitude, y = Latitude),
             inherit.aes = FALSE)
z

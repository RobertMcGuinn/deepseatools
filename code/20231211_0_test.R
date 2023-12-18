#Plot the map of points

library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

theme_set(theme_bw())

indata <-  read.csv('c:/rworking/deepseatools/indata/stn.csv')
indata <- na.omit(indata)

## indata %>% filter(is.na(Latitude) == T)
indata <- indata %>% filter(is.na(Longitude) == T)
indata <- indata %>% filter(is.na(Latitude) == T)

sites_2 = st_shift_longitude(indata)



## check
names(indata)
summary(indata$Longitude)



world = map_data("world2")



map_points = ggplot() + geom_polygon(data=world ,aes(x=long, y=lat, group=group),

                                     size=6, col = "#78909C", fill = "#78909C", lwd = 0)+

  geom_point(data=indata, aes(x=Longitude, y=Latitude),

             size=1.5, shape=1, color="red") +



  coord_map(orientation = c(90,0, 150),
            ylim = c(48, 56),
            xlim = c(-190,-164)) +

  xlab("Longitude (°W)") + ylab("Latitude (°N)") + ggtitle("Aleutian Islands")



map_points



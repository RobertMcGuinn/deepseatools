##### Header #####
## author: Robert McGuinn: rpm@alumni.duke.edu
##

##### filter database by bounding box #####
## set bounding box variables
minlat <- 5
maxlat <- 33
minlon <- -93
maxlon <- -56

## subset data by coordinates
x <- subset(filt, as.numeric(Latitude) > minlat &
              as.numeric(Latitude) < maxlat &
              as.numeric(Longitude) > minlon &
              as.numeric(Longitude) < maxlon)

## also subset data by other variables by other criteria
# x <- x %>% filter(ScientificName == "")

## checking
# dim(x)

## cleaning
rm(filt)

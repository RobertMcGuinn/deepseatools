##### Header #####
## author: Robert McGuinn, rpm@alumni.duke.edu, robert.mcguinn@noaa.gov
## started: 20230915
## purpose: Inspect Northeast Dive Inventory per Heather Coleman's request.

##### packages ######
library(tidyverse)
library(sf)
library(xml2)

##### bring in the KMZ file #####
## You can convert KMZ to KML using only R by unzipping the KMZ file, extracting the KML file within it, and then reading the KML file into R. Here's a step-by-step guide:
## Replace "your_file.kmz" with the path to your KMZ file
kmz_file <- "c:/rworking/deepseatools/indata/20230915-0_NURTEC_Dives_HColeman_PAuster.kmz"

## Define a temporary directory to extract the KML file
temp_dir <- tempdir()

##  the KMZ file to the temporary directory
unzip(kmz_file, exdir = temp_dir)

## List the files in the temporary directory
list.files(temp_dir)

## After running this code, you should see the KML file listed among the extracted files.
## **Read the KML File into R as an 'sf' Object**:
## Now that you have the KML file extracted, you can read it into R as an 'sf' object:
## Construct the path to the extracted KML file
kml_file <- file.path(temp_dir, "doc.kml")

## Parse the KML file
kml_doc <- read_xml(kml_file)

## Extract feature attributes
placemarks <- xml_find_all(kml_doc, ".//Placemark")
attributes_list <- lapply(placemarks, function(placemark) {
  # Extract the name and description attributes (you can customize this)
  name <- xml_text(xml_find_all(placemark, ".//name"))
  description <- xml_text(xml_find_all(placemark, ".//description"))

  # Create a list of attributes
  list(Name = name, Description = description)
})

## Convert the list of attributes to a data frame
attributes_df <- do.call(rbind, attributes_list)

## Read the KML file into an 'sf' object
kmz_sf <- st_read(kml_file)

# Combine the 'sf' object with the attributes data frame
kmz_sf <- cbind(kmz_sf, attributes_df)

##### bring in the CSV file #####
## set option
digits = 12

## variables: manual input
path <- "C:/rworking/deepseatools/indata"
csv <- "20230915_Total Dives 1985-2018 ver 9-13-23 w Proprietary Dives Merged to Main_HColeman_PAuster.csv"

## manual input: load subset
setwd(path)
indata <- read.csv(csv, header = T)
sub <- indata %>%
  filter()

## cleanup
rm(indata)

##### check #####
# View(sub)
# names(sub)
# table(sub$Vessel, useNA = 'always')
# sub %>% group_by(Vessel, Year, Dive.System, Total.Dives) %>%
#   summarize(n=n()) %>% View()
#
# sub %>% group_by(Lat_DD, Lon_DD, Location, Vessel) %>% summarize(n=n()) %>% View()

##### creating a geographic bounding box (Northeasten US Atlantic Ocean) #####
minLon <- -77
maxLon <- -54
minLat <- 33
maxLat <- 48

##### filtering data by bounding box #####
geofilt <-
  filt %>% filter(as.numeric(Latitude) > minLat,
                  as.numeric(Latitude) < maxLat,
                  as.numeric(Longitude) < maxLon,
                  as.numeric(Longitude) > minLon)

##### filter sub data by bounding box #####
geosub <-
  sub %>% filter(as.numeric(Lat_DD) > minLat,
                   as.numeric(Lat_DD) < maxLat,
                   as.numeric(Lon_DD) < maxLon,
                   as.numeric(Lon_DD) > minLon)



##### creating sf object for ndb #####
projcrs <- 4326
geofilt <- st_as_sf(x = geofilt,
                   coords = c("Longitude", "Latitude"),
                   crs = projcrs)

## thin down variables
geofilt <- geofilt %>%
  dplyr::select(Vessel, VehicleName, SamplingEquipment, ObservationYear, ObservationDate, DatasetID)

##### creating sf object for sub #####
projcrs <- 4326
geosub <- st_as_sf(x = geosub,
                    coords = c("Lon_DD", "Lat_DD"),
                    crs = projcrs)

## thin down variables
geosub <- geosub %>%
  dplyr::select(Project.No., Vessel, Dive.System, Year, Date)


##### write shapefile #####
path <- "C:/rworking/deepseatools/indata"
setwd(path)

st_write(geofilt,
         "geofilt.shp",
         delete_dsn = T)

st_write(geosub,
         "geosub.shp",
         delete_dsn = T)

##### spatial intersect #####
## Create buffer around geofilt and find intersecting points in geosub
# Transform an 'sf' object to UTM Zone 18N
geofilt_utm <- st_transform(geofilt, crs = "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")
geosub_utm <- st_transform(geosub, crs = "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")

geofilt_buffer <- st_buffer(geofilt_utm, dist = 5000)
intersection <- st_intersection(geofilt_buffer, geosub_utm)

##### check #####
class(intersection)
plot(intersection)
View(intersection)

##### write out intersection #####
path <- "C:/rworking/deepseatools/indata"
setwd(path)
st_write(intersection,
         "intersect5000.shp",
         delete_dsn = T)

##### check #####
View(intersection)


##### look at matching years #####
matching_records <- intersection[as.numeric(intersection$ObservationYear) == as.numeric(intersection$Year), ]

##### check #####
View(matching_records)
class(matching_records)

##### write out intersection #####
path <- "C:/rworking/deepseatools/indata"
setwd(path)
st_write(matching_records,
         "20230915-0_matching_records_RPMcGuinn.shp",
         delete_dsn = T)

##### group project table #####
summary <- matching_records %>%
  group_by() %>%
  summarize(n=n())

##### check #####
View(summary)



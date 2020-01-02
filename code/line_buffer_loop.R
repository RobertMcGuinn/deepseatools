##### Header #####
# original author: Elizabeth Gugliotti
# code forked date: 20200102 by Robert McGuinn.
# purpose: build buffer around lines and intersect points

##### install #####
#devtools::install_github("yonghah/esri2sf")
library("esri2sf")
library(sf)
url<-"https://service.ncddc.noaa.gov/arcgis/rest/services/OceanExploration/OE_OkeanosDives/MapServer/65"
df<-esri2sf(url)

##### transform #####
# transform to UTM zone 18

df.utm<-st_transform(df, "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83")

##### Create 20 m buffer #####

buf<-st_buffer(df.utm, dist=20)

##### buffer,  tranform projection to WGS84 #####

buf.wgs<-st_transform(buf, "+proj=longlat +ellps=WGS84 +datum=WGS84")

##### indata to data frame #####
indatadf<-data.frame(indata)

##### make buf.wgs$Dive match indata$EventID #####

buf.wgs$Dive<-str_replace_all(buf.wgs$dive,"DIVE", "Dive")

##### Make indata an sf class with crs same as buf.wgs #####
indata_sf<-st_as_sf(x=indata, coords=c("Longitude","Latitude"), crs=st_crs(buf.wgs))

##### separate sf geometry to get lat and lon for map #####
indata_coords <- do.call(rbind, st_geometry(indata_sf)) %>%
  as_tibble() %>% setNames(c("lon","lat"))

##### Put lat and long in indata_sf #####
indata_sf$Lon<-indata_coords$lon
indata_sf$Lat<-indata_coords$lat

##### create empty dataframe (include items needed for leaflet map #####

buffer.df <- data.frame(EventID = character(),
                        SampleID = character(),
                        ScientificName = character(),
                        in.buffer=character(),
                        Longitude = numeric(),
                        Latitude = numeric(),
                        Depth = numeric(),
                        stringsAsFactors=FALSE)

##### loop #####

for (id in buf.wgs$Dive){
  x <- buf.wgs %>% dplyr::filter(Dive==id)
  y <- indata_sf %>% dplyr::filter(EventID==x$Dive)
  z <- st_within(y,x, sparse = FALSE)
  buffer.d <- data.frame(EventID = x$Dive,
                         SampleID = y$SampleID,
                         ScientificName = y$ScientificName,
                         in.buffer = z,
                         Longitude = y$Lon,
                         Latitude = y$Lat,
                         Depth = y$DepthInMeters,
                         stringsAsFactors=FALSE)
  buffer.df <- rbind(buffer.df, buffer.d)
}

##### select only records within the buffer #####
# Filter for only observations that don't fall within the 20 m buffer.
# These should be the same as in the process below

buffer.df<- buffer.df %>%
  dplyr::filter(in.buffer==FALSE)

##### Make leaflet map #####
leaflet(options=leafletOptions(maxZoom=25)) %>%
  addEsriBasemapLayer(esriBasemapLayers$Gray) %>%
  addEsriFeatureLayer(
    url = "https://service.ncddc.noaa.gov/arcgis/rest/services/OceanExploration/OE_OkeanosDives/MapServer/65",markerType ="marker") %>%
  addPolygons(data=buf.wgs, color = "black", fillOpacity =0.1) %>%
  addCircleMarkers(data=buffer.df,
                   lat=buffer.df$Latitude,
                   lng=buffer.df$Longitude,
                   color="red",
                   popup=paste("Dive:",buffer.df$EventID,"<br>",
                               "ScientificName:",buffer.df$ScientificName,"<br>",
                               "SampleID:",buffer.df$SampleID,"<br>",
                               "Latitude:",buffer.df$Latitude,"<br>",
                               "Longitude:",buffer.df$Longitude))

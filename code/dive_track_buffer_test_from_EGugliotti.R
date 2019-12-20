##### Header #####
# author: Elizabeth F. Gugliotti, M.S.. Phone: 843-460-9688
# date started: 20191219
# purpose: This code is to create a 20 m buffer from the line
#   from esri and see if the points from indata fall in this buff

# Need this package to turn the esri line into an sf dataframe
devtools::install_github("yonghah/esri2sf")
library("esri2sf")
library(sf)
url<-"https://service.ncddc.noaa.gov/arcgis/rest/services/OceanExploration/OE_OkeanosDives/MapServer/65"
df<-esri2sf(url)
#st_crs(df)

# transform to UTM zone 18
df.utm<-st_transform(df, "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83")
#st_crs(df.utm)

# Create 20 m buffer
buf<-st_buffer(df.utm, dist=20)

# Turn buffer back into WGS84
buf.wgs<-st_transform(buf, "+proj=longlat +ellps=WGS84 +datum=WGS84")

indatadf<-data.frame(indata)

# by dive
# Dive 1
buf.wgs1<- buf.wgs[buf.wgs$dive=="DIVE01",] # select buffer for dive 1
indatadf1<- indatadf %>%
  dplyr::filter(EventID=="Dive01") # select indata for dive 1

indata_sf1<-st_as_sf(x=indatadf1, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs)) # make indata as sf class using buf.wgs (which is WGS 84) as crs

pnts1<-st_within(indata_sf1, buf.wgs1, sparse=FALSE) # check if points in indata_sf1 fall in the dive 1 buffer, returns matrix of TRUE/FALSE values
pnts1<-data.frame(pnts1) # make as dataframe
pnts1['EventID']="Dive01"
pnts1<-pnts1 %>%
  rename(InsideBuffer=pnts1)

# Dive 2
buf.wgs2<- buf.wgs[buf.wgs$dive=="DIVE02",]
indatadf2<- indatadf %>%
  dplyr::filter(EventID=="Dive02")

indata_sf2<-st_as_sf(x=indatadf2, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts2<-st_within(indata_sf2, buf.wgs2, sparse=FALSE)
pnts2<-data.frame(pnts2)
pnts2['EventID']="Dive02"
pnts2<-pnts2 %>%
  rename(InsideBuffer=pnts2)

# Dive 3
buf.wgs3<- buf.wgs[buf.wgs$dive=="DIVE03",]
indatadf3<- indatadf %>%
  dplyr::filter(EventID=="Dive03")

indata_sf3<-st_as_sf(x=indatadf3, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts3<-st_within(indata_sf3, buf.wgs3, sparse=FALSE)
pnts3<-data.frame(pnts3)
pnts3['EventID']="Dive03"
pnts3<-pnts3 %>%
  rename(InsideBuffer=pnts3)

# Dive 4
buf.wgs4<- buf.wgs[buf.wgs$dive=="DIVE04",]
indatadf4<- indatadf %>%
  dplyr::filter(EventID=="Dive04")

indata_sf4<-st_as_sf(x=indatadf4, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts4<-st_within(indata_sf4, buf.wgs4, sparse=FALSE)
pnts4<-data.frame(pnts4)
pnts4['EventID']="Dive04"
pnts4<-pnts4 %>%
  rename(InsideBuffer=pnts4)

# Dive 5
buf.wgs5<- buf.wgs[buf.wgs$dive=="DIVE05",]
indatadf5<- indatadf %>%
  dplyr::filter(EventID=="Dive05")

indata_sf5<-st_as_sf(x=indatadf5, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts5<-st_within(indata_sf5, buf.wgs5, sparse=FALSE)
pnts5<-data.frame(pnts5)
pnts5['EventID']="Dive05"
pnts5<-pnts5 %>%
  rename(InsideBuffer=pnts5)

# Dive 6
buf.wgs6<- buf.wgs[buf.wgs$dive=="DIVE06",]
indatadf6<- indatadf %>%
  dplyr::filter(EventID=="Dive06")

indata_sf6<-st_as_sf(x=indatadf6, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts6<-st_within(indata_sf6, buf.wgs6, sparse=FALSE)
pnts6<-data.frame(pnts6)
pnts6['EventID']="Dive06"
pnts6<-pnts6 %>%
  rename(InsideBuffer=pnts6)

# Dive 7
buf.wgs7<- buf.wgs[buf.wgs$dive=="DIVE07",]
indatadf7<- indatadf %>%
  dplyr::filter(EventID=="Dive07")

indata_sf7<-st_as_sf(x=indatadf7, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts7<-st_within(indata_sf7, buf.wgs7, sparse=FALSE)
pnts7<-data.frame(pnts7)
pnts7['EventID']="Dive07"
pnts7<-pnts7 %>%
  rename(InsideBuffer=pnts7)

# Dive 8
buf.wgs8<- buf.wgs[buf.wgs$dive=="DIVE08",]
indatadf8<- indatadf %>%
  dplyr::filter(EventID=="Dive08")

indata_sf8<-st_as_sf(x=indatadf8, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts8<-st_within(indata_sf8, buf.wgs8, sparse=FALSE)
pnts8<-data.frame(pnts8)
pnts8['EventID']="Dive08"
pnts8<-pnts8 %>%
  rename(InsideBuffer=pnts8)

# No Dive 9

# Dive 10
buf.wgs10<- buf.wgs[buf.wgs$dive=="DIVE10",]
indatadf10<- indatadf %>%
  dplyr::filter(EventID=="Dive10")

indata_sf10<-st_as_sf(x=indatadf10, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts10<-st_within(indata_sf10, buf.wgs10, sparse=FALSE)
pnts10<-data.frame(pnts10)
pnts10['EventID']="Dive10"
pnts10<-pnts10 %>%
  rename(InsideBuffer=pnts10)

# Dive 11
buf.wgs11<- buf.wgs[buf.wgs$dive=="DIVE11",]
indatadf11<- indatadf %>%
  dplyr::filter(EventID=="Dive11")

indata_sf11<-st_as_sf(x=indatadf11, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts11<-st_within(indata_sf11, buf.wgs11, sparse=FALSE)
pnts11<-data.frame(pnts11)
pnts11['EventID']="Dive11"
pnts11<-pnts11 %>%
  rename(InsideBuffer=pnts11)

# Dive 12
buf.wgs12<- buf.wgs[buf.wgs$dive=="DIVE12",]
indatadf12<- indatadf %>%
  dplyr::filter(EventID=="Dive12")

indata_sf12<-st_as_sf(x=indatadf12, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts12<-st_within(indata_sf12, buf.wgs12, sparse=FALSE)
pnts12<-data.frame(pnts12)
pnts12['EventID']="Dive12"
pnts12<-pnts12 %>%
  rename(InsideBuffer=pnts12)

# Dive 13
buf.wgs13<- buf.wgs[buf.wgs$dive=="DIVE13",]
indatadf13<- indatadf %>%
  dplyr::filter(EventID=="Dive13")

indata_sf13<-st_as_sf(x=indatadf13, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts13<-st_within(indata_sf13, buf.wgs13, sparse=FALSE)
pnts13<-data.frame(pnts13)
pnts13['EventID']="Dive13"
pnts13<-pnts13 %>%
  rename(InsideBuffer=pnts13)

# No Dive 14

# Dive 15
buf.wgs15<- buf.wgs[buf.wgs$dive=="DIVE15",]
indatadf15<- indatadf %>%
  dplyr::filter(EventID=="Dive15")

indata_sf15<-st_as_sf(x=indatadf15, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts15<-st_within(indata_sf15, buf.wgs15, sparse=FALSE)
pnts15<-data.frame(pnts15)
pnts15['EventID']="Dive15"
pnts15<-pnts15 %>%
  rename(InsideBuffer=pnts15)

# No Dive 16

# Dive 17
buf.wgs17<- buf.wgs[buf.wgs$dive=="DIVE17",]
indatadf17<- indatadf %>%
  dplyr::filter(EventID=="Dive17")

indata_sf17<-st_as_sf(x=indatadf17, coords = c("Longitude", "Latitude"), crs=st_crs(buf.wgs))

pnts17<-st_within(indata_sf17, buf.wgs17, sparse=FALSE)
pnts17<-data.frame(pnts17)
pnts17['EventID']="Dive17"
pnts17<-pnts17 %>%
  rename(InsideBuffer=pnts17)

# combine all dataframes for each dive into one
allpnts<-do.call("rbind",list(pnts1,pnts2,pnts3,pnts4,pnts5,pnts6,pnts7,pnts8,pnts10,pnts11,pnts12,pnts13,pnts15,pnts17))
indata$EventID<-as.character(indata$EventID)
indataBuffer<-cbind(indata, allpnts$InsideBuffer)
indataBuffer<- indataBuffer %>%
  filter(allpnts$InsideBuffer==FALSE)


# Map to visualize points outside 20 m buffer
leaflet(options=leafletOptions(maxZoom=25)) %>%
  addEsriBasemapLayer(esriBasemapLayers$Gray) %>%
  addEsriFeatureLayer(
    url = "https://service.ncddc.noaa.gov/arcgis/rest/services/OceanExploration/OE_OkeanosDives/MapServer/65",markerType ="marker") %>%
  addCircleMarkers(data=indataBuffer,
                   lat=indataBuffer$Latitude,
                   lng=indataBuffer$Longitude,
                   color="red",
                   popup=paste("Dive:",indataBuffer$EventID,"<br>",
                               "ScientificName:",indataBuffer$ScientificName,"<br>",
                               "SampleID:",indataBuffer$SampleID,"<br>",
                               "Latitude:",indataBuffer$Latitude,"<br>",
                               "Longitude:",indataBuffer$Longitude))


# Elizabeth F. Gugliotti, M.S.
# National Centers for Coastal Ocean Science, Marine Spatial Ecology Division
# Deep Coral Ecology Lab
# 219 Fort Johnson Road
# Charleston, SC 29412
# Phone: 843-460-9688
#
# Contractor, CSS-Dynamac


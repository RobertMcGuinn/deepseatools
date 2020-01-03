##### Installation/Loading of Packages #####
#install.packages("pacman")
library(pacman)
#pacman::p_load(captioner, bundesligR)
library(captioner, bundesligR)
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
#install.packages("extrafont")
library(extrafont)
library(RColorBrewer)
library(googlesheets)
library(googledrive)
#install.packages('measurements')
library(measurements)


##### Bringing in input datasets #####
setwd("C:/rworking/digs/indata")

d1 <- read.csv("C:/rworking/digs/indata/DSCRTP_NatDB_20171214_0_Subset_TH.csv", header = T)
d2 <- read.csv("C:/rworking/digs/indata/DSCRTP_NatDB_20171214_0_Subset_TH_MCZ.csv", header = T)
d3 <- read.csv("C:/rworking/digs/indata/DSCRTP_NatDB_20171214_0_Subset_TH_Harter2016.csv", header = T)
d4 <- read.csv("C:/rworking/digs/indata/DSCRTP_NatDB_20171214_0_Subset_TH_ReedFarrington2017.csv", header = T)
d5 <- read.csv("C:/rworking/digs/indata/DSCRTP_NatDB_20171214_0_Subset_TH_NMNH.csv", header = T)

# filt <-  d %>%
#  filter(Flag == "0")

##### checking tables #####
# table(d1$Flag)
# table(d2$Flag)
# table(d3$Flag)
# table(d4$Flag)
# table(d5$Flag)
#
# setdiff(names(d1), names(d2))
# setdiff(names(d2), names(d1))
#
# setdiff(names(d1), names(d3))
# setdiff(names(d3), names(d1))
#
# setdiff(names(d1), names(d4))
# setdiff(names(d4), names(d1))
#
# setdiff(names(d1), names(d5))
# setdiff(names(d5), names(d1))


##### corrections to Farrington set #####
#View(d4)
#d4$VerbatimLatitude
# use negation in grepl selection.
d4a <- d4 %>% filter(!(grepl('W', VerbatimLongitude)))
d4b <- d4 %>% filter((grepl('W', VerbatimLongitude)))

table(factor(d4a$VerbatimLongitude))
table(factor(d4a$VerbatimLatitude))

table(factor(d4b$VerbatimLongitude))
table(factor(d4b$VerbatimLatitude))

setdiff(names(d4a), names(d4b))
setdiff(names(d4b), names(d4a))

d4b$Latitude <-  gsub('°', ' ', d4b$VerbatimLatitude)
d4b$Latitude <-  gsub('N', '', d4b$Latitude)
d4b$Latitude <-  gsub('\'', '', d4b$Latitude)
d4b$Latitude <- trimws(d4b$Latitude, which = c("both"))
table(d4b$Latitude)

d4b$Longitude <-  gsub('°', ' ', d4b$VerbatimLongitude)
d4b$Longitude <-  gsub('W', '', d4b$Longitude)
d4b$Longitude <-  gsub('\'', '', d4b$Longitude)
d4b$Longitude <- trimws(d4b$Longitude, which = c("both"))

# convert from decimal minutes to decimal degrees
d4b$Latitude = (measurements::conv_unit(d4b$Latitude, from = 'deg_dec_min', to = 'dec_deg'))
d4b$Longitude = (measurements::conv_unit(d4b$Longitude, from = 'deg_dec_min', to = 'dec_deg'))
d4b$Latitude = (as.numeric(d4b$Latitude))
d4b$Longitude = -1 * (as.numeric(d4b$Longitude))

d4b$Longitude <- as.character(d4b$Longitude)
d4b$Latitude <- as.character(d4b$Latitude)

d4a$Latitude = as.character(d4a$VerbatimLatitude)
d4a$Longitude = as.character(d4a$VerbatimLongitude)

d4a$Latitude <- trimws(d4a$Latitude, which = c("both"))
d4a$Longitude <- trimws(d4a$Longitude, which = c("both"))

d4corr <- rbind(d4a, d4b)
d4corr$Latitude <- as.numeric(d4corr$Latitude)
d4corr$Longitude <- as.numeric(d4corr$Longitude)


##### making sure they are all character #####
d1$Latitude <- as.numeric(trimws(as.character(d1$Latitude), which = c('both')))
d1$Longitude <- as.numeric(trimws(as.character(d1$Longitude), which = c('both')))

d2$Latitude <- as.numeric(trimws(as.character(d2$Latitude), which = c('both')))
d2$Longitude <- as.numeric(trimws(as.character(d2$Longitude), which = c('both')))

d3$Latitude <- as.numeric(trimws(as.character(d3$Latitude), which = c('both')))
d3$Longitude <- as.numeric(trimws(as.character(d3$Longitude), which = c('both')))

d4corr$Latitude <- as.numeric(trimws(as.character(d4corr$Latitude), which = c('both')))
d4corr$Longitude <- as.numeric(trimws(as.character(d4corr$Longitude), which = c('both')))

d5$Latitude <- as.numeric(trimws(as.character(d5$Latitude), which = c('both')))
d5$Longitude <- as.numeric(trimws(as.character(d5$Longitude), which = c('both')))

##### rbind all ####
d <- do.call("rbind", list(d1, d2, d3, d4corr, d5))
##### checking #####
#dim(d)
#table(d$Flag)
#table(d$Flag)

##### writing a copy of the clean set
setwd("C:/rworking/digs/outdata")
write.csv(d, 'DSCRTP_NatDB_20171214_0_Subset_Plus_Ocean_Sciencies_RPMcGuinn.csv', row.names = F, quote = T)

##### mapit #####
x <- d[d$Flag == '0' | d$Flag == '2',]

##### map #####

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap")
m <- addCircleMarkers(m, data=x,
                      radius=5,
                      weight=0,
                      fillColor= "blue",
                      fillOpacity=1,
                      popup = paste(
                        "<b><em>","Flag:","</b></em>", x$Flag, "<br>",
                        "<b><em>","Catalog Number:","</b></em>", x$CatalogNumber, "<br>",
                        "<b><em>","Record Type:","</b></em>", x$RecordType, "<br>",
                        "<b><em>","DatasetID:","</b></em>", x$DatasetID, "<br>",
                        "<b><em>","AccessionID:","</b></em>", x$AccessionID, "<br>",
                        "<b><em>","DataProvider:","</b></em>", x$DataProvider, "<br>",
                        "<b><em>","ObservationYear:","</b></em>", x$ObservationYear, "<br>",
                        "<b><em>","Vessel:","</b></em>", x$Vessel, "<br>",
                        "<b><em>","Locality:","</b></em>", x$Locality, "<br>",
                        "<b><em>","Scientific Name:","</b></em>", x$ScientificName, "<br>",
                        "<b><em>","Depth (meters):","</b></em>", x$DepthInMeters, "<br>",
                        "<b><em>","Survey ID:","</b></em>", x$SurveyID, "<br>",
                        "<b><em>","Event ID:","</b></em>", x$EventID, "<br>",
                        "<b><em>","Latitude:","</b></em>", x$Latitude, "<br>",
                        "<b><em>","Longitude:","</b></em>", x$Longitude, "<br>",
                        "<b><em>","Image:","</b></em>",x$ImageURL)

)
m









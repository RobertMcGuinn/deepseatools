##### Header #####
# Author: Robert P. McGuinn
# Started on: 20190620
# Purpose: looking at invertebrate data from Alaska Groundfish Survey

##### install.packages #####
library(tidyverse)

##### importing data #####

setwd("C:/rworking/digs/indata")
d <- read.table("20190620-0_invertebrates_groundfish_observer_data_RPMcGuinn.txt", header = T, sep=",", fill = TRUE)

##### checking #####
# class(d)
# dim(d)
# table(d$YEAR)
# table(d$SPECIES, useNA = 'always')
# table(d$SPECN, useNA = 'always')
# names(d)
# table(d$LAT400SQKM, useNA = 'always')
# table(d$LON400SQKM, useNA = 'always')


##### species #####

# Picked out this list of SPECIES / SPECN
# Black Coral	818
# Gorgonian	817
# Hydrocoral	815
# *** Hydroids Unidentified	835 (do not use)
# *** Invertebrate Unidentified	902 (do not use)
# Red Tree Coral	833
# Sea Pen-Sea Whip Unidentified	58
# Soft Coral	819
# *** Sponge unidentified	26 (use separately)
# Stony Coral	816

##### select just the corals (see list above) #####
y <- d %>% filter(SPECN == '818' |
                    SPECN == '817' |
                    SPECN == '815' |
                    SPECN == '833' |
                    SPECN == '58' |
                    SPECN == '819' |
                    SPECN == '816'
)

y$SPECIES <- factor(y$SPECIES)

##### write the subset of data that just includes corals #####

setwd("C:/documents/management_area_analyses/20190620-0_Alaska_Mapping_Priorities/tabular_data")

y %>%
  write.csv("20190621-0_corals_only_RPMcGuinn.csv", row.names = FALSE)

##### #####

names(y)


##### summary grouped by SPECIES/SPECN #####

names(d)

x <- d %>%
  filter(SPECIES == "Red Tree Coral") %>%
  group_by(SPECIES, SPECN, LAT400SQKM, LON400SQKM) %>%
  summarise(n=n(),
           sum_kg = sum(KG))

setwd("C:/documents/management_area_analyses/20190620-0_Alaska_Mapping_Priorities/tabular_data")

x %>%
  write.csv("20190620-0_red_tree_coral_observer_data_RPMcGuinn.csv", row.names = FALSE)


#View(x)


##### build an extraction from the national database #####

z <- filt %>%
  filter(
     ScientificName == 'Primnoa pacifica',
    # Phylum == 'Porifera',
    Latitude > 47
    )

##### map it #####

library(leaflet)
m <- leaflet()
m <- addProviderTiles(m, "Esri.OceanBasemap") #Esri.OceanBasemap, "CartoDB.DarkMatter"
m <- addCircleMarkers(m, data=x,
                      lat = x$LAT400SQKM,
                      lng = x$LON400SQKM,
                      radius= ~ log(sum_kg),
                      weight=0,
                      fillColor= "green",
                      fillOpacity=1,
                      popup = paste(
                        "SPECIES:", x$SPECIES, "<br>",
                        "SPECN:", x$SPECN, "<br>",
                        "sum_KG:", x$sum_kg, "<br>",
                        "n:", x$n
                       ))

m <- addCircleMarkers(m, data=z,
                      # lat = x$LAT400SQKM,
                      # lng = x$LON400SQKM,
                      radius= 2,
                      weight=0,
                      fillColor= "red",
                      fillOpacity=1,
                      )


m






##### get the matching records from ERDDAP #####

##### Header #####
## author: Robert P. McGuinn, robert.mcguinn@noaa.gov, rpm@alumni.duke.edu
## startdate:
## purpose:

##### linkage #####
filename <- '' ## manual: for this code file name, match to redmine
github_path <- 'https://github.com/RobertMcGuinn/deepseatools/blob/master/code/'
github_link <- paste(github_path, filename, '.R', sep = '')
browseURL(github_link)
redmine_path <- 'https://vlab.noaa.gov/redmine/issues/'
issuenumber <- filename
redmine_link <- paste(redmine_path, issuenumber, sep = '')
browseURL(redmine_link)

##### packages #####
library(tidyverse)
library(sf)
library(remotes)
library(redmineR)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

##### source ndb #####
source("c:/rworking/deepseatools/code/mod_load_current_ndb.R")

##### make a map #####
## Convert dataframe to sf object
ndb_geo <- st_as_sf(filt,
                    coords = c("Longitude", "Latitude"),
                    crs = 4326)

## Download world map from naturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

## Plot the map
map <- ggplot() +
  geom_sf(data = world) +
  geom_sf(data = ndb_geo, color = "red", size = 3) +
  theme_minimal()

##### save the plot #####
ggsave("c:/rworking/deepseatools/images/map.png",
       width = 8,
       height = 5.57,
       units = "in")

##### Arvind Shantharam's Code #####
##### load database ####
## note: make sure to swap out published version of database for the annual update
# setwd("C:/rworking/deepseatools/indata/")
# indata=read.csv("DSCRTP_NatDB_20240115-0.csv", header = T)
##### filter flagged records #####
filt <- indata %>% filter(Flag == 0)

##### make map #####
theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass="sf")

map <- ggplot(data=world) +
  geom_point(data=filt,
             aes(x=as.numeric(Longitude), y=as.numeric(Latitude)),
             size=0.5,
             shape=16,
             fill="red") +
  labs(title = "Observations of Deep-Sea Coral and Sponge Occurrences
          \n from the NOAA National Deep-Sea Coral and Sponge Database, 1842-Present",
       x = "Longitude", y="Latitude", caption = "Database v. 20240115-0")+
  geom_sf()

##### save the plot #####
ggsave("c:/rworking/deepseatools/images/map2.png",
       width = 8,
       height = 5.57,
       units = "in")

##### creating lonlat file #####
lonlat = filt[49:48]
write.table(lonlat,
            file="c:/rworking/deepseatools/indata/lonlat.txt",
            sep="\t", col.names = F, row.names = F)

##### check #####
head(lonlat)











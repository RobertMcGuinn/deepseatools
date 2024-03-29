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

##### Arvind Shantharam's Code #####
##### load datbase ####
## note: make sure to swap out published version of database for the annual update
setwd("C:/rworking/deepseatools/indata/")
indata=read.csv("DSCRTP_NatDB_20240115-0.csv", header = T)

theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass="sf")
ggplot(data=world) +
  geom_point(data=indata, aes(x=Longitude, y=Latitude),
             size=0.5, shape=16, fill="red") +
  labs(title = "Observations of Deep-Sea Coral and Sponge Occurrences
          \n from the NOAA National Deep-Sea Coral and Sponge Database, 1842-Present",
       x = "Longitude", y="Latitude", caption = "Database v. 20240115-0")+
  geom_sf()

##### Creating lonlat file #####
lonlat = indata[49:48]
write.table(lonlat,
            file="C:/Users/arvind.shantharam/Documents/NGI DM/National Database/r_working/lonlat/0145037_lonlat.txt",
            sep="\t", col.names = F, row.names = F)






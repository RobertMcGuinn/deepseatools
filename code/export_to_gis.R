##### header #####
# author: Robert P. McGuinn
# date_started: 20191121
# purpose: transform point frame to spdf and feature-class
# input: x (dataframe) with coordinates
# output: x_geo (spdf) | x_geo (feature-class)

##### load packages #####
library(tidyverse)
library(rgdal)
library(arcgisbinding)
arc.check_product()

##### __OR__ import the individual data frame from disk #####

x <- "20201001-4_NOAA_OER_EX1811_Oceano_Profundo_2018_2018"
setwd("C:/rworking/deepseatools/indata")
x <- read.csv(paste(x,'.csv', sep = ''), header = T)

##### ***OR*** load current database(from Google Drive)#####
## set the file name (user supplied th file name root, without extension).
filename <- 'DSCRTP_NatDB_20220801-0_CSV' #
# find the file in google drive by name
x <- drive_find(q = paste("name contains ", "'", filename,".zip", "'", sep = ''))
## getting the id as a character string
y <- x$id
## download the zip file
dl <- drive_download(as_id(y), path = "C:/rworking/deepseatools/indata/file.zip", overwrite = TRUE)
## extract just the file of interest from the zip file
sub <- read.csv(unz(dl$local_path, paste(substr(filename, 1, 23), ".csv", sep = '')), fileEncoding = 'latin9')
flagged <- sub %>%  filter(Flag == "1")
## change all 'missing' values in factors to explicit NA's
# filt <- filt %>% mutate_if(is.factor,
#                       fct_explicit_na,
#                       na_level = "to_impute")

## cleanup
rm(dl)
rm(x)
rm(y)


##### __OR__ create x from sub #####

x <- sub


##### modify Lats/Longs in some way #####

x <- x %>%  mutate(Latitude = Latitude-1.169330553,
            Longitude = Longitude+2.110869447

)

##### filter data #####

# get rid of any missing Latitudes or Longitudes
x <- x %>% filter(Latitude != -999 | Longitude != -999)
# make copy to turn into spatial points data frame.
x_geo <- x

##### create spatial points data fram #####

coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### create feature-class #####

fgdb_path <- 'C:/rworking/sf/sf.gdb'
arc.write(file.path(fgdb_path, 'x_geo'), data=x_geo, overwrite = TRUE)


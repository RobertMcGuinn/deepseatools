##### Header #####
# author: Robert P. McGuinn
# started on: 20190127
# purpose: Working on the Report to Congress 2020

##### Getting old corals and sponges #####

setwd("C:/rworking/deepseatools/indata")
indata_old<-read.csv("DSCRTP_NatDB_20170807-1.csv", header = T)
filt_old <- indata_old %>%
  filter(Flag == "0")

##### Getting new corals and sponges #####

setwd("C:/rworking/deepseatools/indata")
indata_new<-read.csv("DSCRTP_NatDB_20191217-0.csv", header = T)
filt_new <- indata_new %>%
  filter(Flag == "0")

##### cleaning up #####
rm(indata_old)
rm(indata_new)

##### Create a not-in function #####
`%ni%` = Negate(`%in%`)

##### Find only new records since after FY17 #####
filt_new_only <- filt_new %>% filter(CatalogNumber %ni% filt_old$CatalogNumber)

##### creating just the xy's #####
old <- filt_old %>% dplyr::select(CatalogNumber, Latitude, Longitude)
new <- filt_new_only %>% dplyr::select(CatalogNumber, Latitude, Longitude)

##### install packages #####

library(arcgisbinding)
arc.check_product()

##### create spdf #####

old_geo <- old
coordinates(old_geo) <- c("Longitude", "Latitude")
proj4string(old_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

new_geo <- new
coordinates(new_geo) <- c("Longitude", "Latitude")
proj4string(new_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"


##### create feature-class #####

fgdb_path <- 'C:/data/aprx/RTC2020/RTC2020.gdb'
arc.write(file.path(fgdb_path, 'old_geo'), data=old_geo, overwrite = TRUE)

fgdb_path <- 'C:/data/aprx/RTC2020/RTC2020.gdb'
arc.write(file.path(fgdb_path, 'new_geo'), data=new_geo, overwrite = TRUE)

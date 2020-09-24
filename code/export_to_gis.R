##### header #####
# author: Robert P. McGuinn
# date_started: 20191121
# purpose: transform point fram to spdf and feature-class
# input: x (dataframe) with coordinates
# output: x_geo (spdf) | x_geo (feature-class)

##### load packages #####

library(arcgisbinding)
arc.check_product()

##### __OR__ import the data frame from disk #####

# x <- "20191112-1_HBOI_Walton_Smith_Cuba_Reed_Farrington_2017_2017"
# setwd("C:/rworking/deepseatools/indata")
# x <- read.csv(paste(x,'.csv', sep = ''), header = T)

##### __OR__ create x from sub #####

x <- sub %>% filter(Flag == 1, grepl("depth is questionable", FlagReason))

##### filter data #####

# x <- sub %>% filter(VernacularNameCategory == 'gorgonian coral')
x_geo <- x

##### create spdf #####

coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### create feature-class #####

fgdb_path <- 'C:/rworking/sf/sf.gdb'
arc.write(file.path(fgdb_path, 'x_geo'), data=x_geo, overwrite = TRUE)


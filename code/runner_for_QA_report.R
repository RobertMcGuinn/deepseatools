##### Header #####
##### Author: Robert McGuinn #####
##### Started: 20200921

##### packages #####
library(curl)
library(rmarkdown)
library(googledrive)

##### render the QA dashboard #####
# add the 'AccessionID' of the data set you want to report on as 'x'
filename <- "20210105-1_NOAA_GFNMS_CBNMS_SH-18-09_Graiff_2018_2018"

rmarkdown::render("C:/rworking/deepseatools/code/20210303_rmd_accession_qa_dashboard.rmd",
       output_file =  paste(filename,".docx", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')

###### manually inpect word document in folder, develop checklist, then PDF #####

##### Upload PDF report to specific folder on Google Drive #####
setwd("C:/rworking/deepseatools/reports")
folderurl <- "https://drive.google.com/drive/folders/153wK0nPTgQQgPZw1G-DOJoXHclbu8-_g"
drive_upload(paste(filename,".PDF", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".PDF", sep=''),
             overwrite = T)

##### checking #####
# filt %>% filter(grepl("Shimada", Vessel)) %>% pull(Vessel) %>% table()
# filt %>% filter(grepl("Greater Farallones", DataProvider)) %>% pull(DataProvider) %>% table()

##### ##export to GIS## #####
##### load packages #####
library(tidyverse)
library(rgdal)
library(arcgisbinding)
arc.check_product()

##### create x from sub #####
x <- sub

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


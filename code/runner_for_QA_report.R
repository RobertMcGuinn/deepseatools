##### Header #####
##### Author: Robert McGuinn #####
##### Started: 20200921

##### packages #####
library(curl)
library(rmarkdown)
library(googledrive)

##### render the QA dashboard #####
# add the 'AccessionID' of the data set you want to report on as 'x'
filename <- "20210105-2_NOAA_NCCOS_Nancy_Foster_NF-19-01_Carribean_2019_2019"

rmarkdown::render("C:/rworking/deepseatools/code/20210303_rmd_accession_qa_dashboard.rmd",
       output_file =  paste(filename,".docx", sep=''),
       output_dir = 'C:/rworking/deepseatools/reports')

###### MANUAL inspection of QA report in Word, then save to PDF. Develop Redmine Checklist #####

##### Upload PDF report to specific folder on Google Drive #####
folderurl <- "https://drive.google.com/drive/folders/1WUqpMZ2b8gtwrT0fjF9Ti_ajF1Oi3Pn9"
setwd("C:/rworking/deepseatools/reports")
drive_upload(paste(filename,".PDF", sep=''),
             path = as_id(folderurl),
             name = paste(filename,".PDF", sep=''),
             overwrite = T)

##### checking #####
# filt %>% filter(grepl("Shimada", Vessel)) %>% pull(Vessel) %>% table()
# filt %>% filter(grepl("Greater Farallones", DataProvider)) %>% pull(DataProvider) %>% table()
# filt %>% filter(grepl("National Centers for Coastal Ocean Science", DataProvider)) %>% pull(DataProvider) %>% table()

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

##### create spatial points data frame #####
coordinates(x_geo) <- c("Longitude", "Latitude")
proj4string(x_geo) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

##### create feature-class #####
fgdb_path <- 'C:/rworking/sf/sf.gdb'
arc.write(file.path(fgdb_path, 'x_geo'), data=x_geo, overwrite = TRUE)


##### Header #####
## author: Robert McGuinn
## started: 20221020
## purpose: google zip file extract

##### packages #####
library(bookdown)
library(tidyverse)
library(curl)
library(rmarkdown)
library(googledrive)
library(googlesheets4)

##### authorizations #####
drive_auth(email = "robert.mcguinn@noaa.gov")
gs4_auth(email = "robert.mcguinn@noaa.gov")

##### name the file #####
## MANUAL CHANGE: add the 'AccessionID' of the data set you want to report on as 'x'
filename <- "20220908-0_ROPOS_2019_Dive_2110"

##### find file in google drive by filename #####
x <- drive_find(q = paste("name contains ", "'", filename,".zip", "'", sep = ''))

##### getting the id as a character string #####
y <- x$id

##### download the zip file ######
dl <- drive_download(as_id(y), path = "C:/rworking/deepseatools/indata/file.zip", overwrite = TRUE)

##### extract csv inside zip to the dataframe #####
sub <- read.csv(unz(dl$local_path,
                    paste(filename,
                          ".csv",
                          sep = '')),
                fileEncoding="latin1")

##### check #####
table(sub$DatabaseVersion)
table(sub$DatasetID)
table(sub$Flag)

##### OPTIONAL: find another file to row bind to this one
##### name another file #####
## MANUAL CHANGE: add the 'AccessionID' of the data set you want to report on as 'x'
filename <- "20220907-3_ROPOS_2019_Dive_2109"

##### find file in google drive by filename #####
x <- drive_find(q = paste("name contains ", "'", filename,".zip", "'", sep = ''))

##### getting the id as a character string #####
y <- x$id

##### download the zip file ######
dl <- drive_download(as_id(y), path = "C:/rworking/deepseatools/indata/file.zip", overwrite = TRUE)

##### extract csv inside zip to the dataframe #####
sub2 <- read.csv(unz(dl$local_path,
                    paste(filename,
                          ".csv",
                          sep = '')),
                fileEncoding="latin1")

##### check #####
table(sub2$DatabaseVersion)
table(sub2$DatasetID)
table(sub2$Flag)

##### merging two files ######
sub3 <- rbind(sub,sub2)

##### write out the merged file #####
write.csv(sub3,
          "c:/rworking/deepseatools/indata/20221020-0_NOAA_HB_19_03_pt_2.csv"
)


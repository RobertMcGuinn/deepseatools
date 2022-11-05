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

##### manual input: load latest version of NDB #####
setwd("C:/rworking/deepseatools/indata")
indata <- read.csv("DSCRTP_NatDB_20220801-0.csv", header = T)
filt <- indata %>%
  filter(Flag == "0", is.na(Phylum) == F)

##### manual input: name the file #####
## MANUAL CHANGE: add the 'AccessionID' of the data set you want to report on as 'x'
filename <- "20221031-0_NOAA_EX1304_Northeast_US_SBingo_2013"

##### ***** section below performs the operation with a zip drive housed on google drive containing a CSV ***** #####

##### find zip file in google drive by filename #####
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

##### ***** section below does it with a text file rather that a zip file ***** #####

##### find file in google drive by filename #####
x <- drive_find(q = paste("name contains ", "'", filename, "'", sep = ''))

##### getting the id as a character string #####
y <- x$id

##### download the text file from Google Drive ######
dl <- drive_download(as_id(y), path = "C:/rworking/deepseatools/indata/file.txt", overwrite = TRUE)

##### extract txt file to the dataframe #####
sub <- read.delim(dl$local_path, sep = "\t", header = T)

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

x <- filt %>%
  filter(grepl("OCNMS", SurveyID)) %>%
  group_by(DatasetID, SurveyID, Vessel, ObservationYear, PI, DataProvider) %>%
  summarize(n=n())

write.csv(x,
          "c:/rworking/deepseatools/indata/NDB_subset_version_20220801-0_surveyID_contains_OCNMS.csv")

##### merging two files ######
sub3 <- rbind(sub,sub2)

##### write out the merged file #####
write.csv(sub3,
          "c:/rworking/deepseatools/indata/20221020-0_NOAA_HB_19_03_pt_2.csv"
)


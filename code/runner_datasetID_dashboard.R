##### Header #####
# Author: Robert P. McGuinn
# Date started: 2018-08-14
# Purpose: Execute RMarkdown documents on dashboards for each DatasetID.
#   4 groups: Cruise, Literature, Program, Repository, and then Repository-MBARI
#(because it is so huge it can't have an interactive map)

##### install packages #####
# install.packages('xlsx')
#install.packages('openxlsx')
library(openxlsx)
library(sp)
library(tidyverse)
library(rerddap)
#install.packages('leaflet')
library(leaflet)
# install.packages('extrafont')
library(extrafont)
# install.packages('RColorBrewer')
library(RColorBrewer)
# install.packages('googlesheets')
library(googlesheets)
# install.packages('googledrive')
library(googledrive)
library(rmarkdown)
library(knitr)
#install.packages("maps")
library(maps)
#install.packages("rgdal")
library(rgdal)
#install('raster')
library(raster)
#install.packages("spocc")
library(spocc)
#install.packages('arcgisbinding')
# library(arcgisbinding)
# arc.check_product()
#install.packages('refinr')
library(refinr)
# install.packages('marmap')
library(marmap) #yo
#install.packages('prettydoc')
library(prettydoc)
#install.packages('robis')
library(robis)
#install.packages('devtools')
library(devtools)
library(httr)
library(jsonlite)

##### set an option #####
options(lifecycle_disable_warnings = TRUE)

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

##### ***OR*** read current database from disk #####
sub <- read.csv("C:/rworking/deepseatools/indata/DSCRTP_NatDB_20220801-0.csv", fileEncoding = "latin9")
flagged <- sub %>%  filter(Flag == "1")

##### change name of 'sub' to 'indata' and create 'filt' #####
indata <- sub
filt <- indata %>% filter(Flag == 0)

## cleanup
rm(sub)

##### define database version (this variable called in RMD files) #####
version <- unique(filt$DatabaseVersion)
version <- as.character(version)

##### ***OR*** bringing in datasetID key from xls stored on Google Drive ####
## create a list of files (or single file) that meets title query (Manual change)
x <- drive_find(q = "name contains '20220801-1_DatasetID_Key_DSCRTP'") #

## browse to it
# x %>% drive_browse()

# getting the id as a character string
y <- x$id

# this downloads the file to the specified path (manual change required)
dl <- drive_download(as_id(y),
                     path = "C:/rworking/deepseatools/indata/20220801-1_DatasetID_Key_DSCRTP.xlsx",
                     overwrite = TRUE)

## read the file into R as a data frame
key <- read.xlsx(dl$local_path)

## clean up
rm(y)
rm(x)
##### ***OR*** bring in old datasetID key from local path #####
# key <- read.xlsx("C:/rworking/deepseatools/indata/20211207-1_DatasetID_Key_DSCRTP.xlsx")

## checking
# oldkey <- key
# setdiff(oldkey$DatasetID, key$DatasetID)
# setdiff(key$DatasetID, oldkey$DatasetID)

##### checking #####
# setdiff(filt$DatasetID, key$DatasetID)
# setdiff(key$DatasetID, filt$DatasetID)
#
# x <- setdiff(filt$DatasetID, key$DatasetID)
# x <- filt %>% filter(DatasetID %in% x) %>%
#   group_by(ObservationDate, DatasetID, RecordType, SamplingEquipment, VehicleName, DataProvider, Repository, Locality, SurveyComments, Citation, Vessel, WebSite) %>%
#   summarize(n=n())
# View(x)
#
# filt %>% filter(grepl("NOAA", DatasetID)) %>% pull(DatasetID) %>% table()
#
# key %>%
#   filter(DatasetID == "NOAA_PC-11-05-L1") %>%
#   pull(abstract)
#
# x <- setdiff(filt$DatasetID, key$DatasetID)
# x <- filt %>% filter(DatasetID %in% x) %>%
#   group_by(DatasetID) %>%
#   summarize(n=n())
# View(x)
#
# x <- "NOAA_CINMS_SW-19-06"
# x <- filt %>% filter(DatasetID == x) %>% pull(Locality) %>% unique()
# View(x)
#
# x <- "Nancy Foster"
# x <- filt %>% filter(grepl(x,Vessel)) %>%
#   group_by(Vessel, SurveyID, ObservationYear, DatasetID) %>%
#   summarize(n=n())
# View(x)
#
# id <- "BOEM_Mid-Atlantic_Canyons"
# x <- paste("https://deepseacoraldata.noaa.gov/Dataset%20Summaries/", id, ".html", sep = '')
# browseURL(x)
#
#
#
# View(x)
#
#
# rm(x)
#
# setdiff(indata$DatasetID, key$DatasetID)
# setdiff(key$DatasetID, indata$DatasetID)

##### MANUAL STEP: go update the DatasetID key with the new values #####
# setdiff(filt$DatasetID, key$DatasetID)

##### checking #####
# x <- filt %>%
#   filter(DatasetID == "NOAA_PC-16-05") %>%
#   group_by(Vessel, VehicleName, WebSite) %>%
#   summarize(n=n())
#
# x
#
# x <- filt %>%
#   filter(DatasetID == "NOAA_PC-16-05") %>% pull(Citation) %>% unique()
#
# x

##### write the citation information #####

filt$CitationMaker <- paste(filt$DataProvider,'. ',
                            filt$ObservationYear,
                            ' to ',
                            filt$ObservationYear,
                            '. ',
                            'Coral or sponge occurrence observations submitted to the NOAA National Database for Deep Sea Corals (www.deepseacoraldata.noaa.gov)', '. ',
                            'DSCRTP Dataset ID: ', filt$DatasetID, '. ',
                            'DSCRTP Accession ID: ',filt$AccessionID, '. ',
                            'Record type: ', filt$RecordType, '. ',
                            'Vessel(s): ', filt$Vessel,'. ',
                            'Sampling vehicle: ', filt$VehicleName,'. ',
                            'Survey ID: ', filt$SurveyID,'. ',
                            'Principle investigator: ', filt$PI,'. ',
                            'Data contact: ', filt$DataContact,'. ',
                            'Reporter: ', filt$Reporter,'. ',
                            'Repository: ', filt$Repository,'. ',
                            'Web site [last accessed on YYYY-MM-DD]: ', filt$WebSite,'.',
                            sep = '')

##### checking: looking at the Citation #####

# # [1] "NOAA_HB-14-02" "NOAA_HB-17-04"
# # [3] "OET_NA122"     "OET_NA116"
# # [5] "OET_NA077"     "OET_NA110"
#
# x <- "OET_NA114"
#
#
# filt %>% filter(DatasetID == x) %>%
#   pull(CitationMaker) %>%
#   unique()
#
# filt %>% filter(DatasetID == x) %>%
#   pull(Citation) %>%
#   unique()
#
# filt %>%
#   filter(DatasetID == x) %>%
#   pull(ObservationDate) %>% unique()
#
# filt %>%
#   filter(DatasetID == x) %>%
#   pull(WebSite) %>% unique()
#
# filt %>%
#   filter(DatasetID == x) %>%
#   pull(Locality) %>% unique()

##### ***OR*** bring in new MANUALLY UPDATED datasetID key from local path #####
# key <- read.xlsx("C:/rworking/deepseatools/indata/20220426-1_DatasetID_Key_DSCRTP.xlsx")

##### ***OR*** bring in new MANUALLY UPDATED datasetID key from Google Drive #####
# ## create a list of files (or single file) that meets title query (Manual change)
#
# x <- drive_find(q = "name contains '20220801-1_DatasetID_Key_DSCRTP'") #
#
# ## browse to it
# # x %>% drive_browse()
#
# # getting the id as a character string
# y <- x$id
#
# # this downloads the file to the specified path (manual change required)
# dl <- drive_download(as_id(y),
#                      path = "C:/rworking/deepseatools/indata/20220426-0_DatasetID_Key_DSCRTP.xlsx",
#                      overwrite = TRUE)
#
# ## read the file into R as a data frame
# key <- read.xlsx(dl$local_path)


##### ***OPTIONAL*** updating DatasetID key with new 'n' #####
# ## build a frequency table by DatasetID from new file
# x <- filt %>% group_by(DatasetID) %>% summarize(n=n())
#
# # strip 'n' from existing key
# names(key)
# y <- key[,1:8]
# names(y)
#
# ## merge new numbers to create old key + new counts
# z <- merge(y,x)
#
# ## write out result
# write.xlsx(z, "C:/rworking/deepseatools/indata/20220801-0_DatasetID_Key_DSCRTP.xlsx",
#            overwrite = TRUE)

##### manual upload new key to Google Drive #####
##### ***OPTIONAL*** subsetting of indata to d (optional step for testing purposes) #####
# d <- filt %>%
#   filter(
#     DatasetID == 'NOAA_HB-19-03' |
#     DatasetID == 'OET_NA114' #|
#     # DatasetID == 'NOAA_AFSC_Longline_Survey' #Program
#   )

##### ***OR*** full run set 'filt' to d #####
d <- filt

##### cleanup #####
rm(indata)
rm(filt_old)
rm(indata_old)
rm(x)
rm(z)
rm(y)

##### checking #####
# table(unique(factor(d$DataProvider)))
# table(unique(factor(d$RecordType)))
# table(unique(factor(d$DataContact)))
# table(factor(d$DatasetID), useNA = 'always')
#
# yo <- key %>%
#   # filter(class == 'Cruise') %>%
#   group_by(class) %>%
#   summarise(DatasetID = paste(unique(DatasetID), collapse = " | "),
#             n=n())
# View(yo)

#### checking: summary view of new datasets #####
# x <- d %>%
#   arrange(ObservationYear) %>%
#   filter(DatasetID %in% setdiff(d$DatasetID, key$DatasetID)) %>%
#   group_by(DatasetID) %>%
#   summarize(
#       ObservationYear_list = toString(unique(ObservationYear)),
#       ObservationDate_list = toString(unique(ObservationDate)),
#       Locality_list = toString(unique(Locality)),
#       RecordType_list = toString(unique(RecordType)),
#       N_Records=n(),
#       # DatasetID_list = toString(unique(DatasetID)),
#       DataProvider_list = toString(unique(DataProvider)),
#       Repository_list = toString(unique(Repository)),
#
#       Vessel_list = toString(unique(Vessel)),
#       VehicleName_list = toString(unique(VehicleName)),
#       WebSite_list = toString(unique(WebSite)),
#       #ImageURL = toString(unique(ImageURL)),
#       PI_list = toString(unique(PI)),
#       PIAffiliation_list = toString(unique(PIAffiliation)),
#       Citation_list = toString(unique(Citation)),
#       DataContact_list = toString(unique(DataContact)),
#       Reporter_list = toString(unique(Reporter)),
#       SurveyID_list = toString(unique(SurveyID))
#     )
#
# View(x)

##### ***OPTIONAL*** Fixing DatasetID Problems (optional) #####
# d <- d %>% mutate(DatasetID =
#                     ifelse(DatasetID == "MCZ_IZ_1968_1880",
#                            'MCZ_IZ',
#                            as.character(DatasetID)))
# d <- d %>% mutate(DatasetID =
#                     ifelse(DatasetID == "NMNH-IZ",
#                            'NMNH_IZ',
#                            as.character(DatasetID)))
#
# d <- d %>% mutate(DatasetID =
#                     ifelse(DatasetID == "CT-13-07",
#                            'NOAA_CT-13-07',
#                            as.character(DatasetID)))

##### checking #####
setdiff(d$DatasetID, key$DatasetID)
setdiff(key$DatasetID, d$DatasetID)
##### merge database with key  #####
d <- merge(d, key, all.x = TRUE)

##### check #####
x <- d %>% filter(DatasetID == "Carranza_etal_2012")
write.csv(test, "c:/rworking/deepseatools/indata/carranza.csv")
test2 <- read_utf8("c:/rworking/deepseatools/indata/carranza.csv")

d %>% filter(CatalogNumber == '618051') %>% pull(Citation)


# table(d$class, useNA = 'always')
#
# x <- d %>%
#   group_by(class) %>%
#   summarize(DatasetsbyClass = length(unique(DatasetID)),
#     n = n())
# View(x)

# setdiff(d$DatasetID, key$DatasetID)
# setdiff(key$DatasetID, d$DatasetID)
# #
# x <- d #%>%
#   # filter(
#   #   is.na(class) == T,
#   #   #DataProvider == "Temple University",
#   #   #class == "Literature"
#   #   )
#
# table(factor(x$class), useNA = 'always')
#
# library(stringr)
# test <- stringr::str_conv(d, "UTF-8")

##### *** run the reports *** #####
##### _create the folders for each type of report #####

dir.create('C:/rworking/deepseatools/reports/datasetid/cruise')
dir.create('C:/rworking/deepseatools/reports/datasetid/literature')
dir.create('C:/rworking/deepseatools/reports/datasetid/program')
dir.create('C:/rworking/deepseatools/reports/datasetid/repository')

##### _cruise #####
#cruise subset
yo <- d %>%
  filter(
    class == 'Cruise'
  )

# run RMD
library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_cruise.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/cruise')
}

##### _literature ######
yo <- d %>%
  filter(
    class == 'Literature'
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_literature.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/literature')
}

##### _program #####
yo <- d %>%
  filter(
    class == 'Program'
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_program.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/program')
}

##### _repository #####
yo <- d %>%
  filter(
    class == 'Repository',
    DatasetID != 'MBARI'
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_repository.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/repository')
}

##### _repository:MBARI #####
yo <- d %>%
  filter(
    DatasetID == 'MBARI'
  )

library("rmarkdown")
for (id in unique(yo$DatasetID)){
  sub <- yo[yo$DatasetID == id,]
  render("C:/rworking/deepseatools/code/rmd_datasetid_repository_MBARI.rmd" ,
         output_file =  paste(id,".html", sep=''),
         output_dir = 'C:/rworking/deepseatools/reports/datasetid/repository')
}

##### _checking #####
cruise <- list.files('C:/rworking/deepseatools/reports/datasetid/cruise/')
literature <- list.files('C:/rworking/deepseatools/reports/datasetid/literature/')
program <- list.files('C:/rworking/deepseatools/reports/datasetid/program/')
repository <- list.files('C:/rworking/deepseatools/reports/datasetid/repository/')

length(cruise)
length(literature)
length(program)
length(repository)

biglist <- c(cruise, literature, program, repository)
biglist <- gsub(pattern = "\\.html$", "", biglist)

setdiff(biglist, unique(filt$DatasetID))
length(unique(filt$DatasetID))
length(biglist)


##### publish a list of datasetID's for review sheet #####
x <- d %>% group_by(DatasetID, class) %>%
  summarize(n = n()) %>%
  arrange(class, DatasetID)

View(x)

##### write CSV of x #####
setwd("C:/rworking/deepseatools/indata")
x %>%
  write.csv(paste("20190718-0_DatasetID_Dashboard_Review",".csv", sep = ''), row.names = FALSE)

##### updating DatasetID key with new 'n' #####

# build a frequency table by DatasetID
x <- filt %>% group_by(DatasetID) %>% summarize(n=n())
names(key)

# strip n from fieds
y <- key[,1:8]
names(y)

# merge new numbers
z <- merge(y,x)
names(z)

# write out result
write.xlsx(z, "C:/rworking/deepseatools/indata/20191217-0_DatasetID_Key_DSCRTP.xlsx",
overwrite = TRUE)


##### write a subset of the NDB for testing purposes
## filter
export <- sub %>% filter(Flag == "0",
                         FishCouncilRegion == "New England")
## write
setwd("C:/rworking/deepseatools/indata")
export %>%
  write.csv(paste("yo",".csv", sep = ''), row.names = FALSE)

## checking
table(sub$Flag)
filt <- sub %>% filter(Flag == "0")
filt %>% filter(CatalogNumber == "618055") %>% pull(Latitude)





##### check #####
filt %>%
  pull(DatasetID) %>% unique()

filt %>%
  filter(EndLatitude != "-999") %>%
  pull(DatasetID) %>% table()


sub %>%
  filter(CatalogNumber == "912379") %>%
  dplyr::select(Latitude, Longitude)
